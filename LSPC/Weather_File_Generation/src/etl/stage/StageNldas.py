from pathlib import Path
from typing import (List, Tuple, Any)
from dataclasses import dataclass
import pandas as pd
from datetime import datetime
import time
import xarray as xr
import logging


# Package imports
from src.core.models import (ProjectControl, DataRequest)
from ..base import DataStager
from ..util.helpers import (XarrayHelpers, SpatialHelpers, DatetimeHelpers, time_block)
from ..util.StageNldasHelpers import (NldasSpatialSubsetter, NldasVariableTransformer)

@dataclass(frozen=True)
class StageNldasRequest(DataRequest):
    nldas_ids: List[int]
    input_dir: Path
class StageNldasValidators:
    pass

class StageNldasHelpers:
    
    def __init__(self):
        self.xr = XarrayHelpers()
        self.dt = DatetimeHelpers()
        self.sp = SpatialHelpers()
        self.transformer = NldasVariableTransformer()
        self.subsetter = NldasSpatialSubsetter()        

class StageNldas(DataStager):

    def __init__(self):
        self._validators = StageNldasValidators()
        self._helpers = StageNldasHelpers()
        
        # Setup logging
        self._logger = logging.getLogger(__name__)
        self._logger.setLevel(logging.INFO)
        Path("logs").mkdir(exist_ok=True)
        logfile = Path("logs/StageNldas.log")
        self._file_handler = logging.FileHandler(logfile)
        formatter = logging.Formatter(
            "%(asctime)s | %(levelname)s | %(message)s",
            "%Y-%m-%d %H:%M:%S"
        )
        self._file_handler.setFormatter(formatter)
        self._logger.addHandler(self._file_handler)
        
        

    def create_stage_requests(self, project: ProjectControl) -> StageNldasRequest:
        """Generates a stage nldas data request from project control.
            
            **NOTE** Assumes all user-input is provided in Pacific TZ.
        """
        # Extract necessary data from Project Control
        request_control = project.request_control
        storage = project.storage
        nldas_ids = project.nldas.data['nldas_id'].tolist()
        helpers = self._helpers

        # Create request
        start_datetime = helpers.dt.date_to_datetime(request_control.start_date)
        start_datetime = helpers.dt.month_edge(start_datetime)
        end_datetime = helpers.dt.date_to_datetime(request_control.end_date, eod=True)
        end_datetime = helpers.dt.month_edge(end_datetime, end=True)

        return StageNldasRequest(
            start_datetime=start_datetime,
            end_datetime=end_datetime,
            overwrite=request_control.overwrite,
            output_dir=storage.nldas.staged,
            input_dir=storage.nldas.raw,
            nldas_ids=nldas_ids
        )

        
    def create_netcdf_filelist(self, request: StageNldasRequest) -> List[Path]:
        """
        1. Queries raw nldas storage for monthly netcdfs in the datetime range
            requested in the `StageNldasRequest` and creates a list of corresponding
            filepaths in chronological order.
        2. Validates the list of netcdf file paths by confirming all months exist in the
            requested date range in `StageNldasRequest`. If any months are missing a user 
            exception is thrown.

        **Assumptions**:
            - Netcdf files are in format 'nldas_<start_datetime>_<end_datetime>.nc'
                where <start_datetime> & <end_datetime> have format `%Y-%m-%dT%H--%M--%S`
        """
        
        # Create chronological list of all netcdf files
        filepaths = list(request.input_dir.glob("*.nc"))
        date_format = '%Y-%m-%dT%H--%M--%S'
        start_datetimes = []
        for f in filepaths:
            file_stem_comps = f.stem.split("_")
            start_datetime = datetime.strptime(file_stem_comps[1], date_format)
            # end_datetime = datetime.strptime(file_stem_comps[2], date_format)
            start_datetimes.append(start_datetime)
        filelist = pd.Series(filepaths, index=pd.DatetimeIndex(start_datetimes), name="path").sort_index()
        # filelist = sorted(filelist, key=lambda r: r[0]) # sort files by date

        # Create complete datetime range based on request start and end datetime
        requested_start_datetimes = pd.date_range(start=request.start_datetime, end=request.end_datetime, freq="MS")

        # Locate requested_start_datetimes missing from 
        missing_start_datetimes = requested_start_datetimes.difference(
            pd.DatetimeIndex(filelist.index)
        )

        if not missing_start_datetimes.empty:
            raise ValueError('During StageNldas, the following months of nldas data requested by the user are missing ' +\
                f'in raw nldas storage:\n{[dt for dt in missing_start_datetimes]}')
        
        return filelist[requested_start_datetimes].values.tolist()

    
    def stage(self, project: ProjectControl) -> None:
        """Stage NLDAS data based on project control settings."""
        
        log = self._logger
        ctx = {"project": project.request_control.project_name}

        helpers = self._helpers
        subsetter = self._helpers.subsetter
        transformer = self._helpers.transformer
        
        # Implementation for staging NLDAS data goes here
        print(f'\tStaging NLDAS data for project: {project.request_control.project_name} | {self._helpers.dt.now()}\n')

        # Create stage nldas request object
        with time_block(log, "Staging NLDAS data for project", extra=ctx):
            nldas_request = self.create_stage_requests(project=project)
        
        # create chronological list of raw monthly nldas netcdf files and verify all months in user requested date range exist
        with time_block(log, "Create netcdf filelist", extra=ctx):
            raw_nldas_files = self.create_netcdf_filelist(request=nldas_request)

        # Create subset preprocess
        # subsetter.extract_point_position_indexes(raw_nldas_files, nldas_ids=nldas_request.nldas_ids)
        subset_preprocess = subsetter.create_subset_preprocess(
            nldas_netcdf_file=raw_nldas_files[0],
            nldas_ids=nldas_request.nldas_ids)

        ds = None
        try:
            with time_block(log, "Load netcdf files as dataset", extra=ctx):                
                
                ds = xr.open_mfdataset(
                    raw_nldas_files,
                    preprocess=subset_preprocess,
                    combine="nested",
                    concat_dim="time",
                    join='exact',
                    coords='minimal',
                    data_vars="all",
                    compat='equals',                    
                    parallel=False,
                    decode_cf=True,
                    # chunks={'time': 744} # ~744 hours per month
                )

                ds.load()
            
            # variables = ["Tair","Qair","PSurf","Wind_E","Wind_N","Rainf","SWdown"]
            # ds = ds[variables] # Extract only necessary variables
            
            # Transformations            
            # ds = transformer.convert.potevap_mm_to_in(ds)
            with time_block(log, "Convert Rainf from [km/mg^2] to [in]", extra=ctx):
                ds = transformer.convert.rainf_kgm2_to_in(ds)
            
            with time_block(log, "Convert SWdown from [W/m^2] to [ly]", extra=ctx):
                ds = transformer.convert.swdown_watts_to_ly(ds)
            
            with time_block(log, "Calculate windspeed from Wind_E and Wind_N", extra=ctx):
                ds = transformer.calculate.windspeed(ds)
            
            with time_block(log, "Calculate Dew Point Temperature", extra=ctx):
                ds = transformer.calculate.tdpt(ds)
            
            with time_block(log, "Convert Tair from [K] to [F]", extra=ctx):
                ds = transformer.convert.tair_k_to_f(ds)
            
            with time_block(log, "Setting negatives to nan", extra=ctx):
                # NOTE: After fixing nldas_id grid, set check to see that interpolation results in no nan values for all var                
                ds = helpers.xr.set_negative_to_nan(ds) # This should be done regardless of whether negatives already exist
            
            with time_block(log, "Filling nan with linear interpolation along time dim", extra=ctx):
                # NOTE: After fixing nldas_id grid, set check to see that interpolation results in no nan values for all var
                if helpers.xr.nans_exist_in_dataset(ds, exclude_vars=['nldas_id']):
                    ds = helpers.xr.fill_linear_interp_along_time(ds)
                
            with time_block(log, "Filling nan with long-term hour of year average along time dim", extra=ctx):
                # NOTE: After fixing nldas_id grid, set check to see that interpolation results in no nan values for all var
                if helpers.xr.nans_exist_in_dataset(ds, exclude_vars=['nldas_id']):
                    ds = helpers.xr.fill_avg_along_time(ds, hourly=True)
                
            with time_block(log, "Filling nan with nearest along time dim", extra=ctx):
                # NOTE: After fixing nldas_id grid, set check to see that interpolation results in no nan values for all var
                if helpers.xr.nans_exist_in_dataset(ds, exclude_vars=['nldas_id']):
                    ds = helpers.xr.fill_nearest_along_time(ds)
                            
            if helpers.xr.nans_exist_in_dataset(ds, exclude_vars=['nldas_id']): # This only happens if there is a  grid with all missing data (ocean grid for example) and needs to be caught earlier
                raise ValueError(f'Insufficient Nldas data in requested data range prevents interpolation of some missing values.')

            with time_block(log, "Extracting grids and writing to csv", extra=ctx):
                vars = ['Rainf', 'Tair','Tdpt','SWdown','Wind'] # TODO: Set this based on snow flag
                filename_date_format = "%Y-%m-%dT%H--%M--%S"
                infile_date_format = "%m/%d/%Y %H:%M:%S"
                
                start_datetime = datetime.strftime(
                    nldas_request.start_datetime,
                    format=filename_date_format,
                )
                end_datetime = datetime.strftime(
                    nldas_request.end_datetime,
                    format=filename_date_format
                )

                for i in ds['point'].values:
                    with time_block(log, f"\tGrid {i} ...", extra=ctx):
                        nldas_id = int(ds['nldas_id'].isel(point=i).values)
                        # print(f'nldas_id {i}: {nldas_id} ...')
                        pt = ds.isel(point=i).to_dataframe().reset_index()
                        pt = pt.set_index(["nldas_id","local_time"])
                        pt = pt[vars].round(5)

                        # Remove existing file corresponding to nldas id if it exists in staged storage
                        for file in nldas_request.output_dir.glob(f'nldas_{nldas_id}_*.csv'):
                            file.unlink() # Remove existing file for this nldas_id if it exists before writing new file

                        outfilename = f'nldas_{nldas_id}_{start_datetime}_{end_datetime}.csv'
                        outfilepath = nldas_request.output_dir / outfilename
                        pt.to_csv(outfilepath, date_format=infile_date_format)

        except Exception as e:
            print(f'Dataset processing failed: {e}')
        finally:
            if ds is not None:
                ds.close()