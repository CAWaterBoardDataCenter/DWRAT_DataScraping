from pathlib import Path
from typing import (Union, List, Tuple, Any, cast)
from dataclasses import dataclass
import pandas as pd
import numpy as np
from datetime import datetime
import time
import xarray as xr
import logging

# Package imports
from src.core.models import (ProjectControl, DataRequest)
from ..base import DataStager
from ..util.helpers import (XarrayHelpers, SpatialHelpers, DatetimeHelpers, time_block)
from ..util.StageCimisHelpers import (CimisSpatialSubsetter, CimisVariableTransformer)

# TODO: Build out peek_staged_nldas_file

@dataclass(frozen=True)
class StageCimisPreprocessRequest(DataRequest):
    raw_cimis_dir: Path
    fill_linear_interp: bool
    cimis_ids: List[int] # all cimis_ids requested

@dataclass(frozen=True)
class StageCimisRequest(DataRequest):
    nldas_id: int
    cimis_ids: List[int] # cimis_ids requested linked nldas_id
    raw_cimis_dir: Path
    staged_nldas_file: Path

class StageCimisValidators:
    pass

class StageCimisHelpers:

    def __init__(self):
        self.xr = XarrayHelpers()
        self.dt = DatetimeHelpers()
        self.sp = SpatialHelpers()
        self.subsetter = CimisSpatialSubsetter()
        self.transformer = CimisVariableTransformer()

class StageCimis(DataStager):

    def __init__(self):
        self._validators = StageCimisValidators()
        self._helpers = StageCimisHelpers()


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

    def create_stage_requests(self, project: ProjectControl) -> List[StageCimisRequest]:
        """Generates a stage nldas data request from project control.
            
            **NOTE** Assumes all user-input is provided in Pacific TZ.
    
        - Extract request info from Project control (start & end dates, etc)
        - Load airmap table that links cimis_id -> nldas_id
            - group by nldas_id
        - Create a list of StageCimisRequests by looping through nldas_id groups
            - **Peek** into staged nldas storage to validate that data is available for
                requested date range.
            - Each request will have 1 nldas_id and a list of cimis_ids
        
        """
        helpers = self._helpers

        # Get start and end datetimes
        start_dttm = helpers.dt\
            .date_to_datetime(project.request_control.start_date)
        
        end_dttm = helpers.dt\
            .date_to_datetime(project.request_control.end_date)
        
        # Extract airfile mapping table (links cimis_ids -> nldas_ids)
        airmap = project.airMap.data

        # Remove duplicate cimis_id values linked to different prism_id (if any exist) # TODO: Add validation that ensures all duplicate cimis_id's have same nldas_id
        airmap = airmap.drop_duplicates(subset=['cimis_id'])

        # Create list of staged nldas cvs
        staged_nldas_files = list(project.storage.nldas.staged.glob("*csv"))

        # Loop through nldas_id groups and create stage cimis requests
        missing_nldas_files: List[str] = []
        requests: List[StageCimisRequest] = []
        for key, grp in airmap.groupby('nldas_id'):
            assert not isinstance(key, tuple)

            nldas_id: int = int(cast(int,key)) # NOTE: Hacky convertsion - not sure if key is np.int64 or int but want to enfore int

            # Peek into raw nldas storage to see if file exists for requested nldas_id and check
                # that necessary date exists too    
            _gen = (f for f in staged_nldas_files if str(nldas_id) in f.name)
            nldas_file = next(_gen, None)

            if not nldas_file:
                missing_nldas_files.append(str(nldas_id))
                continue
                
            self.peek_staged_nldas_file(
                staged_nldas_file=nldas_file,
                datetime_range=(start_dttm,end_dttm)
            )

            requests.append(
                StageCimisRequest(
                    start_datetime=start_dttm,
                    end_datetime=end_dttm,
                    overwrite=project.request_control.overwrite,
                    output_dir=project.storage.cimis.staged,
                    raw_cimis_dir=project.storage.cimis.raw,
                    nldas_id=nldas_id,
                    staged_nldas_file=nldas_file,
                    cimis_ids=grp['cimis_id'].tolist()
                )
            )

        if missing_nldas_files:
            raise ValueError(f'Staged NLDAS storage is missing monthly files for the following requested`nldas_ids`: {missing_nldas_files}')

        return requests


    def create_stage_preprocess_request(self, project: ProjectControl) -> StageCimisPreprocessRequest:
        """
        - Extracts user input data from `ProjectControl` and creates `StageCimisPreprocessRequest`
            used to inform stage preprocessing step.
        - Peeks into raw_nldas storage to see if all monthly files needed exists
            for what is being requested
        """
        helpers = self._helpers

        # Get start and end datetimes
        start_dtm = helpers.dt\
            .date_to_datetime(project.request_control.start_date)
        
        end_dtm = helpers.dt\
            .date_to_datetime(project.request_control.end_date)
        
        # Peek into raw cimis storage to see if file exist for requested date range
        self.peek_raw_cimis_storage(
            raw_storage_dir=project.storage.cimis.raw,
            datetime_range=(start_dtm, end_dtm)
        )

        return StageCimisPreprocessRequest(
            start_datetime=start_dtm,
            end_datetime=end_dtm,
            overwrite=project.request_control.overwrite,
            output_dir=project.storage.cimis.staged,
            raw_cimis_dir=project.storage.cimis.raw,
            fill_linear_interp=False,
            cimis_ids=project.cimis.data['cimis_id'].tolist()
        )


    def stage(self, project) -> None:
        """Stage CIMIS data based on project control settings.
        
        Does the following:
            1. Extracts user request data from project and creates 
                list of stage cimis data requests.
            2. Performs stage cimis preprocessing to create singular CIMIS
                xarray dataset with requested CIMIS grids.
            3. Loops through cimis data request list and executes staging
                process. 
        """
        # Implementation for staging CIMIS data goes here
        print(f"\tStaging CIMIS data for project: {project.request_control.project_name}")

        log = self._logger
        ctx = {"project": project.request_control.project_name}
        helpers = self._helpers

        # Create preprocessing request
        preprocess_request = self.create_stage_preprocess_request(project=project)
        
        # Create StageNLDAS requests
        requests = self.create_stage_requests(project=project)
        
        # Execute Stage Preprocessing
        cimis_dataset = self.execute_stage_preprocess_request(preprocess_request=preprocess_request)

        # Execute Staging

        # Pad time dimensions then Interpolate/fill missing data with long-term doy average
        xrhelper = self._helpers.xr
        
        cimis_dataset = xrhelper.pad_time(
            cimis_dataset,
            start_datetime=preprocess_request.start_datetime,
            end_datetime=preprocess_request.end_datetime,
            time_scale='1d') # Pad/Reindex to acheive requested date range even if nan's introduced
        
        
        ### Filling no-data ### # TODO: Discuss order (doy b4 linear or visa-versa)
        
        # Set negative values to nan
        cimis_dataset = xrhelper.set_negative_to_nan(cimis_dataset)

        # Linaer interpolation with max window size of 30 days
        if xrhelper.nans_exist(cimis_dataset['Eto']):
            cimis_dataset = xrhelper.fill_linear_interp_along_time(cimis_dataset, pd.Timedelta('30D'))
        
        # Day of year average
        if xrhelper.nans_exist(cimis_dataset['Eto']):
            cimis_dataset = xrhelper.fill_avg_along_time(cimis_dataset, hourly=False)

        # Forward filling and backfilling (use value of nearest time point)
        if xrhelper.nans_exist(cimis_dataset['Eto']):
            cimis_dataset = xrhelper.fill_nearest_along_time(cimis_dataset)

        # recheck nans all remove
        if xrhelper.nans_exist(cimis_dataset['Eto']):
            nans = np.isnan(cimis_dataset['Eto'])
            cimis_ids_with_nan = cimis_dataset['Eto'].where(nans, drop=True)['cimis_id'].values.tolist()
            raise ValueError(f'Insufficient Cimis data in requested data range prevents interpolation of some missing values for cimis_ids: {cimis_ids_with_nan}')
        
        # Perform unified transforms (unit conversions)
        transformer = self._helpers.transformer
        cimis_dataset = transformer.convert.eto_mm_to_in(cimis_dataset) # unit conversion
        
        for request in requests:
            try:
                self.execute_stage_request(
                    cimis_dataset=cimis_dataset, request=request)
            except Exception as e:
                print(e)
            finally:
                pass
        
    def execute_stage_preprocess_request(self, preprocess_request: StageCimisPreprocessRequest) -> xr.Dataset:
        """
        Compiles raw CIMIS data for stage cimis requests to operate on.

        - Creates a chronological list of monthly CIMIS netcdf files
        - Creates a preprocess used to subset raw cimis netcdf files
            to include only requested CIMIS points.
        - Loads the monthly CIMIS netcdf files into memory as a single
            xarray dataset while simultaneously performing spatial subset
            preprocess.
        - (Maybe) Temporally pads dataset to requested datetime range (in days)
        - Gap fills missing data based on long term hour of year averages
            - **NOTE** Alternatively use linear interpolation with a set window
                as this is what was prevoiusly performed
        """
        
        # Store helpers for conveinience
        subsetter = self._helpers.subsetter

        # Create chronological list of monthly cimis netcdf files
        raw_cimis_files = self.create_raw_cimis_filelist(
            raw_cimis_dir=preprocess_request.raw_cimis_dir,
            datetime_range=(
                preprocess_request.start_datetime,
                preprocess_request.end_datetime
            ))
        
        subset_preprocess = subsetter.create_subset_preprocess(
            cimis_netcdf_file=raw_cimis_files[0],
            cimis_ids=preprocess_request.cimis_ids)
        
        ds = None
        try:
            ds = xr.open_mfdataset(
                raw_cimis_files,
                preprocess=subset_preprocess,
                combine='nested',
                concat_dim='time',
                data_vars='all',
                parallel=False,
                decode_cf=True
            )

            ds = ds.load()
            return ds
        except Exception as e:
            if ds is not None:
                ds.close()
            raise ValueError(f'Failed to preprocess Cimis data: {e}')

    def execute_stage_request(self, cimis_dataset: xr.Dataset, request: StageCimisRequest) -> None:
        """
        Does the following for each `cimis_id` in the request ...
            1. Extracts corresponding NLDAS hourly SWdown timeseries
            2. Extract the CIMIS ETo daily timeseries
            3. Dissagregates ETo (CIMIS) from daily -> hourly using diurnal 
                distribution of SWdown (NLDAS).
            4. Writes data to CSV by CIMIS_ID.
        """
        
        # Subset to selected Cimis grids
        cimis_subset = cimis_dataset.sel(cimis_id=request.cimis_ids)

        # Extract Nldas shortwave downward radiation (SWdown) timeseries
        nldas_swdown = self.extract_staged_nldas(request.staged_nldas_file)

        # TODO: Validate Nldas SWdown has complete dataset without gaps
        # nldas_swdown = self.validate_nldas_swdown()
        
        # Calculate percent daily SWdown per hour (PDSWdown)
        nldas_pdswdown = self.calculate_percent_daily_swdown(nldas_swdown)

        # Disaggregate daily Cimis ETo to hourly with hourly PDSWdown (ETo x PDSWdown)
        cimis_subset = self.disaggregate_cimis_eto(cimis_subset, nldas_pdswdown=nldas_pdswdown)

        # TODO: Perform peeking and/or overwriting of staged storage

        # Loop through Cimis grid and write to staged storage (csv)
        for cimis_id in cimis_subset['cimis_id']:
            self.write_staged_cimis(cimis_subset['Eto'], output_dir=request.output_dir, cimis_id=int(cimis_id))

    def peek_raw_cimis_storage(self, raw_storage_dir: Path, 
        datetime_range: Tuple[datetime, datetime]) -> bool:
        """
        Peeks into raw cimis storage to determine if there exists
            a complete set of raw monthly cimis netcdf files that
            satisfies the date range requesteed.
        
        Returns:
            boolean indicating that the storage satisifies the requestest
                date range
        """

        dt = self._helpers.dt
        
        # Create datetime sequence of start of months from the date range
        req_dttm: pd.DatetimeIndex = pd.date_range(start=datetime_range[0], end=datetime_range[1], freq="MS")
        
        # Create a sequence of available start of months from raw cimis file storage
        avail_files = raw_storage_dir.glob("*.nc")
        avail_dttm_list: List[pd.Timestamp] = []
        for f in avail_files:
            dttm = pd.Timestamp(f.stem.split("_")[2])
            avail_dttm_list.append(dttm)
        
        avail_dttm: pd.DatetimeIndex = pd.DatetimeIndex(avail_dttm_list)

        # Check all requested datetimes exist in available datetimes, throw if not
        missing_dttm = req_dttm.difference(avail_dttm)
        gt_origin = missing_dttm >= pd.Timestamp("2003-11-01") # origin starts at 2003-11-01
        missing_dttm = missing_dttm[gt_origin] # remove those b4 origin | NOTE: Used for backward interpolation b4 origin

        if not missing_dttm.empty:
            raise ValueError('During `StageCimis`, the following months of cimis data requested by the user are missing ' +\
                f'in raw cimis storage:\n{[dt for dt in missing_dttm]}')
        
        return True
    
    def peek_staged_nldas_file(self, staged_nldas_file: Path, 
        datetime_range: Tuple[datetime,datetime]) -> bool:
        """
        Peeks into a staged nldas file to determine if all necessary
            data exists for the requested datetime range.
        
        Notes:
            - Assumes data is stored chronologically in csv
            - Only check start date and end date to see if the
                bounds contains the requested date range.
            - TODO: Find efficient way to ensure complete storage range
        """
        return True # TODO: Implement this
    
    def create_raw_cimis_filelist(self,
            raw_cimis_dir: Path,
            datetime_range: Union[Tuple[datetime, datetime], None]
        ) -> List[Path]:

        """
        Creates a list raw cimis files and their start datetime stored
            chronological order.

        """

        # Get list of available raw cimis files in date range requested
        raw_cimis_files: List[Path] = list(raw_cimis_dir.glob("*.nc"))   

        if datetime_range:

            # filter available raw cimis files to those in the requested date range    
            req_dttms: pd.DatetimeIndex = pd.date_range(
                start=datetime_range[0],
                end=datetime_range[1],
                freq="MS"
            )

            raw_cimis_files = list(
                filter(
                    lambda f: pd.Timestamp(f.stem.split("_")[2]) in req_dttms,
                    raw_cimis_files
                )
            )

        # Sort cimis files chronologically by date
        raw_cimis_files = list(
            sorted(raw_cimis_files, 
                   key=lambda f: pd.Timestamp(f.stem.split("_")[2]
            )))
        
        return raw_cimis_files
    
    def extract_staged_nldas(self, staged_nldas_file: Path) -> pd.Series:
        """
        Extracts nldas downward shortwave radiation from staged storage csv file
        """

        df = pd.read_csv(
            staged_nldas_file,
            usecols=["local_time", "SWdown"],   # load only these columns
            parse_dates=["local_time"],         # parse datetime
            index_col="local_time"              # set as index
        )

        return df['SWdown']

    def calculate_percent_daily_swdown(self, nldas_swdown: pd.Series) -> pd.Series:
        """Calculates percent daily shortwave downward radiation associated
            with each hourly element of `nldas_swdown`
            
        Args:
            nldas_swdown: hourly time series of nldas downward shortwave radiation, with DatetimeIndex.
        """
        
        swdown = nldas_swdown.to_frame()
        swdown.columns = ['hourly']

        if not isinstance(swdown.index, pd.DatetimeIndex):
            raise ValueError(f'nldas_swdown does not have pandas DatetimeIndex as its index.')
        
        swdown['date'] = swdown.index.normalize()

        daily = swdown['hourly'].resample('1d').sum().to_frame()
        daily.columns = ['daily']
        daily['date'] = daily.index

        swdown = pd.merge(swdown.reset_index(drop=False),daily,how='left',on='date')
        swdown = swdown.set_index(swdown.columns[0])
        swdown['pdswdown'] = swdown['hourly']/swdown['daily']

        return swdown['pdswdown']
        

    def disaggregate_cimis_eto(self, cimis_dataset: xr.Dataset, nldas_pdswdown: pd.Series) -> xr.Dataset:
        """
        Uses nldas hourly percent daily shortwave downward radiation (pdswdown) to disaggregate
            daily cimis Eto.
        """

        # TODO: Assert daily sums are ~1 (or handle percent scale)
            # daily_sums = nldas_pdswdown.groupby(nldas_pdswdown.index.floor("D")).sum()
        # assert np.allclose(daily_sums.values, 1.0, rtol=0, atol=1e-6)

        # TODO: (Maybe) Validate that data time coverage for Nldas is a subset of Cimis | this is done when making requests

        ds = cimis_dataset
        
        # Create a daily key on CIMIS time
        ds = ds.assign_coords(time_day=ds['time'].dt.floor('D'))

        # Convert nldas_pdswdown from pd.Series -> xr.DataArray
        pdswdown = xr.DataArray(
            nldas_pdswdown.astype('float64'),
            dims=('time_hourly',),
            coords={'time_hourly': ('time_hourly', nldas_pdswdown.index)},
            name='pdswdown',
        )

        # Add time_day to pdswdown
        pdswdown = pdswdown.assign_coords(time_day=pdswdown['time_hourly'].dt.floor('D'))

        # restrict hourly weights to cimis days only
        mask = xr.DataArray(            
            np.isin(pdswdown['time_day'].values, ds['time_day'].values),
            dims=('time_hourly',),
            coords={'time_hourly': pdswdown['time_hourly']}
        )

        pdswdown = pdswdown.where(mask, drop=True) # NOTE: Ensures all times from cimis exist in nldas

        if pdswdown.sizes['time_hourly'] == 0:
            raise ValueError(f'No SWdown Nldas exists for requested Cimis data range.')

        # Make time_day the 'join dimension' for CIMIS (so sel behaves predictably)
        ds = ds.swap_dims({'time': 'time_day'})
        if 'time' in ds.variables:
            ds = ds.drop_vars('time')

        # Select daily rows using the hourly day key -> dims (time_hourly, point)
        ds = ds.sel(time_day=pdswdown['time_day'])
        
        # Rename time_day -> hourly and reassign based on pdswdown
        ds = ds.assign_coords(time_hourly=pdswdown['time_hourly'])

        # Add pdswdown and compute hourly Eto
        ds = ds.assign(Eto=ds['Eto'] * pdswdown)

        return ds


    def write_staged_cimis(self, cimis_dataset: xr.DataArray, cimis_id: int, output_dir: Path) -> None:
        """
        Extracts ETo (hourly) cimis data for requested cimis_id from cimis_dataset and writes
        to a csv files in in staged cimis storage.
        """
    
        outfilename = f'cimis_{str(cimis_id)}.csv'
        outfilepath = output_dir / outfilename

        cimis_eto_da: xr.DataArray = cimis_dataset.sel(cimis_id=cimis_id)
        cimis_eto_df: pd.DataFrame = cimis_eto_da.to_dataframe().reset_index()
        cimis_eto_df = cimis_eto_df.set_index(['time_hourly'])
        cimis_eto_df.round(5).to_csv(outfilepath, 
            date_format="%m/%d/%Y %H:%M:%S")





        


    