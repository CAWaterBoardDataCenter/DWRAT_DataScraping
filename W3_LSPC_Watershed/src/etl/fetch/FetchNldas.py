from typing import (Callable, Optional, List, Tuple, cast)
import warnings
import os
from pathlib import Path
import numpy as np
import pandas as pd
import xarray as xr
from datetime import (datetime, timedelta)
from zoneinfo import (ZoneInfo)
from dataclasses import dataclass


# Package imports
from src.core.models import (ProjectControl, DataRequest)
from ..base import DataFetcher
from ..util.helpers import (XarrayHelpers, SpatialHelpers, DatetimeHelpers)


class FetchNldasRequest(DataRequest):
    pass
 
class FetchNldasValidators:

    def validate_user_input(self, project: ProjectControl) -> None:
        return None # TODO: Add

    def validate_params(self, params: List[str]) -> List[str]:
        """Validate the requested parameters against valid NLDAS parameters."""
        VALID_PARAMS = ['Rainf', 'PotEvap', 'SWdown', 'Qair', 'Tair', 'Wind_E', 'Wind_N', 'PSurf']
        if not params:
            return VALID_PARAMS
        invalid_params = [p for p in params if p not in VALID_PARAMS]
        if invalid_params:
            raise ValueError(f"Invalid parameters requested: {invalid_params}. Valid parameters are: {VALID_PARAMS}")
        return params
    
    def validate_nldas_ids(self, nldas_ids: List[int]) -> List[int]:
        """Validate the provided NLDAS IDs."""
        # Placeholder for actual validation logic
        return nldas_ids
    
    def validate_datetime_range(self, start_datetime: datetime, end_datetime: datetime) -> None:
        """Validate the provided date range for NLDAS data availability by comparing request time (Pacific) against estimated
        available NLDAS updates (Pacific)
            **NOTE** Assumes start_datetime and end_datetime provided in Pacific Standard Time Zone (Etc/GMT+8) and 
                compares against threshold that is also in Pacific time.
        """
        min_datetime = datetime(2003, 10, 1)
        max_datetime = datetime.now(ZoneInfo("Etc/GMT+8"))
        max_datetime = datetime(max_datetime.year, max_datetime.month, max_datetime.day, 23)
        max_datetime = max_datetime - timedelta(days=3)
        
        if (start_datetime > end_datetime):
            raise ValueError(f"Start date {start_datetime} cannot be after end date {end_datetime}.")
        
        if (start_datetime < min_datetime) or (end_datetime > max_datetime):
            raise ValueError(f"Date range must be between {min_datetime} and {max_datetime}. Provided: {start_datetime} to {end_datetime}.")

    
    def validate_fetch_text_response(self, response: str, request: FetchNldasRequest) -> str:
        response_record_count = len(response[response.find('Data\n')+len('Data\n'):].split('\n'))
        request_record_count = ((request.end_datetime - request.start_datetime).days + 1) * 24 # hourly data
        
        if response_record_count == 0:
            raise ValueError("Error: No data returned from NLDAS request.")
        if response_record_count != request_record_count:            
            raise ValueError(f"Error: Incomplete data returned from NLDAS request. Expected {request_record_count} records but received {response_record_count}.")
        
        return response

    def validate_nldas_request_dataset(self, dataset: xr.Dataset) -> None: # TODO: Build me
        """Validates an xarray dataset intended created from nldas granules to ensure it has the
        dimensions, coordinates, and variables required to carry out data extraction procedures
        in the fetch nldas module.
        """
        pass

class FetchNldasHelpers:

    def __init__(self):
        self.xr = XarrayHelpers()
        self.dt = DatetimeHelpers()
        self.sp = SpatialHelpers()

class FetchNldas(DataFetcher):

    VALID_PARAMS = ['Rainf', 'PotEvap', 'SWdown', 'Qair', 'Tair', 'Wind_E', 'Wind_N', 'PSurf']

    def __init__(self):
        self._validators = FetchNldasValidators()
        self._helpers = FetchNldasHelpers()

        import earthaccess
        self._ea = earthaccess
        # self._ea.login = earthaccess.login(strategy="environment", persist=False)
        self._ea_auth_verified = False
        self._ea_auth_attempts = 0
        username = os.environ.get("EARTHDATA_USERNAME")
        password = os.environ.get("EARTHDATA_PASSWORD")
        
        if username and password:
            try:
                self._ea_auth = earthaccess.login(persist=False, strategy="environment")
                self._ea_auth_verified = True
                self._ea_auth_attempts += 1
                print("Validated Earthdata with credentials provided in .env.")
            except Exception as e:
                print("Earthdata credentials provided in `.env` are invalid, reverting to manually entry.")
        
        while (not self._ea_auth_verified) and (self._ea_auth_attempts < 4):
            self._ea_auth_attempts += 1
            try:
                self._ea_auth = earthaccess.login(persist=False, strategy="interactive")
                self._ea_auth_verified = True
            except Exception as e:
                print(f"Failed to Authenticate Earthdata Account: {e}. Try Again. Max attempts set to 3.")

        # if not self._ea_auth_verified: # TODO: Decide if we want to throw exception for failed connection. This will require exception handling for runner class.
        #     raise ConnectionError(f'Unable to authenticate Nasa Earth Data Credentials.')

    def fetch(self, project) -> None:
        """Fetch NLDAS data based on project control settings."""
        # Implementation for fetching NLDAS data goes here
        print(f"\tFetching NLDAS data for project: {project.request_control.project_name}")

        # TODO: Uncommonent
        # Validate User-input (data request and cimis targets)
        self._validators.validate_user_input(project)

        # Create data requests (units of work) grouped by nldas_id and variable
        nldas_requests = self.create_fetch_requests(project)  # TODO: Find a way to peek into existing storage to determine what months need be pulled
        # if overwrite = False, then only retrieve data for month that don't exist or are incomplete (avail_time coordinate has 0 values)
        
        # Download entire granules, open then as single xr dataset, subset to requested coorindates, add meta-data (avail_time, pacific_time coordiantes), write to single file
        # use overwrite logic where required
        
        for request in nldas_requests: # date range grouping (network request level)
            try:
                self.execute_fetch_request(request)
            except Exception as e:
                print(f"Error executing fetch request for {request.start_datetime} to {request.end_datetime}: {e}") # TODO: Add to log
                                    
    def create_fetch_requests(self, project: ProjectControl) -> List[FetchNldasRequest]:
        """Create NLDAS data requests based on project control settings.
        
            **NOTE** Assumes all user-input is provided in Pacific TZ and preserves this.
        """
        # Extract request variable from 
        request_control = project.request_control
        storage = project.storage
        nldas = project.nldas.data
        validators = self._validators
        helpers = self._helpers
        variables = FetchNldas.VALID_PARAMS

        # Validate Requested Grids | NOTE: May not be required
        # nldas_ids = validators.validate_nldas_ids(nldas["nldas_id"].tolist())


        # Validate Date Range | Must be >= 2003-10-01 & < current day - 3 (that's what is available)
        start_datetime = helpers.dt.date_to_datetime(request_control.start_date)
        end_datetime = helpers.dt.date_to_datetime(request_control.end_date, eod=True)
        validators.validate_datetime_range(
            start_datetime=start_datetime, end_datetime=end_datetime
        )        

        # Create monthly date range intervals
        datetime_ranges = helpers.dt\
            .split_date_range_into_monthly_intervals(
                start_datetime, 
                end_datetime,
                force_boundaries=True # NOTE: Forces start and end of months regardless of request start and end
            )  

        # Convert user-input datetimes from Pacific to UTC # NOTE: Removed --> converted when requests are performed
        # start_datetime = helpers.dt.naive_to_local_to_utc(start_datetime)
        # end_datetime = helpers.dt.naive_to_local_to_utc(end_datetime)

        # Create requests
        requests = []

        coords = helpers.sp.coords_from_lat_lon_arrays(
            lat=project.nldas.data["lat"].tolist(),
            lon=project.nldas.data["lon"].tolist()
        )

        for datetime_range in datetime_ranges:

            if not project.request_control.overwrite:
                
                # Peak into existing storage and update date ranges according to data already available.                
                datetime_range = self.peek_existing_nldas_data(
                    output_dir=project.storage.nldas.raw,
                    datetime_range=datetime_range
                )

            if datetime_range:
                requests.append(
                    FetchNldasRequest(
                        start_datetime=datetime_range[0],
                        end_datetime=datetime_range[1],
                        overwrite=project.request_control.overwrite,
                        output_dir=project.storage.nldas.raw
                ))

        return requests

    def peek_existing_nldas_data(self, output_dir: Path, datetime_range: Tuple[datetime, datetime]) -> Tuple[datetime, datetime] | None:
        """Checks output_dir for existing nldas
        
        1. Looks for matching file name for month date_range
        2. If file does not exist returns full date range (assumes no data)
        3. If file does exist, opens file and checks for 100% availability
            a. If 100% availability, returns None
            b. If partial availability, creates new date_range bound by minimum
                unavailable time point and maximum unavailable time point.
        
        """
        
        _fmt = "%Y-%m-%dT%H--%M--%S" # TODO: Globalize this for this module
        new_datetime_range = datetime_range

        start_dttm = pd.to_datetime(datetime_range[0]).strftime(_fmt)
        end_dttm = pd.to_datetime(datetime_range[1]).strftime(_fmt)
        filename = f"nldas_{start_dttm}_{end_dttm}.nc"
        filepath = output_dir / filename
        
        if filepath.exists():
            
            with xr.open_dataset(filepath) as dataset:
                available = dataset["available"]
                num_available = available.sum()
                num_time_points = available.shape[0]
            
                if (num_available != num_time_points): # less than 100% available
                    unavailable_times = dataset["time"].where(~available, drop=True)
                    min_unavailable_time = cast(np.datetime64, unavailable_times.values.min())
                    max_unavailable_time = cast(np.datetime64, unavailable_times.values.max())
                    
                    new_datetime_range = (
                        self._helpers.dt.datetime64_to_datetime(min_unavailable_time),
                        self._helpers.dt.datetime64_to_datetime(max_unavailable_time)
                    )
                    
                else:
                    new_datetime_range = None

        return new_datetime_range

    def download_nldas_granules(self, request: FetchNldasRequest) -> List[Path]:
        """Fetches full granules for NLDAS data from the GES DISC API based on date range in NldasRequestCollection."""

        # TODO: Convert nldas request start and end times from Pacifc -> UTC
        naive_to_local_to_utc = self._helpers.dt.naive_to_local_to_utc
        _start_datetime = naive_to_local_to_utc(request.start_datetime)
        _end_datetime = naive_to_local_to_utc(request.end_datetime)

        _short_name = "NLDAS_FORA0125_H"
        _version = "2.0"
        _date_format = "%Y-%m-%dT%H:%M:%S"
        _start = _start_datetime.strftime(_date_format)
        _end = _end_datetime.strftime(_date_format)
        _n_requested_hours = int(
            1 + ((request.end_datetime - request.start_datetime).total_seconds() // 3600)
        )
        
        output_dir = request.output_dir / "tmp"
        output_dir.mkdir(exist_ok=True)
        
        granules = self._ea.search_data(
            short_name=_short_name,
            version=_version,
            temporal=(_start,_end)
        )

        # TODO: Determine how to handle instances where number of granules doesn't equal number hours requested
        if len(granules) != _n_requested_hours:
            warnings.warn(
                "Based on NASA CMR (meta data repository) the number of hourly NLDAS granules"\
                + f"available is {len(granules)}, where as the number of hours requested by the client"\
                + f"is {_n_requested_hours}.")

        downloaded_granule_filepaths = self._ea.download(
            granules=granules,
            local_path=output_dir,
            show_progress=True
        )

        # TODO: Make custom error related to this hi-level networking case
        if len(downloaded_granule_filepaths) != len(granules):
            raise ValueError("Failure: Number of granules downloaded does not match number requested.")

        return downloaded_granule_filepaths
        
    
    def preprocess_nldas_granule(self, granule: xr.Dataset) -> xr.Dataset:
        """Performs composite procedure of preprocessing steps to be preformed on NLDAS granules during loading of multi-file datasets."""
        index = self._helpers.xr.create_incremental_spatial_id_variable
        subset = self._helpers.xr.subset_spatial_extent
        return subset(index(granule, varname='nldas_id', flip_lat=True))

    
    def validate_nldas_granules(self, granules: List[Path]) -> List[bool]:
        
        is_valid = []
        for granule in granules:
            try:
                with xr.open_dataset(granule) as ds:
                    pass
                is_valid.append(True)
            except Exception as e:
                is_valid.append(False)

        return is_valid

    def read_nldas_granules_to_xarray(self, granules: List[Path], 
            preprocess: Optional[Callable[[xr.Dataset], xr.Dataset]] = None) -> xr.Dataset:
        """Reads a list of NLDAS granules into memory as an xarray dataset object."""

        granule_validator = self.validate_nldas_granules

        # validate that all the paths are netcdf files
        if not all([p.suffix == ".nc" for p in granules]):
            raise ValueError(f"Granule file path list provided contains subjects that are not in required netcdf format.")
        
        # Validate granules are netcdf files that can be opened with netcdf4 engine
        valid_granules = granule_validator(granules)
        if not all(valid_granules):
            # TODO: Decide how to handle invalid granules | for now omit them
            granules = [granules[i] for i in range(len(granules)) if valid_granules[i]]
        
        # NOTE: Hard-coding subsetting function to get CA 
        with xr.open_mfdataset(
            granules,
            preprocess=preprocess,
            combine="nested",
            concat_dim="time",
            join='exact',
            coords='minimal',
            data_vars="all",
            compat='equals',
            parallel=False,
            decode_cf=True
        ) as ds:
            ds_loaded = ds.load()

        return ds_loaded

    
    def flag_available_timepoints_in_nldas_dataset(self, dataset: xr.Dataset) -> xr.Dataset:
        """Flags available timepoints in nldas request dataset by adding new auxilliary coordinate
        with boolean values
        """
        available = np.repeat(True, dataset['time'].shape[0])
        dataset['available'] = (("time"), available)
        return dataset
    
    def pad_time_dimension_in_nldas_dataset(self, dataset: xr.Dataset, request: FetchNldasRequest) -> xr.Dataset:
        """
        - Reindexes the time dimension (and coordinate) on the nldas request dataset to pad missing time points
        in the start and end of month being requested (Pacific Time). 
        - Padded timepoints contain Nan values for coordinates and variables
            for the time indexes being inserted.
        - **ASSUMES** request.start_datetime and request.end_datetime are in Pacific TZ and converts to UTC to
            conform to the netcdf storage schema.
        """
        pad_time = self._helpers.xr.pad_time
        naive_to_utc = self._helpers.dt.naive_to_local_to_utc
        # month_edge = self._helpers.dt.month_edge

        # month_start_datetime_pacific = month_edge(request.start_datetime, end=False)
        # month_start_datetime_pacific = month_edge(request.start_datetime)
        month_start_datetime_pacific = request.start_datetime
        month_start_datetime_utc = naive_to_utc(month_start_datetime_pacific)
        
        # month_end_datetime_pacific = month_edge(request.end_datetime, end=True)
        # month_end_datetime_pacific = month_edge(request.end_datetime)
        month_end_datetime_pacific = request.end_datetime
        month_end_datetime_utc = naive_to_utc(month_end_datetime_pacific)

        return pad_time(dataset, start_datetime=month_start_datetime_utc, end_datetime=month_end_datetime_utc)
    
    def add_local_time_coordinate_to_nldas_dataset(self, dataset: xr.Dataset) -> xr.Dataset:
        
        local_times = self._helpers.xr.extract_local_time_from_utc(dataset, to_tz="Etc/GMT+8")
        return dataset.assign_coords(local_time=("time", local_times))

    def inject_available_data(self, from_dataset: xr.Dataset, to_dataset: xr.Dataset) -> xr.Dataset:
        """Injects all variables in from_dataset into to_dataset along time dimension where
        True in the `available` variable of from_dataset
        
        
        Assumption: 
            1. Both from_dataset and to_dataset have same dimensions and coordinates (with same values)
            and both datasets have the same variables, but these variable values are not the same (but could be).
            2. There exists an `available` variable. 
            3. Available has only the `time` dimension linked to it.

        **NOTE** Only replace values where the variables have the dimensions of the mask variable "available"
            otherwise it broadcasts variable along dimensions they are missing. This was previously causing
            `nldas_id` to broadcast with additional time dimension.
        """

        # TODO: Consider adding validation that both datasets have same dimensions, coordinates, and variables

        # Verify "availabe" exists in from_var
        if not ("available" in from_dataset.data_vars):
            raise ValueError(f"Data variable `available` must exists in from_dataset.")
        
        # Apply selective data injection based on available flag
        replacements = (from_dataset["available"] == 1)

        # Identify variables with time dimension
        result = to_dataset.copy()
        # vars_with_time_dim = [v for v in result.data_vars if 'time' in result[v].dims]
        vars_with_replacement_dims = [v for v in result.data_vars if set(result[v].dims).issuperset(set(from_dataset["available"].dims))]
        for var in vars_with_replacement_dims:
            if var not in from_dataset.data_vars:
                raise ValueError(f"Variable {var} missing in from_dataset where it should exist; from_datast and to_datset should have same variables.")
            result[var] = result[var].where(~replacements, other=from_dataset[var])
        
        return result


    
    def write_request_dataset_to_raw_storage(self, dataset: xr.Dataset, request: FetchNldasRequest):
        """Writes the fetched request dataset to csv file in raw storage
        
        - If overwrite = True, replaces new data in existing dataset and writes to new file
        - Assumes that earlier checks will remove any granules that already exist in request date range if `overwrite` is set to false.
        - Assumes number of elements in time dimension is a valid chunk size for writing.
        - Uses encoding for writing new dataset.
        """
        _fmt = "%Y-%m-%dT%H--%M--%S"        
        overwrite = request.overwrite
        # start_dttm = pd.to_datetime(dataset['time'].values.min()).strftime(_fmt) # assumes start and end date should already be padded to start/end of month
        # end_dttm = pd.to_datetime(dataset['time'].values.max()).strftime(_fmt)
        start_dttm = pd.to_datetime(request.start_datetime).strftime(_fmt)
        end_dttm = pd.to_datetime(request.end_datetime).strftime(_fmt)
        outfilename = f"nldas_{start_dttm}_{end_dttm}.nc"
        outfilepath = request.output_dir / outfilename
        
        if (not overwrite) and (outfilepath.exists()):
            # TODO: Open existing file and replace values from current
            dataset = self.inject_available_data(
                from_dataset=dataset,
                to_dataset=xr.load_dataset(outfilepath)
            )

        # Assume time dimension size is valid chunk size
        dataset = dataset.chunk({"time": -1})

        # Write to netCDF
        dataset.to_netcdf(outfilepath)
    
    def purge_request_temporary_storage(self, request: FetchNldasRequest):
        """Removes full size granules from temporary storage"""
        tmp_storage_dir = request.output_dir / "tmp"
        tmp_file_list = tmp_storage_dir.glob("*.nc")
        for tmp_file in tmp_file_list:
            Path.unlink(tmp_file, missing_ok=True)

    def execute_fetch_request(self, request: FetchNldasRequest) -> None:

        attempts = 0
        max_attempts = 5
        downloaded_granule_filepaths: List[Path] = []
        success = False
        while (not success) and (attempts < max_attempts):
            try:
                downloaded_granule_filepaths = self.download_nldas_granules(request)
                success = True
            except Exception as e:
                print(f"Error fetching NLDAS data for request {request}: {e}")
                attempts += 1

        if len(downloaded_granule_filepaths) == 0:
            raise ValueError(f"Failure downloading nldas granules.")

        # Load the granules into memory as an xarray dataset while performing spatial preprocessing (indexing and subsetting)
        try:
            preprocess = self.preprocess_nldas_granule
            request_dataset = self.read_nldas_granules_to_xarray(downloaded_granule_filepaths, preprocess=preprocess)
        except Exception as e:
            raise ValueError(f"Failure loading nldas granules into xarray dataset: {e}")

        # NOTE: (If Required) Validate the xarray dataset (dimensions, coorindates, etc)
        # self._validators.validate_nldas_request_dataset(request_dataset)

        # Add avail_time coordinate to flag available times from download before padding time dimension
        request_dataset = self.flag_available_timepoints_in_nldas_dataset(dataset=request_dataset)

        # Pad time dimensions on dataset (reindex) to complete month, Nan values filled for hours that don't exist
        request_dataset = self.pad_time_dimension_in_nldas_dataset(dataset=request_dataset, request=request)

        # Add Pacific time coordinate tied to point dimension (converted from UTC)
        request_dataset = self.add_local_time_coordinate_to_nldas_dataset(dataset=request_dataset)                        

        # Write to monthly netcdf file | Update existing data if overwrite is true and data exists
        self.write_request_dataset_to_raw_storage(dataset=request_dataset, request=request)

        # Remove full sized granules from temporary storage
        self.purge_request_temporary_storage(request)
