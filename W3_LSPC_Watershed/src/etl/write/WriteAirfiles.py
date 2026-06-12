from pathlib import Path
from typing import (Union, List, Tuple, Any, cast)
from dataclasses import dataclass
import pandas as pd
import numpy as np
from datetime import datetime
import csv
from tqdm import tqdm

# Package Imports
from src.core.models import (ProjectControl, DataRequest)
from ..base import DataWriter
from ..util.helpers import (DatetimeHelpers, time_block)
# from ..util.WriteAirFileHelpers import ()

@dataclass(frozen=True)
class WriteAirFileRequest(DataRequest):
    prism_id: int
    cimis_id: int
    nldas_id: int
    cimis_file: Path

@dataclass(frozen=True)
class WriteAirFileRequestGroup(DataRequest):
    nldas_id: int
    nldas_file: Path
    template_file: Path
    requests: List[WriteAirFileRequest]
    vars: List[str] # variables to include in airfile

class WriteAirFileValidators:
    pass

class WriteAirFileHelpers:

    def __init__(self):
        self.dt = DatetimeHelpers()

class WriteAirFiles(DataWriter):

    def __init__(self):
        self._validators = WriteAirFileValidators()
        self._helpers = WriteAirFileHelpers()

    def create_write_requests(self, project: ProjectControl) -> List[WriteAirFileRequestGroup]:
        """
        Creates a list of WriteAirFileRequestGroup objects, each containing WriteAirFileRequests.
            Each WriteAirFileRequest has instructions for writing airfiles for the requested user 
            input (time range, cimis, nldas).
        
        Notes:
            - Each WriteAirFileRequestGroup groups cimis data grids by shared nldas grids.
            - Peeks Staged cimis storage to verify all request cimis_ids and nldas_ids exist
                and that data is available for requested range.

        """
        
        dthelper = self._helpers.dt
        airfile_template_path = Path(__file__).parent / "AirFileTemplate.txt"
        requested_vars = ['Eto'] # TODO: Get user input for this

        # Get start and end datetimes
        start_dttm = dthelper\
            .date_to_datetime(project.request_control.start_date)
        
        end_dttm = dthelper\
            .date_to_datetime(project.request_control.end_date, eod=True)
        
        # Extract airfile mapping table (links prism_id -> cimis_ids -> nldas_ids)
        airmap = project.airMap.data

        # Create list of staged nldas csv files
        staged_nldas_files = list(project.storage.nldas.staged.glob("*csv"))

        # Create list of staged cimis csv files
        staged_cimis_files = list(project.storage.cimis.staged.glob("*csv"))

        
        missing_nldas: List[Tuple[str,int]] = [] # list of nldas_ids with missing data (file or insufficient data coverage)
        missing_cimis: List[Tuple[str,int]] = [] # list of cimis_ids with missing data (file or insufficient data coverage)
        request_groups: List[WriteAirFileRequestGroup] = []
        # Group by nldas_id and loop through groups
        grps = airmap.groupby('nldas_id')
        for key, grp in tqdm(grps, desc="Peeking Files in Nldas Group", total=len(grps)):
            assert not isinstance(key, tuple)

            nldas_id: int = int(cast(int,key)) # NOTE: Hacky convertsion - not sure if key is np.int64 or int but want to enfore int

            # Peek into raw nldas storage to see if file exists for requested nldas_id and check
                # that necessary date exists too    
            
            nldas_file = self.find_file_in_filelist(str(nldas_id),
                filelist=staged_nldas_files)
            
            if not nldas_file:
                missing_nldas.append((str(nldas_id),2))
                continue

            has_data = self.peek_csv_file(file=nldas_file,
                dttm_rng=(start_dttm,end_dttm),
                dttm_col='local_time')
            
            if not has_data:
                missing_nldas.append((str(nldas_id),1))
                continue
            
            requests: List[WriteAirFileRequest] = []
            for i, row in grp.iterrows():
                cimis_id: int = int(cast(int,row['cimis_id']))
                prism_id: int = int(cast(int,row['prism_id']))

                cimis_file = self.find_file_in_filelist(
                    file_id=str(cimis_id),
                    filelist=staged_cimis_files
                )

                if not cimis_file:
                    missing_cimis.append((str(cimis_id),2))
                    continue

                has_data = self.peek_csv_file(
                    file=cimis_file,
                    dttm_rng=(start_dttm,end_dttm),
                    dttm_col='time_hourly')
                
                if not has_data:
                    missing_cimis.append((str(cimis_id), 1))
                    continue

                requests.append(
                    WriteAirFileRequest(
                        start_datetime=start_dttm,
                        end_datetime=end_dttm,
                        overwrite=project.request_control.overwrite,
                        output_dir=project.storage.air.curated,
                        prism_id=prism_id,
                        cimis_id=cimis_id,
                        nldas_id=nldas_id,
                        cimis_file=cimis_file
                    )
                )

            request_groups.append(
                WriteAirFileRequestGroup(
                    start_datetime=start_dttm,
                    end_datetime=end_dttm,
                    overwrite=project.request_control.overwrite,
                    output_dir=project.storage.air.curated,
                    nldas_id=nldas_id,
                    nldas_file=nldas_file,
                    vars=requested_vars,
                    template_file=airfile_template_path,
                    requests=requests
                )
            )

        if missing_nldas or missing_cimis:
            error_message = f'Staged Nldas storage is missing data for the following files: {missing_nldas}\n\n'
            error_message += f'Staged Cimis storage is missing data for the following files: {missing_cimis}\n\n'
            raise ValueError(error_message)
        
        return request_groups


    def write(self, project) -> None:
        """Write AirFiles data based on project control settings."""
        # Implementation for writing AirFiles data goes here
        print(f"\tWriting AirFiles data for project: {project.request_control.project_name}")

        request_groups = self.create_write_requests(project=project)

        for grp in tqdm(request_groups, desc='Writing data by WriteAirFile Group', total=len(request_groups)):
            
            # Extract Header from template and restrict to vars
            hdr = self.extract_airfile_header_from_template(grp.template_file, vars=grp.vars)

            # Extract nldas (if variables requested)
            nldas = None
            nldas_vars = [v for v in grp.vars if v != 'Eto']
            if nldas_vars:
                nldas = self.extract_nldas(grp.nldas_file, 
                    vars=nldas_vars, 
                    dttm_rng=(grp.start_datetime, grp.end_datetime))
            
            # Execute individual write requestes (grouped by nldas_id)
            for request in grp.requests:
                self.execute_write_request(request, nldas=nldas, hdr=hdr)
   
    def execute_write_request(self, request: WriteAirFileRequest, hdr: str,
        nldas: pd.DataFrame | None) -> None:
        """
        Writes an LSPC airfile to currated storage using Cimis and Nldas (optional) data provided
            with the provided header string. 
        """
        
        outdir = request.output_dir
        outname = f'{str(request.prism_id)}.air'
        outpath = outdir/outname

        # Extract airfile data from cimis and nldas (optional), merge, and format
        air = self.extract_cimis(request.cimis_file, 
            dttm_rng=(request.start_datetime,request.end_datetime))
        
        if not (nldas is None):
            air = pd.merge(air, nldas, how='left', on='datetime')
        
        air = self.format_airfile_dataframe(air, prism_id=request.prism_id)

        # Update heaader and write to file
        hdr = self.add_station_id_to_header(hdr=hdr, station_id=str(request.prism_id))
        with open(outpath, 'w') as f:
            f.write(hdr)
        
        # Write (append) airfile data to file
        date_fmt = '%Y-%m-%d %H:%M:%S'
        air.to_csv(outpath, mode='a', sep='\t',
            index=False, header=False,
            date_format=date_fmt)

    def extract_airfile_header_from_template(self, template_file: Path, vars: List[str]) -> str:
        """
        Extracts airfile header from template file and trims header to include
            requested variables.
        
        Notes:
            - Assumes the order of vars is the inteded order of variables in header
            - Has default line mapping associated with variables.
        """

        VALIDVAR = ['Eto','Tair','Wind','SWdown','Tdpt','ccf']

        # Check requested vars are valid
        invalid_vars = [v for v in vars if v not in VALIDVAR]
        if invalid_vars:
            raise ValueError(f'The following airfile variables requested to be extracted from template are not valid: {vars}.')

        # Extract raw header template
        hdr_lines: List[str] = []
        with open(template_file, 'r') as f:
            hdr_lines = f.readlines()

        # Split into sections
        top_lines = hdr_lines[0:11]
        var_lines = hdr_lines[11:17]
        bottom_lines = hdr_lines[17:]

        # Map variable line to a dict
        var_dict = {}
        for i, var in enumerate(VALIDVAR):
            var_dict[var] = var_lines[i]
        
        # Select var_lines for header
        selected_var_lines: List[str] = []
        for var in vars:
            selected_var_lines.append(var_dict[var])

        return "".join(top_lines + selected_var_lines + bottom_lines)

    
    def add_station_id_to_header(self, hdr: str, station_id: str) -> str:
        """
        Replaces all occurences of `station_id` in the header string with the specified
            station_id substing.
        """
        return hdr.replace('station_id', station_id)
    
    def extract_nldas(self, nldas_file: Path, vars: List[str], dttm_rng: Tuple[datetime,datetime] | None) -> pd.DataFrame | None:
        """
        Extract requested variables defined by var from staged nldas file.
            Returns None if no vars requested. Filters data to requested
            datetime range.
        
        Notes:
            - Returns variable columns in the same order as `vars`. This should
                correspond with the airfile header (up to the user/client).
            - Uses `local_time` to represent datetime, which is Pacific Time.
        """
        column_rename_map = {'local_time': 'datetime'}

        nldas = pd.read_csv(nldas_file, parse_dates=['local_time'])
        
        if 'local_time' not in nldas.columns:
            raise ValueError(f'`local_time` field missing from staged nldas file: {nldas_file}.')
        
        missing_vars = [v for v in vars if v not in nldas.columns]
        if missing_vars:
            raise ValueError(f'Fields {missing_vars} missing from staged nldas file: {nldas_file}.')
        
        nldas = nldas.rename(columns=column_rename_map)

        if dttm_rng:
            min_dttm = pd.Timestamp(dttm_rng[0])
            max_dttm = pd.Timestamp(dttm_rng[-1])
            nldas = nldas.loc[(nldas['datetime'] >= min_dttm) & (nldas['datetime'] <= max_dttm)]
            if nldas.empty:
                raise ValueError(f'Insufficient Nldas data for requested datetime range {dttm_rng} by Nldas file: {nldas_file}.')


        return nldas[['datetime'] + vars]

    def extract_cimis(self, cimis_file: Path, dttm_rng: Tuple[datetime,datetime] | None) -> pd.DataFrame:
        """
        Extracts Cimis Eto and datetime from cimis_file.

        Notes:
            - `time_hourly` corresponds to hourly `local_time` in Nldas,
                which is Pacific Time Zone.
        """

        column_rename_map = {'time_hourly': 'datetime'}

        cimis = pd.read_csv(cimis_file, parse_dates=['time_hourly'])

        if 'time_hourly' not in cimis.columns:
            raise ValueError(f'`time_hourly` field missing from staged file file: {cimis_file}.')
        
        if 'Eto' not in cimis.columns:
            raise ValueError(f'`Eto` field missing from staged file file: {cimis_file}.')
        
        cimis = cimis.rename(columns=column_rename_map)

        if dttm_rng:
            min_dttm = pd.Timestamp(dttm_rng[0])
            max_dttm = pd.Timestamp(dttm_rng[-1])
            cimis = cimis.loc[(cimis['datetime'] >= min_dttm) & (cimis['datetime'] <= max_dttm)]
            if cimis.empty:
                raise ValueError(f'Insufficient Cimis data for requested datetime range {dttm_rng} in Cimis file: {cimis_file}.')

        return cimis[['datetime','Eto']]
        
    def format_airfile_dataframe(self, df: pd.DataFrame, prism_id: int) -> pd.DataFrame:
        """Formats dataframe containing airfile variables to conform to
            lspc air file table format. Injects prism_id as first column.

        Notes:
            - Assumes data columns are already in correct order corresponding
                to header file format.
        """
        dttm_cols = ['year','month','day','hour','minute']
        data_cols = [c for c in df.columns if c != 'datetime']
        
        dttms = pd.DatetimeIndex(df['datetime'])
        
        df = df.assign(
            id=prism_id,
            year=dttms.year,
            month=dttms.month,
            day=dttms.day,
            hour=dttms.hour,
            minute=dttms.minute
        )

        return df[['id'] + dttm_cols + data_cols]
    
    def peek_csv_file(self, file: Path,
        dttm_rng: Tuple[datetime,datetime],
        dttm_col: str) -> bool:
        """
        Peeks into a staged csv file (nldas or cimis) to determine if all necessary
            data exists for the requested datetime range.
        
        args:
            file Path: Path of csv file to search
            dttm_range Tuple[datetime,datetime]: start and end datetime required
                for data in file.
            dttm_col [str]: Name of field in csv that contains datetime.
        
        Notes:
            - Assumes data is stored chronologically in csv
            - Only check start date and end date to see if the
                bounds contains the requested date range.
            - TODO: Find efficient way to ensure complete storage range
        
        Returns:
            - True: If file has data in requested date range
            - False: If file does not have data in requested date range
        """
        # return True # TODO: Implement this    
        
        # Peek into file to see if necessary data exists for date range
        first_last: pd.DataFrame = self.first_and_last_line_csv(file, has_hdr=True)
        
        if first_last.empty:
            raise ValueError(f'File is missing data: {file}.')
        
        if dttm_col not in first_last.columns:
            raise ValueError(f'Datetime field {dttm_col} is missing from file: {file}.')
        
        first_last_dttm: pd.DatetimeIndex = pd.DatetimeIndex(first_last[dttm_col])
        has_data = (first_last_dttm[0] <= dttm_rng[0]) and (first_last_dttm[-1] >= dttm_rng[-1])
        if not has_data:
            return False
        
        return True
    
    def find_file_in_filelist(self, file_id: str, filelist: List[Path]) -> Path | None:
        """
        Returns the first matching file in the filelist that contains the file_id
            specified in the file basename.
        """

        _gen = (f for f in filelist if file_id in f.name)
        file = next(_gen, None)
        return file

    
    def first_and_last_line_csv(self, file: Path, encoding="utf-8", has_hdr: bool = True) -> pd.DataFrame:
        """Assuming """
        
        with file.open() as f:
            reader = csv.reader(f)

            hdr = next(reader) if has_hdr else None

            first = None
            last = None

            for row in reader:
                if not row:
                    continue
                if first is None:
                    first = row
                last = row
            
            if first is None:
                return pd.DataFrame(columns=hdr)
            
            df = pd.DataFrame([first,last], columns=hdr)
            return df

        
        


