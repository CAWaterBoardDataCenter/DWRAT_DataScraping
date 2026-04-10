import os
from pathlib import Path
from typing import Any, List, Tuple
import shutil
import pandas as pd
import numpy as np
from ..base import DataWriter
from src.core.miscellaneous import (DateRanger,read_nldas_pre,read_prism)
from src.core.models import (ProjectControl, DataRequest)

class WritePreFiles(DataWriter):
    
    def create_write_requests(self, project: ProjectControl) -> Any:
        print("\tTranslating project control data for data writer.")
        pass
    
    def write(self, project: ProjectControl):
        request_control = project.request_control
        staged_gage = project.storage.gage.staged
        staged_prism_disaggregated_storage = Path(project.storage.prism.staged)
        curated_pre_storage =Path(project.storage.pre.curated)
        prism_maping = project.prismMap.data
        start_datetime = request_control.start_date
        end_datetime = request_control.end_date
        gage_table = project.gage.data
        pre_mapping = pd.merge(prism_maping,gage_table,on='gage_id')
        pre_mapping['station_id'] = pre_mapping['station_id'].str.replace(':', '_')

        # Loop over all rows
        for index, row in pre_mapping.iterrows():
            # Read Staged Gage data (Hourly)
            gage_file_prefix = str(row['station_id'])
            gage_file = next(staged_gage.glob(f"{gage_file_prefix}*"),None)
            
            # Read Staged PRISM data (Hourly)
            prism_file_prefix = str(row['prism_id'])
            prism_file = next(staged_prism_disaggregated_storage.glob(f"{prism_file_prefix}*"),None)

            # Create hybrid PRE files
            hybrid_pre = WritePreFiles.create_hybrid_pre_file(gage_file,prism_file,start_datetime,end_datetime)

            # Save hybrid pre
            pre_filename = f"{row['prism_id']}_{row['station_id']}_{row['agency_id']}.pre"
            pre_filepath = curated_pre_storage / pre_filename
            hybrid_pre.to_csv(pre_filepath,date_format="%m/%d/%Y %H:%M:%S",header=False,index=False)

            # Copy standalone PRISM PRE files to the curated folder
            WritePreFiles.copy_files(prism_file,curated_pre_storage)


    @staticmethod
    # Function to hybrid PRISM and observed data
    def create_hybrid_pre_file(gage_input_filename,prism_diaggregated_filename,start_date,end_date):
        gage_data = pd.read_csv(gage_input_filename,header=None)
        gage_data.columns = ['datetime','hourly_data','Code']
        gage_data['datetime']  = pd.to_datetime(gage_data['datetime'])
        end_exclusive = end_date + pd.Timedelta(days=1)
        gage_data = gage_data[(gage_data['datetime'] >= start_date) & (gage_data['datetime'] < end_exclusive)]
        gage_data = gage_data.rename({'hourly_data': 'hourly_obs_data'}, axis = 1)
        gage_data_unchanged = gage_data[gage_data['Code'] != 255]
        gage_data_unchanged = gage_data_unchanged[['datetime','hourly_obs_data']]
        gage_data_tochange = gage_data[gage_data['Code'] == 255]
        
        prism_data = pd.read_csv(prism_diaggregated_filename,header=None)
        prism_data.columns = ['datetime','hourly_data']
        prism_data['datetime']  = pd.to_datetime(prism_data['datetime'])
        prism_data = prism_data[(prism_data['datetime'] >= start_date) & (prism_data['datetime'] < end_exclusive)]

        gage_prism = pd.merge(prism_data, gage_data_tochange, on='datetime', how = 'right')
        gage_prism = gage_prism[~gage_prism['hourly_data'].isnull()]
        gage_prism = gage_prism[['datetime','hourly_data']]
        gage_prism = gage_prism.rename({'hourly_data': 'hourly_obs_data'}, axis = 1)
        gage_prism_final = pd.concat([gage_prism,gage_data_unchanged],axis=0)
        gage_prism_final = gage_prism_final.sort_values(by="datetime")
        gage_prism_final = gage_prism_final[gage_prism_final['hourly_obs_data']>0]
        return gage_prism_final

    @staticmethod
    # Function to copy files 
    def copy_files(path_filename, output_mapping_folder):
        if os.path.exists(path_filename):
            shutil.copy(path_filename, output_mapping_folder)
        else:
            print(f"File not found: {path_filename}")
