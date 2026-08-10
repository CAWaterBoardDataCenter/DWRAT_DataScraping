"""
Author: Ashish Kondal (ashish.kondal@ulteig.com)
Date: 08/07/2026

Purpose: Script to prepend and append dummy data to weather files (.air and .pre files)

Required Inputs: Weather files 
            and EndDate
"""

import os
import pandas as pd
import numpy as np
from tqdm import tqdm
import sys

base_dir = r"Input/Weather"  # Folder where weather files (air and pre) are stored.
out_dir = r"Input/Weather"    # Output folder where updated/fixed weather files will be saved. This folder will automatically created if not already exists.

# Update the EndDate whenever you create new AIR or PRE files.
# The EndDate must be later than the final date in the weather files.
# For example, if the weather files are generated through 06/30/2026, set the EndDate to 07/01/2026 00:00:00 or any subsequent date.
 
EndDate = "07/01/2026 00:00:00"      # Datetime: MM/DD/YYYY HH:MM:SS
StartDate = "12/01/1999 00:00:00"     # No changes are required to StartDate for SDA projects. This entry will be prepended to all .pre files and serves as dummy data.

# ---------------------------------------------------------------- No changes required below. ---------------------------------------------------------------------------
EndDate = pd.to_datetime(EndDate)
StartDate = pd.to_datetime(StartDate)

if not os.path.exists(out_dir):
    os.mkdir(out_dir)

def get_file_contents(inp_file):
    with open(inp_file, 'r') as f:
        lines = f.readlines()
        return lines


def get_start_line(start_string, file_contents, start=0):
    for i in range(len(file_contents[start:])):
        line_no = i + start
        line_lower = file_contents[line_no].strip().lower()
        start_string_lower = start_string.lower().strip()
        if line_lower.startswith(start_string_lower):
            return i
    # raise error if start line of section not found
    raise KeyError('Start line for string {} not found'.format(start_string))



# Go through all files in the directory

for filename in os.listdir(base_dir):
    file_path = os.path.join(base_dir, filename)
    # Check if it's a file (not a subdirectory)
    if os.path.isfile(file_path):
        try:
            base_name, extension_id = os.path.splitext(filename)
            if (extension_id == '.air'):
                lines = get_file_contents(file_path)
                station = lines[0].split(' ')[0]
                start_txt = station + '    Date/time                      Values\n'
                data_start = get_start_line(start_txt, lines)
                air_columns = ['STA', 'Year', 'Month', 'Day', 'Hour', 'Minute', 'EVAP']
                air_df = pd.read_csv(file_path, header=None, sep='\t', skiprows=data_start + 1,names=air_columns)
                air_duplicated = air_df.iloc[[-1]].copy()
                air_duplicated.loc[:, ['Year', 'Month', 'Day', 'Hour', 'Minute']] = [EndDate.year,EndDate.month,EndDate.day,EndDate.hour,EndDate.minute]
                air_duplicated['EVAP'] = 0.0
                air_data = pd.concat([air_df,air_duplicated],axis=0)
                air_header = pd.read_csv(file_path,header=None, sep='\t',nrows=data_start+1)
                air_header.columns = ['STA']
                final_air_data = pd.concat([air_header,air_data],axis=0)
                final_air_data[['Year', 'Month', 'Day', 'Hour', 'Minute']]=final_air_data[['Year', 'Month', 'Day', 'Hour', 'Minute']].apply(lambda x: pd.to_numeric(x, errors='coerce').astype('Int64'))
                final_air_data.to_csv(os.path.join(out_dir,filename),sep='\t',header=None, index=False)
                
            if (extension_id == '.pre'):
                try:
                    pre_df = pd.read_csv(file_path, names=["datetime", "value", "QAflag"], header=0)
                    pre_df = pre_df[["datetime", "value"]]
                except:
                    pre_df = pd.read_csv(file_path, names=["datetime", "value"], header=None) 
                pre_df['datetime'] = pd.to_datetime(pre_df["datetime"])
                pre_df['datetime'] = pre_df['datetime'].dt.strftime('%m/%d/%Y %H:%M:%S') 
                pre_data = pd.concat([pd.DataFrame({'datetime': [StartDate], 'value': [0.0]}),pre_df,pd.DataFrame({'datetime': [EndDate], 'value': [0.0]})],ignore_index=True)
                pre_data['datetime'] = pd.to_datetime(pre_data['datetime']).dt.strftime('%m/%d/%Y %H:%M:%S')
                pre_data.to_csv(os.path.join(out_dir,filename),header=None,index=False)
        except:
            print("Issue Reading: ", file_path)      
