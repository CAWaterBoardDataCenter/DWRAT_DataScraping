##This script reads in the daily intermediate files created for the ground based station and generates flags
"""
Output Excel file1: "Ground_stations_Flagged_1_2_3.xlsx"
 - This file outputs all the ground-based stations at columns along with 3 Flags
  - Flag 1: Flags daily rainfall greater than the NOAA-100yr-24 hr storm - Use the "Flag1" Column to filter for flagged cells - Highlighted in Red bold text.  
  - Flag 2A: Spatial outliers: This flags rainfall that are more than two standard deviation greater than the surrounding rainfall for that day. Currently flags storms > 1 inch - Highlighted as light blue cell
  - Flag 2B: Temporal outliers: This flags rainfall that are more than 1 inches if the prior and post 3 days have 0 rainfall - color PURPLE
  - Flag 3: Continuous rainfall: For eachs station, flags if rainfall is continuous for more than 15 days - Highlights the cells as brown
  
Output Excel file2: "Flag4_Monthly_Only_CompleteData"
  - Flag 4: Calculates the monthly sum of precipitation for each station - using only complete months - and flags for stations with rainfall more than two standard deviation (highlighted as Red) - and less than two standard deviation - as blue cell. 
          

"""
import os
import shutil
import pandas as pd
import numpy as np

from src.core.models import ProjectControl
from src.etl.util.helpers import StationNameHelpers

# Function to copy the raw daily gage data into candidate folder. Hourly data is skipped from QC.
def consolidate_gage_data(project: ProjectControl) -> None:
    """
    Copy all gage data files from raw subdirectories into candidate folder.

    Args:
        project: ProjectControl with storage paths
    """
    candidate_dir = project.storage.gage.candidate

    intermediate_dir = candidate_dir / "IntermediateFiles"
    intermediate_dir.mkdir(parents=True, exist_ok=True)

    source_dirs = {
        'cdec': project.storage.gage.cdec.raw,
        'lcd': project.storage.gage.lcd.raw,
        'noaa': project.storage.gage.noaa.raw,
        'raws': project.storage.gage.raws.raw,
    }

    copied_count = 0
    skipped_count = 0
    hourly_skipped = []

    for source_name, source_dir in source_dirs.items():
        if not source_dir.exists():
            print(f"Warning: {source_name} directory not found at {source_dir}, skipping...")
            continue

        csv_files = list(source_dir.glob('*.csv'))
        pre_files = list(source_dir.glob('*.pre'))
        all_files = csv_files + pre_files

        print(f"Found {len(all_files)} files in {source_name}/")

        for source_file in all_files:
            if '_hourly' in source_file.stem.lower():
                hourly_skipped.append(source_file.name)
                skipped_count += 1
                continue

            destination_file = intermediate_dir / f"{source_file.stem}.pre" # force pre extension

            if destination_file.exists():
                print(f"Warning: {destination_file.name} already exists in candidate folder, skipping...")
                skipped_count += 1
                continue

            shutil.copy2(source_file, destination_file)
            copied_count += 1

    print(f"Consolidation complete: {copied_count} files copied, {skipped_count} files skipped")

    if hourly_skipped:
        print(f"\nNote: Skipped {len(hourly_skipped)} hourly resolution files (QC only processes daily data):")
        for filename in hourly_skipped:
            print(f"  - {filename}")


################################################################# Main QC Part #####################################################

def run_obs_data_qc(project: ProjectControl):
    ## Step 1: Import Data
    consolidate_gage_data(project)

    candidate_dir = project.storage.gage.candidate

    intermediate_dir = candidate_dir / "IntermediateFiles"
    intermediate_dir.mkdir(parents=True, exist_ok=True)

    qc_dir = candidate_dir / "QCSpreadsheets"
    qc_dir.mkdir(parents=True, exist_ok=True)

    start_date = project.request_control.start_date
    end_date = project.request_control.end_date

    date_range = pd.date_range(start=start_date, end=end_date)
    df_master = pd.DataFrame(date_range, columns=['Date'])
    df_master['Date'] = pd.to_datetime(df_master['Date'])
    df_master['Date'] = df_master['Date'].dt.strftime('%m-%d-%Y')


    ObsDailyFiles = [] 
    ObsDailyFiles += [each for each in os.listdir(intermediate_dir) if each.endswith('.pre')]
    for filename in ObsDailyFiles: # Loop through each file in the input directory
        if filename.endswith('.pre'):
            input_csv = os.path.join(intermediate_dir, filename)
            staname = filename.rsplit('.', 1)[0]
            #staname = staname.upper().replace(" ", "")
            df = pd.read_csv(input_csv, header=None)
            print(input_csv)
            df = df.iloc[:, :2]
            df.columns = ['Date', staname]
            df['Date'] = pd.to_datetime(df['Date'])
            df['Date'] = df['Date'].dt.strftime('%m-%d-%Y')
            df_master = pd.merge(df_master, df, on='Date', how='left')      
    #print(df_master)
    num_cols = df_master.select_dtypes(include="number").columns
    df_master[num_cols] = df_master[num_cols].mask(df_master[num_cols] < 0, np.nan)
    df_master['Date'] = pd.to_datetime(df_master['Date'])
    print("Processing Flag 1............")

    #Reading the ground staton NOAA-100yr-24hr storm from a csv file
    #Ground_station_data = pd.read_csv("CSV file with the station name and NOAA-100yr-24hr storm.csv")

    #creating a style map that combines the styles for all different flags
    style_map = pd.DataFrame('',index = df_master.index, columns = df_master.columns)

    ### Flag 1: flagging rainfall that has rainfall values greater than the NOAA_100_Year for each station

    station_id_list = df_master.columns[1:].tolist()
    agencies = ['NOAA', 'RAWS', 'CDEC', 'LCD']
    cleaned_station_ids = []

    for station_id in station_id_list:
        # split by underscores
        parts = station_id.split('_')

        # find where the agency name appears
        agency_idx = None
        for i, part in enumerate(parts):
            if part.upper() in agencies:
                agency_idx = i
                break

        # extract everything before the agency name
        if agency_idx is not None:
            clean_id = '_'.join(parts[:agency_idx])
        else:
            # remove 'daily' or 'hourly' suffix if present
            if parts[-1] in ['daily', 'hourly']:
                clean_id = '_'.join(parts[:-1])
            else:
                clean_id = station_id

        cleaned_station_ids.append(clean_id)

    # get clean_id -> gage_id mapping from Gage sheet
    gage_sheet = project.gage.data

    id_to_gage = {}
    for _, row in gage_sheet.iterrows():
        station_id = row['station_id']
        id_to_gage[station_id] = row['gage_id']

    # get gage_id -> noaa_100 mapping from GageMapping sheet
    gage_mapping = project.gageMap.data
    gage_to_threshold = gage_mapping.set_index('gage_id')['noaa_100'].to_dict()


    # create final station_id -> noaa_100 threshold lookup
    thresholds = {}
    missing_stations = []

    # Create a mapping: original_name -> cleaned_name
    station_name_map = dict(zip(station_id_list, cleaned_station_ids))

    for original_name, cleaned_id in station_name_map.items():
        gage_id = id_to_gage.get(cleaned_id)

        if gage_id is None and '_' in cleaned_id:
            colon_version = cleaned_id.replace('_', ':', 1) # remove any underscores and replace with colon if present
            matching_row = gage_sheet[gage_sheet['station_id'] == colon_version]

            if not matching_row.empty:
                gage_id = matching_row.iloc[0]['gage_id']

        if gage_id is None:
            missing_stations.append((original_name, f"'{cleaned_id}' not in Gage sheet"))
            continue

        threshold = gage_to_threshold.get(gage_id)

        if threshold is None or pd.isna(threshold):
            missing_stations.append((original_name, f"No noaa_100 for gage_id={gage_id}"))
            continue

        # Store with ORIGINAL name (with suffix) to match df_master columns
        thresholds[original_name] = threshold

    print(f"\nLoaded thresholds for {len(thresholds)}/{len(station_id_list)} stations")

    if missing_stations:
        print(f"\nWarning: {len(missing_stations)} stations missing thresholds:")
        for station_id, reason in missing_stations:
            print(f"  - {station_id}: {reason}")

    Rainfall_outlier_NOAA_100_yr = []
    # Loop through each column and row

    #updating the station list so that it loops through only the stations in the df_master
    station_list = df_master.columns[1:]

    #Initializing a set to 
    flag1_rows = pd.Series(False, index=df_master.index)
    for station in station_list:
        max_val = thresholds[station]
        #print(station, max_val)
        for idx, val in df_master[station].items():
            if val > max_val:
                Rainfall_outlier_NOAA_100_yr.append({
                    'Date': df_master.iloc[idx,0],
                    'Row': idx + 2,  #For indexing beginning 1 and counting the column header as index 1
                    'Column': station, 
                    'Value': val, 
                    'NOAA-100-yr-24hr': max_val})
                style_map.at[idx, station] += 'background-color: red; font-weight: bold'
                
                #Marking the row as needing a flag
                flag1_rows.at[idx] = True

            
    ##
    Rainfall_outlier_NOAA_100_yr = pd.DataFrame(Rainfall_outlier_NOAA_100_yr)

    output_file = qc_dir / "Flag1_NOAA_100_yr_storm.csv"
    Rainfall_outlier_NOAA_100_yr.to_csv(output_file)


    print("Processing Flag 2............part 1")
    ### Flag 2 - part 1: Outlier storm - Spatial evaluation
    #check for each day across each station and flag if value in a certain station is greater than 4 times the std dev

    spatial_outliers = []
    flag2A_rows = pd.Series(False, index=df_master.index)

    for index, row in df_master.iterrows():
        row = row[station_list].replace(-9999,np.nan)
        #print(index)    
        mean = row.mean(skipna = True)
        std = row.std(skipna = True)
        threshold = mean + 4 * std
        
        
        for col in station_list:
            val = row[col]
            #print(val)
            if pd.notna(val) and val != -9999 and val > threshold:
                if threshold > 0.9:
                    spatial_outliers.append({
                        'Row': row.name + 2,  # +2 to account for Excel-style row (1-based + header row)
                        'Column': col,
                        'Value': val
                    })
                    
                    # Apply light blue style to the full row
                    style_map.at[index, col] += 'background-color: lightblue; font-weight: bold'
                    #Marking the row as needing a flag
                    flag2A_rows.at[index] = True
            

    print("Processing Flag 2............part 2")
    ### Flag 2 - part 2: Outlier storm - temporal evaluation
    #For each station,flag a storm event greater than 1 inch if the prior
    flag2B_rows = pd.Series(False, index=df_master.index)
    for station in station_list:
        # Replace known missing value code with NaN
        series = df_master[station].replace(-9999, np.nan)

        # Loop through each value and check the conditions
        for i in range(3, len(series) - 3):
            current = series.iloc[i]
            if current >= 1:
                before = series.iloc[i-3:i]
                after = series.iloc[i+1:i+4]

                # Check if all values in the window are 0, NaN, or -9999 (already replaced)
                if all((before.fillna(0) == 0)) and all((after.fillna(0) == 0)):
                    row_index = series.index[i]
                    style_map.at[row_index, station] += 'background-color: plum; font-weight: bold'
                    flag2B_rows.iloc[row_index] = True



    print("Processing Flag 3............")
    # ##Flag 3: Continuous rainfall for more than 10 days at each station
    flag3_rows = pd.Series(False, index=df_master.index)
    for station in station_list:
    # Create a boolean mask of >0 values
        series = df_master[station].replace(-9999,np.nan)
        mask = series > 0

        # Convert boolean mask to groups of consecutive True values
        group = (mask != mask.shift()).cumsum()  # ID run groups
        grouped = mask.groupby(group)

        for grp_id, grp in grouped:
            if grp.all() and len(grp) >= 15:
                # Highlight the matching rows for this column
                style_map.loc[grp.index, station] = 'background-color:orange'  
                
                flag3_rows.loc[grp.index] = True
                



    print("Processing Flag 4............")
    # ## Flag 4: Look at the monthly and annual across the station and highight the month and year if greater than 3 std dev when evaluated for each month across stations
    df_master_clean = df_master.replace(-9999, np.nan)
    df_master_clean['Date'] = pd.to_datetime(df_master_clean['Date'])

    # Add Year and Month columns for grouping
    df_master_clean['Year'] = df_master_clean['Date'].dt.year
    df_master_clean['Month'] = df_master_clean['Date'].dt.month

    # Group by Year and Month
    monthly_groups = df_master_clean.groupby(['Year', 'Month'])

    # Prepare a monthly summary DataFrame
    monthly_sums = monthly_groups[station_list].sum(min_count=1)

    # Count valid days per month per station
    monthly_counts = monthly_groups[station_list].count()
    calendar_days = monthly_groups.size().unstack(fill_value=0)

    # Get a DataFrame with the expected number of days per month
    expected_days = df_master_clean.groupby(['Year', 'Month']).size()

    # Create completeness mask: True if station has all days filled in that month
    monthly_complete = monthly_counts.eq(expected_days, axis=0)

    # Create style map for output styling
    monthly_style_map = pd.DataFrame('', index=monthly_sums.index, columns=station_list)

    # Perform outlier detection per month across stations (only if complete)
    for (year, month), row in monthly_sums.iterrows():
        #print(year, month)
        completeness_row = monthly_complete.loc[(year, month)]
        #print(completeness_row)
        valid_stations = [s for s in station_list if completeness_row[s]]
        #print(valid_stations)    
        if len(valid_stations) >= 2:  # Require at least 2 valid stations
            values = row[valid_stations]
            mean = values.mean()
            std = values.std()
            high_threshold = mean + 3 * std
            low_threshold = mean - 3 * std
            low_threshold = 0 if low_threshold < 0 else low_threshold

            for station in valid_stations:
                if row[station] > high_threshold:
                    monthly_style_map.at[(year, month), station] = 'font-weight: bold; background-color:red'
                elif row[station] <= low_threshold:
                    monthly_style_map.at[(year, month), station] = 'font-weight: bold; background-color:blue'
    # Style and export
    styled_monthly = monthly_sums.style.apply(lambda _: monthly_style_map, axis=None)

    output_file = qc_dir / "Flag4_Monthly_Only_CompleteData.xlsx"
    styled_monthly.to_excel(output_file, engine='openpyxl', index=True)




    ###Adding the flag columns as well as styling (Flags 1, 2, and 3)
    # Add the 'Flag' column to df_master using flag_rows
    df_master['Flag1'] = flag1_rows.replace({True: 'flag1', False: None})
    df_master['Flag2A'] = flag2A_rows.replace({True: 'flag2A', False: None})   
    df_master['Flag2B'] = flag2B_rows.replace({True: 'flag2B', False: None})      
    df_master['Flag3'] = flag3_rows.replace({True: 'flag3', False: None})     
    styled_df = df_master.style.apply(lambda _: style_map, axis=None)

    output_file = qc_dir / "Ground_stations_Flagged_1_2_3.xlsx"
    styled_df.to_excel(output_file, engine='openpyxl', index=False)







