import os
from pathlib import Path
from typing import Any, List, Tuple
import xarray as xr
import pandas as pd
import numpy as np
from ..base import DataStager
from src.core.miscellaneous import (DateRanger,read_nldas_pre,read_prism)
from src.core.models import (ProjectControl, DataRequest)
import warnings
warnings.filterwarnings("ignore")


# just for Terminal Runs
#from src.etl.base import DataStager

class StagePrism(DataStager):

    def create_stage_request(self, project: ProjectControl):
        pass
    
    def stage(self, project: ProjectControl):
        print(f"\tStaging PRISM data for project: {project.request_control.project_name}")
        request_control = project.request_control
        prism_disaggregated_storage = Path(project.storage.prism.staged)
        prism_transformed_storage = Path(project.storage.prism.candidate)
        nldas_staged_storage = Path(project.storage.nldas.staged)
        prism_input = project.prismMap.data
        start_datetime = request_control.start_date
        end_datetime = request_control.end_date

        # Write Candidate PRISM data (slightly transformed)--> unit converted to inches 
        StagePrism.write_prism_grid_csv(project)

        # Read staged NLDAS data for disaggregation
        for index, row in prism_input.iterrows():
            # File naming
            nldas_file_prefix = "nldas_" + str(row['nldas_id'])
            nldas_file = next(nldas_staged_storage.glob(f"{nldas_file_prefix}*"),None)
            #print(nldas_file)

            prism_file_prefix = str(row['prism_id'])
            prism_file = next(prism_transformed_storage.glob(f"{prism_file_prefix}*"),None)
            #print(prism_file)

            # Disaggregate PRISM monthly to Hourly.
            disaggregated_PRISM, modified_months_information_each_grid  = StagePrism.create_gridded_pre_file(nldas_file, prism_file,start_datetime,end_datetime,row['prism_id'])

            # #Drop 0 values from PRE files
            disaggregated_PRISM = disaggregated_PRISM[disaggregated_PRISM['hourly_data'] > 0]
            disaggregated_PRISM = disaggregated_PRISM.dropna()

            # Write Staged PRISM or Standalone PRISM pre files.
            prism_output_filename = prism_disaggregated_storage / (str(row['prism_id']) +'.pre')
            disaggregated_PRISM.to_csv(prism_output_filename,index=False,header=False,sep=",",na_rep="NaN", date_format = '%m/%d/%Y %H:%M:%S')   

        return print('PRISM data is staged and standalone PRE files are ready')

    # Function to disaggregate the PRISM data using upscaled remote sensing data
    @staticmethod
    def create_gridded_pre_file(nldas_file, prism_file,start_date,end_date,PRISM_ID):
        nldas_data_merged = read_nldas_pre(nldas_file,start_date,end_date)
        prism_filled = read_prism(prism_file,start_date,end_date)
        merged_prism_and_nldas_data = StagePrism.merge_prism_nldas(nldas_data_merged, prism_filled)
        # 1. If PRISM is non-zero and nldas is non-zero too or (PRISM = NLDAS = 0) OR (PRISM = 0 and NLDAS > 0) 
        other_data = merged_prism_and_nldas_data.loc[~((merged_prism_and_nldas_data['monthly_value_NLDAS'] <= 0) & (merged_prism_and_nldas_data['monthly_value_PRISM'] > 0))].copy()
        other_data['hourly_prism_data'] = other_data['fraction'] * other_data['monthly_value_PRISM']
        # 2. If PRISM is non-zero and NLDAS is zero. 
        zero_nldas_val_prism = merged_prism_and_nldas_data.loc[(merged_prism_and_nldas_data['monthly_value_NLDAS'] <= 0) & (merged_prism_and_nldas_data['monthly_value_PRISM'] > 0)].copy()
        
        if zero_nldas_val_prism.empty:
            combined_df = other_data.copy()
            modified_months_information_each_grid = pd.DataFrame() # no information
        else:
            zero_nldas_val_prism_new, modified_months_information_each_grid = StagePrism.find_closest_fraction(zero_nldas_val_prism,other_data, PRISM_ID)

            #check to make sure that the modified and original zero_nldas_val_prism has the same number of rows and columns
            if zero_nldas_val_prism.shape != zero_nldas_val_prism_new.shape:
                raise ValueError(f"The modified and orignial zero_nldas_val_prism do not have the same shape for prism file: {prism_file}")
            
            combined_df = pd.concat([other_data, zero_nldas_val_prism_new])                                                                 # Appending data to the previously created empty folder
            combined_df = combined_df.sort_values('datetime')
        
        # QA/QC : Confirming derived hourly values - Are they sum up to the Monthly PRISM value?
        combined_df = combined_df.set_index('datetime')
        PRISM_hourly_precip_aggregated_monthly = combined_df['hourly_prism_data'].resample('M',level=0).sum()
        PRISM_monthly_precip_data_raw = combined_df['monthly_value_PRISM'].resample('M',level=0).mean()
        if (round(PRISM_hourly_precip_aggregated_monthly.sum() - PRISM_monthly_precip_data_raw.sum(),5))  != 0:
            raise ValueError(f'The PRISM monthly raw precip data and disaggreaged hourly data do not sum up to the same value for file: {prism_file}')
                
        # Extracting required columns only and sorting the data datetime wise
        combined_df.reset_index(inplace=True)
        combined_df = combined_df[['datetime','hourly_prism_data']]
        combined_df.columns = ['datetime','hourly_data']
        combined_df = combined_df.sort_values('datetime')
        combined_df['datetime']  = pd.to_datetime(combined_df['datetime'])
        return combined_df, modified_months_information_each_grid
    
    
    @staticmethod
    def merge_prism_nldas(nldas_data_merged, prism_filled):
        prism_nldas_data_merged = pd.merge(nldas_data_merged,prism_filled, on='dateMonth')
        prism_nldas_data_merged['hourly_prism_data'] = np.nan
        prism_nldas_data_merged = prism_nldas_data_merged[['datetime','dateMonth','monthly_value_NLDAS','fraction','monthly_value_PRISM','hourly_prism_data']]
        return prism_nldas_data_merged
    
    @staticmethod
    def find_closest_fraction(filtered_dataframe,other_dataframe, PRISM_ID):
        '''
        Modified to search the monthly fraction value to use for the monthly disaggreation of PRISM for which the corresponding NLDAS has zero monthly precipitation
        - based on: same month across all years and precipitatin amount
        - if the NLDAS precipitatin amount is not within 10% of the PRISM monthly precipitation - also search for the month prior and after to see if there is a NLDAS month
        which has monthly precipitation close to the PRISM monthly being disaggregated
        '''
        modified_zero_nldas_val_prism = pd.DataFrame(columns = filtered_dataframe.columns)
        unique_months = filtered_dataframe['dateMonth'].unique()
        
        modified_months_information_each_grid = pd.DataFrame()
               
        #print("PRISM has a value and NLDAS Doesnt for these months - ",unique_months)
        for target in unique_months:
            #print(target)
            #working only on the subset dataframe for the "target" month
            target_month_dataframe = filtered_dataframe[filtered_dataframe['dateMonth'] == target]
            #Getting the PRISM precipitation that needs to be matched with the NLDAS precip
            Target_month_PRISM_Precip = target_month_dataframe['monthly_value_PRISM'].mean()
            #Getting the month of the PRISM precipitation
            Target_month_number = target.to_timestamp().month
            #Target_month_number = pd.to_datetime(target).month
            #Filtering the complete dataframe for only the target month
            month_df = other_dataframe[other_dataframe['datetime'].dt.month == Target_month_number]
            
            if Target_month_number == 2:
                data_point_in_target_month = target_month_dataframe['datetime'].dt.date.nunique()
                # Group by 'dateMonth' and count unique days per group
                month_lengths = month_df.groupby('dateMonth')['datetime'].apply(lambda x: x.dt.date.nunique())
                valid_feb_months = month_lengths[month_lengths == data_point_in_target_month].index
                # Only keep rows for Februarys with matching number of days
                month_df = month_df[month_df['dateMonth'].isin(valid_feb_months)]
                
            #Finding the month that closely matches the NLDAS monthy precip to the PRISM monthly precip
            month_df_subset = month_df[['dateMonth', 'monthly_value_NLDAS']].drop_duplicates()
            month_df_subset['diff'] = (month_df_subset['monthly_value_NLDAS'] - Target_month_PRISM_Precip).abs()
            min_diff = month_df_subset['diff'].min()
                
            closest_months = month_df_subset[month_df_subset['diff'] == min_diff].sort_values('dateMonth')
            #if there are more than 1 closest months, chose the first month to get the associated rainfall fraction:
            selected_month = closest_months.iloc[0]['dateMonth']
            #subsetting the month dataframe for the selected month
            selected_month_dataframe = month_df[month_df['dateMonth'] == selected_month]
            #replacing the 'monthly_value_NLDAS' and 'fraction' from the selected month dataframe in the target_month_dataframe
            columns_to_replace = ['monthly_value_NLDAS','fraction']
            target_month_dataframe.loc[:,columns_to_replace] = selected_month_dataframe[columns_to_replace].values
            #calculating the PRISM hourly disaggregated value based on the selected NLDAS month
            target_month_dataframe['hourly_prism_data'] =  target_month_dataframe['monthly_value_PRISM']*target_month_dataframe['fraction']
            #appending to the final modified dataframe
            modified_zero_nldas_val_prism = pd.concat([modified_zero_nldas_val_prism,target_month_dataframe])
                        
            #getting the information to append
            information_to_append = pd.DataFrame([{"PRISM_ID": PRISM_ID, "PRISM_Target_month":target, "PRISM_Target_month_precip":Target_month_PRISM_Precip, "selected_NLDAS_month":selected_month,
                                                "Selected_NLDAS_month_precip":selected_month_dataframe['monthly_value_NLDAS'].mean()}])
            modified_months_information_each_grid = pd.concat([modified_months_information_each_grid, information_to_append], axis = 0)                
        
        return modified_zero_nldas_val_prism, modified_months_information_each_grid
    
    @staticmethod
    def write_prism_grid_csv(project: ProjectControl):
        print("\tTranslating project control data for data stager.")
        request_control = project.request_control
        raw_storage = Path(project.storage.prism.raw)
        transformed_storage = Path(project.storage.prism.candidate)
        #os.makedirs(transformed_storage, exist_ok=True)

        start_datetime = request_control.start_date
        end_datetime = request_control.end_date
        
        # Read Lookup table for PRISM_ID and Lat-Lon combination
        #prism_lookup_filepath = Path("..\src\core\PRISM_Centroids_WGS84_CA_LookUp.csv").resolve()
        #prism_lookup = pd.read_csv(prism_lookup_filepath,usecols=['PRISM_ID','lat','lon'])
        prism_lookup = project.prism.data['prism_id'].unique()
        datetime_ranges = DateRanger(start_datetime,end_datetime)
        combined_df = pd.DataFrame()    # Create empty dataframe to append the data later on.
        # Read Raw PRISM data and write into a intermediate files (.csv for each grid)
        for date_range in datetime_ranges:
            ncfilename = r"prism_ppt_us_25m_" + str(date_range) +".nc"          
            prism_netcdf_filepath = raw_storage / ncfilename

            # Read netcdf file
            prism_df = StagePrism.read_prism_netcdf(prism_netcdf_filepath,date_range)
    
            # Combine monthly data in a single data frame. Column format is like this ['PRISM_ID','Lat','Lon','YYYY1MM1','YYYY1MM2',.....]
            if combined_df.empty:
                combined_df = prism_df.copy()
            else:
                combined_df = pd.merge(combined_df,prism_df,on=['PRISM_ID','lat','lon'],how='outer')
        # Write grid-wise data in csv.
        combined_df_CA = combined_df[combined_df['PRISM_ID'].isin(prism_lookup)]
        StagePrism.save_grids_as_csv(combined_df_CA,transformed_storage)

    @staticmethod
    def read_prism_netcdf(filepath:Path,year_month:str) ->  pd.DataFrame:
        """
        Read Raw PRISM .nc file and transform data into dataframe with format: PRISM_ID, lat, lon, YYYYMM
        Also, convert units (mm) to inches
        """
        # Read netcdf file
        prism_nc = xr.open_dataset(filepath)
        
        # Extract Band1 variable. It corresponds to 'precipitation'.
        band = prism_nc['Band1'].values

        # Extract coordinates
        lat = prism_nc['lat'].values
        lon = prism_nc['lon'].values

        # Truncate coordinates to 5 decimals 
        lat = np.trunc(lat * 1e5) / 1e5
        lon = np.trunc(lon * 1e5) / 1e5
            
        # Convert to dataframe with rownames as Lat and columnnames as Lon.    
        df_band = pd.DataFrame(band,index=lat,columns=lon)

        # Flipped the order of rows (first row in original dataframes becomes Last). 
        # This is done to match the PRISM_IDs developed from previous scripts that utilize .bil format.
        df_band_flipped = df_band.iloc[::-1,:]      
        df_band_flipped_transposed = df_band_flipped.T.reset_index()

        df_band_flipped_transposed.rename(columns={'index': 'lon'}, inplace=True)

        # Convert the dataframe from wider to long format       
        melted = pd.melt(df_band_flipped_transposed, id_vars=['lon'], var_name='lat', value_name='Band1')

        # Enforcing lat and lon columns to be float
        melted['lat'] = melted['lat'].astype(float)
        melted['lon'] = melted['lon'].astype(float)

        # Row Index becomes PRISM_IDs. Renaming the index to 'PRISM_ID' for readability
        melted.reset_index(inplace=True)
        melted.rename(columns={'index': 'PRISM_ID'}, inplace=True)
        melted['PRISM_ID'] = melted['PRISM_ID']+1                                       # This is done to start the numbering from 1 instead of 0
        melted = melted.dropna()                                                        # Droping rows with Nans
        melted['Band1'] = melted['Band1']/25.4                                          # Unit conversion from mm to inches
        melted = melted[melted['Band1']>.01]                                            # Filter out entries with ppt > 0.01 inch
        melted.rename(columns={'Band1':str(year_month)},inplace=True)                   # Rename the 'Band1' column as "YYYYMM"
        return melted       


    @staticmethod
    def save_grids_as_csv(nc_dataframe, output_dir):
        """
        Save each row of a DataFrame as a separate CSV.
        First three cells are ignored in the content but used for naming the file.
        File name format: cell1_cell2_cell3.csv
        """
        print("\t Saving PRISM data in separate grid-wise CSVs.")
        for idx, row in nc_dataframe.iterrows():
            # Extract first three cells for naming
            name_parts = [f"{row.iloc[i]}" for i in range(3)]
            name_parts[0] = name_parts[0].split(".")[0]
            file_name = "_".join(name_parts) + ".csv"
            file_path = os.path.join(output_dir, file_name)

            # Ignore first three cells in content and thus, Convert remaining cells to DataFrame
            row_data = row.iloc[3:].to_frame().reset_index()
            row_data = row_data.dropna()

            # Convert first column to datetime
            row_data.iloc[:, 0] = pd.to_datetime(row_data.iloc[:, 0], format='%Y%m').dt.date
            # Save to CSV
            try: 
                row_data.to_csv(file_path, index=False,header=False)
            except:
                print("Error saving:",file_name)    
        print('\t Done saving Candidate files!')
