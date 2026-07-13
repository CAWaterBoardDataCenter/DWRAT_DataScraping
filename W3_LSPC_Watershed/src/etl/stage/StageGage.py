import os
from pathlib import Path
from typing import Any, List, Tuple
import pandas as pd
import numpy as np
from ..base import DataStager
from src.core.miscellaneous import (DateRanger,read_nldas_pre,read_prism)
from src.core.models import (ProjectControl, DataRequest)

class StageGage(DataStager):

    def create_stage_request(self, project: ProjectControl):
        pass
    
    def stage(self, project: ProjectControl):
        print(f"\tStaging Gage data for project: {project.request_control.project_name}")
        request_control = project.request_control
        gage_storage = project.storage.gage.candidate / "QCIntermediateFiles"         # QC Gaged data
        gage_stage_storage = project.storage.gage.staged 
        nldas_staged_storage = project.storage.nldas.staged                          # Staged Nldas file
        start_datetime = request_control.start_date
        end_datetime = request_control.end_date
        gage_stage_storage.mkdir(parents=True, exist_ok=True)
      
        gage_lookup = project.gageMap.data                                              # PRE mapping table.
        gage_table = project.gage.data
        gage_mapping = pd.merge(gage_lookup,gage_table,on='gage_id')
        gage_mapping['station_id'] = gage_mapping['station_id'].str.replace(':', '_')

        # Loop over all gages
        for index, row in gage_mapping.iterrows():
            # Read Hourly NLDAS data 
            print("Reading NLDAS file")
            nldas_file_prefix = "nldas_" + str(row['nldas_id'])
            nldas_file = next(nldas_staged_storage.glob(f"{nldas_file_prefix}*"),None)
            print(nldas_file)
            NLDAS_hourly_daily_merged = StageGage.read_nldas_gage(nldas_file,start_datetime,end_datetime)

            # Read Daily Gage data
            print("Reading Gage file")
            gage_file_prefix = str(row['station_id'])
            gage_file = next(gage_storage.glob(f"{gage_file_prefix}*"),None)
            print(gage_file)
            Ground_station_daily_data = StageGage.read_gage(gage_file,start_datetime,end_datetime)

            print("Merge NLDAS-Gage Data")
            # Merge NLDAS and Gage Together
            Ground_station_and_NLDAS_merged_data = StageGage.merge_gage_nldas(NLDAS_hourly_daily_merged,Ground_station_daily_data)

            # Disaggregate Gage Daily to Hourly.
            print("Disaggregation begins")
            #print(Ground_station_and_NLDAS_merged_data.head(5))
            #Ground_station_and_NLDAS_merged_data.to_csv(r"C:\Users\ashish.kondal\Desktop\MainDirectory\Repos\test\Mattole\candidate\gage\QCIntermediateFiles\GHCND_US1CAHM0091_NOAA_daily_Trial.pre")
            downscaled_observation  = StageGage.downscale_observation(Ground_station_and_NLDAS_merged_data)

            # Filling gap with -9999.
            print("Gap Filling Done")
            downscaled_observation_gapfilled = StageGage.gap_filler(downscaled_observation,start_datetime,end_datetime)

            # Write Staged Gaged Data.
            print("Write Gage Data")
            gage_output_filename = gage_stage_storage / f"{row['station_id']}_{str(row['agency_id']).upper()}_hourly.pre"
            downscaled_observation_gapfilled.to_csv(gage_output_filename,index=False,header=False,sep=",",na_rep="NaN", date_format = '%m/%d/%Y %H:%M:%S')   
        return print('Staging Done for Gage data')

     
    @staticmethod
    def read_nldas_gage(nldas_file,start_date,end_date):
        try:
            NLDAS_data = pd.read_csv(nldas_file, header =0)
            NLDAS_data = NLDAS_data[['local_time','Rainf']]
        except FileNotFoundError:
            print(f"NLDAS File not found: {nldas_file}") 
        # Forcing the "Rainf" column to be numeric. We saw that in observed data that this column does have string type sometimes. 
        NLDAS_data['Rainf'] = pd.to_numeric(NLDAS_data['Rainf'],errors='coerce')
        if (NLDAS_data['Rainf'].isna().any()):
            print("NA's introuduced during Coercion = ",NLDAS_data['Rainf'].isna().sum())
            raise ValueError(f"Missing values detected in 'hourly_value' column in file: {nldas_file}")
        
        NLDAS_data['local_time'] = pd.to_datetime(NLDAS_data['local_time'])                                 # Making sure that the "local_time" columns has correct data type.
        end_exclusive = end_date + pd.Timedelta(days=1)
        NLDAS_data = NLDAS_data[(NLDAS_data['local_time'] >= start_date) & (NLDAS_data['local_time'] < end_exclusive)]
        NLDAS_data = NLDAS_data.rename(columns = {'local_time':'datetime','Rainf':'NLDAS_hourly_value'})

        #Aggregating the hourly precipitatio data to daily data.
        NLDAS_data = NLDAS_data.set_index('datetime')
        NLDAS_data_daily = NLDAS_data.resample('D',level=0).sum()
        NLDAS_data_daily = NLDAS_data_daily.reset_index()
        NLDAS_data_daily = NLDAS_data_daily.rename(columns = {'datetime':'date','NLDAS_hourly_value':'NLDAS_daily_value'})
        #NLDAS_data_daily.columns = ['date','NLDAS_daily_value']
        NLDAS_data = NLDAS_data.reset_index()

        #Merging the hourly and daily data and generating the hourly distribution of rainfall as a fraction of the daily total
        NLDAS_hourly_daily_merged = pd.merge_asof(NLDAS_data, NLDAS_data_daily,left_on='datetime',right_on='date')
        NLDAS_hourly_daily_merged['NLDAS_hourly_fraction'] = NLDAS_hourly_daily_merged['NLDAS_hourly_value']/NLDAS_hourly_daily_merged['NLDAS_daily_value']
        NLDAS_hourly_daily_merged['NLDAS_hourly_fraction'] = NLDAS_hourly_daily_merged['NLDAS_hourly_fraction'].fillna(0)
        NLDAS_hourly_daily_merged['NLDAS_hourly_fraction'] = NLDAS_hourly_daily_merged['NLDAS_hourly_fraction'].replace([np.inf, -np.inf],0)
        return  NLDAS_hourly_daily_merged

    @staticmethod
    def read_gage(gage_file,start_date,end_date):
        end_exclusive = end_date + pd.Timedelta(days=1)
        #Extracting and reading the corresponding ground station precipitation data
        Ground_station_daily_data = pd.read_csv(gage_file, header = None)
        ##Keeping only the first two columns and removing any additional column
        Ground_station_daily_data = Ground_station_daily_data.iloc[:,0:2]
        Ground_station_daily_data.columns = ['date','Obs_daily_value']
        #formatting the value column to numeric and the datetime column as date; removing rows with nan
        Ground_station_daily_data['Obs_daily_value'] = pd.to_numeric(Ground_station_daily_data['Obs_daily_value'], errors = 'coerce')
        Ground_station_daily_data =  Ground_station_daily_data[Ground_station_daily_data['Obs_daily_value']>=0]
        Ground_station_daily_data['date'] = pd.to_datetime(Ground_station_daily_data['date'])
        Ground_station_daily_data = Ground_station_daily_data[(Ground_station_daily_data['date'] >= start_date) & (Ground_station_daily_data['date'] < end_exclusive)]
        Ground_station_daily_data = Ground_station_daily_data.dropna(subset = ['Obs_daily_value'])
        return Ground_station_daily_data


    @staticmethod
    def merge_gage_nldas(NLDAS_hourly_daily_merged, Ground_station_daily_data):
        ##Merging the NLDAS and Ground_station_daily_data
        Ground_station_and_NLDAS_merged_data = pd.merge(NLDAS_hourly_daily_merged, Ground_station_daily_data, on='date')
        #Creating an empty column to downscale the observed daily precipitation to hourly data
        Ground_station_and_NLDAS_merged_data['Obs_hourly_disaggregated_value'] = np.nan
        return Ground_station_and_NLDAS_merged_data
    
    @staticmethod
    # Function to downscale the observed data using upscaled remote sensing data
    def downscale_observation(Ground_station_and_NLDAS_merged_data):
        """
        This function downscales the daily ground station observed precipitation into hourly data.
        For each day:
            Scenario 1: If the ground station observed precip and NLDAS both are zero, disaggregates as 0 in precip for all hours
            Scenario 2: If the ground station observed precip = 0 but NLDAS !=0, the ground station data is used to override the NLDAS and
                        disaggregates to 0 for all hours of the day
            Scenario 3: If the ground station observed precip !=0 but NLDAS =0, the code samples all days for the specific month that has NLDAS cumulative daily precip
                        close to the ground station observed precip, and uses the distribution (fraction each hour of the NLDAS) to disaggregate the ground station observed
                        daily precipitation.
            Scenarion 4: If the ground station observed precip != 0 and NLDAS != 0, the code uses the daily value of the ground station observed precip, and the hourly fraction
                        caluculated using the NLDAS to disaggregate the ground station observed daily precipitation.
        """
        
        # Segregating data in 4 group. Following scenarios could occur:
        # 1. If Observation = 0, NLDAS == 0, at any given day. Set the downscaled hourly observed precip as 0
        zero_obs_zero_nldas = Ground_station_and_NLDAS_merged_data.loc[(Ground_station_and_NLDAS_merged_data['Obs_daily_value'] <= 0) & (Ground_station_and_NLDAS_merged_data['NLDAS_daily_value'] <= 0)].copy()
        zero_obs_zero_nldas[['Obs_hourly_disaggregated_value']] = 0
        
        # QA/QC - checking both the Ground station and NLDAS has 0 precipitation for filter above
        if (zero_obs_zero_nldas['Obs_daily_value'].sum() <= 0 and zero_obs_zero_nldas['NLDAS_daily_value'].sum().round(3) == 0):
            pass
            # print("Filter: Observation = 0, NLDAS == 0 Has NO issues!")
        else:
            #raise ValueError("Error! - check zero_obs_zero_nldas")
            print("Error! - check zero_obs_zero_nldas")

        # 2. If Observation = 0, NLDAS != 0, at any given day. Set the downscaled hourly observed precip as 0
        zero_obs_val_nldas = Ground_station_and_NLDAS_merged_data.loc[(Ground_station_and_NLDAS_merged_data['Obs_daily_value'] <= 0) & (Ground_station_and_NLDAS_merged_data['NLDAS_daily_value'] > 0)].copy()
        zero_obs_val_nldas[['Obs_hourly_disaggregated_value']] = 0  

        # QA/QC - checking for condition 2: Observation = 0, NLDAS!= 0
        if (zero_obs_val_nldas['Obs_daily_value'].sum() <= 0 and zero_obs_val_nldas['NLDAS_daily_value'].sum().round(3) > 0):
            pass
            #print("Filter: Observation = 0, NLDAS != 0 Has NO issues!")
        else:
            #raise ValueError("Error! - check zero_obs_val_nldas")
            print(f'Error! - check zero_obs_val_nldas')      
                

        # 3. If Observation != 0, NLDAS = 0, Scan NLDAS data for same location to have similar precipitation (as of Observation) at any other day and use the distribution of that day.
        val_obs_zero_nldas = Ground_station_and_NLDAS_merged_data.loc[(Ground_station_and_NLDAS_merged_data['Obs_daily_value'] > 0) & (Ground_station_and_NLDAS_merged_data['NLDAS_daily_value'] <= 0)].copy()
        
        #Check for situation if the condition for condition 3 is met
        if (len(val_obs_zero_nldas) > 0):
            #creating an empty dataframe that will store the disaggregated hourly 
            modified_val_obs_zero_nldas = pd.DataFrame(columns = val_obs_zero_nldas.columns)
            
            # Loop to find nearest nearest daily remote sensing value to the observed daily value. 
            for target in val_obs_zero_nldas['date'].unique():                                                                                                       # Identifying all the observed daily values for which remote sensing valuers are zero
                #print(target)
                #extract only the dataframe for the day
                temp_dataframe = val_obs_zero_nldas[val_obs_zero_nldas['date']==target]
            
                ##get the Observed precip that needs to match the NLDAS daily precip
                Target_NLDAS_daily_precip = temp_dataframe['Obs_daily_value'].mean()
                
                #get the month of the missing data
                Target_month = target.month
                #Target_month = pd.to_datetime(target).month
                #Target_month = target.to_timestamp().month
                #filtering the main dataframe for only the target month
                month_df = Ground_station_and_NLDAS_merged_data[Ground_station_and_NLDAS_merged_data['date'].dt.month == Target_month]
                
                #subset the dataframe for the month of month_to_match and find the NLDAS daily precip that most closely matches the observed precip
                daily_df = month_df[['date', 'NLDAS_daily_value']].drop_duplicates()
                #Finding the absolute difference to target value
                daily_df['diff'] = (daily_df['NLDAS_daily_value'] - Target_NLDAS_daily_precip).abs()
                min_diff = daily_df['diff'].min()
                closest_days = daily_df[daily_df['diff'] == min_diff].sort_values('date')
                
                #if the closest days are more than 1, select the first closest day
                selected_day = closest_days.iloc[0]['date']
                
                #subsetting the month dataframe for the selected day
                selected_day_dataframe = month_df[month_df['date'] == selected_day]
                
                #replacing the NLDAS_hourly_value NLDAS_daily_value, and NLDAS_hourly_fraction to the temp_dataframe
                columns_to_replace = ['NLDAS_hourly_value','NLDAS_daily_value','NLDAS_hourly_fraction']
                #temp_dataframe.loc[:,columns_to_replace] = selected_day_dataframe[columns_to_replace].values
                
                # 1. Align selected_day_dataframe to temp_dataframe's index
                if len(temp_dataframe) != len(selected_day_dataframe):
                    # Definitely a problem: reindex
                    # selected_aligned = selected_day_dataframe.reindex(temp_dataframe.index, method='ffill')
                    raise ValueError("Missing hours in NLDAS file (row count mismatch)")
                else:
                    # Same length, but are timestamps actually identical?
                    #if not temp_dataframe.index.equals(selected_day_dataframe.index):
                    #    print("Warning: same number of rows, but timestamps differ. Reindexing to be safe.")
                    #    selected_aligned = selected_day_dataframe.reindex(temp_dataframe.index, method='ffill')
                    #else:
                    selected_aligned = selected_day_dataframe

                # 2. Assign aligned values into temp_dataframe
                temp_dataframe.loc[:, columns_to_replace] = selected_aligned[columns_to_replace].to_numpy()

                #calculating the Obs_hourly_disaggregated_value based on the selected NLDAS daily value and fraction
                temp_dataframe['Obs_hourly_disaggregated_value'] = temp_dataframe['Obs_daily_value']*temp_dataframe['NLDAS_hourly_fraction']

                #appending to the final dataframe
                modified_val_obs_zero_nldas = pd.concat([modified_val_obs_zero_nldas, temp_dataframe],ignore_index=True,sort=False)             
                
            # QA/QC 9: Confirming derived hourly values - Are they sum up to the daily observed value?
            daily_sum_Obs_hourly_disaggregated_value = modified_val_obs_zero_nldas.groupby(modified_val_obs_zero_nldas['datetime'].dt.date)['Obs_hourly_disaggregated_value'].sum().reset_index()
            Ground_station_observed_daily_value = modified_val_obs_zero_nldas.groupby(modified_val_obs_zero_nldas['datetime'].dt.date)['Obs_daily_value'].mean().reset_index()
            
            if (daily_sum_Obs_hourly_disaggregated_value['Obs_hourly_disaggregated_value']- Ground_station_observed_daily_value['Obs_daily_value']).sum().round(2) <= 0:
                pass
                #print("Non-Zero observations and Non-Zero NLDAS - Hourly Values are okay!")
            else:
                #raise ValueError('Error! - check modified_val_obs_zero_NLDAS') 
                err_value = (daily_sum_Obs_hourly_disaggregated_value['Obs_hourly_disaggregated_value']- Ground_station_observed_daily_value['Obs_daily_value']).sum().round(2)
                print(f'Error! - check modified_val_obs_zero_NLDAS: {err_value}')        
            
        # 4. If Observation != 0, NLDAS != 0,. Directly use the %-age daily distribution to convert observed daily data into hourly.
        val_obs_val_nldas = Ground_station_and_NLDAS_merged_data.loc[(Ground_station_and_NLDAS_merged_data['Obs_daily_value'] > 0) & (Ground_station_and_NLDAS_merged_data['NLDAS_daily_value'] > 0)].copy()
        val_obs_val_nldas[['Obs_hourly_disaggregated_value']]  = val_obs_val_nldas[['Obs_hourly_disaggregated_value']] .astype(float)
        val_obs_val_nldas['Obs_hourly_disaggregated_value'] = val_obs_val_nldas['NLDAS_hourly_fraction'] * val_obs_val_nldas['Obs_daily_value']
            
        # QA/QC 
        if (val_obs_val_nldas['Obs_daily_value'].sum() > 0 and val_obs_val_nldas['NLDAS_daily_value'].sum() > 0):
            pass
            #print("Filter: Observation != 0, NLDAS != 0 Has NO issues!")
        else:
           #raise ValueError("Error! - check val_obs_val_nldas")
           print("Error! - check zero_obsval_obs_val_nldas_zero_nldas")
            
        # QA/QC - Confirming derived hourly values sum up to the daily observed values)
        
        daily_sum_Obs_hourly_val_obs_val_nldas = val_obs_val_nldas.groupby(val_obs_val_nldas['datetime'].dt.date)['Obs_hourly_disaggregated_value'].sum().reset_index() 
        daily_value_val_obs_val_nldas= val_obs_val_nldas.groupby(val_obs_val_nldas['datetime'].dt.date)['Obs_daily_value'].mean().reset_index()
        
        if (daily_sum_Obs_hourly_val_obs_val_nldas['Obs_hourly_disaggregated_value'] - daily_value_val_obs_val_nldas['Obs_daily_value']).sum().round(3) == 0:
            pass
            #print("Observation != 0, NLDAS != 0 - downscaled hourly values match daily observed values!")
        else:
            raise ValueError("Error! - check val_obs_val_rs for hourly values -")
              
        # Combining data in one-file
        combined_data = pd.concat([zero_obs_zero_nldas, zero_obs_val_nldas, modified_val_obs_zero_nldas, val_obs_val_nldas])
        combined_data = combined_data.sort_index()
        
        #check to see that the final modified dataframe matches the length of the datframe before processing
        if len(combined_data) != len(Ground_station_and_NLDAS_merged_data):
            raise ValueError("Discrepancy in the dataframe length before and after disaggregation")

        #print("FINAL COMBINED Data")        
        combined_data = combined_data[['datetime','Obs_hourly_disaggregated_value','Obs_daily_value']]
        #combined_data = combined_data.sort_values('datetime')
        return pd.DataFrame(combined_data)
    
    @staticmethod
    def gap_filler(downscaled_observation,start_date,end_date):
        ##Creating a complete dataset from start to enddata. Filling the missing Ground station observed days with Nan
        downscaled_observation['Code'] = 1
        downscaled_observation = downscaled_observation[['datetime','Obs_hourly_disaggregated_value','Code']]
        # Convert start and end dates to datetime objects
        start_datetime = pd.to_datetime(start_date, format='%m/%d/%Y %H:%M:%S')
        end_datetime = pd.to_datetime(end_date, format='%m/%d/%Y %H:%M:%S')

        # 1. Convert the 'date' column to datetime if it's not already
        downscaled_observation['datetime'] = pd.to_datetime(downscaled_observation['datetime'])

        # 2. Set the 'date' column as the index
        downscaled_observation = downscaled_observation.set_index('datetime')

        # 3. Create a complete date range with hourly frequency
        date_range = pd.DataFrame(pd.date_range(start=start_datetime, end=end_datetime, freq='H'))
        date_range.columns = ['datetime']
        # 4. Reindex the DataFrame with the complete date range
        df_filled = pd.merge(date_range,downscaled_observation,on="datetime",how="left")

        df_filled['Obs_hourly_disaggregated_value'] = df_filled['Obs_hourly_disaggregated_value'].fillna(-9999) 
        df_filled['Code'] = df_filled['Code'].fillna(255)
        
        df_filled.columns = ['datetime', 'hourly_obs_data','Code']
        return df_filled

