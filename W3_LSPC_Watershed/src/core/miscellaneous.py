from dateutil.relativedelta import relativedelta
import pandas as pd
import numpy as np
import geopandas as gpd
from shapely.geometry import Point


# Generate full list of datetime range between start and end datetimes in YYYYMM format to insert in PRISM fetching url.
def DateRanger(startdate,enddate):
    datetime_ranges = []
    current = startdate.replace(day=1)  # Normalize to first day of month      
    while current <= enddate:
        datetime_ranges.append(current.strftime("%Y%m"))
        current += relativedelta(months=1)  # Move to next month
    
    return datetime_ranges



def read_nldas_pre(nldas_file,start_date,end_date):
    try:
        nldas_data = pd.read_csv(nldas_file, header =0)
        nldas_data = nldas_data[['local_time','Rainf']]
    except FileNotFoundError:
        print(f"NLDAS File not found: {nldas_file}")    
    end_exclusive = end_date + pd.Timedelta(days=1)
    # Forcing the "Rainf" column to be numeric. We saw that in observed data that this column does have string type sometimes. 
    nldas_data['Rainf'] = pd.to_numeric(nldas_data['Rainf'],errors='coerce')
    if (nldas_data['Rainf'].isna().any()):
        print("NA's introuduced during Coercion = ",nldas_data['Rainf'].isna().sum())
        #nldas_data = nldas_data.dropna(subset = ['Rainf'])                              # Droping NA's - that would be there if there's any "string" in "Rainf" column
        raise ValueError(f"Missing values detected in 'hourly_value' column in file: {nldas_file}")
    nldas_data['local_time'] = pd.to_datetime(nldas_data['local_time'])                 # Making sure that the "local_time" columns has correct data type.
    nldas_data = nldas_data[(nldas_data['local_time'] >= start_date) & (nldas_data['local_time'] < end_exclusive)]
    
    # Upscale the remote sensing file - Aggregating hourly data into monthly scale
    nldas_data_upscaled = (nldas_data
                           .groupby(nldas_data['local_time'].dt.to_period('M'))['Rainf']
                           .sum()
                           .reset_index()
                           .rename(columns={'local_time': 'dateMonth', 'Rainf': 'monthly_value_NLDAS'}))
    
    # Convert 'dateMonth' from Period to datetime
    #nldas_data_upscaled['dateMonth'] = pd.to_datetime(nldas_data_upscaled['dateMonth'], format='%Y-%m').dt.to_period('M')   
    nldas_data_upscaled.columns = ['dateMonth','monthly_value_NLDAS']
    nldas_data['dateMonth'] = nldas_data['local_time'].dt.to_period('M')
    # Generate distribution using %-monthly data occuring at each hour      
    nldas_data_merged = pd.merge(nldas_data.sort_values('local_time'),nldas_data_upscaled.sort_values('dateMonth'),left_on='dateMonth',right_on='dateMonth')         # Merging the hourly and daily remote sensing dataframes.
    nldas_data_merged['fraction'] = nldas_data_merged['Rainf']/nldas_data_merged['monthly_value_NLDAS']                                                             # Generating the distribution of precipitation at each hour of the day.
    
    # Finding count on NA's (when hourly and monthly sum = 0, there division would lead to NA):- nldas_data_merged['fraction'].isna().sum()
    # Checking if there is any problem in distribution - The following number should be zero
    if (np.isinf(nldas_data_merged['fraction']).sum() != 0):
        print("There's problem in distribution - The following number should be zero - Final value: ", np.isinf(nldas_data_merged['fraction']).sum())
        raise ValueError(f'Infinity value exists in the NLDAS hourly fraction estimate of monthly values for file: {nldas_file}')
    
    nldas_data_merged['fraction'] = nldas_data_merged['fraction'].fillna(0)
    nldas_data_merged['fraction'] = nldas_data_merged['fraction'].replace([np.inf, -np.inf],0)

    # Fact checking previous step
    nldas_data_fraction = nldas_data_merged[['local_time','fraction']]
    nldas_data_fractioncheck = nldas_data_fraction.set_index('local_time').resample('M',level=0).sum() 
    if (round(nldas_data_fractioncheck.max().max(),2)> 1):
        print("Maximum fraction value (should be less than or equal to 1): ", nldas_data_fractioncheck.max())  
    if (round(nldas_data_fractioncheck.min().min(),2)< 0):
        print("Maximum fraction value (should be greater than or equal to 0): ", nldas_data_fractioncheck.min())  
    # Summing the monthly fraction and raising error if less than 0 or greater than 1
    monthly_fraction_sum = nldas_data_merged.groupby(nldas_data_merged['dateMonth'])['fraction'].sum()
    tolerance = 1e-6
    invalid_months = monthly_fraction_sum[
            ~((monthly_fraction_sum - 1).abs() <= tolerance) & 
            ~((monthly_fraction_sum - 0).abs() <= tolerance)
        ]
    # Raise an error if any invalid months found
    if not invalid_months.empty:
        raise ValueError(f"Monthly fraction sums not equal to 1 detected for file {nldas_file}:\n{invalid_months}")
    nldas_data_merged= nldas_data_merged.rename(columns={'local_time':'datetime'})
    return  nldas_data_merged


def read_prism(prism_file,start_date,end_date):
    try:
        prism_data = pd.read_csv(prism_file, header=None)
    except FileNotFoundError:
        print(f"PRISM File not found: {prism_file}")    

    # Making sure the observed data has two columns (datetime and value)
    if (prism_data.shape[1] != 2):                 # If the observed data only have one column, split the data into two.
        raise ValueError(f'PRISM file is missing a column data (Date or Value) for file: {prism_file}')

    # Naming Columns and removing any strings that could be there in "obs_daily_value" column
    prism_data.columns = ['dateMonth','monthly_value_PRISM']
    
    #if NA in PRISM data raise a valuerror
    if prism_data['monthly_value_PRISM'].isna().any():
        raise ValueError(f"Missing values detected in 'monthly_value' column in file: {prism_file}")

    prism_data['dateMonth'] = pd.to_datetime(prism_data['dateMonth']).dt.to_period('M')
    #prism_data['dateMonth'] = prism_data['dateMonth'].dt.strftime('%Y-%m')
    
    #Generating a complete PRISM dataframe that has 0 for months with no precipitation data
    date_range = pd.DataFrame(pd.date_range(start=start_date, end=end_date, freq='M'))
    date_range.columns = ['dateMonth']
    date_range['dateMonth'] = pd.to_datetime(date_range['dateMonth']).dt.to_period('M')
    #date_range['dateMonth'] = date_range['dateMonth'].dt.strftime('%Y-%m')
    # 4. Reindex the DataFrame with the complete date range
    prism_filled = pd.merge(date_range,prism_data,on="dateMonth",how="left")
    prism_filled['monthly_value_PRISM'] = prism_filled['monthly_value_PRISM'].fillna(0)
    return prism_filled

