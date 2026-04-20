import os
import requests
import pandas as pd
from datetime import date, timedelta

import pandas as pd
import requests
from io import StringIO

def downloadUSGSFlow(site, start_date, end_date):
    print("✅ NEW downloadUSGSFlow is running")

    url = "https://waterservices.usgs.gov/nwis/dv/"
    params = {
        "format": "rdb",
        "sites": str(site),
        "parameterCd": "00060",
        "startDT": start_date,
        "endDT": end_date,
        "siteStatus": "all",
    }

    r = requests.get(url, params=params, timeout=60)
    r.raise_for_status()

    # Debug: confirm we're not getting HTML
    ct = r.headers.get("Content-Type", "")
    if "html" in ct.lower() or r.text.lstrip().startswith("<"):
        print("❌ Got HTML instead of RDB.")
        print("Final URL:", r.url)
        print("Content-Type:", ct)
        print("First 300 chars:\n", r.text[:300])
        raise ValueError("USGS response was HTML, not RDB/TSV.")

    df = pd.read_csv(StringIO(r.text), sep="\t", comment="#", dtype=str)


    # Save the raw gage data to a file
    writePath = os.path.join('examples', 'RR_connected_example', 'calpella_gage_' + str(date.today()) + '.csv')

    df.to_csv(writePath, index = False)


    # Keep only actual data rows (drops the RDB spec row too)
    if "agency_cd" in df.columns:
        df = df[df["agency_cd"] == "USGS"].copy()

    if "datetime" not in df.columns:
        raise ValueError(f"'datetime' column not found. Columns: {list(df.columns)}")

    discharge_cols = [c for c in df.columns if "00060" in c and not c.endswith("_cd")]
    if not discharge_cols:
        raise ValueError(f"No discharge value column found. Columns: {list(df.columns)}")
    flow_col = discharge_cols[0]

    out = df[["datetime", flow_col]].rename(columns={"datetime": "Date", flow_col: "cfs"}).copy()
    out["Date"] = pd.to_datetime(out["Date"], errors="coerce")
    out["cfs"] = pd.to_numeric(out["cfs"], errors="coerce")
    out = out.dropna(subset=["Date"]).sort_values("Date").reset_index(drop=True)

    return out



# def downloadUSGSFlow(site, start_date, end_date):
#     """Downloads USGS discharge data for the specified time period.

#     Args:
#         site (int): USGS site ID code.
#         start_date (str): Start date in YYYY-MM-DD format.
#         end_date (str): End date in YYYY-MM-DD format.

#     Returns:
#         pandas.DataFrame: A DataFrame containing the downloaded data.
#     """
#     base_url = 'https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb'
#     dynamic_url = f'{base_url}&site_no={site}&legacy=&referred_module=sw&period=&begin_date={start_date}&end_date={end_date}&siteStatus=all'
#     #####
#     # The url below will retrieve 15-min interval flow, the url above will retrieve daily mean
#     #####
#     # base_url = "https://waterservices.usgs.gov/nwis/iv/?sites={}&parameterCd=00060".format(site)
#     # dynamic_url = f"{base_url}&startDT={start_date}&endDT={end_date}&siteStatus=all&format=rdb"

#     response = requests.get(dynamic_url)
#     response.raise_for_status()

#     lines = response.text.splitlines()
#     data = [line.split('\t') for line in lines if not line.startswith('#')]
#     print("data", data)
#     flowTimeseries = pd.DataFrame(data[2:], columns=data[0])

#     flowTimeseries = flowTimeseries.iloc[:,[2,-2]] # TODO: this assumes that the third column is always the datetime and the second-to-last column is always discharge
#     flowTimeseries.columns=['Date','cfs']

#     flowTimeseries['cfs'] = pd.to_numeric(flowTimeseries['cfs'])
#     flowTimeseries['Date'] = pd.to_datetime(flowTimeseries['Date'])

#     return flowTimeseries


def getLakeMendoInflow(LakeMendoBalance_FileLocation):
    LakeMendo_CFS_df = pd.read_excel(LakeMendoBalance_FileLocation, engine='openpyxl')
    # Drop Unnecessary Columns and Standardize Headers
    LakeMendo_CFS_df = LakeMendo_CFS_df.drop('Tunnel Diversion Observed (cfs)', axis=1)
    LakeMendo_CFS_df.columns = ['Date', 'cfs']
    return LakeMendo_CFS_df


def getSCWAForecast(SCWAForecast_FileLocation, ForecastKeep, current_date):
    SCWAForecast_CFS_df = pd.read_excel(SCWAForecast_FileLocation, engine='openpyxl')

    #Set Headers and trim off empty rows and useless columns
    SCWA_Headers = ['Blank1', 'Date', 'Delete1','Delete2','Delete3','Delete4','Delete5','Delete6','Delete7','Delete8','Var_Similar','Var_Dry','NoVar_Similar','NoVar_Dry']
    SCWAForecast_CFS_df.columns = SCWA_Headers
    SCWAForecast_CFS_df = SCWAForecast_CFS_df.iloc[4:, :]
    SCWAForecast_CFS_df = SCWAForecast_CFS_df.drop(['Blank1', 'Delete1','Delete2','Delete3','Delete4','Delete5','Delete6','Delete7','Delete8'], axis=1)

    #Trim columns based on forecast type
    SCWA_HeaderTrim = ['Var_Similar','Var_Dry','NoVar_Similar','NoVar_Dry']
    SCWA_HeaderTrim.remove(ForecastKeep)
    SCWAForecast_CFS_df = SCWAForecast_CFS_df.drop(SCWA_HeaderTrim, axis=1)

    #Trim rows based on current date and standardize headers
    # Ensure Date column is in datetime format
    SCWAForecast_CFS_df['Date'] = pd.to_datetime(SCWAForecast_CFS_df['Date'])

    # Convert current_date to datetime for comparison
    current_date_dt = pd.to_datetime(current_date)

    # Find the matching row index
    matching_rows = SCWAForecast_CFS_df.index[SCWAForecast_CFS_df['Date'] == current_date_dt].tolist()

    # If an exact match is found, use it; otherwise, use the latest available date
    if matching_rows:
        row_index = int(matching_rows[0])  # Take the first match
    else:
        # Use the latest available forecast (last row)
        row_index = SCWAForecast_CFS_df.index[-1]
        print(f"Warning: No forecast found for {current_date}, using latest available date: {SCWAForecast_CFS_df.loc[row_index, 'Date']}.")

    # Ensure we don't go out of bounds when slicing
    row_index = max(row_index - 4, 0)

    # Slice the dataframe from the selected row onward
    SCWAForecast_CFS_df = SCWAForecast_CFS_df.iloc[row_index:, :]

    # Standardize headers
    SCWAForecast_CFS_df.columns = ['Date', 'cfs']
    return SCWAForecast_CFS_df


def makeMonthlyACFT(LakeMendo_CFS_df, Calpella_CFS_df, SCWAForecast_CFS_df):
    CFS_df = pd.concat([LakeMendo_CFS_df,Calpella_CFS_df,SCWAForecast_CFS_df], ignore_index = True)
    CFS_df['Date'] = pd.to_datetime(CFS_df['Date'])
    CFS_df.set_index('Date', inplace = True)
    CFS_df['acft'] = CFS_df['cfs']*3600*24/43559.9

# Create a new data frame of monthly average resampled CFS
    MonthlyACFT_df = CFS_df.resample('ME').sum().drop('cfs', axis=1)

# Trim the date column to exclude the day
    MonthlyACFT_df = MonthlyACFT_df.reset_index()
    MonthlyACFT_df['Date'] = MonthlyACFT_df['Date'].astype(str).str[:-3]
    MonthlyACFT_df.set_index('Date', inplace = True)


    # Zero out PVP
    MonthlyACFT_df['acft'] = 0

    return MonthlyACFT_df


def makeMonthlyET(ET_xlsx_location, dates):
    ET_df = pd.read_excel(ET_xlsx_location, engine='openpyxl')
    ET_df.set_index('Watershed', inplace = True)
    ET_df = ET_df.loc[:,pd.to_datetime(dates).strftime('%b')]
    return ET_df


def makeConfigFiles(MonthlyACFT_df, dates):
    configdata_headers = ['INPUT_NAME']+dates
    # Initialize the URR config file and set the Lake mendo row to 0's
    urr_configfile_df = pd.DataFrame(columns = configdata_headers)
    urr_configfile_df.loc[0] = ['LAKE_MENDO']+[0]*len(dates) # <- input any non-0 Lake Mendo values here

    # Set the PVP_Flow row of the config file to reference the Monthly average CFS values from MonthlyCFS_df
    urr_configfile_df.loc[1] = MonthlyACFT_df['acft'].reindex(urr_configfile_df.columns, fill_value = 'PVP_FLOW')

    # Import ET Data and standardize the headers based on current WY, import URR ET data as final row of config file, replace empty forecasts with 0's
    ET_xlsx_location = os.path.join('examples','RR_connected_example','_inputs','ET.xlsx')
    ET_df = makeMonthlyET(ET_xlsx_location, dates)

    urr_configfile_df.loc[2] = ['EVAP_LOSS'] + ET_df.iloc[0].tolist()
    MonthColumns = urr_configfile_df.columns.difference(['INPUT_NAME'])
    urr_configfile_df[MonthColumns] = urr_configfile_df[MonthColumns].replace('PVP_FLOW', 0)

    # Create LRR config file based on URR, drop the Lake Mendo and ET rows, replace ET row with LRR ET data, replace empty forecasts with 0's
    lrr_configfile_df = urr_configfile_df.drop(index = [0,2])
    lrr_configfile_df.loc[2] = ['EVAP_LOSS'] + ET_df.iloc[1].tolist()
    MonthColumns = lrr_configfile_df.columns.difference(['INPUT_NAME'])
    lrr_configfile_df[MonthColumns] = lrr_configfile_df[MonthColumns].replace('PVP_FLOW', 0)
    return urr_configfile_df,lrr_configfile_df


def preprocessPVPFlows(
        dates,
        ForecastKeep,
        LakeMendoBalance_FileLocation,
        SCWAForecast_FileLocation):
    """"
    """
    ###
    last_flow_date = date(*[int(d) for d in (dates[-1]+'-1').split('-')])
    last_flow_date = (last_flow_date.replace(day=1) + timedelta(days=32)).replace(day=1) - timedelta(days=1)
    current_date = date.today()
    if current_date < last_flow_date:
        obs_end_date = (current_date-timedelta(days=1)).strftime('%Y-%m-%d')
    else:
        obs_end_date = last_flow_date.strftime('%Y-%m-%d')
    start_date = dates[0]+'-1'
    ### pandas .replace() warning silencing
    pd.set_option("future.no_silent_downcasting", True)
    ### Lake Mendocino Balance Eq. Inflow Import ###
    LakeMendo_CFS_df = getLakeMendoInflow(LakeMendoBalance_FileLocation)
    ### USGS Calpella Gauge Scraper (11461500) ###
    Calpella_CFS_df = downloadUSGSFlow(
        site=11461500,
        start_date=start_date,
        end_date=obs_end_date)
    ### SCWA Lake Mendo Forecast Import ###
    SCWAForecast_CFS_df = getSCWAForecast(
        SCWAForecast_FileLocation, ForecastKeep, current_date)
    ### Final DF construction and monthly time-step conversion ###
    MonthlyACFT_df = makeMonthlyACFT(
        LakeMendo_CFS_df, Calpella_CFS_df, SCWAForecast_CFS_df)


    ### Create Config File DFs
    urr_configfile_df, lrr_configfile_df = makeConfigFiles(MonthlyACFT_df, dates)
    return urr_configfile_df,lrr_configfile_df


def createConfigFiles(
        dates,
        urr_config_file,
        lrr_config_file,
        LakeMendoBalance_FileLocation,
        SCWAForecast_FileLocation,
        ForecastKeep
):
    """
    """
    urr_configfile_df, lrr_configfile_df = preprocessPVPFlows(
        dates,
        ForecastKeep,
        LakeMendoBalance_FileLocation,
        SCWAForecast_FileLocation)

    urr_configfile_df.to_csv(urr_config_file,index=False)
    lrr_configfile_df.to_csv(lrr_config_file,index=False)



# boolVal = False

# def downloadUSGSData(start_date, end_date):
#     """Downloads USGS discharge data for the specified time period.

#     Args:
#         start_date (str): Start date in YYYY-MM-DD format.
#         end_date (str): End date in YYYY-MM-DD format.

#     Returns:
#         pandas.DataFrame: A DataFrame containing the downloaded data.
#     """

#     base_url = "https://waterservices.usgs.gov/nwis/iv/?sites=11461500&parameterCd=00060"
#     dynamic_url = f"{base_url}&startDT={start_date}&endDT={end_date}&siteStatus=all&format=rdb"

#     response = requests.get(dynamic_url)
#     response.raise_for_status()

#     lines = response.text.splitlines()
#     data = [line.split('\t') for line in lines if not line.startswith('#')]
#     df = pd.DataFrame(data[2:], columns=data[0])

#     df = df.iloc[:,[2,-2]] # TODO: this assumes that the third column is always the datetime and the second-to-last column is always discharge
#     df.columns=['Date','Discharge']

#     df['Discharge'] = pd.to_numeric(df['Discharge'])
#     df['Date'] = pd.to_datetime(df['Date'])

#     return df

# # Define time frame
# start_date = '2023-10-01'
# end_date = '2024-09-30'

# # Download USGS data
# df = downloadUSGSData(start_date, end_date)


# if boolVal:

#     #%%
#     # Calculate median monthly discharge
#     df['Month'] = df['Date'].dt.strftime('%Y-%m')
#     monthly_discharge = df.groupby('Month')['Discharge'].median().reset_index()

#     # Save median monthly discharge to CSV
#     monthly_discharge.to_csv('PVP_median_discharge.csv', index=False)

#     # ... (Rest of the script, including ET data processing and configuration file generation)

#     # Note: The remaining parts of the script, especially those related to ET data processing and configuration file generation, would require more specific information about the structure of the ET data and the desired format of the configuration files. 

#     # However, the general approach would involve:
#     # 1. Reading the ET data using pandas' read_excel function.
#     # 2. Creating pandas DataFrames for URR and LRR configurations.
#     # 3. Populating these DataFrames with the appropriate values.
#     # 4. Saving the DataFrames as CSV files.

#     # You might consider using libraries like `openpyxl` for more complex Excel operations.

#     # Read ET data from Excel
#     et_data = pd.read_excel(os.path.join('input_RR','ET.xlsx'), sheet_name=['URR', 'LRR'])

#     # # Read PVP median discharge data
#     # monthly_discharge = pd.read_csv('PVP_median_discharge.csv')

#     #%%

#     def createConfig(pvp,evap,upper=False):
#         configIndex =['PVP_FLOW', 'EVAP_LOSS']
#         if upper:
#             configIndex.insert(0, 'LAKE_MENDO')
#         configCols = pvp['Month'].to_list()

#         config = pd.DataFrame(
#             index=pd.Index(configIndex,name='INPUT_NAME'),
#             columns=configCols)
#         config.loc['PVP_FLOW'] = pvp['Discharge'].to_list()
#         config.loc['EVAP_LOSS'] = evap.set_index('Month').loc[pd.to_datetime(pvp['Month']).dt.strftime('%b').to_list(),'ET'].to_list()
#         if upper:
#             config.loc['LAKE_MENDO'] = 0
        

#         return config

#     configLRR = createConfig(monthly_discharge,et_data['LRR'],upper=False)
#     configURR = createConfig(monthly_discharge,et_data['URR'],upper=True)

#     configLRR.to_csv('lrr_config_file.csv')
#     configURR.to_csv('urr_config_file.csv')

#     #%%
#     # # Create a function to create configuration file
#     # def createConfigFile(data, file_name, include_lake_mendo=False):
#     #     """Creates a configuration file based on the given data.

#     #     Args:
#     #         data (pandas.DataFrame): DataFrame containing the data.
#     #         file_name (str): Name of the output file.
#     #         include_lake_mendo (bool, optional): Whether to include the LAKE_MENDO row. Defaults to False.
#     #     """

#     #     config_data = {
#     #         'INPUT_NAME': ['PVP_FLOW', 'EVAP_LOSS']
#     #     }

#     #     if include_lake_mendo:
#     #         config_data['INPUT_NAME'].insert(0, 'LAKE_MENDO')

#     #     config_data['Month'] = data.columns[1:]

#     #     config_df = pd.DataFrame(config_data)
#     #     config_df.iloc[0, 1:] = monthly_discharge['Discharge'].values
#     #     config_df.iloc[1:, 1:] = data.iloc[0:, 1:].values

#     #     config_df.to_csv(file_name, index=False)

#     # # Create configuration files for URR and LRR
#     # createConfigFile(et_data['URR'], 'urr_config.csv', include_lake_mendo=True)
#     # createConfigFile(et_data['LRR'], 'lrr_config.csv', include_lake_mendo=False)


