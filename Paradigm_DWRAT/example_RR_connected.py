#%%
import os
from dwrat.preprocessing import basinProcessing as bp
from dwrat.preprocessing import supplyProcessing as sp
from dwrat.preprocessing import demandProcessing as dp
from dwrat.preprocessing import PVP_Processor as pvp
from dwrat.modeling import dwrat
from dwrat.utils import RR

# add the path to your supply file
supply_file = os.path.join('examples','RR_connected_example','_inputs','Raw_Flows.csv')
# add the path to your demand file
demand_file = os.path.join('examples','RR_connected_example','_inputs',
                           'RR_2017_2024_MDT_2025-04-04.csv')
# add the path to your basins file
basin_file = os.path.join('examples','RR_connected_example','_inputs','basins.csv')
# add the path to your config files
urr_config_file = os.path.join('examples','RR_connected_example','_inputs',
                               'urr_config_file.csv')
lrr_config_file = os.path.join('examples','RR_connected_example','_inputs',
                               'lrr_config_file.csv')

##############################################################################
#% PREPROCESSING for Upper RR

outlet = 'R_13_M'
basinConnectivity, basinInfo = bp.makeBasinConnectivityMatrix(
    outlet=outlet,
    basinFilePath=basin_file)
urrFlows = sp.processPRMSFlows(
    basinFilePath=basin_file,
    supplyFilePath=supply_file
    ).loc[basinInfo.index] # remove unnecessary basins

dates = urrFlows.columns[urrFlows.columns!='FLOWS_TO'].to_list()

##############################################################################
# PVP preprocessing
LakeMendoBalance_FileLocation = os.path.join(
    'examples','PVP_example_files',
    'PVP_Transfers_Observed_WY1960_WY2023.xlsx')

SCWAForecast_FileLocation = os.path.join(
    'examples','PVP_example_files',
    'PotterValleyProjectProjection_DWRAT_20260105.xlsx')
### Set Forecast Type ###
Variance = 'NoVar' # <- PVP Infrastructure Variance, 'Var' or 'NoVar'
SimilarDry = 'Dry' # <- Similar WY or Dry WY, 'Similar' or 'Dry'; set to Dry if you're using 
                        # SPI data to forecast hydrological flows 
                        # (this is the case for the Oct- Feb portion of the water year)
ForecastKeep = Variance + '_' + SimilarDry
###
pvp.createConfigFiles(
    dates, urr_config_file, lrr_config_file,
    LakeMendoBalance_FileLocation, SCWAForecast_FileLocation, ForecastKeep)
##############################################################################

urrFlows,pvp = RR.processConnectedURRFlows(
    urr_config_file,
    basinInfo,
    urrFlows,
    dates=dates)
urrFlows.loc['R_02_M',dates] = pvp

riparian,appropriative = dp.processDemand(
    basinConnectivityMatrix=basinConnectivity,
    demandFilePath=demand_file)

##############################################################################
#% MODELING for Upper RR

upperModel = dwrat.Model(
    modelName='URR_Connected_DD_2017_2024_20260212',
    riparian=riparian,
    appropriative=appropriative,
    flows=urrFlows,
    basinConnectivity=basinConnectivity,
    basinInfo=basinInfo,
    dates=dates
)

upperModel.run()

##############################################################################
#%% PREPROCESSING for Lower RR

outlet = 'R_21_M'
basinConnectivity, basinInfo = bp.makeBasinConnectivityMatrix(
    outlet=outlet,
    basinFilePath=basin_file)
lrrFlows = sp.processPRMSFlows(
    basinFilePath=basin_file,
    supplyFilePath=supply_file
    ).loc[basinInfo.index] # remove unnecessary basins

lrrFlows = RR.processConnectedLRRFlows(
    lrr_config_file,
    upperModel,
    lrrFlows,
    dates=dates
)

riparian,appropriative = dp.processDemand(
    basinConnectivityMatrix=basinConnectivity,
    demandFilePath=demand_file)

##############################################################################
#% MODELING for Lower RR

lowerModel = dwrat.Model(
    modelName='LRR_Connected_DD_2017_2024_20260212',
    riparian=riparian,
    appropriative=appropriative,
    flows=lrrFlows,
    basinConnectivity=basinConnectivity,
    basinInfo=basinInfo,
    dates=dates
)

lowerModel.run()

##############################################################################
#%% OUTPUT for both Upper and Lower RR

# output upper and lower separately
for model in [upperModel,lowerModel]:
    outputPath = os.path.join('examples',
                              'RR_connected_example',
                              model.name)
    if not os.path.exists(outputPath):
        os.mkdir(outputPath)
    model.writeOutputs(directoryPath=outputPath)

# output upper and lower combined
RR.outputCombinedConnected(
    upperModel,
    lowerModel,
    name=os.path.join('examples','RR_connected_example'))


# %%
