#%%
import os
from dwrat.preprocessing import basinProcessing as bp
from dwrat.preprocessing import supplyProcessing as sp
from dwrat.preprocessing import demandProcessing as dp
from dwrat.modeling import dwrat

outlet = None#'NC_001'
# add your supply file name
supply_file = 'C:/Users/aprashar/Documents/Github/DWRAT_DataScraping/Demand/OutputData/GL_formatted_supply_35.csv'
#os.path.join('examples','Mad_example','_inputs','formatted_supply.csv')
# add the name of your demand file
demand_file = 'C:/Users/aprashar/Documents/Github/DWRAT_DataScraping/Demand/OutputData/GL_formatted_demand_35.csv'
#os.path.join('examples','Mad_example','_inputs','formatted_demand.csv')
# add the name of your basins file
basin_file = 'C:/Users/aprashar/Documents/Github/DWRAT_DataScraping/Demand/OutputData/GL_generated_basins_35.csv'
#os.path.join('examples','Mad_example','_inputs','generated_basins.csv')

#%%###########################################################################
# 1. PREPROCESSING

basinConnectivity, basinInfo = bp.makeBasinConnectivityMatrix(
    outlet=outlet,
    basinFilePath=basin_file)

flows = sp.processPRMSFlows(
    basinFilePath=basin_file,
    supplyFilePath=supply_file)

riparian,appropriative = dp.processDemand(
    basinConnectivityMatrix=basinConnectivity,
    demandFilePath=demand_file)

#%%###########################################################################
# 2. MODELING

dates = flows.columns[flows.columns!='FLOWS_TO'].values

model = dwrat.Model(
    modelName='Paradigm_DWRAT',#'Mad_example',
    riparian=riparian,
    appropriative=appropriative,
    flows=flows,
    basinConnectivity=basinConnectivity,
    basinInfo=basinInfo,
    dates=dates
)

model.run()

#%%###########################################################################
# 3. POSTPROCESSING

model.writeOutputs(
    directoryPath='C:/Users/aprashar/Documents/Github/DWRAT_DataScraping/Paradigm_DWRAT/dwrat/output/'.format(model.name)
    #'C:/Users/aprashar/Documents/GitHub/Paradigm_DWRAT/examples/Mad_example/{}'.format(model.name)
)

print(model.log)

#%%
