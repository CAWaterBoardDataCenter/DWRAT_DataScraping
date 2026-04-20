import os
import numpy as np
import pandas as pd
from dwrat.preprocessing import basinProcessing as bp
from dwrat.preprocessing import supplyProcessing as sp
from dwrat.preprocessing import demandProcessing as dp
from dwrat.modeling import dwrat
from dwrat.postprocessing import output

def disconnected(
        basinFilePath,
        supplyFilePath,
        demandFilePath,
        upperConfigFilePath,
        lowerConfigFilePath,
        demandOverrideFilePath=None,
        name='disconnected',
):
    """
    """
    ##########################################################################
    # Read flows for all PRMS modeled basins
    all_flows = sp.processPRMSFlows(
        basinFilePath=basinFilePath,
        supplyFilePath=supplyFilePath)
    ##########################################################################
    # Specify manual deman overrides for certain applications:
    #     The override variable is defined as a list that has dictionary and/
    #     or dataframe inputs for manual demand overrides.
    #     Dictionary inputs are assumed to be single key/value pairs where the
    #     value corresponds to the new demand value for every month.
    #     Dataframe inputs are assumed to have the application ID as the index
    #     and a new demand value for each month in the dataframe.
    demandOverride = [
        { # set Potter Valley demands to zero
            'A013557': 0.0002022,
            'S014313': 0.0002022
        },
        pd.read_csv( # other demand overrides
            demandOverrideFilePath,
            index_col=0
        ) if demandOverrideFilePath else None
    ]
    ##########################################################################
    # UPPER ##################################################################
    model_upper1, model_upper2 = disconnect_mod(
        basinFilePath=basinFilePath,
        demandFilePath=demandFilePath,
        demandOverride=demandOverride,
        configFilePath=upperConfigFilePath,
        all_flows=all_flows,
        upper_or_lower='upper',
        name=name,
    )
    ##########################################################################
    # LOWER ##################################################################
    model_lower1, model_lower2 = disconnect_mod(
        basinFilePath=basinFilePath,
        demandFilePath=demandFilePath,
        demandOverride=demandOverride,
        configFilePath=lowerConfigFilePath,
        all_flows=all_flows,
        upper_or_lower='lower',
        name=name,
    )
    
    return [model_upper1,model_upper2,model_lower1,model_lower2]


def combineModuleAllocations(
        Allocation1,
        Allocation2,
):
    """
    """
    return Allocation1.where(Allocation1!=0,Allocation2)

def combineDisconnectedResults(
        upper1UserGroup,
        upper2UserGroup,
        lower1UserGroup,
        lower2UserGroup
):
    """
    """
    upperBasinAllocation = combineModuleAllocations(
        upper1UserGroup.basinAllocations,
        upper2UserGroup.basinAllocations
    )
    lowerBasinAllocation = combineModuleAllocations(
        lower1UserGroup.basinAllocations,
        lower2UserGroup.basinAllocations
    )
    basinAllocation = pd.concat([
        upperBasinAllocation,
        lowerBasinAllocation
    ])
    userAllocation = pd.concat([
        upper1UserGroup.userAllocations,
        upper2UserGroup.userAllocations,
        lower1UserGroup.userAllocations,
        lower2UserGroup.userAllocations
    ])

    return basinAllocation, userAllocation


def outputDisconnectedResults(
        model_upper1,
        model_upper2,
        model_lower1,
        model_lower2,
        name
):
    if name is None:
        name = ''
    elif not os.path.exists(name):
        os.mkdir(name)
    for model in [model_upper1,
                  model_upper2,
                  model_lower1,
                  model_lower2]:
        if not os.path.exists(os.path.join(name,model.name)):
            os.mkdir(os.path.join(name,model.name))
        model.writeOutputs(directoryPath=os.path.join(name,model.name))
        
def outputCombinedDisconnected(
        model_upper1,
        model_upper2,
        model_lower1,
        model_lower2,
        name
):
    """
    """
    if name is None:
        name = ''
    elif not os.path.exists(name):
        os.mkdir(name)
    # [appBasinAllocation,
    # appUserAllocation,
    # ripBasinAllocation,
    # ripUserAllocation] = combineAllDisconnectedResults(
    #     model_upper1,
    #     model_upper2,
    #     model_lower1,
    #     model_lower2)
    
    # appBasinAllocation.to_csv(
    #     os.path.join(
    #         name,'appropriative_basin_allocation_table.csv'))
    # ripBasinAllocation.to_csv(
    #     os.path.join(
    #         name,'riparian_basin_allocation_table.csv'))
    
    # appUserAllocation.to_csv(
    #     os.path.join(
    #         name,'appropriative_user_allocation_table.csv'))
    # ripUserAllocation.to_csv(
    #     os.path.join(
    #         name,'riparian_user_allocation_table.csv'))

    [appropriativeBasinOutput,
    riparianBasinOutput,
    appropriativeUserOutput,
    riparianUserOutput] = combineAllDisconnectedOutputs(
        model_upper1,
        model_upper2,
        model_lower1,
        model_lower2)
    
    riparianBasinOutput.to_csv(os.path.join(
        name,'basin_riparian_output_'+os.path.split(name)[-1]+'.csv'))
    appropriativeBasinOutput.to_csv(os.path.join(
        name,'basin_appropriative_output_'+os.path.split(name)[-1]+'.csv'))
    
    riparianUserOutput.to_csv(os.path.join(
        name,'user_riparian_output_'+os.path.split(name)[-1]+'.csv'))
    appropriativeUserOutput.to_csv(os.path.join(
        name,'user_appropriative_output_'+os.path.split(name)[-1]+'.csv'))
    
    preferredOutput = output.makePreferredOutput([
        model_upper1.appropriative,
        model_upper1.riparian,
        model_upper2.appropriative,
        model_upper2.riparian,
        model_lower1.appropriative,
        model_lower1.riparian,
        model_lower2.appropriative,
        model_lower2.riparian
    ])
    preferredOutput.to_csv(os.path.join(
        name,'_preferred_output_'+os.path.split(name)[-1]+'.csv'))


def combineAllDisconnectedOutputs(
        model_upper1,
        model_upper2,
        model_lower1,
        model_lower2
):
    """
    """
    allBasins = model_upper1.basins+model_lower1.basins
    dates = model_upper1.dates

    appropriativeBasinOutput = pd.DataFrame(
    index=pd.Index(allBasins,name='BASIN'),
    columns=np.array([[d+'_ALLOCATIONS',d+'_AVAILABLE_FLOW',d+'_DEMAND'] for d in dates]).flatten())

    riparianBasinOutput = pd.DataFrame(
    index=pd.Index(allBasins,name='BASIN'),
    columns=np.array([[d+'_ALLOCATIONS',d+'_DEMAND',d+'_FLOW',d+'_PROPORTIONS'] for d in dates]).flatten())

    appropriativeUserOutput = pd.DataFrame(
    index=pd.Index(
        np.concat([
            model_upper1.appropriative.users,
            model_upper2.appropriative.users,
            model_lower1.appropriative.users,
            model_lower2.appropriative.users]),
        name='USER'),
    columns=np.array([[d+'_ALLOCATIONS',d+'_CURTAILMENT',d+'_DEMAND',d+'_SHORTAGE_%'] for d in dates]).flatten())

    riparianUserOutput = pd.DataFrame(
    index=pd.Index(
        np.concat([
            model_upper1.riparian.users,
            model_upper2.riparian.users,
            model_lower1.riparian.users,
            model_lower2.riparian.users]),
        name='USER'),
    columns=np.array([[d+'_ALLOCATIONS',d+'_CURTAILMENT',d+'_DEMAND',d+'_SHORTAGE_%'] for d in dates]).flatten())

    for model in [model_upper1,
              model_upper2,
              model_lower1,
              model_lower2]:
        if model.name.__contains__('Module1'):
            basins = model.basinInfo[model.basinInfo['MAINSTEM']=='N'].index
        else:
            basins = model.basinInfo[model.basinInfo['MAINSTEM']=='Y'].index

    ##########################################################################
    # appropriative basin output
    #
        availableFlow = model.availableFlow.loc[basins].add_suffix('_AVAILABLE_FLOW')
        appropriativeBasinOutput.loc[basins,availableFlow.columns] = availableFlow.loc[basins]
    #
        allocations = model.appropriative.basinAllocations.loc[basins].add_suffix('_ALLOCATIONS')
        appropriativeBasinOutput.loc[basins,allocations.columns] = allocations.loc[basins]
    #
        demand = model.appropriative.userMatrix.dot(model.appropriative.modelDemand).loc[basins].add_suffix('_DEMAND')
        appropriativeBasinOutput.loc[basins,demand.columns] = demand.loc[basins]
    ##########################################################################
    # riparian basin output
    #
        allocations = model.riparian.basinAllocations.loc[basins].add_suffix('_ALLOCATIONS')
        riparianBasinOutput.loc[basins,allocations.columns] = allocations.loc[basins]
    #
        demand = model.riparian.userMatrix.dot(model.riparian.modelDemand).loc[basins].add_suffix('_DEMAND')
        riparianBasinOutput.loc[basins,demand.columns] = demand.loc[basins]
    #
        flow = model.flows.loc[basins].add_suffix('_FLOW')
        riparianBasinOutput.loc[basins,flow.columns] = flow.loc[basins]
    # 
        proportions = model.riparian.basinProportions.loc[basins].add_suffix('_PROPORTIONS')
        riparianBasinOutput.loc[basins,proportions.columns] = proportions.loc[basins]
    ##########################################################################
    # user outputs
    #
        for userGroup in [model.appropriative,model.riparian]:
            allocations = userGroup.userAllocations.add_suffix('_ALLOCATIONS')
            if userGroup.groupName == 'Riparian':
                riparianUserOutput.loc[allocations.index,allocations.columns] = allocations
            else:
                appropriativeUserOutput.loc[allocations.index,allocations.columns] = allocations
        #
            demand = userGroup.modelDemand.add_suffix('_DEMAND')
            if userGroup.groupName == 'Riparian':
                riparianUserOutput.loc[demand.index,demand.columns] = demand
            else:
                appropriativeUserOutput.loc[demand.index,demand.columns] = demand
        #
            shortage = ((userGroup.modelDemand-userGroup.userAllocations)/userGroup.modelDemand).add_suffix('_SHORTAGE_%')
            shortage = shortage*100
            shortage[shortage<1] = 0
            if userGroup.groupName == 'Riparian':
                riparianUserOutput.loc[shortage.index,shortage.columns] = shortage
            else:
                appropriativeUserOutput.loc[shortage.index,shortage.columns] = shortage
        #
            curtailment = (shortage>0).astype(int)
            curtailment.columns = curtailment.columns.str.replace('_SHORTAGE_%','_CURTAILMENT')
            if userGroup.groupName == 'Riparian':
                riparianUserOutput.loc[curtailment.index,curtailment.columns] = curtailment
            else:
                appropriativeUserOutput.loc[curtailment.index,curtailment.columns] = curtailment
    return appropriativeBasinOutput,riparianBasinOutput,appropriativeUserOutput,riparianUserOutput

        
def combineAllDisconnectedResults(
        model_upper1,
        model_upper2,
        model_lower1,
        model_lower2
):
    """
    """
    appBasinAllocation, appUserAllocation = combineDisconnectedResults(
    upper1UserGroup=model_upper1.appropriative,
    upper2UserGroup=model_upper2.appropriative,
    lower1UserGroup=model_lower1.appropriative,
    lower2UserGroup=model_lower2.appropriative)

    ripBasinAllocation, ripUserAllocation = combineDisconnectedResults(
    upper1UserGroup=model_upper1.riparian,
    upper2UserGroup=model_upper2.riparian,
    lower1UserGroup=model_lower1.riparian,
    lower2UserGroup=model_lower2.riparian)
    return appBasinAllocation,appUserAllocation,ripBasinAllocation,ripUserAllocation

def disconnect_mod(
        basinFilePath,
        demandFilePath,
        demandOverride,
        configFilePath,
        all_flows,
        upper_or_lower,
        name
):
    """
    """
    outlet = 'R_21_M' if upper_or_lower == 'lower' else 'R_13_M'

    basinConnectivity, basinInfo = bp.makeBasinConnectivityMatrix(
        outlet=outlet,
        basinFilePath=basinFilePath,
        basinSubset={'UPPER_RUSSIAN':('N' if upper_or_lower == 'lower' else 'Y')}
    )

    flows = all_flows.loc[
        basinInfo.loc[
            basinInfo['UPPER_RUSSIAN']==('N' if upper_or_lower == 'lower' else 'Y')
        ].index
    ]

    dates = flows.columns[flows.columns!='FLOWS_TO'].values

    # module 1 (tributaries)
    model1 = module1(
        demandFilePath,
        demandOverride,
        basinConnectivity,
        basinInfo,
        flows,
        dates,
        upper_or_lower,
        name)
    model1.run()

    # module 2 (mainstem)
    flows2 = mod2_flows(flows,dates,configFilePath,model1,upper_or_lower)
    model2 = module2(
        demandFilePath,
        demandOverride,
        basinConnectivity,
        basinInfo,
        dates,
        flows2,
        upper_or_lower,
        name)
    model2.run()

    return model1, model2


def module2(
        demandFilePath,
        demandOverride,
        basinConnectivity,
        basinInfo,
        dates,
        flows2,
        upper_or_lower,
        name
):
    
    if upper_or_lower == 'upper':
        priorityOverride = {'A012919A': 10000}
    else:
        priorityOverride = {
            'A015736': 9999,
            'A015737': 9998}

    riparian2,appropriative2 = dp.processDemand(
        basinConnectivityMatrix=basinConnectivity,
        demandFilePath=demandFilePath,
        demandOverride=demandOverride,
        demandSubset={
            'MAINSTEM_RR':'Y',
            'UPPER_RUSSIAN':('N' if upper_or_lower == 'lower' else 'Y')},
        priorityOverride=priorityOverride
    )

    model2 = dwrat.Model(
        modelName='RR_{}_Module2'.format(upper_or_lower),
        riparian=riparian2,
        appropriative=appropriative2,
        flows=flows2,
        basinConnectivity=basinConnectivity,
        basinInfo=basinInfo,
        dates=dates
    )
    
    return model2

def mod2_flows(
        flows,
        dates,
        config_file_path,
        model1,
        upper_or_lower
):
    pvp_flow_mod = pd.DataFrame(data = 0, index = flows.index, columns = dates, dtype = float)

    flows_proportional = pd.DataFrame(data = 0, index = flows.index, columns = dates)
    flows_proportional.insert(0, "FLOWS_TO", flows["FLOWS_TO"].values)

    basin_app = model1.appropriative.basinAllocations
    basin_rip = model1.riparian.basinAllocations

    config_file = pd.read_csv(config_file_path, index_col = "INPUT_NAME")
    UPPER_LOWER = upper_or_lower.upper()

    for date in dates:
        flows_proportional[date] = np.divide(flows[date], sum(flows[date])).values
        sum_flow = sum(flows[date])
        sum_allocations = sum(basin_app[date] + basin_rip[date])
        net = sum_flow - sum_allocations
        evap_loss_share = config_file.loc["EVAP_LOSS"][date] / 3
        def_surp = net - evap_loss_share
        pvp = max(0, config_file.loc["PVP_FLOW"][date] - evap_loss_share)
        flows_proportional[date] = flows_proportional[date] * max(0, def_surp)
        if def_surp > 0 and UPPER_LOWER == "UPPER":
            pvp_flow_mod.loc["R_02_M", date] = pvp_flow_mod.loc["R_02_M"][date] + pvp
            net_PVP = pvp
        elif def_surp > 0 and UPPER_LOWER == "LOWER":
            pvp_flow_mod.loc["R_17_M", date] = pvp_flow_mod.loc["R_17_M"][date] + pvp
            net_PVP = pvp
        elif def_surp < 0 and UPPER_LOWER == "UPPER":
            pvp_flow_mod.loc["R_02_M", date] = pvp_flow_mod.loc["R_02_M"][date] + pvp + def_surp       
            net_PVP = pvp + def_surp
        elif def_surp < 0 and UPPER_LOWER == "LOWER":
            pvp_flow_mod.loc["R_17_M", date] = pvp_flow_mod.loc["R_17_M"][date] + pvp + def_surp       
            net_PVP = pvp + def_surp

    flows2 = flows_proportional
    return flows2


def module1(
        demandFilePath,
        demandOverride,
        basinConnectivity,
        basinInfo,
        flows,
        dates,
        upper_or_lower,
        name
):
    
    riparian,appropriative = dp.processDemand(
        basinConnectivityMatrix=basinConnectivity,
        demandFilePath=demandFilePath,
        demandOverride=demandOverride, 
        # In a dictionary, we can specify any number of column/value pairs to
        # create a subset based on the columns in the original demand file.
        demandSubset={'MAINSTEM_RR':'N'},
        # Here, the demand data is filtered to include only those rows where
        # the column 'MAINSTEM_RR' has a value of 'N'.
    )

    model = dwrat.Model(
        modelName='RR_{}_Module1'.format(upper_or_lower),
        riparian=riparian,
        appropriative=appropriative,
        flows=flows,
        basinConnectivity=basinConnectivity,
        basinInfo=basinInfo,
        dates=dates
    )
    
    return model

def processConnectedURRFlows(
        configFilePath,
        basinInfo,
        flows,
        dates=None
):
    """
    """
    if dates is None:
        dates = flows.columns[flows.columns!='FLOWS_TO'].to_list()
    
    urrConfig = pd.read_csv(configFilePath,index_col=0).loc[:,dates]
    pvp = urrConfig.loc['PVP_FLOW',:]
    
    # Calculate the adjusted flows
    urrNaturalFlow = flows.loc[basinInfo['UPPER_RUSSIAN']=='Y',flows.columns!='FLOWS_TO']
    urrTotalNatural = urrNaturalFlow.sum(axis=0)

    urrTotalSupply = urrTotalNatural+pvp
    urrPercentET = urrConfig.loc['EVAP_LOSS']/urrTotalSupply
    urrReduction = 1 - urrPercentET

    # Multiply the simulated flows in the URR by urr_reduction
    urrPVPReduced = pvp*urrReduction
    urrFlows = urrNaturalFlow*urrReduction
    return urrFlows,urrPVPReduced

def processConnectedLRRFlows(
        configFile,
        upperModel,
        flows,
        dates = None
):
    """
    """
    if dates is None:
        dates = flows.columns[flows.columns!='FLOWS_TO'].to_list()

    lrrConfig = pd.read_csv(configFile,index_col=0).loc[:,dates]

    upperRipAllocations = upperModel.riparian.basinAllocations.sum()
    upperAppAllocations = upperModel.appropriative.basinAllocations.sum()

    upperAllocations = upperRipAllocations+upperAppAllocations
    # calculate surplus pvp
    surplusPVP = upperModel.flows.sum()-upperAllocations
    # TODO: should this use RAW urr flows rather than the reduced flows?
    #       seems like the reduced flows would better represent what is really
    #       available to the lower river from upstream.
    #       (Reason for raw flows: After processing, the "Available Flows" data has 
    #        some flows duplciated in its subbasin values. For example, PVP
    #        flows may appear in multiple downstream subbasins' total values.)

    # Using "flows" is fine
    # These are the pre-adjusted flows, and they represent flows generated in each sub-basin
    # (Not total available flow available in that location)

    lrrTotalSupply = flows.loc[:,dates].sum(axis=0)+surplusPVP
    lrrPercentET = lrrConfig.loc['EVAP_LOSS']/lrrTotalSupply
    lrrReduction = 1 - lrrPercentET
    surplusReduced = surplusPVP*lrrReduction

    flows.loc[:,dates] = flows.loc[:,dates]*lrrReduction.astype(float)

    flows.loc['R_17_M',dates] = surplusReduced
    # TODO: in the current flows_generator.R this is set for R_17, but that
    #       tributary already has flow associated with it, and it seems more
    #       intuitive that the upper river would flow into the mainstem rather
    #       than into a tributary. Easily fixed if incorrect.
    return flows

def outputCombinedConnected(
        upperModel,
        lowerModel,
        name=None
):
    """
    """
    if name is None:
        name = ''
    elif not os.path.exists(name):
        os.mkdir(name)

    appropriativeBasinOutput = pd.concat([
        upperModel.appropriative.basinAllocations,
        lowerModel.appropriative.basinAllocations])
    riparianBasinOutput = pd.concat([
        upperModel.riparian.basinAllocations,
        lowerModel.riparian.basinAllocations])
    
    appropriativeUserOutput = pd.concat([
        upperModel.appropriative.userAllocations,
        lowerModel.appropriative.userAllocations])
    riparianUserOutput = pd.concat([
        upperModel.riparian.userAllocations,
        lowerModel.riparian.userAllocations])
    
    riparianBasinOutput.to_csv(os.path.join(
        name,'basin_riparian_output_'+os.path.split(name)[-1]+'.csv'))
    appropriativeBasinOutput.to_csv(os.path.join(
        name,'basin_appropriative_output_'+os.path.split(name)[-1]+'.csv'))
    
    riparianUserOutput.to_csv(os.path.join(
        name,'user_riparian_output_'+os.path.split(name)[-1]+'.csv'))
    appropriativeUserOutput.to_csv(os.path.join(
        name,'user_appropriative_output_'+os.path.split(name)[-1]+'.csv'))
    
    preferredOutput = output.makePreferredOutput([
        upperModel.appropriative,
        upperModel.riparian,
        lowerModel.appropriative,
        lowerModel.riparian
    ])
    preferredOutput.to_csv(os.path.join(
        name,'_preferred_output_'+os.path.split(name)[-1]+'.csv'))













