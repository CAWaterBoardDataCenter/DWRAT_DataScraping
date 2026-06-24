import os
import numpy as np
import pandas as pd
import datetime
from numpy import typing as npt
from dwrat.preprocessing.demandProcessing import UserGroup


def riparianBasinOutput(
        userMatrix: pd.DataFrame,
        modelDemand: pd.DataFrame,
        flows: pd.DataFrame,
        riparianBasinProportions: pd.DataFrame,
        name: str,
        directoryPath: os.PathLike = '',
):
    """
    """
    if userMatrix.empty:
        raise ValueError('The user matrix provided is empty. Please ensure the dataframe has valid data.')

    if modelDemand.empty:
        raise ValueError('The model demand provided is empty. Please ensure the dataframe has valid data.')

    basinDemand = userMatrix.dot(modelDemand)
    basinAllocations = riparianBasinProportions.multiply(basinDemand)

    # riparian basin output
    flows_table = flows.add_suffix('_FLOW')
    demand_table = basinDemand.add_suffix('_DEMAND')
    basin_riparian_output = pd.merge(flows_table, demand_table,  left_index=True, right_index=True)
    proportions_table = riparianBasinProportions.add_suffix('_PROPORTIONS')
    basin_riparian_output = pd.merge(basin_riparian_output, proportions_table,  left_index=True, right_index=True)
    allocations = basinAllocations.add_suffix('_ALLOCATIONS')
    basin_riparian_output = pd.merge(basin_riparian_output, allocations,  left_index=True, right_index=True)
    basin_riparian_output = basin_riparian_output.sort_index(axis=1)

    basin_riparian_output.to_csv(os.path.join(directoryPath,'basin_riparian_output' + name + '.csv'))


def appropriativeBasinOutput(
        appropriative: UserGroup,
        availableFlow: pd.DataFrame,
        appropriativeBasinAllocations: pd.DataFrame,
        name: str,
        directoryPath: os.PathLike = '',
):
    """
    """
    if availableFlow.empty:
        raise ValueError('The available flow provided is empty. Please ensure the dataframe has valid data.')

    if appropriativeBasinAllocations.empty:
        raise ValueError('The basin allocations provided are empty. Please ensure the dataframe has valid data.')

    app_basin_demand_df = appropriative.userMatrix.dot(appropriative.modelDemand)

    # appropriative basin output
    flows_table_app = availableFlow.add_suffix('_AVAILABLE_FLOW')
    demand_table_app = app_basin_demand_df.add_suffix('_DEMAND')
    basin_appropriative_output = pd.merge(flows_table_app, demand_table_app,  left_index=True, right_index=True)
    allocations_app = appropriativeBasinAllocations.add_suffix('_ALLOCATIONS')
    basin_appropriative_output = pd.merge(basin_appropriative_output, allocations_app,  left_index=True, right_index=True)
    basin_appropriative_output = basin_appropriative_output.sort_index(axis=1)

    basin_appropriative_output.to_csv(os.path.join(directoryPath,'basin_appropriative_output' + name + '.csv'))


def userOutput(
        dates: npt.ArrayLike,
        users: npt.ArrayLike,
        userData: pd.DataFrame,
        modelDemand: pd.DataFrame,
        userAllocations: pd.DataFrame,
        userGroup: str,
        name: str,
        directoryPath: os.PathLike = '',
):
    """
    """
    # user shortage
    userShortage = pd.DataFrame(columns=dates,index=users)
    for day in dates:
        userShortage[day] = np.divide(
            (modelDemand[day]-userAllocations[day]),
            modelDemand[day],
            out=np.zeros_like(
                modelDemand[day]-userAllocations[day]),
                where=modelDemand[day]!=0)*100
        userShortage.loc[userShortage[day]<1,day] = 0
        # userShortage[day][userShortage[day]<1] = 0
    # user curtailment
    userCurtailment = pd.DataFrame(columns=dates,index=users)
    for day in dates:
        userCurtailment.loc[userShortage[day]>0,day] = 1
        userCurtailment.loc[userShortage[day]==0,day] = 0
    #
    userShortage = userShortage.add_suffix('_SHORTAGE_%')
    userCurtailment = userCurtailment.add_suffix('_CURTAILMENT')
    # userData = None
    #
    userAllocations = userAllocations.add_suffix('_ALLOCATIONS')
    userOutput = pd.merge(userData,userAllocations,left_index=True,right_index=True)
    modelDemand = modelDemand.add_suffix('_DEMAND')
    userOutput = pd.merge(userOutput,modelDemand,left_index=True,right_index=True)
    userOutput = pd.merge(userOutput,userShortage,left_index=True,right_index=True)
    userOutput = pd.merge(userOutput,userCurtailment,left_index=True,right_index=True)
    userOutput = userOutput.sort_index(axis=1)

    userOutput.to_csv(os.path.join(directoryPath,'user_{}_output{}.csv'.format(userGroup,name)))

def makePreferredOutput(
        userGroups,
        dwratDates = None
):
    def convert_to_float(value):
        if isinstance(value, np.floating):
            return value.item()
        return value

    columns=['USER','BASIN','Demand','Allocations','Shortage','Curtailment',
         'Curtailment_YN','PRIORITY','Rank','Year','Month Number','Date']
    data = {c: [] for c in columns}

    for userGroup in userGroups:
        maxPriority = int(convert_to_float(max(userGroup.allData['PRIORITY'])))
        for user in userGroup.users:
            userData = userGroup.allData.loc[user]
            userAllocations = userGroup.userAllocations.loc[user]
            if dwratDates is None:
                dates = userAllocations.index
            for date in dates:
                year,month = date.split('-')
            
                data['USER'] += [user]
                data['BASIN'] += [userData['BASIN']]
                data['Demand'] += [convert_to_float(userData['{}_MEAN_DIV'.format(datetime.date(int(year),int(month),1).strftime('%b').upper())])]
                data['Allocations'] += [convert_to_float(userAllocations[date])]
                data['Shortage'] += [np.nan] # will be calculated below
                data['Curtailment'] += [np.nan] # will be calculated below
                data['Curtailment_YN'] += [np.nan] # will be calculated below
                data['PRIORITY'] += [int(convert_to_float(userData['PRIORITY']))]
                data['Rank'] += [1+maxPriority-data['PRIORITY'][-1]]
                data['Year'] += [int(year)]
                data['Month Number'] += [int(month)]
                data['Date'] += ['{}/1/{}'.format(month,year)]

    preferredOutput = pd.DataFrame.from_dict(data)

    preferredOutput['Shortage'] = (100*(preferredOutput['Demand']-preferredOutput['Allocations'])/preferredOutput['Demand']).astype(float).round(2)
    preferredOutput['Curtailment_YN'] = preferredOutput['Curtailment_YN'].astype(str)
    preferredOutput.loc[preferredOutput['Demand']>preferredOutput['Allocations'],['Curtailment','Curtailment_YN']] = [1,'Y']
    preferredOutput.loc[preferredOutput['Demand']<=preferredOutput['Allocations'],['Curtailment','Curtailment_YN']] = [0,'N']
    preferredOutput['Curtailment'] = preferredOutput['Curtailment'].astype(int)

    preferredOutput = preferredOutput.set_index('USER',drop=True)
    
    return preferredOutput








