import os
import typing
import numpy as np
import pandas as pd
import numpy.typing as npt
from datetime import datetime
from typing import Tuple,List, Callable, Any, Iterable, Union


class UserGroup(): # TODO:
    # potential attributes:
    #   - user list
    #   - user matrix
    #   - user connectivity matrix
    #   - monthly demand
    #   - user data (basin, priority)
    """
    """
    def __init__(
            self,
            groupName: str,
            demandDataFrame: pd.DataFrame,
            basinConnectivityMatrix: pd.DataFrame,
    ):
        self.groupName = groupName
        self.allData = demandDataFrame
        self.users = demandDataFrame.index.values
        self.data = demandDataFrame[['BASIN','PRIORITY']]

        self.monthlyDemand = getMonthlyDemand(demandDataFrame)
        self.modelDemand = None

        userMatrix,userConnectivity = makeUserMatrices(demandDataFrame,basinConnectivityMatrix)
        self.userMatrix = userMatrix
        self.connectivityMatrix = userConnectivity

    def getModelDemand(
            self,
            dates: npt.ArrayLike,
            inplace: bool = False,
    ) -> pd.DataFrame | None:
        """
        """
        if len(dates) == 0:
            raise ValueError(f"No dates were provided, at least one month is required for modeling.")

        # Check that all dates are in the format 'YYYY-MM'
        for date in dates:
            try:
                datetime.strptime(date, '%Y-%m')  # Attempt to parse the date
            except ValueError:
                raise ValueError(f"Date '{date}' is not in the required format 'YYYY-MM'.")

        modelDemand = pd.DataFrame(
            data=self.monthlyDemand.sort_index(axis='index',inplace=False)[pd.to_datetime(dates).strftime('%b').str.upper()+'_MEAN_DIV'].to_numpy(),
            index=self.monthlyDemand.index.sort_values(),
            columns=dates
        )
        if inplace:
            self.modelDemand = modelDemand
        else:
            return modelDemand


def overrideDemand(
        demandDataFrame: pd.DataFrame,
        demandOverride: Union[dict,pd.DataFrame] = None
    ):
    """
    describe demand override function
    """
    if type(demandOverride) is dict: # when demand override is a dictionary
        for user in demandOverride.keys():
            if user in demandDataFrame.index.to_list():
                cols = demandDataFrame.columns[demandDataFrame.columns.str.contains('_MEAN_DIV')]
                demandDataFrame.loc[user,cols] = demandOverride[user]
    elif type(demandOverride) is pd.DataFrame: # when demand override is a dataframe
        for user in demandOverride.index:
            if user in demandDataFrame.index.to_list():
                demandDataFrame.loc[user,demandOverride.columns] = demandOverride.loc[user].to_list()

    return demandDataFrame



def processDemand(
        # TODO: specify columns for demand file
        basinConnectivityMatrix: pd.DataFrame,
        demandFilePath: typing.Union[str,os.PathLike] = None,
        demandOverride: list = None,
        demandSubset: dict = None,
        priorityOverride: dict = None,
        outputToFile: bool = False,
        outputFilePath: typing.Union[str,os.PathLike] = None,
) -> typing.Tuple[pd.DataFrame,pd.DataFrame]:
    """

    """
    # # create monthly demand column names
    # # these should already exist in demand file
    # # TODO: verify these columns exist

    # # TODO: include test if monthly demand columns exist in file
    demand = pd.read_csv(demandFilePath,index_col='APPLICATION_NUMBER')

    # filter out entries in basins not included in the domain
    demand = demand.loc[demand['BASIN'].isin(basinConnectivityMatrix.index)]

    # name the index
    demand.index.name = 'USER'

    if demandOverride is not None:
        for override in demandOverride:
            demand = overrideDemand(
                demandDataFrame=demand,
                demandOverride=override
            )

    if demandSubset is not None:
        for key in list(demandSubset.keys()):
            demand = demand.loc[demand[key]==demandSubset[key]]

    riparian,appropriative,replicatedPriorityDates = separateUserGroups(demand,priorityOverride)
    # TODO: optional save

    riparian = UserGroup('Riparian',riparian,basinConnectivityMatrix)
    appropriative = UserGroup('Appropriative',appropriative,basinConnectivityMatrix)
    appropriative.replicatedPriorityDates = replicatedPriorityDates
    
    return riparian,appropriative

def getMonthlyDemand(
        demand: pd.DataFrame,
) -> pd.DataFrame:
    """
    """
    demandCols = []
    for m in range(12):
        demandCols += ['{}_MEAN_DIV'.format(datetime(
            year=1,month=m+1,day=1).strftime('%b').upper())]
    demand[demandCols] = demand[demandCols].replace(0,0.00002022)
    demand[demandCols] = demand[demandCols].fillna(0.00002022) # TODO: should it be 0.0002022 or 0.00002022?
    return demand[demandCols]

def separateUserGroups(
        demandDataFrame: pd.DataFrame,
        priorityOverride: dict = None,
) -> Tuple[pd.DataFrame,pd.DataFrame]:
    """
    """
    riparian = demandDataFrame.loc[demandDataFrame['RIPARIAN']=='Y'].sort_index(axis='index',inplace=False)
    riparian.insert(
        len(riparian.columns),
        'PRIORITY',
        10000000
    )

    if demandDataFrame.columns.__contains__('ASSIGNED_PRIORITY_DATE'):
        priorityCol = 'ASSIGNED_PRIORITY_DATE'
    else:
        priorityCol = 'ASSIGNED_PRIORITY_DATE_SUB'
    appropriative = demandDataFrame.loc[demandDataFrame["RIPARIAN"]=='N']
    appropriative.insert(
        len(appropriative.columns),
        'PRIORITY',
        appropriative[priorityCol]\
            .sample(frac=1)\
            .rank(axis=0,method='first')\
            .reindex_like(appropriative[priorityCol])
    ) # TODO: what to do when multiple users have the same assigned priority date?

    appropriative = appropriative.sort_index(axis='index',inplace=False)

    for demand in [appropriative,riparian]:
        if priorityOverride is not None:
            for user in list(priorityOverride.keys()):
                if user in demand.index.to_list():
                    demand.loc[user,'PRIORITY'] = priorityOverride[user]

    ##########################################################################
    # When multiple users have the same assigned priority date:
    # 
    replicatedPriorityDates = {}
    uniqueDates = appropriative[priorityCol].unique()
    for uDate in uniqueDates:
        users = appropriative.loc[appropriative[priorityCol]==uDate].index.to_list()
        if len(users)>1:
            replicatedPriorityDates[uDate] = users
    ##########################################################################

    return riparian,appropriative,replicatedPriorityDates

def makeUserMatrices(
        usersDataFrame: pd.DataFrame,
        basinConnectivityMatrix: pd.DataFrame,
        outputToFile: bool = False,
        outputFilePath: typing.Union[str,os.PathLike] = None,
) -> typing.Tuple[pd.DataFrame,pd.DataFrame]:
    """

    """
    if usersDataFrame.empty or basinConnectivityMatrix.empty:
        raise ValueError("Input parameters should not be empty. Please provide a valid user dataframe and basin connectivity matrix.")

    basins = basinConnectivityMatrix.index.values
    userMatrix = makeUserMatrix(usersDataFrame, basins)
    userConnectivity = makeUserConnectivityMatrix(basinConnectivityMatrix, basins, userMatrix)
    return userMatrix,userConnectivity

def makeUserConnectivityMatrix(
        basinConnectivityMatrix: pd.DataFrame,
        basins: Iterable,
        userMatrix: pd.DataFrame,
) -> pd.DataFrame:
    """
    """
    if basinConnectivityMatrix.empty or userMatrix.empty or basins.size == 0:
        raise ValueError("Input parameters should not be empty. Please provide a valid basin connectivity and user matrices.")

    if set(basinConnectivityMatrix.index) != set(userMatrix.index):
        raise ValueError("Basin connectivity and user matrices have mismatching basins.")

    basinConnectivity = basinConnectivityMatrix.sort_index(axis='index')
    basinConnectivity = basinConnectivity.sort_index(axis='columns')
    basinConnectivity = basinConnectivity.to_numpy()
    userConnectivity = np.matmul(basinConnectivity.T,userMatrix)
    userConnectivity.index = pd.Index(basins,name='BASIN')

    return userConnectivity

def makeUserMatrix(
        usersDataFrame: pd.DataFrame,
        basins: Iterable,
) -> pd.DataFrame:
    """
    """
    users = usersDataFrame.index.values
    userLocation = usersDataFrame['BASIN'].to_numpy()

    basinUse = {user: userLocation[i] for i, user in enumerate(users)}
    indexDict = {basin: [k] for k, basin in enumerate(basins)}

    userMatrix = np.zeros([np.size(users),np.size(basins)],dtype=int)

    for i,user in enumerate(users):
        basin = basinUse.get(user)
        if basin is not None and basin in indexDict: # Ensure the basin exists
            userMatrix[i][indexDict[basin]] = 1

    userMatrix = userMatrix.T

    userMatrix = pd.DataFrame(userMatrix, index = basins)
    userMatrix.index.name = 'BASIN'
    userMatrix.columns = users
    return userMatrix
