import os
import typing
import numpy as np
import pandas as pd
import pandera as pa
from pandera.typing import DataFrame,Series
from typing import Iterable, Union


class basinRelationshipSchema(pa.DataFrameModel):
    BASIN: Series[object]
    FLOWS_TO: Series[object]


# TODO: CREATE WAY FOR USER TO SPECIFY BASINS TO USE


def inferOutletBasin(
        basinRelationships: DataFrame[basinRelationshipSchema] = None,
) -> str:
    """
    Use the basin relationship table to infer which basin is the outlet basin.
    """
    # The basins that flow to a basin that is not in basins column are
    # potential outlet basins
    outlets = basinRelationships.loc[
        ~basinRelationships['FLOWS_TO'].isin(basinRelationships['BASIN']),
        'BASIN'].tolist()
    # The basins that flow to themselves are potential outlet basins
    outlets += basinRelationships.loc[basinRelationships[['BASIN','FLOWS_TO']]
                                      .nunique(axis=1)==1,'BASIN'].tolist()
    outlets = np.unique(outlets)
    if len(outlets) < 1: # No potential outlets found
        raise ValueError(
            'Outlet basin cannot be inferred! '+\
            'Please specify an outlet basin!')
    elif len(outlets) > 1: # More than one potential outlet found
        raise ValueError(
            'More than one outlet basin can be inferred! '+\
            'Please specify an outlet basin!')
    elif len(outlets) == 1:
        return outlets[0]


def readBasinInputFile(
        basinFilePath: typing.Union[str,os.PathLike] = None,
        basinSubset: Union[list,dict] = None,
) -> DataFrame[basinRelationshipSchema]:
    """
    Reads the input basin file and confirms the schema
    """
    basinRelationships = pd.read_csv(basinFilePath)
    basinRelationships = DataFrame[basinRelationshipSchema](basinRelationships)

    if basinSubset is not None: # TODO: move to a different function?
        if type(basinSubset) is list:
            basinRelationships = basinRelationships.loc[basinRelationships.index.isin(basinSubset)]
        elif type(basinSubset) is dict:
            for key in list(basinSubset.keys()):
                basinRelationships = basinRelationships.loc[basinRelationships[key]==basinSubset[key]]

    if basinRelationships.columns.__contains__('MAINSTEM'):
        if basinRelationships.columns.__contains__('UPPER_RUSSIAN'):
            basinInfo = basinRelationships.set_index('BASIN')[['FLOWS_TO','MAINSTEM','UPPER_RUSSIAN']]
        else:
            basinInfo = basinRelationships.set_index('BASIN')[['FLOWS_TO','MAINSTEM']]
    else:
        if basinRelationships.columns.__contains__('UPPER_RUSSIAN'):
            basinInfo = basinRelationships.set_index('BASIN')[['FLOWS_TO','UPPER_RUSSIAN']]
        else:
            basinInfo = basinRelationships.set_index('BASIN')[['FLOWS_TO']]

    return basinRelationships, basinInfo


def makeBasinConnectivityMatrix(
        outlet: str = None,
        basinFilePath: typing.Union[str,os.PathLike] = None,
        basinRelationships: DataFrame[basinRelationshipSchema] = None,
        basinSubset: Union[list,dict] = None,
        outputToFile: bool = False,
        outputFilePath: typing.Union[str,os.PathLike] = None,
) -> typing.Tuple[pd.DataFrame, pd.DataFrame]:
    """

    """
    # If basin relationships are not specified we must read them from file
    if basinRelationships is None:
        # If basin file path is not specified, we can try and find the default
        if basinFilePath is None:
            # TODO: should this be the default input filepath?
            basinFilePath = os.path.join('input','generated_basins.csv')
        # Read basin file
        basinRelationships, basinInfo = readBasinInputFile(basinFilePath=basinFilePath,basinSubset=basinSubset)
    
    # If specified outlet basin is NOT the downstream-most basin we want to remove everything downstream
    if outlet is not None:
        # TODO: move below to another function?
        basinIDs = getAllUpstreamBasins(outlet,basinRelationships[['BASIN','FLOWS_TO']])
        basinIDs = np.append([outlet],basinIDs)
        basinRelationships = basinRelationships.loc[basinRelationships['BASIN'].isin(basinIDs)]
        basinInfo = basinInfo.loc[basinInfo.index.isin(basinIDs)]
    # If no outlet is specified, we can try to infer the outlet from the file
    elif outlet is None:
        outlet = inferOutletBasin(basinRelationships)

    basins = basinRelationships['BASIN'].values
    flowsTo = basinRelationships['FLOWS_TO'].to_numpy()

    # Dictionaries
    flowsToDict = {basins[k] : flowsTo[k] for k in range(len(basins))}
    indexDict = {basins[k] : [k] for k in range(len(basins))}

    # Initialize empty basin x basin identity matrix
    connectivityMatrixArray = np.identity(np.size(basins), dtype = int)

    for k, basin in enumerate(basins):
        while basin != outlet:
            connectivityMatrixArray[k][indexDict[flowsToDict[basin]]] = 1
            basin = flowsToDict[basin]

    basinConnectivityMatrix = pd.DataFrame(
        connectivityMatrixArray,
        index = pd.Index(basins,name='BASIN'),
        columns = basins
    )

    # Output dataframe to CSV file
    if outputToFile:
        if outputFilePath is None:
            # TODO: should this be the default output filepath?
            outputFilePath = 'basin_connectivity_matrix.csv'
        basinConnectivityMatrix.to_csv(
            outputFilePath, index = True)

    return basinConnectivityMatrix, basinInfo


def getAllUpstreamBasins(
        ID: str,
        routingTable: pd.DataFrame, # TODO: this should be a two-column dataframe where the first column is ID and second column is downstream ID
        upstreamIDs = None,
        originalID = None
    ) -> np.array:
    """
    """
    if ID == originalID:
        raise(ValueError('Cyclic routing detected in basin relationships!'))
    if originalID is None:
        originalID = ID
    # Init upstreamIDs if not provided
    if upstreamIDs is None:
        upstreamIDs = np.array([])

    # Add id if not yet processed
    upstreamIDs = np.unique(np.append(upstreamIDs,ID))

    IDcol, dsIDcol = routingTable.columns.to_list()
    usIDs = routingTable.loc[routingTable[dsIDcol]==ID,IDcol].values

    usIDs = usIDs[usIDs!=ID] # if, for whatever reason, a drainage routes to itself this routine would loop infinitely

    upstreamIDs = np.append(upstreamIDs,usIDs)
    for usID in usIDs:
        upstreamIDs = getAllUpstreamBasins(
            ID=usID,
            routingTable=routingTable,
            upstreamIDs=upstreamIDs,
            originalID=originalID)

    return upstreamIDs


def getAllDownstreamBasins( # TODO: this function may not be necessary
        ID: str,
        routingTable: pd.DataFrame, # TODO: this should be a two-column dataframe where the first column is ID and second column is downstream ID
        downstreamIDs: Iterable = np.array([],dtype=object)
) -> np.array:
    """
    """
    IDcol, dsIDcol = routingTable.columns.to_list()
    dsID = routingTable.loc[routingTable[IDcol]==ID,dsIDcol].values
    if len(dsID) == 1:
        dsID = dsID.item()
    elif len(dsID) == 0:
        print('{}: No downstream ID found, assuming drains to outlet.'.format(ID))
        dsID = '0' # a value of zero corresponds to nothing downstream
    downstreamIDs = np.append(downstreamIDs,[dsID])
    if dsID in routingTable[IDcol].tolist():
        downstreamIDs = getAllDownstreamBasins(dsID,routingTable,downstreamIDs)
    elif dsID != '0':
        print('{}: Downstream ID ({}) not included as a PROJID'.format(ID,dsID))
    return downstreamIDs