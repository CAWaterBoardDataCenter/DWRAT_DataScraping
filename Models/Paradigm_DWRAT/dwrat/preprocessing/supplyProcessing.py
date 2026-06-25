import os
import typing
import pandas as pd
from pandera.typing import DataFrame
from .basinProcessing import basinRelationshipSchema, readBasinInputFile


def processPRMSFlows(
        basinRelationships: DataFrame[basinRelationshipSchema] = None,
        basinFilePath: typing.Union[str,os.PathLike] = None, # TODO: is there a better way to specify the basin IDs in the flow file than to read a different file?
        supplyFilePath: typing.Union[str,os.PathLike] = None,
        outputToFile: bool = False,
        outputFilePath: typing.Union[str,os.PathLike] = None,
) -> pd.DataFrame:
    """
    

    """
    if basinRelationships is None:
        if basinFilePath is None:
            # TODO: should this be the default?
            basinFilePath = os.path.join('dwrat', 'input','generated_basins.csv')
    if supplyFilePath is None:
        # TODO: should this be the default?
        supplyFilePath = os.path.join('dwrat', 'input','formatted_supply.csv')

    # TODO: include test regarding number of basins and number of columns in supply csv
    
    if basinRelationships is None:
        basinRelationships,_ = readBasinInputFile(basinFilePath=basinFilePath)

    if basinRelationships.columns.__contains__('MAINSTEM'):
        headwaterBasins = basinRelationships[basinRelationships['MAINSTEM']=='N']
        headwaterBasins = headwaterBasins[['BASIN','FLOWS_TO']]
        headwaterBasins.set_index('BASIN',inplace=True,drop=True)
        mainstemBasins = basinRelationships[basinRelationships['MAINSTEM']=='Y']
        mainstemBasins = mainstemBasins[['BASIN','FLOWS_TO']]
        mainstemBasins.set_index('BASIN',inplace=True)

    flows = pd.read_csv(supplyFilePath)
    # TODO: include test regarding date format for supply file?
    flows['Date'] = pd.to_datetime(flows['Date']).dt.strftime('%Y-%m')

    if basinRelationships.columns.__contains__('MAINSTEM'):
        for date in flows['Date'].values:
            mainstemBasins[date] = 0
            headwaterBasins[date] = 0

    flows = flows.set_index('Date',drop=True)
    flows.index.name = None

    if basinRelationships.columns.__contains__('MAINSTEM'):
        flows.columns = headwaterBasins.index
    else:
        flows.columns = basinRelationships['BASIN']
    flows = flows.T
    flows.index.name = 'BASIN'
    if basinRelationships.columns.__contains__('MAINSTEM'):
        flows.insert(0,'FLOWS_TO',headwaterBasins['FLOWS_TO'].values)
    else:
        flows.insert(0,'FLOWS_TO',basinRelationships['FLOWS_TO'].values)

    if basinRelationships.columns.__contains__('MAINSTEM'):
        flows = pd.concat([flows,mainstemBasins],axis=0)
    
    flows = flows.sort_index()
    # flows.columns.name = None

    # if basinRelationships.columns.__contains__('MAINSTEM'):
    #     for basin in flows.index[basinRelationships['MAINSTEM']=='Y']:
    #         flows.loc[basin,flows.columns[flows.columns!='FLOWS_TO']] = flows.loc[flows['FLOWS_TO']==basin,flows.columns[flows.columns!='FLOWS_TO']].sum(axis=0)

    # TODO: optional save

    return flows