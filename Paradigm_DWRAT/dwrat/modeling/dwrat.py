import os
import pulp
import numpy as np
import pandas as pd
from numpy import typing as npt
from datetime import datetime
from dwrat.preprocessing import demandProcessing as dp
from dwrat.postprocessing import output
from dwrat.utils import logger
from typing import Dict
import dwrat.utils.riparian_lp_utils as rlp
import dwrat.utils.appropriative_lp_utils as alp

printModelingOutputs = False

class Model():
    """
    """
    def __init__(
            self,
            riparian: dp.UserGroup,
            appropriative: dp.UserGroup,
            flows: pd.DataFrame,
            basinConnectivity: pd.DataFrame,
            basinInfo: pd.DataFrame,
            dates: npt.ArrayLike = None,
            modelName: str = None,
            printLinearProblemMsg: bool = False
    ):
        nowTime = datetime.now()
        if modelName is not None:
            self.name = modelName
        else: # default name is hexadecimal timpestamp in seconds from 1970-01-01
            self.name = hex(round(nowTime.timestamp()))
        self.log = '{} MODEL LOG\n\n'.format(self.name)
        self.log += logger.logInstantiation(nowTime)
        if modelName is None:
            self.log += 'No model name specified, using {}.\n\n'.format(self.name)

        self.riparian = riparian
        self.log += logger.logUserSummary(self.riparian.users,'riparian')

        self.appropriative = appropriative
        self.log += logger.logUserSummary(self.appropriative.users,'appropriative')

        self.basinConnectivity = basinConnectivity
        self.basins = basinConnectivity.index.to_list()
        self.basinInfo = basinInfo
        self.log += logger.logBasins(self.basins)

        if dates is None:
            self.dates = flows.columns[flows.columns!='FLOWS_TO'].values
            self.log += logger.logAssumeDates(self.dates)
        else:
            # Check that all dates are in the format 'YYYY-MM'
            for date in dates:
                try:
                    datetime.strptime(date, '%Y-%m')  # Attempt to parse the date
                except ValueError:
                    raise ValueError(f"Date '{date}' is not in the required format 'YYYY-MM'.")

            self.dates = dates
            self.log += logger.logGivenDates(self.dates)

        self.printLinearProblemMsg = printLinearProblemMsg

        self.flows = flows[self.dates]

        self.riparian.getModelDemand(self.dates,inplace=True)
        self.appropriative.getModelDemand(self.dates,inplace=True)

    def initializeOutputs(
            self
    ):
        """
        """
        self.log += '    Model outputs initialized at {}.\n\n'.format(datetime.now().strftime('%H:%M:%S'))
        # Riparian outputs
        self.riparian.basinProportions = pd.DataFrame(
            columns=self.dates,
            index=self.basins)
        self.riparian.userAllocations = pd.DataFrame(
            columns=self.dates,
            index=self.riparian.users)
        self.riparian.basinAllocations = pd.DataFrame(
            columns=self.dates,
            index=self.basins)
        # Appropriative outputs
        self.appropriative.userAllocations = pd.DataFrame(
            columns=self.dates,
            index=self.appropriative.users)
        self.appropriative.basinAllocations = pd.DataFrame(
            columns=self.dates,
            index=self.basins)

    def run(
            self
    ):
        """
        """
        startTime = datetime.now()
        self.log += 'Model run began at {}.\n\n'.format(startTime.strftime('%H:%M:%S'))

        if self.flows.empty:
            raise ValueError('The flow data provided is empty. Please check the model inputs and ensure they are valid.')

        if self.basinConnectivity.empty:
            raise ValueError('The basin connectivity data provided is empty. Please check the model inputs and ensure they are valid.')

        # Check that all dates are in the format 'YYYY-MM'
        for date in self.dates:
            try:
                datetime.strptime(date, '%Y-%m')  # Attempt to parse the date
            except ValueError:
                raise ValueError(f"Date '{date}' is not in the required format 'YYYY-MM'.")

        self.initializeOutputs()

        self.riparian.userAllocations, self.riparian.basinAllocations, \
            self.riparian.basinProportions, self.availableFlow = riparianLP(
                self.dates,
                self.riparian,
                self.basinConnectivity,
                self.basins,
                self.flows,
                self.riparian.basinProportions,
                printLinearProblemMsg=self.printLinearProblemMsg
            )

        self.appropriative.userAllocations, \
            self.appropriative.basinAllocations = appropriativeLP(
                self.dates,
                self.appropriative,
                self.basins,
                self.availableFlow,
                self.appropriative.userAllocations,
                self.appropriative.basinAllocations,
                printLinearProblemMsg=self.printLinearProblemMsg
            )
            
            
        self.checkReplicatedPriorityDates()

        endTime = datetime.now()
        self.log += 'Model run finished at {}.\n\n'.format(endTime.strftime('%H:%M:%S'))
        self.log += 'Approximate model runtime: {} seconds\n\n'.format((endTime-startTime).seconds)
    
    def checkReplicatedPriorityDates(
            self
    ):
        if len(self.appropriative.replicatedPriorityDates)==0:
            return
        pWarning = True
        replicatedPriorityDates = self.appropriative.replicatedPriorityDates
        for uDate in replicatedPriorityDates:
            users = replicatedPriorityDates[uDate]
            demand = self.appropriative.modelDemand.loc[users]
            allocations = self.appropriative.userAllocations.loc[users]
            compare = (demand.astype(float).round(4)).compare(allocations.astype(float).round(4))
            if len(compare)==0:
                continue
            else:
                if pWarning:
                    print('{}: Check log output for potential curtailment inconsistencies!'.format(self.name))
                    self.log += '!!! !!! WARNING !!! !!!\n\n'
                self.log += 'The following users have the same priority date ({}):\n'.format(uDate)
                self.log += '    {}\n'.format(users)
                self.log += '    and may show inconsistent curtailment for month(s):\n'
                self.log += '    {}\n'.format(compare.columns.get_level_values(0).unique().tolist())
                pWarning = False

    def writeOutputs(
            self,
            directoryPath: os.PathLike = None
    ):
        """
        """
        if directoryPath is None:
            directoryPath = ''
        elif not os.path.exists(directoryPath):
            os.mkdir(directoryPath)

        output.riparianBasinOutput(
            userMatrix=self.riparian.userMatrix,
            modelDemand=self.riparian.modelDemand,
            flows=self.flows,
            riparianBasinProportions=self.riparian.basinProportions,
            name='_'+self.name,
            directoryPath=directoryPath
        )
        output.userOutput(
            dates=self.dates,
            users=self.riparian.users,
            userData=self.riparian.data,
            modelDemand=self.riparian.modelDemand,
            userAllocations=self.riparian.userAllocations,
            userGroup='riparian',
            name='_'+self.name,
            directoryPath=directoryPath
        )
        output.appropriativeBasinOutput(
            appropriative=self.appropriative,
            availableFlow=self.availableFlow,
            appropriativeBasinAllocations=self.appropriative.basinAllocations,
            name='_'+self.name,
            directoryPath=directoryPath
        )
        output.userOutput(
            dates=self.dates,
            users=self.appropriative.users,
            userData=self.appropriative.data,
            modelDemand=self.appropriative.modelDemand,
            userAllocations=self.appropriative.userAllocations,
            userGroup='appropriative',
            name='_'+self.name,
            directoryPath=directoryPath
        )

        self.flows.to_csv(
            os.path.join(
                directoryPath,
                'flows.csv'))

        self.basinConnectivity.to_csv(
            os.path.join(
                directoryPath,
                'basin_connectivity_matrix.csv'))

        self.appropriative.allData.to_csv(
            os.path.join(
                directoryPath,
                'appropriative_demand.csv'))

        self.appropriative.connectivityMatrix.to_csv(
            os.path.join(
                directoryPath,
                'appropriative_user_connectivity_matrix.csv'))

        self.appropriative.userMatrix.to_csv(
            os.path.join(
                directoryPath,
                'appropriative_user_matrix.csv'))

        self.riparian.allData.to_csv(
            os.path.join(
                directoryPath,
                'riparian_demand.csv'))

        self.riparian.connectivityMatrix.to_csv(
            os.path.join(
                directoryPath,
                'riparian_user_connectivity_matrix.csv'))

        self.riparian.userMatrix.to_csv(
            os.path.join(
                directoryPath,
                'riparian_user_matrix.csv'))
        
        with open(os.path.join(
            directoryPath,
            '_{}.log'.format(self.name)
        ),'w') as f:
            f.write(self.log)

        preferredOutput = output.makePreferredOutput(
            [self.appropriative,self.riparian])
        preferredOutput.to_csv(
            os.path.join(
                directoryPath,
                '_preferred_output.csv'))

def riparianLP(
        dates: npt.ArrayLike,
        riparian: dp.UserGroup,
        basin_connectivity: pd.DataFrame,
        basins: npt.ArrayLike,
        flows: pd.DataFrame,
        riparianBasinProportions: pd.DataFrame,
        printLinearProblemMsg: bool = False
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    Riparian Linear Program

    Parameters:
    - dates: Array of valid dates in 'YYYY-MM' format
    - riparian: Riparian user group with model demand, priority, and connectivity matrix
    - basins: Array of basins
    - flows: DataFrame containing flow data for each basin
    - riparianBasinProportions: DataFrame to store the user allocations
    - printLinearProblemMsg: Boolean that suppresses the LP solver messages

    Returns:
    - Tuple of the updated riparianBasinAllocations and riparianUserAllocations as well as the basinProportions and availableFlow
    """

    # Check for matching basins
    rlp.validateBasins(flows, basin_connectivity)

    # Initialize the downstream penalty
    downstream_penalty = rlp.initializeRiparianDownstreamPenalty(basin_connectivity, basins)

    for day in dates:
        # Build the riparian linear problem for the current date
        riparian_lp, basin_proportions, basin_demand = rlp.buildRiparianLinearProblem(
            riparian,
            basin_connectivity,
            flows,
            downstream_penalty,
            basins,
            day
        )

        # Solve the problem using pulp solver
        riparian_lp.solve(pulp.PULP_CBC_CMD(msg=printLinearProblemMsg))

        if printModelingOutputs:
            # Print relevant outputs
            rlp.printRiparianModelingOutputs(
                riparian_lp,
                basin_proportions,
                basin_demand,
                basins
            )

        # Populate the basin proportions
        rlp.populateRiparianBasinProportions(
            basin_proportions,
            riparianBasinProportions,
            basins,
            day
        )

    # Calculate the riparian linear problem outputs
    riparianUserAllocations, riparianBasinAllocations, basin_proportion_matrix = rlp.calculateRiparianOutputs(
        riparian,
        riparianBasinProportions,
        dates,
        basins
    )

    # Calculate the resulting available flow
    availableFlow = rlp.calculateNetAvailableFlow(
        riparian,
        basin_connectivity,
        flows,
        basin_proportion_matrix,
        dates,
        basins
    )

    return riparianUserAllocations, riparianBasinAllocations, riparianBasinProportions, availableFlow

def appropriativeLP(
        dates: npt.ArrayLike,
        appropriative: dp.UserGroup,
        basins: npt.ArrayLike,
        availableFlow: pd.DataFrame,
        appropriativeUserAllocations: pd.DataFrame,
        appropriativeBasinAllocations: pd.DataFrame,
        printLinearProblemMsg: bool = False
):
    """
    Appropriative Linear Program

    Parameters:
    - dates: Array of valid dates in 'YYYY-MM' format
    - appropriative: Appropriative user group with model demand, priority, and connectivity matrix
    - basins: Array of basins
    - availableFlow: DataFrame with available flow data for every basin on each date
    - appropriativeUserAllocations: DataFrame to store the user allocations
    - appropriativeBasinAllocations: DataFrame to store the basin allocations
    - printLPMsg: Boolean that suppresses the LP solver messages

    Returns:
    - Tuple of the updated appropriativeUserAllocations and appropriativeBasinAllocations DataFrames
    """

    if not alp.checkAvailableFlowDates(availableFlow, dates):
        raise ValueError("The dates in availableFlow do not match the provided dates.")

    # Loop through all dates
    for c,day in enumerate(dates):
        if printModelingOutputs:
            print(day)

        # Get the demand and shortage penalty for the current date
        app_demand, shortage_penalty = alp.getAppropriativeDemandAndPenalty(appropriative, day)

        # Available flow in each basin for the current date
        app_available_flow = {basin: availableFlow[day][basin] for k,basin in enumerate(basins)}

        # Build the linear problem
        appropriative_lp, user_allocation = alp.buildAppropriativeLinearProblem(appropriative, basins, app_available_flow, app_demand, shortage_penalty)

        # Solve the linear problem
        appropriative_lp.solve(pulp.PULP_CBC_CMD(msg=printLinearProblemMsg))

        if printModelingOutputs:
            print('status:',pulp.LpStatus[appropriative_lp.status])
            print('Objective = ',pulp.value(appropriative_lp.objective))

        # Update the user allocations and calculate the basin allocations based on LP result
        user_allocations = alp.updateAppropriativeUserAllocations(user_allocation, appropriative.users)
        app_basin_allocations = np.dot(appropriative.userMatrix.to_numpy(), user_allocations)

        if printModelingOutputs:
            print('Basin Appropriative Allocations:')
            print(app_basin_allocations)

        # Update/Populate output tables
        alp.populateAppropriativeUserAllocations(appropriativeUserAllocations, user_allocation, appropriative.users, day)

        if printModelingOutputs:
            print(c + 1, 'of', len(dates), 'complete. Processing day:', day)

        alp.populateAppropriativeBasinAllocations(appropriativeBasinAllocations, basins, app_basin_allocations, day)

    return appropriativeUserAllocations, appropriativeBasinAllocations