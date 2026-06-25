import pandas as pd
import numpy as np
import numpy.typing as npt
import pulp
from typing import Dict
import dwrat.preprocessing.demandProcessing as dp

def validateBasins(flows: pd.DataFrame, basin_connectivity: pd.DataFrame) -> None:
    """ Check that the basins in the flow data match the basins in the basin connectivity matrix """

    if flows.index.to_list() != basin_connectivity.index.to_list():
        raise ValueError('The basin connectivity and flow matrix have mismatching basins.')

def getRiparianBasinDemand(riparian: dp.UserGroup, day: str) -> Dict[str, float]:
    """ Generate the basin demand for the riparian linear problem based on the provided date """

    # Validation checks
    if len(riparian.modelDemand) == 0 or riparian.modelDemand.shape[1] == 0:
        raise ValueError(f'It appears that the provided user group is missing a model demand.')

    if day not in riparian.modelDemand.columns.to_list():
        raise ValueError(f'The date provided ({day}) does not have associated model demand data.')

    # Calculate basin-level demand
    # - Basin-wide demand is the sum of user demand upstream of each basin.
    # - 1 x i list of user demand ∙ i x k user connectivity matrix  = 1 x k basin demand matrix
    return riparian.userMatrix.dot(riparian.modelDemand[day]).to_dict()

def initializeRiparianAvailableFlow(basin_connectivity: pd.DataFrame, flows: pd.DataFrame, day: str) -> Dict[str, float]:
    """ Initialize the available flow for the riparian linear problem """

    # Validation checks
    if not basin_connectivity.index.to_list() == flows.index.to_list():
        raise ValueError(f'The provided basin connectivity and flow data have mismatching shapes.')

    if day not in flows.columns.to_list():
        raise ValueError(f'The date provided ({day}) does not have associated flow data.')

    # Calculate the available flow for each basin
    return basin_connectivity.T.dot(flows[day]).to_dict()

def initializeRiparianDownstreamPenalty(basin_connectivity: pd.DataFrame, basins: list) -> pd.Series:
    """ Initialize the downstream penalty for the riparian linear problem """

    # Validation checks
    if not basins == basin_connectivity.index.to_list():
        raise ValueError('The provided basins do not match the basins in the connectivity matrix')

    # Calculate the downstream penalty
    return basin_connectivity.T.sum(axis=1).divide(np.count_nonzero(basins))

def getRiparianAllocations(riparian: dp.UserGroup, basin_proportions_list: npt.ArrayLike,
        basins: list, day: str) -> tuple[Dict[str, pulp.LpVariable], Dict[str, pulp.LpVariable]]:
    """ Generate the user and upstream allocations for the riparian linear problem """

    # Validation checks
    if not basins == riparian.connectivityMatrix.index.to_list():
        raise ValueError('There is a mismatch in basins between the provided list of basins and the user group connectivity matrix.')

    if not day in riparian.modelDemand.columns.to_list():
        raise ValueError('There is no demand data for the provided date. Please check that the modeling periods match.')

    # User Allocation:
    user_allocation_list = np.multiply(np.dot(basin_proportions_list.T,riparian.userMatrix),riparian.modelDemand[day].values)

    # Upstream Allocation:
    upstream_allocation_list = np.dot(riparian.connectivityMatrix.to_numpy(), user_allocation_list)

    # Convert the user and upstream allocations to dictionaries
    user_allocation_dict = {ru: ua for ru,ua in zip(riparian.users, user_allocation_list)}
    upstream_allocation_dict = {basins[k]: upstream_allocation_list[k] for k in range(len(basins))}

    return user_allocation_dict, upstream_allocation_dict

def addRiparianObjectiveFunction(
        riparian_lp: pulp.LpProblem,
        riparian: dp.UserGroup,
        user_allocation: Dict[str, pulp.LpVariable],
        basin_proportions: Dict[str, pulp.LpVariable],
        downstream_penalty: pd.Series,
        day: str,
        basins: list
) -> Dict[str, float]:
    """ Add the objective function to the riparian linear problem """

    # Validation checks
    if not basins == downstream_penalty.index.to_list():
        raise ValueError('The basins provided do not match the basins in the downstream penalty.')

    if set(riparian.users) != set(user_allocation.keys()):
        raise ValueError('The list of users in the provided user group does not match the users in the user allocations')

    # Get the basin demand for the given date
    basin_demand = getRiparianBasinDemand(riparian, day)

    # Create list of user allocations
    user_allocations = [user_allocation[user] for user in riparian.users]

    # Calculate the basin penalties
    basin_penalties = [
        basin_proportions[basin] * downstream_penalty[basin] * basin_demand[basin]
        for basin in basins
    ]

    # Add the objective function to the linear problem
    riparian_lp += pulp.lpSum(user_allocations) - pulp.lpSum(basin_penalties)

    return basin_demand

def addRiparianConstraints(
        riparian_lp: pulp.LpProblem,
        upstream_allocation: Dict[str, pulp.LpVariable],
        basin_connectivity: pd.DataFrame,
        basin_proportions: Dict[str, pulp.LpVariable],
        flows: pd.DataFrame,
        day: str,
        basins: list
) -> None:
    """ Add constraints to the riparian linear problem """

    # Validation check
    if not basins == list(basin_proportions.keys()):
        raise ValueError('There is a mismatch between the provided basins and the basin proportion keys.')

    # Initialize available flow for the given date
    available_flow = initializeRiparianAvailableFlow(basin_connectivity, flows, day)

    # Validation check
    if not basins == list(available_flow.keys()):
        raise ValueError('There is a mismatch between the provided basins and the available flow keys.')

    # Add constraints for each basin
    for k in basins:
        # Mass balance constraint
        riparian_lp += upstream_allocation[k] <= available_flow[k]

        # Downstream basins for each basin based on connectivity
        downstream_basins = list(basin_connectivity.index[basin_connectivity[k]==1])

        for j in downstream_basins:
            # Proportionality constraint
            # - Upstream basin's proportion cannot exceed any downstream basins
            # - Need k by i downstream proportions matrix
            riparian_lp += basin_proportions[j] <= basin_proportions[k]

def buildRiparianLinearProblem(
        riparian: dp.UserGroup,
        basin_connectivity: pd.DataFrame,
        flows: pd.DataFrame,
        downstream_penalty: pd.Series,
        basins: list,
        day: str
) -> tuple[pulp.LpProblem, Dict[str, pulp.LpVariable], Dict[str, float]]:
    """ Build the riparian linear problem """

    # Validation check
    if len(riparian.users) < 1:
        raise ValueError('The provided user group has no users. Please check that the user group is initialized correctly.')

    # Define the riparian linear problem
    riparian_lp = pulp.LpProblem('RiparianAllocation', pulp.LpMaximize)

    # Define the decision variables
    basin_proportions = pulp.LpVariable.dicts('Proportions',basins,0,1,cat='Continuous')

    # Convert dictionary of decision variables to an array
    basin_proportions_list = np.array(list(basin_proportions.values()))

    # Get user and upstream allocations
    user_allocation, upstream_allocation = getRiparianAllocations(riparian, basin_proportions_list, basins, day)

    # Define and add the objective function
    basin_demand = addRiparianObjectiveFunction(
        riparian_lp,
        riparian,
        user_allocation,
        basin_proportions,
        downstream_penalty,
        day,
        basins
    )

    # Define and add the constraints
    addRiparianConstraints(
        riparian_lp,
        upstream_allocation,
        basin_connectivity,
        basin_proportions,
        flows,
        day,
        basins
    )

    return riparian_lp, basin_proportions, basin_demand

def printRiparianModelingOutputs(
        riparian_lp: pulp.LpProblem,
        basin_proportions: Dict[str, pulp.LpVariable],
        basin_demand: Dict[str, float],
        basins: list
) -> None:
    """ Print the results of the riparian linear problem optimization """

    # Validation check
    if not basins == list(basin_demand.keys()):
        raise ValueError('There is a mismatch between the provided basins and the basin demand keys.')

    # Print the status of the linear problem
    print('Status: ', pulp.LpStatus[riparian_lp.status])

    # Print the values of the decision variables
    for v in riparian_lp.variables():
        print(v.name, '=', v.varValue)

    # Print the value of the objective function
    print('Objective = ', pulp.value(riparian_lp.objective))

    # Calculate total basin allocations
    basin_allocation = []
    for k, basin in enumerate(basins):
        basin_allocation.append(basin_proportions[basin].value() * basin_demand[basin])

    # Print the total basin allocations
    print('Basin Total Allocations', basin_allocation)

def populateRiparianBasinProportions(
        basin_proportions: Dict[str, pulp.LpVariable],
        riparianBasinProportions: pd.DataFrame,
        basins: list,
        day: str
):
    """ Populate the riparian basin proportions """

    # Validation checks
    if riparianBasinProportions.empty:
        raise ValueError('The provided riparian basin proportions is empty. Please ensure it is correctly initialized in the user group.')

    if not basins == riparianBasinProportions.index.to_list():
        raise ValueError('There is a mismatch between the provided basins and the riparian basin proportions index.')

    if not day in riparianBasinProportions.columns.to_list():
        raise ValueError(f'There is no column in the riparian basin proportions for the provided date ({day}).')

    # Update the dataframe with the basin proportions for the given date
    for k in basins:
        riparianBasinProportions.loc[k,[day]] = basin_proportions[k].varValue

def calculateRiparianOutputs(
        riparian: dp.UserGroup,
        riparianBasinProportions: pd.DataFrame,
        dates: list,
        basins: list
) -> tuple[pd.DataFrame, pd.DataFrame, npt.ArrayLike]:
    """ Calculate the riparian user and basin allocations as well as the basin proportion matrix """

    # Validation checks
    if not basins == riparian.connectivityMatrix.index.to_list():
        raise ValueError('There is a mismatch in basins between the provided list of basins and the user group connectivity matrix.')

    if not np.array_equal(dates, riparian.modelDemand.columns.to_list()):
        raise ValueError('There is a mismatch in dates between the provided list of dates and the user group model demand.')

    # Create the basin proportion matrix
    basin_proportion_matrix = createBasinProportionMatrix(riparianBasinProportions)

    # Calculate the user allocations
    riparianUserAllocations, rip_user_allocations_matrix = calculateRiparianUserAllocations(
        riparian,
        basin_proportion_matrix,
        dates
    )

    # Aggregate the user allocations to obtain the basin allocations
    riparianBasinAllocations = aggregateRiparianBasinAllocations(
        riparian,
        rip_user_allocations_matrix,
        dates,
        basins
    )

    return riparianUserAllocations, riparianBasinAllocations, basin_proportion_matrix

def createBasinProportionMatrix(riparian_basin_proportions: pd.DataFrame) -> npt.ArrayLike:
    """ Convert the riparian basin proportions into an array """

    # Validation check
    if riparian_basin_proportions.empty:
        raise ValueError('The provided riparian basin proportions dataframe is empty. Please ensure the dataframe is initialized correctly.')

    # Sort the index and columns
    riparian_basin_proportions.sort_index(axis='index', inplace=True)
    riparian_basin_proportions.sort_index(axis='columns', inplace=True)

    # Convert to a numpy array
    return np.array(riparian_basin_proportions)

def calculateRiparianUserAllocations(
        riparian: dp.UserGroup,
        basin_proportion_matrix: npt.ArrayLike,
        dates: list
) -> tuple[pd.DataFrame, npt.ArrayLike]:
    """ Calculate the user allocations """

    # Validation checks
    if not np.array_equal(dates, riparian.modelDemand.columns.to_list()):
        raise ValueError('There is a mismatch in dates between the provided list of dates and the user group model demand.')

    if not len(riparian.users) > 0:
        raise ValueError('The user group has no users. Please ensure the user group is being initialized correctly.')

    if not len(riparian.userMatrix.index.to_list()) > 0:
        raise ValueError("The user group's userMatrix has no users. Please ensure the user group is being initialized correctly.")

    # Transpose the user matrix (align with basin proportions)
    user_matrix = riparian.userMatrix.to_numpy().transpose()

    # Get model demand data
    model_demand = riparian.modelDemand.to_numpy()

    # Calculate the allocation matrix
    allocation_matrix = np.dot(user_matrix, basin_proportion_matrix)

    # Apply model demand to the allocation matrix
    riparian_user_allocations = allocation_matrix * model_demand

    # Convert the user allocations to a dataframe
    riparian_user_allocations_df = pd.DataFrame(
        riparian_user_allocations,
        columns=dates,
        index=riparian.users
    )
    riparian_user_allocations_df.index.name = 'USER'

    return riparian_user_allocations_df, np.array(riparian_user_allocations_df)

def aggregateRiparianBasinAllocations(
        riparian: dp.UserGroup,
        riparian_user_allocations_matrix: npt.ArrayLike,
        dates: list,
        basins: list
) -> pd.DataFrame:
    """ Aggregate the user allocations into basin-level allocations for each date """

    # Validation checks
    if not basins == riparian.connectivityMatrix.index.to_list():
        raise ValueError('There is a mismatch in basins between the provided list of basins and the user group connectivity matrix.')

    if not np.array_equal(dates, riparian.modelDemand.columns.to_list()):
        raise ValueError('There is a mismatch in dates between the provided list of dates and the user group model demand.')

    if not len(riparian.users) > 0:
        raise ValueError('The user group has no users. Please ensure the user group is being initialized correctly.')

    if not len(riparian_user_allocations_matrix) > 0:
        raise ValueError('The provided riparian user allocation matrix has no elements.')

    # Aggregate basin riparian allocations
    user_matrix = riparian.userMatrix.to_numpy()
    basin_allocations = np.dot(user_matrix, riparian_user_allocations_matrix)

    # Convert basin allocations to a dataframe
    riparian_basin_allocations = pd.DataFrame(
        basin_allocations,
        columns=dates,
        index=basins
    )
    riparian_basin_allocations.index.name = 'BASIN'

    return riparian_basin_allocations

def calculateNetAvailableFlow(
        riparian: dp.UserGroup,
        basin_connectivity: pd.DataFrame,
        flows: pd.DataFrame,
        basin_proportion_matrix: npt.ArrayLike,
        dates: list,
        basins: list
) -> pd.DataFrame:
    """ Calculate the net available flow """

    # Validation checks
    if not basins == riparian.connectivityMatrix.index.to_list():
        raise ValueError('There is a mismatch in basins between the provided list of basins and the user group connectivity matrix.')

    if not np.array_equal(dates, riparian.modelDemand.columns.to_list()):
        raise ValueError('There is a mismatch in dates between the provided list of dates and the user group model demand.')

    if not len(riparian.users) > 0:
        raise ValueError('The user group has no users. Please ensure the user group is being initialized correctly.')

    if basin_connectivity.empty:
        raise ValueError('The provided basin connectivity matrix has no data. Please ensure it is initialized correctly.')

    if flows.empty:
        raise ValueError('The provided flows matrix has no data. Please ensure it is initialized correctly.')

    if not len(basin_proportion_matrix) > 0:
        raise ValueError('The provided basin proportion matrix has no elements. Please ensure it is initialized correctly.')

    # Calculate the matrix of shorted basins
    rip_short_basins_matrix = calculateRiparianShortedBasins(
        riparian,
        basin_proportion_matrix,
        dates
    )

    # Adjust basin connectivity for basins with shortages
    rip_short_basins_matrix = adjustRiparianShortedBasinsConnectivity(
        rip_short_basins_matrix,
        basin_connectivity
    )

    # Recalculate cumulative flow matrix
    cumulative_flow_matrix_new = calculateCumulativeFlow(
        basin_connectivity,
        flows,
        rip_short_basins_matrix,
        dates
    )

    # Recalculate user allocations excluding shorted basins
    rip_user_allocations_matrix_new = calculateUserAllocationWithoutShortage(
        riparian,
        rip_short_basins_matrix
    )

    # Recalculate upstream allocations excluding shorted basins
    rip_upstream_allocations_matrix_new = calculateUpstreamAllocationWithoutShortage(
        riparian,
        rip_user_allocations_matrix_new
    )

    # Generate and return the net available flow
    return generateAvailableFlow(
        cumulative_flow_matrix_new,
        rip_upstream_allocations_matrix_new,
        basins,
        dates
    )

def calculateRiparianShortedBasins(
        riparian: dp.UserGroup,
        basin_proportion_matrix: npt.ArrayLike,
        dates: list
) -> npt.ArrayLike:
    """ Calculate the shorted basins for each date """

    # Validation checks
    if not np.array_equal(dates, riparian.modelDemand.columns.to_list()):
        raise ValueError('There is a mismatch in dates between the provided list of dates and the user group model demand.')

    if not len(riparian.users) > 0:
        raise ValueError('The user group has no users. Please ensure the user group is being initialized correctly.')

    if not len(basin_proportion_matrix) > 0:
        raise ValueError('The provided basin proportion matrix has no elements. Please ensure it is initialized correctly.')

    # Initialize a shorted basin matrix
    rip_short_basins_matrix = np.zeros(basin_proportion_matrix.shape)

    # Loop over each basin and date
    for k, basin in enumerate(basin_proportion_matrix):
        for i, date in enumerate(dates):
            # Calculate demand for the current basin and date
            demand = riparian.userMatrix.dot(riparian.modelDemand).iloc[k, i]

            # Checking if the basin is shorted
            if basin_proportion_matrix[k][i] < 1 and demand > 0:
                rip_short_basins_matrix[k][i] = 0
            else:
                rip_short_basins_matrix[k][i] = 1

    return rip_short_basins_matrix

def adjustRiparianShortedBasinsConnectivity(
        rip_short_basins_matrix: npt.ArrayLike,
        basin_connectivity: pd.DataFrame
) -> npt.ArrayLike:
    """ Adjust the shorted basins matrix """

    # Validation checks
    if len(rip_short_basins_matrix) < 1:
        raise ValueError('The provided riparian shortage basin matrix is empty. Please ensure it is initialized correctly.')

    if basin_connectivity.empty:
        raise ValueError('The provided basin connectivity matrix is empty. Please ensure it is initialized correctly.')

    # Loop through the shorted basins matrix
    for k, basin in enumerate(rip_short_basins_matrix):
        # If the basin is shorted check its connecting basins
        if rip_short_basins_matrix[k].any() == 0:
            for i, connectivity in enumerate(basin_connectivity.to_numpy().T[k]):
                # If a basin is connected to a shorted basin, mark it as shorted
                if connectivity == 1:
                    rip_short_basins_matrix[i] = 0

    return rip_short_basins_matrix

def calculateCumulativeFlow(
        basin_connectivity: pd.DataFrame,
        flows: pd.DataFrame,
        rip_short_basins_matrix: npt.ArrayLike,
        dates: list
) -> npt.ArrayLike:
    """ Calculate the cumulative flow for each basin """

    # Validation checks
    if flows.empty:
        raise ValueError('The provided flows dataframe is empty. Please ensure it is initialized correctly.')

    if basin_connectivity.empty:
        raise ValueError('The provided basin connectivity matrix is empty. Please ensure it is initialized correctly.')

    if len(rip_short_basins_matrix) < 1:
        raise ValueError('The provided shortage basins matrix has no elements. Please ensure it is initialized correctly.')

    if not np.array_equal(dates, flows.columns.to_list()):
        raise ValueError('There is a mismatch between the provided dates and the dates in the flows dataframe.')

    # Calculate cumulative flow
    return np.dot(basin_connectivity.to_numpy().T, np.array(flows[dates].values * rip_short_basins_matrix))

def calculateUserAllocationWithoutShortage(
        riparian: dp.UserGroup,
        rip_short_basins_matrix: npt.ArrayLike
) -> npt.ArrayLike:
    """ Calculate user allocations excluding shorted basins """

    # Validation checks
    if len(riparian.users) < 1:
        raise ValueError('The provided user group has no users. Please ensure the user group is initialized correctly.')

    if len(rip_short_basins_matrix) < 1:
        raise ValueError('The provided shortage basins matrix has no elements. Please ensure it is initialized correctly.')

    # Recalculate user allocations excluding the shorted basins
    return np.dot(riparian.userMatrix.to_numpy().T, rip_short_basins_matrix) * riparian.modelDemand.to_numpy()

def calculateUpstreamAllocationWithoutShortage(
        riparian: dp.UserGroup,
        rip_user_allocations_matrix
) -> npt.ArrayLike:
    """ Calculate upstream allocations excluding shorted basins """

    # Validation checks
    if len(riparian.users) < 1:
        raise ValueError('The provided user group has no users. Please ensure the user group is initialized correctly.')

    if len(rip_user_allocations_matrix) < 1:
        raise ValueError('The provided user allocation matrix has no elements. Please ensure it is initialized correctly.')

    # Recalculate upstream allocation excluding the shorted basins
    return np.dot(riparian.connectivityMatrix.to_numpy(), rip_user_allocations_matrix)

def generateAvailableFlow(
        cumulative_flow_matrix: npt.ArrayLike,
        rip_upstream_allocations_matrix: npt.ArrayLike,
        basins: list,
        dates: list
):
    """ Generate the available flow based on cumulative flow and upstream allocations """

    # Validation checks
    if len(basins) < 1:
        raise ValueError('The provided list of basins has no elements. Please ensure it is initialized correctly.')

    if len(dates) < 1:
        raise ValueError('The provided list of dates has no elements. Please ensure it is initialized correctly.')

    if len(cumulative_flow_matrix) < 1:
        raise ValueError('The provided cumulative flow matrix has no elements. Please ensure it is initialized correctly.')

    if len(rip_upstream_allocations_matrix) < 1:
        raise ValueError('The provided upstream allocation has no elements. Please ensure it is initialized correctly.')

    # Generate the available flow
    availableFlow = pd.DataFrame(
        cumulative_flow_matrix - rip_upstream_allocations_matrix,
        index=basins,
        columns=dates
    )
    availableFlow.index.name = 'BASIN'

    return availableFlow