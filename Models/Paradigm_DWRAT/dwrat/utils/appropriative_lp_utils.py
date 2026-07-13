import pandas as pd
import numpy as np
import pulp
import numpy.typing as npt
from typing import Dict
import dwrat.preprocessing.demandProcessing as dp

def checkAvailableFlowDates(available_flow: pd.DataFrame, dates: npt.ArrayLike) -> bool:
    """ Check that the dates in available flow match the provided dates """

    available_flow_dates = available_flow.columns.to_list()
    return sorted(available_flow_dates) == sorted(dates)

def getAppropriativeDemandAndPenalty(appropriative: dp.UserGroup, day: str) -> tuple[Dict[str, float], Dict[str, float]]:
    """ Get the demand and shortage penalty for the given date """

    if day not in appropriative.modelDemand.columns:
        raise ValueError(f'The model demand does not contain data for {day}')

    demand_data = appropriative.modelDemand[day].to_numpy()
    priority = appropriative.data['PRIORITY'].to_numpy()
    shortage_penalty_data = generateAppropriativeShortagePenaltyData(priority, appropriative.users)

    # Create dictionaries for demand and shortage penalty
    app_demand = {appropriative.users[i]: demand_data[i] for i in range(len(appropriative.users))}
    shortage_penalty = {appropriative.users[i]: shortage_penalty_data[i] for i in range(len(appropriative.users))}

    return app_demand, shortage_penalty

def generateAppropriativeShortagePenaltyData(priority: npt.ArrayLike, users: npt.ArrayLike) -> npt.ArrayLike:
    """ Generate shortage penalty data based on priority values """

    if len(priority) < 1:
        raise ValueError("The priority array contains no elements. Ensure the appropriative user group's data is created correctly.")

    if len(users) < 1:
        raise ValueError("The user array contains no elements. Ensure the appropriative user group has valid users.")

    penalty_data = []

    for i in range(len(users)):
        if priority[i] == 0:
            raise ValueError(f'The priority value for user {users[i]} is zero. The minimum priority for a user is 1 (indicating highest priority).')
        penalty_data.append(1000 * (1 / priority[i]))

    return np.array(penalty_data)

def buildAppropriativeLinearProblem(appropriative: dp.UserGroup, basins: npt.ArrayLike, app_available_flow: Dict[str, float],
                   app_demand: Dict[str, float], shortage_penalty: Dict[str, float]) -> tuple[pulp.LpProblem, Dict]:
    """ Build the LP problem, including variables, objective, and constraints. """

    # Validation checks
    if len(appropriative.users) < 1:
        raise ValueError('The list of users in the appropriative user group is empty.')

    if len(basins) < 1:
        raise ValueError('The provided list of basins is empty.')

    if len(list(app_demand.keys())) < 1:
        raise ValueError('The provided appropriative demand for the current date has no data.')

    if len(list(shortage_penalty.keys())) < 1:
        raise ValueError('The provided shortage penalty for the current date has no data.')

    if len(list(app_available_flow.keys())) < 1:
        raise ValueError('The provided appropriative available flow for the current date has no data.')

    # Define the problem
    appropriative_lp = pulp.LpProblem('AppropriativeProblem', pulp.LpMinimize)

    # Define decision variables
    user_allocation = defineAppropriativeDecisionVariables(appropriative)

    # Objective function
    addAppropriativeObjectiveFunction(appropriative_lp, shortage_penalty, app_demand, user_allocation, appropriative.users)

    # Upstream basin allocation
    upstream_dict = calculateAppropriativeUpstreamBasinAllocation(appropriative, user_allocation, basins)

    addAppropriativeConstraints(appropriative_lp, upstream_dict, app_available_flow, app_demand, user_allocation, appropriative)

    return appropriative_lp, user_allocation

def defineAppropriativeDecisionVariables(appropriative: dp.UserGroup) -> Dict[str, pulp.pulp.LpVariable]:
    """ Define the decision variables for each user """

    return pulp.LpVariable.dicts('UserAllocation', appropriative.users, lowBound=0)

def addAppropriativeObjectiveFunction(appropriative_lp: pulp.LpProblem, shortage_penalty: Dict[str, float],
        app_demand: Dict[str, float], user_allocation: Dict[str, pulp.pulp.LpVariable],
        users: npt.ArrayLike) -> None:
    """ Add the objective function to the problem """

    applied_shortage_penalty = applyAppropriativeShortagePenalty(shortage_penalty, app_demand, user_allocation, users)
    appropriative_lp += pulp.lpSum(applied_shortage_penalty)

def applyAppropriativeShortagePenalty(shortage_penalty: Dict[str, np.float64], demand: Dict[str, np.float64],
        user_allocation: Dict[str, pulp.pulp.LpVariable], users: npt.ArrayLike) -> pulp.pulp.LpAffineExpression:
    """ Apply shortage penalty to the objective function """

    # Validation checks
    if not len(user_allocation.items()) == len(users):
        raise ValueError('The list of users and user allocations have mismatching lengths. Please check that all users are present in both parameters.')

    if not len(demand.items()) == len(users):
        raise ValueError('The list of users and appropriative demand have mismatching lengths. Please check that all users are present in both parameters.')

    if not len(shortage_penalty.items()) == len(users):
        raise ValueError('The list of users and shortage penalties have mismatching lengths. Please check that all users are present in both parameters.')

    return [(shortage_penalty[user]) * (demand[user] - user_allocation[user]) for user in users]

def calculateAppropriativeUpstreamBasinAllocation(appropriative: dp.UserGroup, user_allocation: Dict[str, pulp.pulp.LpVariable],
        basins: npt.ArrayLike) -> Dict[str, float]:
    """ Calculate the upstream basin allocations based on user allocations """

    # Validation checks
    if not len(user_allocation.items()) == len(appropriative.users):
        raise ValueError('The list of users and user allocations have mismatching lengths. Please check that all users are present in both parameters.')

    upstream_basin_allocation = np.dot(
        appropriative.connectivityMatrix.to_numpy(),
        [user_allocation[user] for user in appropriative.users]
    )

    return {basins[k]: upstream_basin_allocation[k] for k in range(len(basins))}

def addAppropriativeConstraints(appropriative_lp: pulp.LpProblem, upstream_dict: Dict[str, float],
        app_available_flow: Dict[str, float], app_demand: Dict[str, float],
        user_allocation: Dict[str, pulp.pulp.LpVariable], appropriative: dp.UserGroup) -> None:
    """ Add constraints to the problem for available flow and user demand """

    # Validation checks
    if not len(app_available_flow.items()) == len(upstream_dict.items()):
        raise ValueError('The appropriative available flow appears to be missing basins. Please verify it is being generated correctly.')

    if not len(app_demand.items()) == len(appropriative.users):
        raise ValueError('The appropriative demand appears to be missing users. Please verify it is being generated correctly.')

    # Constraint 1: Allocation <= available flow
    for basin in upstream_dict:
        appropriative_lp += upstream_dict[basin] <= app_available_flow[basin]

    # Constraint 2: Allocation <= reported demand
    for user in appropriative.users:
        appropriative_lp += user_allocation[user] <= app_demand[user]

def updateAppropriativeUserAllocations(user_allocation: Dict[str, pulp.pulp.LpVariable], users: npt.ArrayLike) -> None:
    """ Convert variable allocations to a list """

    # Validation checks
    if not len(user_allocation.items()) == len(users):
        raise ValueError('The user allocation appears to be missing users. Please verify it is being generated correctly.')

    return [user_allocation[user].value() for user in users]

def populateAppropriativeUserAllocations(userAllocations: pd.DataFrame, user_allocation: Dict[str, pulp.pulp.LpVariable],
        users: npt.ArrayLike, day: str) -> None:
    """ Update user allocations dataframe """

    for i in users:
        userAllocations.loc[i,[day]] = user_allocation[i].varValue

def populateAppropriativeBasinAllocations(basinAllocations: pd.DataFrame, basins: npt.ArrayLike,
        basin_allocations: npt.ArrayLike, day: str) -> None:
    """ Update basin allocations dataframe """

    if not len(basin_allocations) == len(basins):
        raise ValueError('basin_allocations appears to have missing basins. Please verify it is being generated correctly.')

    for k,basin in enumerate(basins):
        basinAllocations.loc[basin,[day]] = basin_allocations[k]