from datetime import datetime
from numpy import typing as npt


def logInstantiation(
        time: datetime,
) -> str:
    log = 'Model instantiated: {}\n\n'.format(time.strftime(r'%Y %B %d, %H:%M:%S'))
    return log


def logAssumeDates(
        dates: npt.ArrayLike,
) -> str:
    log = 'Model dates not explicitly specified.\n' +\
          'Using dates from flow data:\n'
    datesStr = list(dates)+((3-len(dates)%3)*[''] if len(dates)%3>0 else [])
    for a,b,c in zip(datesStr[::3],datesStr[1::3],datesStr[2::3]):
        log += '    {:<15}{:<15}{:<}\n'.format(a,b,c)
    log += '\n'
    return log


def logGivenDates(
        dates: npt.ArrayLike,
) -> str:
    log = 'Using specified dates:\n'
    datesStr = list(dates)+((3-len(dates)%3)*[''] if len(dates)%3>0 else [])
    for a,b,c in zip(datesStr[::3],datesStr[1::3],datesStr[2::3]):
        log += '    {:<15}{:<15}{:<}\n'.format(a,b,c)
    log += '\n'
    return log


def logBasins(
        basins: npt.ArrayLike,
) -> str:
    log = 'Model domain includes {} basins:\n'.format(len(basins))
    basinsStr = list(basins)+((3-len(basins)%3)*[''] if len(basins)%3>0 else [])
    for a,b,c in zip(basinsStr[::3],basinsStr[1::3],basinsStr[2::3]):
        log += '    {:<15}{:<15}{:<}\n'.format(a,b,c)
    log += '\n'
    return log


def logUserSummary(
        users: npt.ArrayLike,
        userGroup: str,
) -> str:
    log = 'Model includes {} {} users.\n'.format(len(users),userGroup)
    log += '\n'
    return log

