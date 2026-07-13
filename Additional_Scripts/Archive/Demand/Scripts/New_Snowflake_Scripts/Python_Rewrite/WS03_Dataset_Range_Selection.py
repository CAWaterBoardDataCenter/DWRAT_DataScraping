# Check the years specified in "WS01_Set_Parameters.py"
# They set the bounds for the demand dataset

# However, there are limitations on what the values can be



#### SETUP ####

import warnings

from datetime import datetime



#### IMPORTANT VARIABLES ####

from WS01_Set_Parameters import startYear, endYear



#### PROCEDURE ####

# Ensure that 'startYear' and 'endYear' are both integers
if type(startYear) != int or type(endYear) != int:
    raise ValueError("In 'WS01_Set_Parameters.py', 'startYear' and 'endYear' should be specified as integer values!")


# 'startYear' should be greater than or equal to 2017
# This is not a hard requirement, but a warning should be given because it is not recommended
if startYear < 2017:
    warnings.warn("Demand datasets generally should avoid data from before 2017 due to differences in how data was structured in the past. In addition, older data tends to be less accurate.", stacklevel = 2)


# 'endYear' should be greater than or equal to 'startYear' too
if endYear < startYear:
    raise ValueError("In 'WS01_Set_Parameters.py', 'endYear' is less than 'startYear'!")


# In addition, 'endYear' should not be greater than today's year
if endYear > datetime.today().year:
    raise ValueError("In 'WS01_Set_Parameters.py', 'endYear' is greater than the current year!")
