#----PURPOSE:-----

# This script was written to calculate the minimum and maximum temperatures for
# the HRUs that produced erroneous results when the PRMS model was run in June 2024.We
# wanted to see how absurd those temperatures were to trigger the error thresholds.

#Last updated by: Payman Alemi on 5/22/2025

# Scenario 1: Successful PRMS Run for 6/4/2024

# Minimum Temperature Calculation ----
tmin_tsta = 11.5# TMIN2 on 6/4/2024
tmin_tlaps = 8.9 # TMIN6 on 6/4/2024

hru_elev = 999.08 #Find out elevation of HRU 25072
tsta_elev = 193.25 #in meters, Temp 2 station (base station)
tlaps_elev = 318.8 #in meters, Temp 6 station (lapse station)
tmin_adj_hru = -0.04185 #for HRU 25072

ef =  (hru_elev  - tsta_elev)/(tlaps_elev - tsta_elev)
print(ef)

tmin_hru = tmin_tsta + (tmin_tlaps - tmin_tsta)*ef - tmin_adj_hru
print(tmin_hru) #-5.15 when we assumed TMIN2 is the base station

# Maximum Temperature Calculation ----
tmax_tsta = 24.3
tmax_tlaps = 33.9
tmax_adj_hru = -0.04185 # for HRU 25072

tmax_hru = tmax_tsta + (tmax_tlaps - tmax_tsta)*ef - tmax_adj_hru
print(tmax_hru) # 85.96 when we assumed TMAX2 is the base station


# Scenario 2: Failed PRMS Run for 6/4/2024
# Minimum Temperature Calculation
tmin_tsta = 10.7
tmin_tlaps = 8.9
tmin_hru = tmin_tsta + (tmin_tlaps - tmin_tsta)*ef - tmin_adj_hru
print(tmin_hru) #-0.81 when we assumed TMIN2 is the base station

# Maximum Temperature Calculation
tmax_tsta = 22.3
tmax_tlaps =  33.9
tmax_hru = tmax_tsta + (tmax_tlaps - tmax_tsta)*ef - tmax_adj_hru
print(tmax_hru)  #96.80 when we assumed TMAX2 is the base station


#Finding Parameters in Parameter File
hru_elev_1st = 728230
hru_elev_25072 = 728230+25071
hru_elev_25072

tmin_adj_1st 
tmin_adj_25072 = tmin_adj_1st + 25071
tmin_adj_25072

tmax_adj_1st - 5823871
tmax_adj_25072 = tmax_adj_1st + 25071
tmax_adj_25072

