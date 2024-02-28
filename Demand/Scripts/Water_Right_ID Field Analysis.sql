/* The purpose of this script to find out if the water_right_id field is a primary key used by multiple tables.
We will spot-check several application_numbers in eWRIMS and look up the submission dates; do the submission dates on 
eWRIMS match the submission dates in annual_water_right_report?	 If so, does the water_right_id field in the annual_water_
right_report match the water_right_id field in the water_use_extended_report? */


--Query 1: annual_water_right_report analysis
Select top 100 *
   --WR_water_right_id
from reportdb.FLAT_FILE.ewrims_flat_file_annual_report
order by water_right_id asc

--Query 2: water_use_report_extended analysis
Select top 100 
application_number, water_right_id, year 
from FLAT_FILE.ewrims_water_use_report_extended
order by application_number asc
