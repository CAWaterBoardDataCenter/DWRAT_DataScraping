--Query 1: Keep the columns required for GIS pre-processing
Select 
APPLICATION_NUMBER, COUNTY, EAST_COORD, HUC_12_NAME, HUC_12_NUMBER, HUC_8_NAME, HUC_8_NUMBER, LATITUDE, 
LONGITUDE, MERIDIAN, NORTH_COORD, OBJECTID, PARCEL_NUMBER, POD_COUNT, POD_ID,
POD_LAST_UPDATE_DATE, POD_NUMBER, POD_NUMBER_GIS, POD_STATUS, POD_TYPE,
QUARTER, QUARTER_QUARTER, RANGE_DIRECTION, RANGE_NUMBER, SECTION_CLASSIFIER, SECTION_NUMBER, SOURCE_NAME, 
TOWNSHIP_DIRECTION, TOWNSHIP_NUMBER, TRIB_DESC, WATER_RIGHT_STATUS, WATER_RIGHT_TYPE, WATERSHED

into #flat_file_pods--drop table flat_file_pods
from reportdb.flat_file.ewrims_flat_file_pod
Select * from #flat_file_pods
--Returns 65,112 records in 30 s on 11/8/2023

--Query 2: Apply the filters
Select * 
Into #flat_file_pods2  --drop table flat_file_pods2
from #flat_file_pods
Where 
POD_STATUS = 'Active' AND
WATER_RIGHT_STATUS IN ('Active', 'Claimed - Local Oversight', 'Certified', 'Claimed', 
						'Completed', 'Licensed', 'Permitted','Registered', '') AND
WATER_RIGHT_TYPE IN ('Appropriative', 'Federal Claims', 'Federal Stockponds', 'Registration Cannabis', 'Registration Domestic',
						'Registration Irrigation', 'Registration Livestock', 'Statement of Div and Use', 'Stockpond', '')
Select * from #flat_file_pods2
--Returns 

--Query 3: Replace Meridian Names with Meridian Short Names
Update #flat_file_pods2
Set MERIDIAN =  
case when MERIDIAN = 'Mount Diablo' Then 'M'
	when MERIDIAN = 'San Bernardino' Then 'S'
	when MERIDIAN = 'Humboldft' Then 'H'
	Else MERIDIAN
END	
Select * from #flat_file_pods2

--Query 4: Convert coordinates to numeric format
Alter Table #Flat_file_pods2
Alter Column latitude decimal(10,8);

Alter Table #Flat_File_Pods2
Alter column longitude decimal(11,8)
Select * from #flat_file_pods2

--Query 5: Add FFMTRS field
SELECT *,
    CONCAT(meridian, township_number, township_direction, range_number, range_direction, section_number) AS FFTMRS
INTO #flat_file_pods3
FROM #flat_file_pods2;
Select * from #flat_file_pods3
