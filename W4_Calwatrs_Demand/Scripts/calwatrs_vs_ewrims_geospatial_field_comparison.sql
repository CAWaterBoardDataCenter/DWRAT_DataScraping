--PURPOSE:
--This script compares the geospatial fields in eWRIMS vs Calwatrs by using the Navarro POD list as the basis of comparison.
--I obtain the Navarro POD list from the WR_SUPPLY_AND_DEMAND schema on the SWRCB_INTERNAL_TEST environment
--Last Updated By: Payman Alemi on 11/13/2026

USE role swrcb_internal_test_wr_supply_and_demand_r_acrole;

--Select distinct gr_feature_type from SWRCB_INTERNAL_PROD.WR_CALWATRS_FLATFILES.GEOSPATIAL_RELATIONSHIPS


/* QUERY 1: Navarro PODs */
select distinct POD_ID, watershed from swrcb_internal_test.wr_supply_and_demand.sda_demand_pod_subbasin;
--Returns 315 POD_IDs--can you find all 315 in the geospatial_relationships table?

/* EWRIMS_FLAT_FILE_POD FIELDS
--We are verifying that these fields have identical values in ewrims and calwatrs


/*
APPLICATION_NUMBER, corresponds to WR_APP_CLAIM_NAME, exists in GEOSPATIAL_RELATIONSHIPS and ANNUAL_REPORTS tables
COUNTY, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_COUNTY
EAST_COORD, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_EASTING
HUC_12_NAME, appears to correspond to GEOSPATIAL_RELATIONSHIPS.GR_WATERCOURSE_NAME
HUC_12_NUMBER, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_HUC12
HUC_8_NAME, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_BASIN_NAME
HUC_8_NUMBER, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_HUC8
LATITUDE, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_LATITUDE
LONGITUDE, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_LONGITUDE
MERIDIAN, doesn't exist in the calwatrs flat files
NORTH_COORD, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_NORTHING
POD_ID, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_LEGACY_NAME
RANGE_DIRECTION, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_RANGE_DIRECTION
RANGE_NUMBER, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_RANGE_NUMBER
SECTION_NUMBER, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_SECTION_NUMBER
SOURCE_NAME, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_SOURCE
TOWNSHIP_DIRECTION, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_TOWNSHIP_DIRECTION
TOWNSHIP_NUMBER, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_TOWNSHIP_NUMBER
TRIB_DESC, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_NHDTRIBUTARY
WATER_RIGHT_STATUS, appears to correspond to GEOSPATIAL_RELATIONSHIPS.WR_APP_CLAIM_STATUS
WATER_RIGHT_TYPE, appears to correspond to WR_APP_CLAIM_TYPE
WATERSHED, appears to correspond to GEOSPATIAL_RELATIONSHIPS.POLY_WATERSHED_NAME
WR_WATER_RIGHT_ID, appears to correspond to AR_LEGACYID
*/

*/

/*CALWATRS GEOSPATIAL RELATIONSHIPS FLAT FILE 
    1. Primary key is GR_NAME--1 value per row
    2. Foreign key is WR_APP_CLAIM_NAME--this is the APPLICATION_NUMBER
    3. GR_FEATURE_TYPE describes whether the entity is a point of diversion, place of use, point of storage, or point of investigation etc. 
    There are 14 possible values including null but for our analysis, let's just filter to "Point of Diversion" Only
    
/*  QUERY 2: PLSS Fields
    This query demonstrates the importance of the meridian field in the context of the other PLSS fields. The meridian
    exists in ewrims but not in calwatrs.  */

With final_pod_list as (
Select 
sda.application_number,
ewrims.application_number as ewrims_application_number,
sda.pod_id,
ewrims.meridian,
ewrims.township_number,
ewrims.township_direction,
ewrims.range_direction,
ewrims.section_number,
ewrims.quarter,
ewrims.quarter_quarter,
calwatrs.*

from swrcb_internal_test.wr_calwatrs_flatfiles.geospatial_relationships as calwatrs
inner join swrcb_internal_test.wr_supply_and_demand.sda_demand_pod_subbasin as sda 
    on sda.pod_id = calwatrs.poly_legacyname
inner join swrcb_internal_prod.wr_ewrims.ewrims_flat_file_pod as ewrims
    on ewrims.POD_ID = sda.pod_id
where 
calwatrs.gr_status = 'Active'
AND calwatrs.gr_feature_type = 'Point of Diversion'
) 
--Returns 308 PODs, 7 PODs are missing from the sda table--which 7?
, missing_pods As (
    Select distinct
    sda.application_number,
    sda.pod_id
    from swrcb_internal_test.wr_supply_and_demand.sda_demand_pod_subbasin as sda
    left join final_pod_list
        on sda.pod_id = final_pod_list.pod_id
    Where final_pod_list.pod_id is NULL
)

Select * from missing_pods;
--Missing:
    -- POD_ID 69638 for water right D032622
    -- POD_ID 72917 for water right H500865
    -- POD_ID 40224 for water right D030768
    -- POD_ID 60145 for water right D032137
    -- POD_ID 40223 for water right D030768
    -- POD_ID 27955 for water right D030689
    -- POD_ID 69206 for water right D032622
    -- POD_ID 59503 for water right S015648

/* QUERY 3: Looking for 8 POD_IDs in calwatrs */
Select 
poly_legacyname, WR_APP_CLAIM_NAME
 from swrcb_internal_test.wr_calwatrs_flatfiles.geospatial_relationships as calwatrs
WHERE  
POLY_LEGACYNAME IN (
    '69638', '72917', '40224', '60145',
    '40223', '27955', '69206', '59503'
)
And GR_Status = 'Active' ;
--There are 12 PODs with these IDs but none of them are active--that's why they are missing from the query above


/* QUERY 4: ewrims_flat_file_pod vs calwatrs.geospatial_relationships
    I am listing all the fields in ewrims_flat_file_pod and their supposed equivalents in the calwatrs geospatial_relationships table.
    All values should be identical. I am intentionally excluding the fields that live in the annual_reports table; those will be analyzed next.
 */


--1st Temporary Table: ewrims_calwatrs_fields_comparison
USE DATABASE SWRCB_INTERNAL_TEST;
USE SCHEMA WR_SUPPLY_AND_DEMAND;

    CREATE Temporary table ewrims_calwatrs_fields_comparison AS
    SELECT
        ewrims.pod_id,
        gr.poly_legacyname,
        ewrims.APPLICATION_NUMBER,
        gr.WR_APP_CLAIM_NAME,
        ewrims.COUNTY,
        gr.POLY_COUNTY,
        ewrims.EAST_COORD,
        gr.POLY_EASTING,
        ewrims.HUC_12_NAME,
        gr.GR_WATERCOURSE_NAME,
        ewrims.HUC_12_NUMBER,
        gr.POLY_HUC12,
        ewrims.HUC_8_NAME,
        gr.POLY_BASIN_NAME,
        ewrims.HUC_8_NUMBER,
        gr.POLY_HUC8,
        ewrims.LATITUDE,
        gr.POLY_LATITUDE,
        ewrims.LONGITUDE,
        gr.POLY_LONGITUDE,
        ewrims.NORTH_COORD,
        gr.POLY_NORTHING,
        ewrims.RANGE_DIRECTION,
        gr.POLY_RANGE_DIRECTION,
        ewrims.RANGE_NUMBER,
        gr.POLY_RANGE_NUMBER,
        ewrims.SECTION_NUMBER,
        gr.POLY_SECTION_NUMBER,
        ewrims.SOURCE_NAME,
        gr.POLY_SOURCE,
        ewrims.TOWNSHIP_DIRECTION,
        gr.POLY_TOWNSHIP_DIRECTION,
        ewrims.TOWNSHIP_NUMBER,
        gr.POLY_TOWNSHIP_NUMBER,
        ewrims.TRIB_DESC,
        gr.POLY_NHDTRIBUTARY,
        ewrims.WATER_RIGHT_STATUS,
        gr.WR_APP_CLAIM_STATUS,
        ewrims.WATER_RIGHT_TYPE,
        gr.WR_APP_CLAIM_TYPE

    from swrcb_internal_test.wr_calwatrs_flatfiles.geospatial_relationships as gr
    inner join swrcb_internal_test.wr_supply_and_demand.sda_demand_pod_subbasin as sda 
        on sda.pod_id = gr.poly_legacyname
    inner join swrcb_internal_prod.wr_ewrims.ewrims_flat_file_pod as ewrims
        on ewrims.POD_ID = sda.pod_id
    where 
    gr.gr_status = 'Active'
    AND gr.gr_feature_type = 'Point of Diversion'

    order by ewrims.pod_id asc;

    select * from ewrims_calwatrs_fields_comparison;

--2nd Temporary Table: Match_Counts
USE role swrcb_internal_test_wr_supply_and_demand_r_acrole;
USE DATABASE SWRCB_INTERNAL_TEST; USE SCHEMA WR_SUPPLY_AND_DEMAND;
    CREATE Temporary table matches AS
    --Creata a temporary table that counts the matches and mismatches
    Select
    pod_id,
    application_number is not distinct from wr_app_claim_name as application_number_match,
    county is not distinct from poly_county as county_match,
    east_coord is not distinct from poly_easting as easting_match,
    huc_12_name is not distinct from gr_watercourse_name as huc_12_name_match,
    huc_12_number is not distinct from poly_huc12 as huc_12_number_match,
    huc_8_name is not distinct from poly_basin_name as huc_8_name_match,
    LATITUDE IS NOT DISTINCT FROM POLY_LATITUDE AS LATITUDE_MATCH,
    LONGITUDE IS NOT DISTINCT FROM POLY_LONGITUDE AS LONGITUDE_MATCH,
    NORTH_COORD IS NOT DISTINCT FROM POLY_NORTHING AS NORTH_COORD_MATCH,
    RANGE_DIRECTION IS NOT DISTINCT FROM POLY_RANGE_DIRECTION AS RANGE_DIRECTION_MATCH,
    RANGE_NUMBER IS NOT DISTINCT FROM POLY_RANGE_NUMBER AS RANGE_NUMBER_MATCH,
    SECTION_NUMBER IS NOT DISTINCT FROM POLY_SECTION_NUMBER AS SECTION_NUMBER_MATCH,
    SOURCE_NAME IS NOT DISTINCT FROM POLY_SOURCE AS SOURCE_NAME_MATCH,
    TOWNSHIP_DIRECTION IS NOT DISTINCT FROM POLY_TOWNSHIP_DIRECTION AS TOWNSHIP_DIRECTION_MATCH,
    TOWNSHIP_NUMBER IS NOT DISTINCT FROM POLY_TOWNSHIP_NUMBER AS TOWNSHIP_NUMBER_MATCH,
    TRIB_DESC IS NOT DISTINCT FROM POLY_NHDTRIBUTARY AS TRIB_DESC_MATCH,
    WATER_RIGHT_STATUS IS NOT DISTINCT FROM WR_APP_CLAIM_STATUS AS WATER_RIGHT_STATUS_MATCH,
    WATER_RIGHT_TYPE IS NOT DISTINCT FROM WR_APP_CLAIM_TYPE AS WATER_RIGHT_TYPE_MATCH

    from ewrims_calwatrs_fields_comparison;

--Select * from matches;


--Temporary Table 3: Match_Counts
    Select
    Count_if(APPLICATION_NUMBER_MATCH = FALSE) as Application_number_mismatches,
    count_if(COUNTY_MATCH = FALSE) as county_mismatches,
    count_if(easting_match = FALSE) as easting_mismatches
    from matches