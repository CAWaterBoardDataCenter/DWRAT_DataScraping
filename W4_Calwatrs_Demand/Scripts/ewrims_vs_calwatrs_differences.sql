-- =====================================================================================
-- eWRIMS vs CalWATRS: field-level differences per POD, statewide
-- Same rules as compare_ewrims_calwatrs.ipynb (norm / crosswalks / match flags).
--
-- Run the statements IN ORDER in one Snowsight worksheet (Run All works):
--   1) Differences       one row per POD and field that disagrees
--   2) SET diff_qid      remembers statement 1's result
--   3) Diff_Value_Pairs  each distinct (field, eWRIMS value, CalWATRS value) + how many PODs
--   4) Diff_Summary      differences per field
--   5) Differences       statement 1 again, with the eWRIMS / CalWATRS values (shown by Run All)
--
-- Run All only displays the LAST statement's result. To see another one, put the cursor
-- inside it and press Ctrl+Enter (statements 3-5 need 1 and 2 to have run first).
--
-- A field is a DIFFERENCE when both systems have a value and neither system's values
-- are contained in the other's (e.g. CalWATRS listing every use on the right is NOT a
-- difference). LOCATION is a difference when the PODs are more than 30 m apart.
-- Scope: only PODs that are ACTIVE in CalWATRS (GR_STATUS); all eWRIMS rows for those PODs.
-- Role: WB_STAFF_ACROLE (+ USE SECONDARY ROLES ALL)
-- =====================================================================================

USE ROLE WB_STAFF_ACROLE;
USE SECONDARY ROLES ALL;
USE WAREHOUSE DWR_WH;
USE DATABASE SWRCB_INTERNAL_PROD;

-- 1) Differences ----------------------------------------------------------------------
WITH
ew_raw AS (   -- eWRIMS POD flat file: trim, collapse spaces, upper-case
    SELECT
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(APPLICATION_NUMBER), '\\s+', ' '))) AS APPLICATION_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POD_ID), '\\s+', ' ')))             AS POD_ID,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POD_STATUS), '\\s+', ' ')))         AS POD_STATUS,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POD_TYPE), '\\s+', ' ')))           AS POD_TYPE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WATER_RIGHT_TYPE), '\\s+', ' ')))   AS WATER_RIGHT_TYPE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(SUB_TYPE), '\\s+', ' ')))           AS SUB_TYPE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WATER_RIGHT_STATUS), '\\s+', ' '))) AS WATER_RIGHT_STATUS,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(FACE_VALUE_AMOUNT), '\\s+', ' ')))  AS FACE_VALUE_AMOUNT,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(FACE_VALUE_UNITS), '\\s+', ' ')))   AS FACE_VALUE_UNITS,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(PRIORITY_DATE), '\\s+', ' ')))      AS PRIORITY_DATE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(USE_CODE), '\\s+', ' ')))           AS USE_CODE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(SOURCE_NAME), '\\s+', ' ')))        AS SOURCE_NAME,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(COUNTY), '\\s+', ' ')))             AS COUNTY,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(HUC_12_NUMBER), '\\s+', ' ')))      AS HUC_12_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(TOWNSHIP_NUMBER), '\\s+', ' ')))    AS TOWNSHIP_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(TOWNSHIP_DIRECTION), '\\s+', ' '))) AS TOWNSHIP_DIRECTION,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(RANGE_NUMBER), '\\s+', ' ')))       AS RANGE_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(RANGE_DIRECTION), '\\s+', ' ')))    AS RANGE_DIRECTION,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(SECTION_NUMBER), '\\s+', ' ')))     AS SECTION_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(DIRECT_DIVERSION_RATE), '\\s+', ' '))) AS DIRECT_DIVERSION_RATE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(DIRECT_DIV_RATE_UNIT), '\\s+', ' ')))  AS DIRECT_DIV_RATE_UNIT,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WATERSHED), '\\s+', ' ')))          AS WATERSHED,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(HUC_8_NAME), '\\s+', ' ')))         AS HUC_8_NAME,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(HUC_8_NUMBER), '\\s+', ' ')))       AS HUC_8_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(EAST_COORD), '\\s+', ' ')))         AS EAST_COORD,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(NORTH_COORD), '\\s+', ' ')))        AS NORTH_COORD,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(HUC_12_NAME), '\\s+', ' ')))        AS HUC_12_NAME,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(TRIB_DESC), '\\s+', ' ')))          AS TRIB_DESC,
        TRY_TO_DOUBLE(TO_VARCHAR(LATITUDE))  AS LAT,
        TRY_TO_DOUBLE(TO_VARCHAR(LONGITUDE)) AS LON
    FROM SWRCB_INTERNAL_PROD.WR_EWRIMS.EWRIMS_FLAT_FILE_POD
),
ew_n AS (     -- blank / NONE / NAN / NULL / NAT -> NULL
    SELECT
        IFF(APPLICATION_NUMBER IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, APPLICATION_NUMBER) AS APPLICATION_NUMBER,
        IFF(POD_ID             IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POD_ID)             AS POD_ID,
        IFF(POD_STATUS         IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POD_STATUS)         AS POD_STATUS,
        IFF(POD_TYPE           IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POD_TYPE)           AS POD_TYPE,
        IFF(WATER_RIGHT_TYPE   IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WATER_RIGHT_TYPE)   AS WATER_RIGHT_TYPE,
        IFF(SUB_TYPE           IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, SUB_TYPE)           AS SUB_TYPE,
        IFF(WATER_RIGHT_STATUS IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WATER_RIGHT_STATUS) AS WATER_RIGHT_STATUS,
        IFF(FACE_VALUE_AMOUNT  IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, FACE_VALUE_AMOUNT)  AS FACE_VALUE_AMOUNT,
        IFF(FACE_VALUE_UNITS   IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, FACE_VALUE_UNITS)   AS FACE_VALUE_UNITS,
        IFF(PRIORITY_DATE      IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, PRIORITY_DATE)      AS PRIORITY_DATE,
        IFF(USE_CODE           IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, USE_CODE)           AS USE_CODE,
        IFF(SOURCE_NAME        IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, SOURCE_NAME)        AS SOURCE_NAME,
        IFF(COUNTY             IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, COUNTY)             AS COUNTY,
        IFF(HUC_12_NUMBER      IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, HUC_12_NUMBER)      AS HUC_12_NUMBER,
        IFF(TOWNSHIP_NUMBER    IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, TOWNSHIP_NUMBER)    AS TOWNSHIP_NUMBER,
        IFF(TOWNSHIP_DIRECTION IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, TOWNSHIP_DIRECTION) AS TOWNSHIP_DIRECTION,
        IFF(RANGE_NUMBER       IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, RANGE_NUMBER)       AS RANGE_NUMBER,
        IFF(RANGE_DIRECTION    IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, RANGE_DIRECTION)    AS RANGE_DIRECTION,
        IFF(SECTION_NUMBER     IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, SECTION_NUMBER)     AS SECTION_NUMBER,
        IFF(DIRECT_DIVERSION_RATE IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, DIRECT_DIVERSION_RATE) AS DIRECT_DIVERSION_RATE,
        IFF(DIRECT_DIV_RATE_UNIT  IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, DIRECT_DIV_RATE_UNIT)  AS DIRECT_DIV_RATE_UNIT,
        IFF(WATERSHED          IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WATERSHED)          AS WATERSHED,
        IFF(HUC_8_NAME         IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, HUC_8_NAME)         AS HUC_8_NAME,
        IFF(HUC_8_NUMBER       IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, HUC_8_NUMBER)       AS HUC_8_NUMBER,
        IFF(EAST_COORD         IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, EAST_COORD)         AS EAST_COORD,
        IFF(NORTH_COORD        IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, NORTH_COORD)        AS NORTH_COORD,
        IFF(HUC_12_NAME        IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, HUC_12_NAME)        AS HUC_12_NAME,
        IFF(TRIB_DESC          IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, TRIB_DESC)          AS TRIB_DESC,
        LAT, LON
    FROM ew_raw
),
ew_k AS (     -- eWRIMS: key + the 24 compared fields, put into CalWATRS terms (crosswalks)
    SELECT * FROM (
        SELECT
            APPLICATION_NUMBER                                           AS APP,
            TO_VARCHAR(TRUNC(TRY_TO_DOUBLE(POD_ID))::NUMBER(38, 0))      AS POD,
            COALESCE(WATER_RIGHT_TYPE = 'STATEMENT OF DIV AND USE', FALSE) AS IS_STMT,
            LAT, LON,
            COUNTY AS CTX_COUNTY, WATERSHED AS CTX_WATERSHED, HUC_8_NAME AS CTX_HUC_8_NAME,
            REGEXP_REPLACE(HUC_8_NUMBER, '^([0-9]+)\\.0$', '\\1')        AS CTX_HUC_8_NUMBER,

            POD_STATUS                                                   AS F_POD_STATUS,
            CASE WHEN CONTAINS(POD_TYPE, 'REDIVERSION') THEN 'POINT OF REDIVERSION'
                 WHEN CONTAINS(POD_TYPE, 'STORAGE') AND NOT CONTAINS(POD_TYPE, 'OFFSTREAM')
                      AND NOT CONTAINS(POD_TYPE, 'OFF-STREAM')         THEN 'POINT OF STORAGE'
                 WHEN CONTAINS(POD_TYPE, 'DIVERSION')                  THEN 'POINT OF DIVERSION'
                 ELSE POD_TYPE END                                       AS F_POD_TYPE,
            CASE WHEN STARTSWITH(WATER_RIGHT_TYPE, 'REGISTRATION')     THEN 'REGISTRATION'
                 WHEN WATER_RIGHT_TYPE = 'STATEMENT OF DIV AND USE'    THEN SUB_TYPE   -- statement: its sub-types are the type list
                 ELSE WATER_RIGHT_TYPE END                               AS F_WATER_RIGHT_TYPE,
            CASE WHEN STARTSWITH(WATER_RIGHT_TYPE, 'REGISTRATION')     THEN WATER_RIGHT_TYPE
                 WHEN WATER_RIGHT_TYPE = 'STATEMENT OF DIV AND USE'    THEN NULL
                 ELSE SUB_TYPE END                                       AS F_SUB_TYPE,
            CASE WATER_RIGHT_STATUS WHEN 'CLAIMED' THEN 'ACTIVE'
                 ELSE WATER_RIGHT_STATUS END                             AS F_WATER_RIGHT_STATUS,
            ROUND(NULLIF(TRY_TO_DOUBLE(FACE_VALUE_AMOUNT), 0), 6)        AS F_FACE_VALUE,       -- 0 = blank
            CASE FACE_VALUE_UNITS WHEN 'ACRE-FEET PER YEAR' THEN 'ACRE-FEET'
                 ELSE FACE_VALUE_UNITS END                               AS F_FACE_VALUE_UNITS,
            TO_VARCHAR(IFF(REGEXP_LIKE(PRIORITY_DATE, '[0-9]{8}(\\.0)?'),
                           TRY_TO_DATE(LEFT(PRIORITY_DATE, 8), 'YYYYMMDD'),
                           COALESCE(TRY_TO_DATE(PRIORITY_DATE), TRY_TO_TIMESTAMP(PRIORITY_DATE)::DATE)),
                       'YYYY-MM-DD')                                     AS F_PRIORITY_DATE,
            USE_CODE                                                     AS F_BENEFICIAL_USE,
            SOURCE_NAME                                                  AS F_SOURCE_NAME,
            COUNTY                                                       AS F_COUNTY,
            REGEXP_REPLACE(HUC_12_NUMBER, '^([0-9]+)\\.0$', '\\1')       AS F_HUC12,            -- 180201630606.0 -> 180201630606
            ROUND(TRY_TO_DOUBLE(TOWNSHIP_NUMBER), 6)                     AS F_TOWNSHIP,
            TOWNSHIP_DIRECTION                                           AS F_TOWNSHIP_DIR,
            ROUND(TRY_TO_DOUBLE(RANGE_NUMBER), 6)                        AS F_RANGE,
            RANGE_DIRECTION                                              AS F_RANGE_DIR,
            ROUND(TRY_TO_DOUBLE(SECTION_NUMBER), 6)                      AS F_SECTION,
            ROUND(ROUND(TRY_TO_DOUBLE(DIRECT_DIVERSION_RATE), 6) *
                  CASE DIRECT_DIV_RATE_UNIT                              -- -> cubic feet per second
                      WHEN 'CUBIC FEET PER SECOND' THEN 1.0::FLOAT
                      WHEN 'GALLONS PER DAY'       THEN 1 / 646316.88::FLOAT
                      WHEN 'GALLONS PER MINUTE'    THEN 1 / 448.831::FLOAT
                      WHEN 'ACRE-FEET PER YEAR'    THEN 1 / 723.97::FLOAT
                      WHEN 'ACRE-FEET PER DAY'     THEN 1 / 1.98347::FLOAT
                      WHEN 'MINER''S INCHES'       THEN 1 / 40.0::FLOAT
                  END, 6)                                                AS F_DIV_RATE_CFS,
            ROUND(TRY_TO_DOUBLE(EAST_COORD), 6)                          AS F_EAST_COORD,
            ROUND(TRY_TO_DOUBLE(NORTH_COORD), 6)                         AS F_NORTH_COORD,
            REGEXP_REPLACE(HUC_8_NUMBER, '^([0-9]+)\\.0$', '\\1')        AS F_HUC8,
            HUC_8_NAME                                                   AS F_HUC8_NAME,
            HUC_12_NAME                                                  AS F_HUC12_NAME,
            TRIB_DESC                                                    AS F_TRIB_DESC
        FROM ew_n)
    WHERE APP IS NOT NULL AND POD IS NOT NULL
),

cw_raw AS (   -- CalWATRS geospatial relationships (not deleted): trim, collapse spaces, upper-case
    SELECT
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_CLAIM_NAME), '\\s+', ' ')))         AS WR_APP_CLAIM_NAME,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_LEGACYNAME), '\\s+', ' ')))           AS POLY_LEGACYNAME,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(GR_STATUS), '\\s+', ' ')))                 AS GR_STATUS,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(GR_FEATURE_TYPE), '\\s+', ' ')))           AS GR_FEATURE_TYPE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_CLAIM_TYPE), '\\s+', ' ')))         AS WR_APP_CLAIM_TYPE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_SUBTYPE), '\\s+', ' ')))            AS WR_APP_SUBTYPE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_CLAIM_STATUS), '\\s+', ' ')))       AS WR_APP_CLAIM_STATUS,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_FACEVALUE), '\\s+', ' ')))          AS WR_APP_FACEVALUE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_FACEVALUE_UNIT), '\\s+', ' ')))     AS WR_APP_FACEVALUE_UNIT,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_CLAIM_PRIORITYDATE), '\\s+', ' '))) AS WR_APP_CLAIM_PRIORITYDATE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(WR_APP_CLAIM_BENEFICIALPURPOSESOFUSE), '\\s+', ' '))) AS WR_APP_CLAIM_BENEFICIALPURPOSESOFUSE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_SOURCE), '\\s+', ' ')))               AS POLY_SOURCE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_COUNTY), '\\s+', ' ')))               AS POLY_COUNTY,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_HUC12), '\\s+', ' ')))                AS POLY_HUC12,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_TOWNSHIP_NUMBER), '\\s+', ' ')))      AS POLY_TOWNSHIP_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_TOWNSHIP_DIRECTION), '\\s+', ' ')))   AS POLY_TOWNSHIP_DIRECTION,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_RANGE_NUMBER), '\\s+', ' ')))         AS POLY_RANGE_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_RANGE_DIRECTION), '\\s+', ' ')))      AS POLY_RANGE_DIRECTION,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_SECTION_NUMBER), '\\s+', ' ')))       AS POLY_SECTION_NUMBER,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_MAXFLOWRATE), '\\s+', ' ')))          AS POLY_MAXFLOWRATE,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_MAXFLOWRATEUNIT), '\\s+', ' ')))      AS POLY_MAXFLOWRATEUNIT,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_EASTING), '\\s+', ' ')))              AS POLY_EASTING,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_NORTHING), '\\s+', ' ')))             AS POLY_NORTHING,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_HUC8), '\\s+', ' ')))                 AS POLY_HUC8,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_BASIN_NAME), '\\s+', ' ')))           AS POLY_BASIN_NAME,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(POLY_NHDTRIBUTARY), '\\s+', ' ')))         AS POLY_NHDTRIBUTARY,
        UPPER(TRIM(REGEXP_REPLACE(TO_VARCHAR(GR_WATERCOURSE_NAME), '\\s+', ' ')))       AS GR_WATERCOURSE_NAME,
        TRY_TO_DOUBLE(TO_VARCHAR(POLY_LATITUDE))  AS LAT,
        TRY_TO_DOUBLE(TO_VARCHAR(POLY_LONGITUDE)) AS LON
    FROM SWRCB_INTERNAL_PROD.WR_CALWATRS_FLATFILES.GEOSPATIAL_RELATIONSHIPS
    WHERE COALESCE(GR_ISDELETED, FALSE) = FALSE
      AND UPPER(TRIM(GR_STATUS)) = 'ACTIVE'      -- active in CalWATRS only (remove this line for all PODs)
),
cw_n AS (     -- blank / NONE / NAN / NULL / NAT -> NULL
    SELECT
        IFF(WR_APP_CLAIM_NAME         IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WR_APP_CLAIM_NAME)         AS WR_APP_CLAIM_NAME,
        IFF(POLY_LEGACYNAME           IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_LEGACYNAME)           AS POLY_LEGACYNAME,
        IFF(GR_STATUS                 IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, GR_STATUS)                 AS GR_STATUS,
        IFF(GR_FEATURE_TYPE           IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, GR_FEATURE_TYPE)           AS GR_FEATURE_TYPE,
        IFF(WR_APP_CLAIM_TYPE         IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WR_APP_CLAIM_TYPE)         AS WR_APP_CLAIM_TYPE,
        IFF(WR_APP_SUBTYPE            IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WR_APP_SUBTYPE)            AS WR_APP_SUBTYPE,
        IFF(WR_APP_CLAIM_STATUS       IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WR_APP_CLAIM_STATUS)       AS WR_APP_CLAIM_STATUS,
        IFF(WR_APP_FACEVALUE          IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WR_APP_FACEVALUE)          AS WR_APP_FACEVALUE,
        IFF(WR_APP_FACEVALUE_UNIT     IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WR_APP_FACEVALUE_UNIT)     AS WR_APP_FACEVALUE_UNIT,
        IFF(WR_APP_CLAIM_PRIORITYDATE IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, WR_APP_CLAIM_PRIORITYDATE) AS WR_APP_CLAIM_PRIORITYDATE,
        IFF(WR_APP_CLAIM_BENEFICIALPURPOSESOFUSE IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL,
            WR_APP_CLAIM_BENEFICIALPURPOSESOFUSE)                                                             AS WR_APP_CLAIM_BENEFICIALPURPOSESOFUSE,
        IFF(POLY_SOURCE               IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_SOURCE)               AS POLY_SOURCE,
        IFF(POLY_COUNTY               IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_COUNTY)               AS POLY_COUNTY,
        IFF(POLY_HUC12                IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_HUC12)                AS POLY_HUC12,
        IFF(POLY_TOWNSHIP_NUMBER      IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_TOWNSHIP_NUMBER)      AS POLY_TOWNSHIP_NUMBER,
        IFF(POLY_TOWNSHIP_DIRECTION   IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_TOWNSHIP_DIRECTION)   AS POLY_TOWNSHIP_DIRECTION,
        IFF(POLY_RANGE_NUMBER         IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_RANGE_NUMBER)         AS POLY_RANGE_NUMBER,
        IFF(POLY_RANGE_DIRECTION      IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_RANGE_DIRECTION)      AS POLY_RANGE_DIRECTION,
        IFF(POLY_SECTION_NUMBER       IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_SECTION_NUMBER)       AS POLY_SECTION_NUMBER,
        IFF(POLY_MAXFLOWRATE          IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_MAXFLOWRATE)          AS POLY_MAXFLOWRATE,
        IFF(POLY_MAXFLOWRATEUNIT      IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_MAXFLOWRATEUNIT)      AS POLY_MAXFLOWRATEUNIT,
        IFF(POLY_EASTING              IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_EASTING)              AS POLY_EASTING,
        IFF(POLY_NORTHING             IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_NORTHING)             AS POLY_NORTHING,
        IFF(POLY_HUC8                 IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_HUC8)                 AS POLY_HUC8,
        IFF(POLY_BASIN_NAME           IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_BASIN_NAME)           AS POLY_BASIN_NAME,
        IFF(POLY_NHDTRIBUTARY         IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, POLY_NHDTRIBUTARY)         AS POLY_NHDTRIBUTARY,
        IFF(GR_WATERCOURSE_NAME       IN ('', 'NONE', 'NAN', 'NULL', 'NAT'), NULL, GR_WATERCOURSE_NAME)       AS GR_WATERCOURSE_NAME,
        LAT, LON
    FROM cw_raw
),
cw_k AS (     -- CalWATRS: key + the 24 compared fields (same rules as the eWRIMS side where they apply to both)
    SELECT * FROM (
        SELECT
            WR_APP_CLAIM_NAME                                            AS APP,
            REGEXP_REPLACE(POLY_LEGACYNAME, '^([0-9]+)\\.0$', '\\1')     AS POD,               -- CalWATRS keeps the eWRIMS POD_ID here
            FALSE                                                        AS IS_STMT,
            LAT, LON,
            POLY_COUNTY                                                  AS CTX_COUNTY,

            GR_STATUS                                                    AS F_POD_STATUS,
            CASE WHEN CONTAINS(GR_FEATURE_TYPE, 'REDIVERSION') THEN 'POINT OF REDIVERSION'
                 WHEN CONTAINS(GR_FEATURE_TYPE, 'STORAGE') AND NOT CONTAINS(GR_FEATURE_TYPE, 'OFFSTREAM')
                      AND NOT CONTAINS(GR_FEATURE_TYPE, 'OFF-STREAM')  THEN 'POINT OF STORAGE'
                 WHEN CONTAINS(GR_FEATURE_TYPE, 'DIVERSION')           THEN 'POINT OF DIVERSION'
                 ELSE GR_FEATURE_TYPE END                                AS F_POD_TYPE,
            WR_APP_CLAIM_TYPE                                            AS F_WATER_RIGHT_TYPE,
            WR_APP_SUBTYPE                                               AS F_SUB_TYPE,
            WR_APP_CLAIM_STATUS                                          AS F_WATER_RIGHT_STATUS,
            ROUND(NULLIF(TRY_TO_DOUBLE(WR_APP_FACEVALUE), 0), 6)         AS F_FACE_VALUE,
            WR_APP_FACEVALUE_UNIT                                        AS F_FACE_VALUE_UNITS,
            TO_VARCHAR(IFF(REGEXP_LIKE(WR_APP_CLAIM_PRIORITYDATE, '[0-9]{8}(\\.0)?'),
                           TRY_TO_DATE(LEFT(WR_APP_CLAIM_PRIORITYDATE, 8), 'YYYYMMDD'),
                           COALESCE(TRY_TO_DATE(WR_APP_CLAIM_PRIORITYDATE),
                                    TRY_TO_TIMESTAMP(WR_APP_CLAIM_PRIORITYDATE)::DATE)),
                       'YYYY-MM-DD')                                     AS F_PRIORITY_DATE,
            WR_APP_CLAIM_BENEFICIALPURPOSESOFUSE                         AS F_BENEFICIAL_USE,
            POLY_SOURCE                                                  AS F_SOURCE_NAME,
            POLY_COUNTY                                                  AS F_COUNTY,
            REGEXP_REPLACE(POLY_HUC12, '^([0-9]+)\\.0$', '\\1')          AS F_HUC12,
            ROUND(TRY_TO_DOUBLE(POLY_TOWNSHIP_NUMBER), 6)                AS F_TOWNSHIP,
            POLY_TOWNSHIP_DIRECTION                                      AS F_TOWNSHIP_DIR,
            ROUND(TRY_TO_DOUBLE(POLY_RANGE_NUMBER), 6)                   AS F_RANGE,
            POLY_RANGE_DIRECTION                                         AS F_RANGE_DIR,
            ROUND(TRY_TO_DOUBLE(POLY_SECTION_NUMBER), 6)                 AS F_SECTION,
            ROUND(ROUND(TRY_TO_DOUBLE(POLY_MAXFLOWRATE), 6) *
                  CASE POLY_MAXFLOWRATEUNIT
                      WHEN 'CUBIC FEET PER SECOND' THEN 1.0::FLOAT
                      WHEN 'GALLONS PER DAY'       THEN 1 / 646316.88::FLOAT
                      WHEN 'GALLONS PER MINUTE'    THEN 1 / 448.831::FLOAT
                      WHEN 'ACRE-FEET PER YEAR'    THEN 1 / 723.97::FLOAT
                      WHEN 'ACRE-FEET PER DAY'     THEN 1 / 1.98347::FLOAT
                      WHEN 'MINER''S INCHES'       THEN 1 / 40.0::FLOAT
                  END, 6)                                                AS F_DIV_RATE_CFS,
            ROUND(TRY_TO_DOUBLE(POLY_EASTING), 6)                        AS F_EAST_COORD,
            ROUND(TRY_TO_DOUBLE(POLY_NORTHING), 6)                       AS F_NORTH_COORD,
            REGEXP_REPLACE(POLY_HUC8, '^([0-9]+)\\.0$', '\\1')           AS F_HUC8,
            POLY_BASIN_NAME                                              AS F_HUC8_NAME,
            GR_WATERCOURSE_NAME                                          AS F_HUC12_NAME,
            POLY_NHDTRIBUTARY                                            AS F_TRIB_DESC
        FROM cw_n)
    WHERE APP IS NOT NULL AND POD IS NOT NULL
),

ew_long AS (  -- one row per POD / field / value (list fields split on , and ;)
    SELECT APP, POD, 'POD_STATUS'         AS FIELD, TO_VARCHAR(F_POD_STATUS)         AS VAL FROM ew_k UNION ALL
    SELECT APP, POD, 'POD_TYPE',                    TO_VARCHAR(F_POD_TYPE)                  FROM ew_k UNION ALL
    SELECT APP, POD, 'SUB_TYPE',                    TO_VARCHAR(F_SUB_TYPE)                  FROM ew_k UNION ALL
    SELECT APP, POD, 'WATER_RIGHT_STATUS',          TO_VARCHAR(F_WATER_RIGHT_STATUS)        FROM ew_k UNION ALL
    SELECT APP, POD, 'FACE_VALUE',                  TO_VARCHAR(F_FACE_VALUE)                FROM ew_k UNION ALL
    SELECT APP, POD, 'FACE_VALUE_UNITS',            TO_VARCHAR(F_FACE_VALUE_UNITS)          FROM ew_k UNION ALL
    SELECT APP, POD, 'PRIORITY_DATE',               TO_VARCHAR(F_PRIORITY_DATE)             FROM ew_k UNION ALL
    SELECT APP, POD, 'SOURCE_NAME',                 TO_VARCHAR(F_SOURCE_NAME)               FROM ew_k UNION ALL
    SELECT APP, POD, 'COUNTY',                      TO_VARCHAR(F_COUNTY)                    FROM ew_k UNION ALL
    SELECT APP, POD, 'HUC12',                       TO_VARCHAR(F_HUC12)                     FROM ew_k UNION ALL
    SELECT APP, POD, 'TOWNSHIP',                    TO_VARCHAR(F_TOWNSHIP)                  FROM ew_k UNION ALL
    SELECT APP, POD, 'TOWNSHIP_DIR',                TO_VARCHAR(F_TOWNSHIP_DIR)              FROM ew_k UNION ALL
    SELECT APP, POD, 'RANGE',                       TO_VARCHAR(F_RANGE)                     FROM ew_k UNION ALL
    SELECT APP, POD, 'RANGE_DIR',                   TO_VARCHAR(F_RANGE_DIR)                 FROM ew_k UNION ALL
    SELECT APP, POD, 'SECTION',                     TO_VARCHAR(F_SECTION)                   FROM ew_k UNION ALL
    SELECT APP, POD, 'DIV_RATE_CFS',                TO_VARCHAR(F_DIV_RATE_CFS)              FROM ew_k UNION ALL
    SELECT APP, POD, 'EAST_COORD',                  TO_VARCHAR(F_EAST_COORD)                FROM ew_k UNION ALL
    SELECT APP, POD, 'NORTH_COORD',                 TO_VARCHAR(F_NORTH_COORD)               FROM ew_k UNION ALL
    SELECT APP, POD, 'HUC8',                        TO_VARCHAR(F_HUC8)                      FROM ew_k UNION ALL
    SELECT APP, POD, 'HUC8_NAME',                   TO_VARCHAR(F_HUC8_NAME)                 FROM ew_k UNION ALL
    SELECT APP, POD, 'HUC12_NAME',                  TO_VARCHAR(F_HUC12_NAME)                FROM ew_k UNION ALL
    SELECT APP, POD, 'TRIB_DESC',                   TO_VARCHAR(F_TRIB_DESC)                 FROM ew_k UNION ALL
    SELECT APP, POD, 'WATER_RIGHT_TYPE',
           IFF(IS_STMT, CASE TOK WHEN 'RIPARIAN' THEN 'RIPARIAN'              -- statement sub-type crosswalk
                                 WHEN 'PRE1914'  THEN 'PRE-1914 APPROPRIATIVE'
                                 WHEN 'PENDING'  THEN 'PENDING APPROPRIATIVE APPLICATION'
                                 WHEN 'OTHER'    THEN 'OTHER'
                                 ELSE TOK END, TOK)
    FROM (SELECT v.APP, v.POD, v.IS_STMT, UPPER(TRIM(REGEXP_REPLACE(s.VALUE, '\\s+', ' '))) AS TOK
          FROM ew_k v, LATERAL SPLIT_TO_TABLE(REPLACE(v.F_WATER_RIGHT_TYPE, ';', ','), ',') s) UNION ALL
    SELECT v.APP, v.POD, 'BENEFICIAL_USE', UPPER(TRIM(REGEXP_REPLACE(s.VALUE, '\\s+', ' ')))
    FROM ew_k v, LATERAL SPLIT_TO_TABLE(REPLACE(v.F_BENEFICIAL_USE, ';', ','), ',') s
),
cw_long AS (
    SELECT APP, POD, 'POD_STATUS'         AS FIELD, TO_VARCHAR(F_POD_STATUS)         AS VAL FROM cw_k UNION ALL
    SELECT APP, POD, 'POD_TYPE',                    TO_VARCHAR(F_POD_TYPE)                  FROM cw_k UNION ALL
    SELECT APP, POD, 'SUB_TYPE',                    TO_VARCHAR(F_SUB_TYPE)                  FROM cw_k UNION ALL
    SELECT APP, POD, 'WATER_RIGHT_STATUS',          TO_VARCHAR(F_WATER_RIGHT_STATUS)        FROM cw_k UNION ALL
    SELECT APP, POD, 'FACE_VALUE',                  TO_VARCHAR(F_FACE_VALUE)                FROM cw_k UNION ALL
    SELECT APP, POD, 'FACE_VALUE_UNITS',            TO_VARCHAR(F_FACE_VALUE_UNITS)          FROM cw_k UNION ALL
    SELECT APP, POD, 'PRIORITY_DATE',               TO_VARCHAR(F_PRIORITY_DATE)             FROM cw_k UNION ALL
    SELECT APP, POD, 'SOURCE_NAME',                 TO_VARCHAR(F_SOURCE_NAME)               FROM cw_k UNION ALL
    SELECT APP, POD, 'COUNTY',                      TO_VARCHAR(F_COUNTY)                    FROM cw_k UNION ALL
    SELECT APP, POD, 'HUC12',                       TO_VARCHAR(F_HUC12)                     FROM cw_k UNION ALL
    SELECT APP, POD, 'TOWNSHIP',                    TO_VARCHAR(F_TOWNSHIP)                  FROM cw_k UNION ALL
    SELECT APP, POD, 'TOWNSHIP_DIR',                TO_VARCHAR(F_TOWNSHIP_DIR)              FROM cw_k UNION ALL
    SELECT APP, POD, 'RANGE',                       TO_VARCHAR(F_RANGE)                     FROM cw_k UNION ALL
    SELECT APP, POD, 'RANGE_DIR',                   TO_VARCHAR(F_RANGE_DIR)                 FROM cw_k UNION ALL
    SELECT APP, POD, 'SECTION',                     TO_VARCHAR(F_SECTION)                   FROM cw_k UNION ALL
    SELECT APP, POD, 'DIV_RATE_CFS',                TO_VARCHAR(F_DIV_RATE_CFS)              FROM cw_k UNION ALL
    SELECT APP, POD, 'EAST_COORD',                  TO_VARCHAR(F_EAST_COORD)                FROM cw_k UNION ALL
    SELECT APP, POD, 'NORTH_COORD',                 TO_VARCHAR(F_NORTH_COORD)               FROM cw_k UNION ALL
    SELECT APP, POD, 'HUC8',                        TO_VARCHAR(F_HUC8)                      FROM cw_k UNION ALL
    SELECT APP, POD, 'HUC8_NAME',                   TO_VARCHAR(F_HUC8_NAME)                 FROM cw_k UNION ALL
    SELECT APP, POD, 'HUC12_NAME',                  TO_VARCHAR(F_HUC12_NAME)                FROM cw_k UNION ALL
    SELECT APP, POD, 'TRIB_DESC',                   TO_VARCHAR(F_TRIB_DESC)                 FROM cw_k UNION ALL
    SELECT v.APP, v.POD, 'WATER_RIGHT_TYPE', UPPER(TRIM(REGEXP_REPLACE(s.VALUE, '\\s+', ' ')))
    FROM cw_k v, LATERAL SPLIT_TO_TABLE(REPLACE(v.F_WATER_RIGHT_TYPE, ';', ','), ',') s UNION ALL
    SELECT v.APP, v.POD, 'BENEFICIAL_USE', UPPER(TRIM(REGEXP_REPLACE(s.VALUE, '\\s+', ' ')))
    FROM cw_k v, LATERAL SPLIT_TO_TABLE(REPLACE(v.F_BENEFICIAL_USE, ';', ','), ',') s
),

ew_a AS (     -- all distinct values per POD and field, as a sorted list
    SELECT APP, POD, FIELD, ARRAY_SORT(ARRAY_AGG(DISTINCT VAL)) AS A
    FROM ew_long WHERE VAL IS NOT NULL AND VAL NOT IN ('', 'NONE', 'NAN', 'NULL', 'NAT')
    GROUP BY 1, 2, 3
),
cw_a AS (
    SELECT APP, POD, FIELD, ARRAY_SORT(ARRAY_AGG(DISTINCT VAL)) AS A
    FROM cw_long WHERE VAL IS NOT NULL AND VAL NOT IN ('', 'NONE', 'NAN', 'NULL', 'NAT')
    GROUP BY 1, 2, 3
),
field_diffs AS (   -- both have values, and neither side's values are contained in the other's (= DIFF)
    SELECT e.APP, e.POD, e.FIELD,
           ARRAY_TO_STRING(e.A, '; ') AS EWRIMS, ARRAY_TO_STRING(c.A, '; ') AS CALWATRS
    FROM ew_a e
    JOIN cw_a c ON c.APP = e.APP AND c.POD = e.POD AND c.FIELD = e.FIELD
    WHERE ARRAY_SIZE(ARRAY_EXCEPT(e.A, c.A)) > 0
      AND ARRAY_SIZE(ARRAY_EXCEPT(c.A, e.A)) > 0
      AND NOT (e.FIELD IN ('FACE_VALUE', 'TOWNSHIP', 'RANGE', 'SECTION', 'DIV_RATE_CFS', 'EAST_COORD', 'NORTH_COORD')   -- numbers equal within 1e-6
               AND ARRAY_SIZE(e.A) = 1 AND ARRAY_SIZE(c.A) = 1
               AND ABS(TRY_TO_DOUBLE(e.A[0]::VARCHAR) - TRY_TO_DOUBLE(c.A[0]::VARCHAR))
                   <= 1e-9 + 1e-6 * ABS(TRY_TO_DOUBLE(c.A[0]::VARCHAR)))
),
ew_loc AS (SELECT APP, POD, AVG(LAT) AS LAT, AVG(LON) AS LON FROM ew_k GROUP BY 1, 2),
cw_loc AS (SELECT APP, POD, AVG(LAT) AS LAT, AVG(LON) AS LON FROM cw_k GROUP BY 1, 2),
loc_diffs AS (     -- PODs more than 30 m apart
    SELECT e.APP, e.POD, 'LOCATION' AS FIELD,
           TO_VARCHAR(ROUND(e.LAT, 6)) || ', ' || TO_VARCHAR(ROUND(e.LON, 6)) AS EWRIMS,
           TO_VARCHAR(ROUND(c.LAT, 6)) || ', ' || TO_VARCHAR(ROUND(c.LON, 6)) AS CALWATRS
    FROM ew_loc e
    JOIN cw_loc c ON c.APP = e.APP AND c.POD = e.POD
    WHERE ROUND(HAVERSINE(e.LAT, e.LON, c.LAT, c.LON) * 1000, 1) > 30
),
ew_ctx AS (        -- where the POD is, for filtering
    SELECT APP, POD,
           LISTAGG(DISTINCT CTX_COUNTY, '; ')       AS COUNTY,
           LISTAGG(DISTINCT CTX_WATERSHED, '; ')    AS WATERSHED,
           LISTAGG(DISTINCT CTX_HUC_8_NAME, '; ')   AS HUC_8_NAME,
           LISTAGG(DISTINCT CTX_HUC_8_NUMBER, '; ') AS HUC_8_NUMBER
    FROM ew_k GROUP BY 1, 2
),
cw_ctx AS (SELECT APP, POD, LISTAGG(DISTINCT CTX_COUNTY, '; ') AS COUNTY FROM cw_k GROUP BY 1, 2)

SELECT d.APP AS APPLICATION_NUMBER, d.POD AS POD_ID, d.FIELD, d.EWRIMS, d.CALWATRS,
       COALESCE(NULLIF(x.COUNTY, ''), y.COUNTY) AS COUNTY, x.WATERSHED, x.HUC_8_NAME, x.HUC_8_NUMBER
FROM (SELECT * FROM field_diffs UNION ALL SELECT * FROM loc_diffs) d
LEFT JOIN ew_ctx x ON x.APP = d.APP AND x.POD = d.POD
LEFT JOIN cw_ctx y ON y.APP = d.APP AND y.POD = d.POD
ORDER BY APPLICATION_NUMBER, POD_ID, FIELD;

-- 2) remember the result of statement 1 -----------------------------------------------
SET diff_qid = LAST_QUERY_ID();

-- 3) Diff_Value_Pairs -------------------------------------------------------------------
SELECT FIELD, EWRIMS, CALWATRS,
       COUNT(*)                           AS N_PODS,
       COUNT(DISTINCT APPLICATION_NUMBER) AS N_RIGHTS,
       MIN(APPLICATION_NUMBER)            AS EXAMPLE_APPLICATION,
       MIN_BY(POD_ID, APPLICATION_NUMBER) AS EXAMPLE_POD_ID
FROM TABLE(RESULT_SCAN($diff_qid))
GROUP BY 1, 2, 3
ORDER BY N_PODS DESC;

-- 4) Diff_Summary: differences per field ----------------------------------------------
SELECT FIELD,
       COUNT(*)                           AS N_DIFFS,
       COUNT(DISTINCT APPLICATION_NUMBER) AS N_RIGHTS,
       COUNT(DISTINCT EWRIMS, CALWATRS)   AS N_DISTINCT_VALUE_PAIRS
FROM TABLE(RESULT_SCAN($diff_qid))
GROUP BY 1
ORDER BY N_DIFFS DESC;

-- 5) Differences again, with the eWRIMS and CalWATRS values ---------------------------
--    (Run All only shows the LAST statement's result, so this one comes last.
--     Uncomment a WHERE line to look at one field or county.)
SELECT APPLICATION_NUMBER, POD_ID, FIELD, EWRIMS AS EWRIMS_VALUE, CALWATRS AS CALWATRS_VALUE,
       COUNTY, WATERSHED, HUC_8_NAME, HUC_8_NUMBER
FROM TABLE(RESULT_SCAN($diff_qid))
-- WHERE FIELD = 'WATER_RIGHT_STATUS'
-- WHERE COUNTY = 'HUMBOLDT'
ORDER BY FIELD, APPLICATION_NUMBER, POD_ID;
