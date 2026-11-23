
USE role WB_STAFF_ACROLE;
-- USE role swrcb_internal_test_wr_supply_and_demand_r_acrole;

-- USE DATABASE SWRCB_INTERNAL_TEST; 

-- TASK: Create queries that check and, if necessary, convert units of users' annual report volumes

USE DATABASE SWRCB_INTERNAL_PROD;

SELECT COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_SCHEMA = 'WR_CALWATRS_FLATFILES'
AND TABLE_NAME = 'ANNUAL_REPORTS';


SELECT COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_SCHEMA = 'WR_CALWATRS_FLATFILES'
AND TABLE_NAME = 'ANNUAL_REPORTS'
AND COLUMN_NAME ILIKE '%UNI%'
ORDER BY COLUMN_NAME asc;
-- above looks for columns that contain "Units" in the header name

SELECT AR_MONTHLYDIVERSIONUNIT,
    COUNT(*) AS frequency
FROM WR_CALWATRS_FLATFILES.ANNUAL_REPORTS
GROUP BY AR_MONTHLYDIVERSIONUNIT
ORDER BY frequency DESC;
-- above looks through the AR_MONTHLYDIVERSIONUNIT column to see what the entries are
-- then, know what conversions to make to put any unit into Acre-Feet
-- 2026/11/19: only units are Acre-feet, null, and Gallons

SELECT 
    AR_JANDIVERSIONVOLUME AS jan_div_volume,
    AR_MONTHLYDIVERSIONUNIT AS div_unit,
    CASE 
        -- put all column text in UPPER case to avoid things falling through cracks due to differences in how data entered into field
        -- IN creates a list that the query checks
        -- if already acre-feet, no change
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('ACRE-FEET', 'AF', 'ACRE FEET')
        THEN AR_JANDIVERSIONVOLUME

        -- if it's in gallons
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('GALLONS', 'GAL')
        THEN AR_JANDIVERSIONVOLUME / 325851.0 -- add .0 so it does not round to whole number

        -- if it's in cubic feet
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('CUBIC FEET', 'CU FT')
        THEN AR_JANDIVERSIONVOLUME / 43560.0 -- add .0 so it does not round to whole number

        -- if it's something else, blank, or null
        ELSE NULL
    END AS volume_in_acre_feet
FROM WR_CALWATRS_FLATFILES.ANNUAL_REPORTS;

USE DATABASE SWRCB_INTERNAL_PROD;
USE SCHEMA WR_CALWATRS_FLATFILES;

WITH months AS (
    SELECT * FROM (
        VALUES
            (1,  'JAN'), (2,  'FEB'), (3,  'MAR'), (4,  'APR'),
            (5,  'MAY'), (6,  'JUN'), (7,  'JUL'), (8,  'AUG'),
            (9,  'SEP'), (10, 'OCT'), (11, 'NOV'), (12, 'DEC')
    ) AS m(month_num, month_abbr)
),
unpivoted AS (
    SELECT
        r.*,
        m.month_num,
        m.month_abbr,
        CASE m.month_abbr
            WHEN 'JAN' THEN r.AR_JANDIVERSIONVOLUME
            WHEN 'FEB' THEN r.AR_FEBDIVERSIONVOLUME
            WHEN 'MAR' THEN r.AR_MARDIVERSIONVOLUME
            WHEN 'APR' THEN r.AR_APRDIVERSIONVOLUME
            WHEN 'MAY' THEN r.AR_MAYDIVERSIONVOLUME
            WHEN 'JUN' THEN r.AR_JUNDIVERSIONVOLUME
            WHEN 'JUL' THEN r.AR_JULDIVERSIONVOLUME
            WHEN 'AUG' THEN r.AR_AUGDIVERSIONVOLUME
            WHEN 'SEP' THEN r.AR_SEPDIVERSIONVOLUME
            WHEN 'OCT' THEN r.AR_OCTDIVERSIONVOLUME
            WHEN 'NOV' THEN r.AR_NOVDIVERSIONVOLUME
            WHEN 'DEC' THEN r.AR_DECDIVERSIONVOLUME
        END AS raw_diversion_vol
    FROM ANNUAL_REPORTS r
    CROSS JOIN months m
)
SELECT
    month_num,
    month_abbr,
    AR_MONTHLYDIVERSIONUNIT AS original_unit,
    raw_diversion_vol AS original_volume,
    CASE
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('ACRE-FEET', 'AF', 'ACRE FEET')
            THEN raw_diversion_vol
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('GALLONS', 'GAL')
            THEN raw_diversion_vol / 325851.0
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('CUBIC FEET', 'CU FT')
            THEN raw_diversion_vol / 43560.0
    END AS diversion_volume_af
FROM unpivoted;

-- repeat above, but include the report year and water right id
WITH months AS (
    SELECT * FROM (
        VALUES
            (1,  'JAN'), (2,  'FEB'), (3,  'MAR'), (4,  'APR'),
            (5,  'MAY'), (6,  'JUN'), (7,  'JUL'), (8,  'AUG'),
            (9,  'SEP'), (10, 'OCT'), (11, 'NOV'), (12, 'DEC')
    ) AS m(month_num, month_abbr)
),
unpivoted AS (
    SELECT
        r.ar_waterrightid,
        r.ar_annualreportyear,
        r.ar_monthlydiversionunit,
        --r.*,
        m.month_num,
        m.month_abbr,
        CASE m.month_abbr
            WHEN 'JAN' THEN r.AR_JANDIVERSIONVOLUME
            WHEN 'FEB' THEN r.AR_FEBDIVERSIONVOLUME
            WHEN 'MAR' THEN r.AR_MARDIVERSIONVOLUME
            WHEN 'APR' THEN r.AR_APRDIVERSIONVOLUME
            WHEN 'MAY' THEN r.AR_MAYDIVERSIONVOLUME
            WHEN 'JUN' THEN r.AR_JUNDIVERSIONVOLUME
            WHEN 'JUL' THEN r.AR_JULDIVERSIONVOLUME
            WHEN 'AUG' THEN r.AR_AUGDIVERSIONVOLUME
            WHEN 'SEP' THEN r.AR_SEPDIVERSIONVOLUME
            WHEN 'OCT' THEN r.AR_OCTDIVERSIONVOLUME
            WHEN 'NOV' THEN r.AR_NOVDIVERSIONVOLUME
            WHEN 'DEC' THEN r.AR_DECDIVERSIONVOLUME
        END AS raw_diversion_vol
    FROM ANNUAL_REPORTS r
    CROSS JOIN months m
)
SELECT
    ar_waterrightid,
    ar_annualreportyear,
    month_num,
    month_abbr,
    AR_MONTHLYDIVERSIONUNIT AS original_unit,
    raw_diversion_vol AS original_volume,
    CASE
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('ACRE-FEET', 'AF', 'ACRE FEET')
            THEN raw_diversion_vol
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('GALLONS', 'GAL')
            THEN raw_diversion_vol / 325851.0
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('CUBIC FEET', 'CU FT')
            THEN raw_diversion_vol / 43560.0
    END AS diversion_volume_af
FROM unpivoted
ORDER BY ar_waterrightid asc, ar_annualreportyear asc, month_num asc;

-- with the following, save the results in a TEMP table
-- the results can be used in subsequent queries in the same snowflake session
CREATE OR REPLACE TEMP TABLE converted_diversions AS
WITH months AS (
    SELECT * FROM (
        VALUES
            (1,  'JAN'), (2,  'FEB'), (3,  'MAR'), (4,  'APR'),
            (5,  'MAY'), (6,  'JUN'), (7,  'JUL'), (8,  'AUG'),
            (9,  'SEP'), (10, 'OCT'), (11, 'NOV'), (12, 'DEC')
    ) AS m(month_num, month_abbr)
),
unpivoted AS (
    SELECT
        r.ar_waterrightid,
        r.ar_annualreportyear,
        r.ar_monthlydiversionunit,
        --r.*,
        m.month_num,
        m.month_abbr,
        CASE m.month_abbr
            WHEN 'JAN' THEN r.AR_JANDIVERSIONVOLUME
            WHEN 'FEB' THEN r.AR_FEBDIVERSIONVOLUME
            WHEN 'MAR' THEN r.AR_MARDIVERSIONVOLUME
            WHEN 'APR' THEN r.AR_APRDIVERSIONVOLUME
            WHEN 'MAY' THEN r.AR_MAYDIVERSIONVOLUME
            WHEN 'JUN' THEN r.AR_JUNDIVERSIONVOLUME
            WHEN 'JUL' THEN r.AR_JULDIVERSIONVOLUME
            WHEN 'AUG' THEN r.AR_AUGDIVERSIONVOLUME
            WHEN 'SEP' THEN r.AR_SEPDIVERSIONVOLUME
            WHEN 'OCT' THEN r.AR_OCTDIVERSIONVOLUME
            WHEN 'NOV' THEN r.AR_NOVDIVERSIONVOLUME
            WHEN 'DEC' THEN r.AR_DECDIVERSIONVOLUME
        END AS raw_diversion_vol
    FROM ANNUAL_REPORTS r
    CROSS JOIN months m
)
SELECT
    ar_waterrightid,
    ar_annualreportyear,
    month_num,
    month_abbr,
    AR_MONTHLYDIVERSIONUNIT AS original_unit,
    raw_diversion_vol AS original_volume,
    CASE
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('ACRE-FEET', 'AF', 'ACRE FEET')
            THEN raw_diversion_vol
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('GALLONS', 'GAL')
            THEN raw_diversion_vol / 325851.0
        WHEN UPPER(AR_MONTHLYDIVERSIONUNIT) IN ('CUBIC FEET', 'CU FT')
            THEN raw_diversion_vol / 43560.0
    END AS diversion_volume_af
FROM unpivoted
ORDER BY ar_waterrightid asc, ar_annualreportyear asc, month_num asc;

-- use the following to select the report IDs, months, and years when units were NULL
-- selection is made from the temporary table created in above query
SELECT
    ar_waterrightid,
    ar_annualreportyear,
    month_num,
    month_abbr,
    original_unit
FROM converted_diversions
WHERE original_unit IS NULL
ORDER BY ar_waterrightid ASC, ar_annualreportyear ASC, month_num ASC;
