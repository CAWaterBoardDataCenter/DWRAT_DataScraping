--Columns from ANNUAL_REPORTS Table
SELECT column_name
FROM information_schema.columns
WHERE table_schema = 'WR_CALWATRS_FLATFILES'
  AND table_name = 'ANNUAL_REPORTS'
  And column_name ilike '%USE%'
ORDER BY column_name ASC;

--Columns from Geospatial_Relationships Table
SELECT column_name
FROM information_schema.columns
WHERE table_schema = 'WR_CALWATRS_FLATFILES'
AND table_name = 'GEOSPATIAL_RELATIONSHIPS'
ORDER by column_name ASC;
--Returns 122 rows