-- LAST UPDATED BY: Payman Alemi on 10/8/2026

/* 1st Common Table Expression (CTE) calculates the annual direct diversion values for all water rights */
With Direct_Total As (
Select
APPL_ID,
YEAR,
SUM(
	CASE WHEN DIVERSION_TYPE = 'DIRECT' THEN TRY_CAST (AMOUNT AS DECIMAL(18,2)) ELSE 0 END
	) As Direct
From ReportDB.FLAT_FILE.ewrims_water_use_report
--WHERE APPL_ID IN ('A019351', 'A025691') AND YEAR BETWEEN 2017 and 2024
GROUP BY APPL_ID, YEAR
)

/* 2nd CTE calculates the annual storage diversion values for all water rights */
, Storage_total AS (
SELECT
APPL_ID,
YEAR,
SUM(
	CASE WHEN DIVERSION_TYPE = 'STORAGE' THEN TRY_CAST (AMOUNT AS DECIMAL(18,2)) ELSE 0 END
	) as Storage
From ReportDB.FLAT_FILE.ewrims_water_use_report
---WHERE APPL_ID IN ('A019351', 'A025691') AND YEAR BETWEEN 2017 and 2024
GROUP BY APPL_ID, YEAR
)

/*3rd CTE appends the direct_total and storage_total CTEs and sums the annual storage and direct diversion
to obtain the total annual diversion values. */
SELECT
Storage_total.APPL_ID,
Storage_Total.YEAR,
Storage_total.Storage,
Direct_Total.Direct,
Direct_Total.Direct + Storage_Total.Storage as Diversion_Total
From Storage_total
Left Join Direct_Total ON Direct_Total.APPL_ID = Storage_total.APPL_ID AND
						  Direct_Total.YEAR = Storage_total.YEAR
Order by APPL_ID, YEAR


/* 4th CTE calculates the monthly direct diversion values for all water rights */
With Direct_Monthly As (
Select
APPL_ID,
YEAR,
TRY_CAST(Month AS Int) as MONTH,
SUM(
	CASE WHEN DIVERSION_TYPE = 'DIRECT' THEN TRY_CAST (AMOUNT AS DECIMAL(18,2)) ELSE 0 END
	) As Direct
From ReportDB.FLAT_FILE.ewrims_water_use_report
--WHERE APPL_ID IN ('A019351', 'A025691') AND YEAR BETWEEN 2017 and 2024
GROUP BY APPL_ID, YEAR, Month
--Order by APPL_ID,YEAR, MONTH
)

/* 5th CTE calculates the monthly storage values from 2017 to 2024 for 
water rights A019351 and A025691 */
, Storage_Monthly AS (
SELECT
APPL_ID,
YEAR,
TRY_CAST(Month AS Int) as MONTH,
SUM(
	CASE WHEN DIVERSION_TYPE = 'STORAGE' THEN TRY_CAST (AMOUNT AS DECIMAL(18,2)) ELSE 0 END
	) as Storage
From ReportDB.FLAT_FILE.ewrims_water_use_report
--WHERE APPL_ID IN ('A019351', 'A025691') AND YEAR BETWEEN 2017 and 2024
GROUP BY APPL_ID, YEAR, Month
--Order by APPL_ID,YEAR, MONTH
)
/* 6th CTE appends the direct_monthly and storage_monthly CTES and sums the monthly storage and direct diversion values
for all water rights */

SELECT
Storage_Monthly.APPL_ID,
Storage_Monthly.YEAR,
Storage_Monthly.MONTH,
Storage_Monthly.Storage,
Direct_Monthly.Direct,
Direct_Monthly.Direct + Storage_Monthly.Storage as Diversion_Total
From Storage_Monthly
Left Join Direct_Monthly ON Direct_Monthly.APPL_ID = Storage_Monthly.APPL_ID AND
						    Direct_Monthly.YEAR = Storage_Monthly.YEAR AND
						    Direct_Monthly.MONTH = Storage_Monthly.MONTH
Order by APPL_ID, YEAR, MONTH

--Returns 5,395,251 rows in 53 s on 10/8/2026