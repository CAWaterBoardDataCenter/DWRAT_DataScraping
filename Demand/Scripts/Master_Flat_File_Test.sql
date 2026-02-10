SELECT DISTINCT
        pod.application_number,
        pod.certificate_id,
        pod.license_id,
        pod.permit_id,
        pod.water_right_status,
        pod.water_right_type,
        pod.wr_water_right_id,
		pod.WATERSHED,
		season.use_code,
		wure.AMOUNT,
		wure.APPLICATION_ACCEPTANCE_DATE,
	wure.APPLICATION_PRIMARY_OWNER,
	wure.APPLICATION_RECD_DATE,
	wure.DIVERSION_TYPE,
	wure.FACE_VALUE_AMOUNT,
	wure.FACE_VALUE_UNITS,
	wure.INI_REPORTED_DIV_AMOUNT,
	wure.INI_REPORTED_DIV_UNIT ,
	wure.MONTH,
	wure.PARTY_ID,
	wure.PRIMARY_OWNER_ENTITY_TYPE,
	wure.PRIORITY_DATE,
	wure.SUB_TYPE,
	wure.YEAR,
	wure.YEAR_DIVERSION_COMMENCED

    FROM reportdb.flat_file.ewrims_flat_file_pod as pod

INNER JOIN reportdb.flat_file.ewrims_flat_file_use_season as season
    ON pod.WR_WATER_RIGHT_ID = season.WR_WATER_RIGHT_ID
	
inner join reportdb.FLAT_FILE.ewrims_water_use_report_extended as wure
	on wure.WR_WATER_RIGHT_ID = season.WR_WATER_RIGHT_ID

where 
pod.watershed LIKE '%Russian%'
AND wure.DIVERSION_TYPE in ('DIRECT', 'STORAGE')
AND WURE.YEAR = '2024'
AND pod.APPLICATION_NUMBER in ('A001029', 'S023515')