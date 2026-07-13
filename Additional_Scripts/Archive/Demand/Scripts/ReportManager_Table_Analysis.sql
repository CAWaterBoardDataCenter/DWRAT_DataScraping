--Water Use Report Extended Flat File
Select top 100
APPLICATION_NUMBER,
YEAR,
MONTH,
AMOUNT,
DIVERSION_TYPE,
MAX_STORAGE,
WATER_RIGHT_TYPE,
SUB_TYPE
FROM ReportDB.FLAT_FILE.ewrims_water_use_report_extended
where SUB_TYPE like 'riparian%'


Select * from ReportDB.FLAT_FILE.ewrims_flat_file_pod
Select top 100 * from ReportDB.FLAT_FILE.ewrims_flat_file_pod
Select top 100 * from ReportDB.FLAT_FILE.ewrims_water_use_report
Select top 10 *  from ReportDb.FLAT_FILE.ewrims_flat_file_use_season
Select top 10 * from ReportDB.FLAT_FILE.ewrims_flat_file_party


--Extract all columns from water_use_report_extended
SELECT DISTINCT
    c.name 'Column Name',
    t.Name 'Data type',
    c.max_length 'Max Length',
    c.is_nullable,
    ISNULL(i.is_primary_key, 0) 'Primary Key'
FROM    
    sys.columns c
INNER JOIN 
    sys.types t ON c.user_type_id = t.user_type_id
LEFT OUTER JOIN 
    sys.index_columns ic ON ic.object_id = c.object_id AND ic.column_id = c.column_id
LEFT OUTER JOIN 
    sys.indexes i ON ic.object_id = i.object_id AND ic.index_id = i.index_id
WHERE
  c.object_id = OBJECT_ID('ReportDB.flat_file.ewrims_water_use_report_extended')
  ---AND c.name like '%year%'


  --Analyze all 177 fields in water_use_report_extended flat file
  Select top 100 *
  From ReportDB.flat_file.ewrims_water_use_report_extended

  --Extract all columns from ewrims_flat_file_annual_report
SELECT DISTINCT
    c.name 'Column Name',
    t.Name 'Data type',
    c.max_length 'Max Length',
    c.is_nullable,
    ISNULL(i.is_primary_key, 0) 'Primary Key'
FROM    
    sys.columns c
INNER JOIN 
    sys.types t ON c.user_type_id = t.user_type_id
LEFT OUTER JOIN 
    sys.index_columns ic ON ic.object_id = c.object_id AND ic.column_id = c.column_id
LEFT OUTER JOIN 
    sys.indexes i ON ic.object_id = i.object_id AND ic.index_id = i.index_id
WHERE
  c.object_id = OBJECT_ID('reportdb.FLAT_FILE.ewrims_flat_file_annual_report')
  AND c.name like '%year%'
 --Consists of 205 columns

 Select	Distinct
 A.Application_Number,
 A.Date_Submitted,
 A.Report_Year,
 B.Year

 From Reportdb.FLAT_FILE.ewrims_flat_file_annual_report as A
 Inner Join ReportDB.FLAT_FILE.ewrims_water_use_report_extended as B
	On A.APPLICATION_NUMBER =   B.Application_Number

---Filter for distinct water right types
Select distinct
WATER_RIGHT_TYPE
FROM ReportDB.FLAT_FILE.ewrims_water_use_report_extended

--What are federal claims?
Select distinct
application_number,
water_right_type
FROM ReportDB.FLAT_FILE.ewrims_water_use_report_extended 
where WATER_RIGHT_TYPE like '%federal%'


--Analyze all fields in ewrims_flat_use_season
SELECT DISTINCT
    c.name 'Column Name',
    t.Name 'Data type',
    c.max_length 'Max Length',
    c.is_nullable,
    ISNULL(i.is_primary_key, 0) 'Primary Key'
FROM    
    sys.columns c
INNER JOIN 
    sys.types t ON c.user_type_id = t.user_type_id
LEFT OUTER JOIN 
    sys.index_columns ic ON ic.object_id = c.object_id AND ic.column_id = c.column_id
LEFT OUTER JOIN 
    sys.indexes i ON ic.object_id = i.object_id AND ic.index_id = i.index_id
WHERE
  c.object_id = OBJECT_ID('reportdb.FLAT_FILE.ewrims_flat_file_use_season')


