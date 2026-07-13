/****** Script for SelectTopNRows command from SSMS  ******/
SELECT
Application_Number
,[PARTY_ID]
,[PARTY_NAME]
,[ENTITY_TYPE]
,[LAST_NAME_OR_COMPANY_NAME]
,[MIDDLE_NAME]
,[FIRST_NAME]
,[PRIMARY_OWNER_NAME]
,[PRIMARY_OWNER_PARTY_ID]
,[PRIMARY_OWNER_ENTITY_TYPE]
  FROM [ReportDB].[FLAT_FILE].[ewrims_flat_file_party]