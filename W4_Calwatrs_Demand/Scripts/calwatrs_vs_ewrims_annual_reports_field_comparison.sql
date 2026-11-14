--PURPOSE: 
--This script will compare the annual report fields in ewrims vs calwatrs by using the Navarro water rights as the basis of comparison.
--The Navarro water rights list is derived from the Navarro MDT, which is stored in the WR_SUPPLY_AND_DEMAND schema in the 
--the SWRCB_INTERNAL_TEST environment. 

--Last Updated By: Payman Alemi on 11/13/2026

USE role swrcb_internal_test_wr_supply_and_demand_r_acrole;
Select * from SWRCB_INTERNAL_TEST.WR_SUPPLY_AND_DEMAND.SDA_DEMAND_MASTER_DEMAND_TABLE as MDT
Inner join swrcb_internal_test.wr_calwatrs_flatfiles.annual_reports AR on AR.WR_APP_CLAIM_NAME = MDT.APPLICATION_NUMBER;
--Confirmed that I can join across different schemas within SWRCB_Internal_Test on 11/13/2026

With AR As 
(Select * from swrcb_internal_test.wr_calwatrs_flatfiles.annual_reports AR)
Select top 10 * from SWRCB_INTERNAL_PROD.WR_EWRIMS.EWRIMS_FLAT_FILE_WATER_USE_REPORT_EXTENDED
--Confirmed with swrcb_internal_test_wr_supply_and_demand_r_acrole and secondary roles turned on, I can pull from WR_EWRIMS schema on Prod 11/13/2026


