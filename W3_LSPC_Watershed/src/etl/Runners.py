from typing import List, Tuple
from src.core.models import (ProjectControl)
from src.etl.qa.ObsDataQC import run_obs_data_qc
from src.etl.qa.QC_Remake_Timeseries_Automation import qc_remake_timeseries_automation
from .Orchestrators import (FetchingOrchestrator, StagingOrchestrator, WritingOrchestrator)

class WeatherDataEtlRunner:
    """Concrete runner with all available ETL workflows."""
    
    def __init__(self):
        self.fetch = FetchingOrchestrator()
        self.stage = StagingOrchestrator()
        self.write = WritingOrchestrator()
    
    ### WeatherDataETL Specific Runners ###
    def fetch_weather_data(self, project: ProjectControl):
        """Runs all extractors"""
        print("Running `fetch_weather_data`")
        self.fetch.prism(project)
        self.fetch.cimis(project)
        self.fetch.nldas(project)
        self.fetch.gage.noaa(project)
        self.fetch.gage.lcd(project)
        self.fetch.gage.cdec(project)
        self.fetch.gage.raws(project)
    
    def stage_weather_data(self, project: ProjectControl):
        print("Running `stage_weather_data`")
        self.stage.nldas(project)
        self.stage.prism(project)
        self.stage.cimis(project)
        # self.stage.gage(project) # hold off until after manual QAQC
    
    def write_lspc_files(self, project: ProjectControl):
        print("Running `write_lspc_files`")
        self.write.pre(project)
        self.write.air(project)
    
    def qc_gage_data(self, project: ProjectControl):
        """Runs QA/QC on observed gage data"""
        print("Running `qc_gage_data`")
        run_obs_data_qc(project)

    def qc_remake_timeseries_automation(self, project: ProjectControl):
        """Runs QA/QC on observed gage data"""
        print("Running `qc_remake_timeseries_automation`")
        qc_remake_timeseries_automation(project)