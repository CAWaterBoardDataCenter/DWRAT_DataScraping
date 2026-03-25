from abc import ABC
from typing import Any, List, Tuple
from ..core.models import (RequestControl, ProjectControl)

class DataFetcher(ABC):

    def create_fetch_requests(self, project: ProjectControl) -> Any:
        print("\tProcessing project control data for data fetcher.")
    
    def fetch(self, project: ProjectControl):
        requests = self.create_fetch_requests(project)
        print("\tFetching data.")

class DataStager(ABC):

    def create_stage_requests(self, project: ProjectControl) -> Any:
        print("\tTranslating project control data for data stager.")
    
    def stage(self, project: ProjectControl):
        requests = self.create_stage_requests(project)
        print("\tStaging Data.")

class DataWriter(ABC):

    def create_write_requests(self, project: ProjectControl) -> Any:
        print("\tTranslating project control data for data writer.")
    
    def write(self, project: ProjectControl):
        requests = self.create_write_requests(project)
        print("\tWriting Data.")