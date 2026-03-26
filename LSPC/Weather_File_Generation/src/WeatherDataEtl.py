# External imports
from typing import List, Tuple, Any
from pathlib import Path
from dotenv import load_dotenv
load_dotenv() # load environment variables

# Package imports
from .etl.Runners import WeatherDataEtlRunner
from .core import MasterControlBuilder

class WeatherDataEtl:
    def __init__(self):
        self._builder = MasterControlBuilder()
        self._runner = WeatherDataEtlRunner()
    
    @property
    def builder(self) -> MasterControlBuilder:
        return self._builder
    
    @property
    def run(self) -> WeatherDataEtlRunner:
        return self._runner

        




        

        
