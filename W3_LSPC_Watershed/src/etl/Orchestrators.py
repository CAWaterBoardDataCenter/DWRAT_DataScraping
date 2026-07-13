from ..core.models import (ProjectControl)
from .fetch import (FetchPrism, FetchCimis, 
    FetchNldas, FetchNoaa, FetchLcd, FetchCdec, FetchRaws)
from .stage import (StagePrism, StageCimis, StageNldas, StageGage)
from .write import (WriteAirFiles, WritePreFiles)

class GageFetchingOrchestrator:
    
    def __init__(self):
        self._noaa = FetchNoaa()
        self._lcd = FetchLcd()
        self._cdec = FetchCdec()
        self._raws = FetchRaws()
    
    def noaa(self, project: ProjectControl) -> None:
        return self._noaa.fetch(project)
    
    def lcd(self, project: ProjectControl) -> None:
        return self._lcd.fetch(project)
    
    def cdec(self, project: ProjectControl) -> None:
        return self._cdec.fetch(project)
    
    def raws(self, project: ProjectControl) -> None:
        return self._raws.fetch(project)

class FetchingOrchestrator:
    
    def __init__(self):
        self._prism = FetchPrism()
        self._cimis = FetchCimis()
        self._nldas = FetchNldas()
        self._gage = GageFetchingOrchestrator()

    def prism(self, project: ProjectControl) -> None:
        return self._prism.fetch(project)
    
    def cimis(self, project: ProjectControl) -> None:
        return self._cimis.fetch(project)
    
    def nldas(self, project: ProjectControl) -> None:
        return self._nldas.fetch(project)
    
    @property
    def gage(self) -> GageFetchingOrchestrator:
        return self._gage

class StagingOrchestrator:
    
    def __init__(self):
        self._prism = StagePrism()
        self._cimis = StageCimis()
        self._nldas = StageNldas()
        self._gage = StageGage()

    def prism(self, project: ProjectControl) -> None:
        return self._prism.stage(project)
    
    def cimis(self, project: ProjectControl) -> None:
        return self._cimis.stage(project)
    
    def nldas(self, project: ProjectControl) -> None:
        return self._nldas.stage(project)
    
    def gage(self, project: ProjectControl) -> None:
        return self._gage.stage(project)

class WritingOrchestrator:
    
    def __init__(self):
        self._air = WriteAirFiles()
        self._pre = WritePreFiles()

    def air(self, project: ProjectControl) -> None:
        return self._air.write(project)

    def pre(self, project: ProjectControl) -> None:
        return self._pre.write(project)