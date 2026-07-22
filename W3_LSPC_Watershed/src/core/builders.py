from typing import Mapping, List, Tuple
from pathlib import Path
import pandas as pd


from .models import (MasterControlTable, RequestControl, ProjectControl,
    ProjectFlags, StorageRegistry,
    PrismTargets, CimisTargets, 
    NldasTargets, GageTargets,
    AirFileMapping, PrismMapping, GageMapping)


class ProjectControlBuilder:

    # def __init__(self, schemas: Mapping[str, SheetSchema]):
    #     self._schemas = schemas
    
    def build(self, request_control: RequestControl) -> ProjectControl:

        # Build ProjectFlags
        try:
            flags_df = pd.read_excel(request_control.project_control_file, sheet_name="Flags")
        except Exception as e:
            raise ValueError(f"")
        flags = ProjectFlags.from_df(flags_df)

        # Build Storage
        try:
            storage_df = pd.read_excel(request_control.project_control_file, sheet_name="Storage")
        except Exception as e:
            raise ValueError(f"")
        storage = StorageRegistry.from_df(storage_df)

        # Build Prism Targets
        try:
            prism_df = pd.read_excel(request_control.project_control_file, sheet_name="Prism")
        except Exception as e:
            raise ValueError(f"")
        prism = PrismTargets.from_df(prism_df)

        # Build Cimis Targets
        try:
            cimis_df = pd.read_excel(request_control.project_control_file, sheet_name="Cimis")
        except Exception as e:
            raise ValueError(f"")
        cimis = CimisTargets.from_df(cimis_df)

        # Build Nldas Targets
        try:
            nldas_df = pd.read_excel(request_control.project_control_file, sheet_name="Nldas")
        except Exception as e:
            raise ValueError(f"")
        nldas = NldasTargets.from_df(nldas_df)

        # Build Gage Targets
        try:
            gage_df = pd.read_excel(request_control.project_control_file, sheet_name="Gage")
        except Exception as e:
            raise ValueError(f"")
        gage = GageTargets.from_df(gage_df)

        # Build AirFileMapper
        try:
            airMap_df = pd.read_excel(request_control.project_control_file, sheet_name="AirFileMapping")
        except Exception as e:
            raise ValueError(f"")
        airMap = AirFileMapping.from_df(airMap_df)

        # Build PrismMapper
        try:
            prismMap_df = pd.read_excel(request_control.project_control_file, sheet_name="PrismMapping")
        except Exception as e:
            raise ValueError(f"")
        prismMap = PrismMapping.from_df(prismMap_df)

        # Build GageMapper
        try:
            gageMap_df = pd.read_excel(request_control.project_control_file, sheet_name="GageMapping")
        except Exception as e:
            raise ValueError(f"")
        gageMap = GageMapping.from_df(gageMap_df)

        # Cross validate AirMapper -> Cimis Targets, Nldas Targets
        self.cross_validate_airMap(airMap=airMap, prism=prism, cimis=cimis, nldas=nldas)

        # Cross validate PrismMapper -> Prism Targets, Nldas Targets, Gage Targets
        self.cross_validate_prismMap(prismMap=prismMap, prism=prism, nldas=nldas, gage=gage)

        # Cross validate GageMapper -> Gage Targets, Nldas Targets, Prism Targets
        self.cross_validate_gageMap(gageMap=gageMap, gage=gage, nldas=nldas)

        return ProjectControl(
            request_control=request_control,
            storage=storage,
            prism=prism,
            cimis=cimis,
            nldas=nldas,
            gage=gage,
            airMap=airMap,
            prismMap=prismMap,
            gageMap=gageMap
        )
    
    def cross_validate_airMap(self, airMap: AirFileMapping,
        prism: PrismTargets, cimis: CimisTargets, nldas: NldasTargets):
        """Raises Exceptions if airMap does not pass cross validation checks."""
        
        missing = {
            "prism_id": [],
            "cimis_id": [],
            "nldas_id": []
        }

        # check all prism_id's exist in PrismTargets 
        prism_id_exist = airMap.data['prism_id'].isin(prism.data['prism_id'])
        missing['prism_id'] +=\
            airMap.data.loc[~prism_id_exist,'prism_id'].tolist()

        # check all cimis_id's exist in CimisTargets
        cimis_id_exist = airMap.data['cimis_id'].isin(cimis.data['cimis_id'])
        missing['cimis_id'] +=\
            airMap.data.loc[~cimis_id_exist,'cimis_id'].tolist()

        # check all nldas_id's exist in NldasTargets
        nldas_id_exist = airMap.data['nldas_id'].isin(nldas.data['nldas_id'])
        missing['nldas_id'] +=\
            airMap.data.loc[~nldas_id_exist,'nldas_id'].tolist()

        valid = (prism_id_exist.all() * cimis_id_exist.all() * nldas_id_exist.all())

        if not valid:
            raise ValueError(
                "AirFileMapping is requesting resources that do not exist ...\n"
                f"\tprism_id not in Prism: {missing['prism_id']}\n" +
                f"\tcimis_id not in Cimis: {missing['cimis_id']}\n" +
                f"\tnldas_id not in Nldas: {missing['nldas_id']}\n"
            )

    def cross_validate_prismMap(self, prismMap: PrismMapping,
        prism: PrismTargets, nldas: NldasTargets, gage: GageTargets):
        """Raises Exceptions if prismMap does not pass cross validation checks."""
        
        missing = {
            "prism_id": [],
            "nldas_id": [],
            "gage_id": []
        }

        # check all prism_id's exist in PrismTargets 
        prism_id_exist = prismMap.data['prism_id'].isin(prism.data['prism_id'])
        missing['prism_id'] +=\
            prismMap.data.loc[~prism_id_exist,'prism_id'].tolist()

        # check all nldas_id's exist in NldasTargets
        nldas_id_exist = prismMap.data['nldas_id'].isin(nldas.data['nldas_id'])
        missing['nldas_id'] +=\
            prismMap.data.loc[~nldas_id_exist,'nldas_id'].tolist()

        # check all gage_id's exist in GageTargets
        gage_id_exist = prismMap.data['gage_id'].isin(gage.data['gage_id'])
        is_na = prismMap.data['gage_id'].isna()
        missing['gage_id'] +=\
            prismMap.data.loc[(~gage_id_exist)*(~is_na),'gage_id'].tolist()

        valid = (prism_id_exist.all() * nldas_id_exist.all() * gage_id_exist[(~is_na)].all())

        if not valid:
            raise ValueError(
                "PrismMapping is requesting resources that do not exist ...\n"
                f"\tprism_id not in Prism: {missing['prism_id']}\n" +
                f"\tnldas_id not in Nldas: {missing['nldas_id']}\n" +
                f"\tgage_id not in Gage: {missing['gage_id']}\n"
            )

    def cross_validate_gageMap(self, gageMap: GageMapping,
        gage: GageTargets, nldas: NldasTargets):
        """Raises Exceptions if gageMap does not pass cross validation checks."""
        
        missing = {
            "gage_id": [],
            "nldas_id": []
        }

        # check all gage_id's exist in GageTargets
        gage_id_exist = gageMap.data['gage_id'].isin(gage.data['gage_id'])
        missing['gage_id'] +=\
            gageMap.data.loc[~gage_id_exist,'gage_id'].tolist()
        
        # check all nldas_id's exist in NldasTargets
        nldas_id_exist = gageMap.data['nldas_id'].isin(nldas.data['nldas_id'])
        missing['nldas_id'] +=\
            gageMap.data.loc[~nldas_id_exist,'nldas_id'].tolist()

        valid = (gage_id_exist.all() * nldas_id_exist.all())

        if not valid:
            raise ValueError(
                "GageMapping is requesting resources that do not exist ...\n"
                f"\tgage_id not in Gage: {missing['gage_id']}\n" +
                f"\tnldas_id not in Nldas: {missing['nldas_id']}\n"
            )

class MasterControlBuilder:
    
    def __init__(self):
        self._projectbuilder = ProjectControlBuilder()

    def build_projects(self, master_control_file: Path) -> List[ProjectControl]:
        
        df = pd.read_excel(master_control_file, sheet_name="Control")
        master_control_table = MasterControlTable.from_df(df)

        project_controls: List[ProjectControl] = [] # list of project controls

        for record in master_control_table.data.to_dict("records"):
            request_control = RequestControl(
                project_id=record["project_id"],
                project_name = record["project_name"],
                start_date = record["start_date"],
                end_date = record["end_date"],
                overwrite = record["overwrite"],
                project_control_file= record["project_control_file"]
            )

            project_controls.append(
                self._projectbuilder.build(request_control=request_control)
            )

        return project_controls
    
    def build_storage_respository(self, project: ProjectControl) -> None:
        
        """Builds storage repository for a given project control."""
        # Prism diirectories
        project.storage.prism.raw.mkdir(parents=True, exist_ok=True)
        project.storage.prism.candidate.mkdir(parents=True, exist_ok=True)
        project.storage.prism.staged.mkdir(parents=True, exist_ok=True)
        
        # Cimis directories
        project.storage.cimis.raw.mkdir(parents=True, exist_ok=True)
        project.storage.cimis.staged.mkdir(parents=True, exist_ok=True)
        
        # Nldas directories
        project.storage.nldas.raw.mkdir(parents=True, exist_ok=True)
        project.storage.nldas.staged.mkdir(parents=True, exist_ok=True)
        
        # Gage directories
        agencies = ['noaa','cdec','lcd','raws','other']
        for agency in agencies:
            gage_storage_registry = getattr(project.storage.gage, agency)
            gage_storage_registry.raw.mkdir(parents=True, exist_ok=True)

        project.storage.gage.candidate.mkdir(parents=True, exist_ok=True)
        project.storage.gage.staged.mkdir(parents=True, exist_ok=True)

        # Curated (LSPC Files)
        project.storage.pre.curated.mkdir(parents=True, exist_ok=True)
        project.storage.air.curated.mkdir(parents=True, exist_ok=True)
        

        
        





