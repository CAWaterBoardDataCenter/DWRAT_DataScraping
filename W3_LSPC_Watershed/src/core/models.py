""" Data models used throughout package """
from typing import List, Tuple, Any
from pathlib import Path
from dataclasses import dataclass
from datetime import date
from enum import Enum
import pandas as pd
from datetime import datetime

from pandera.typing import DataFrame
from .schemas import (MasterControlTableSchema,
    ProjectFlagsSchema, StorageRegistrySchema,
    PrismTargetsSchema, CimisTargetsSchema,
    NldasTargetsSchema, GageTargetsSchema,
    AirFileMappingSchema, PrismMappingSchema,
    GageMappingSchema
)

# TODO: Decide if we store as pandas dataframe after validating for eaase of use (for all of these)
    # Can you type check a pandas dataframe schema for passing and type checking?

# TODO: Current idea: have a class method that initializes the classes from a dataframe like seen below with (from_dict)

@dataclass(frozen=True)
class ProjectFlags:
    
    data: DataFrame[ProjectFlagsSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "ProjectFlags":
        validated = ProjectFlagsSchema.validate(df, lazy=True)
        return cls(validated)

@dataclass(frozen=True)
class RawRegistry:
    raw: Path

@dataclass(frozen=True)
class RawStagedRegistry:
    raw: Path
    staged: Path

@dataclass(frozen=True)
class RawCandidateStagedRegistry:
    raw: Path
    candidate: Path
    staged: Path

@dataclass(frozen=True)
class CuratedRegistry:
    curated: Path

@dataclass(frozen=True)
class GageStorageRegistry:
    noaa: RawRegistry
    lcd: RawRegistry
    raws: RawRegistry
    cdec: RawRegistry
    candidate: Path
    staged: Path

@dataclass(frozen=True)
class StorageRegistry: # TODO: Handle relative path difference for Jupyter Notebooks and main.py for testing
    prism: RawCandidateStagedRegistry
    cimis: RawStagedRegistry
    nldas: RawStagedRegistry
    gage: GageStorageRegistry
    pre: CuratedRegistry
    air: CuratedRegistry

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "StorageRegistry":
        validated = StorageRegistrySchema.validate(df, lazy=True)

        # base roots (schema ensures these exist and are absolute)
        shared_root = Path(
            validated.loc[
                (validated["scope"] == "shared") &
                (validated["level"] == "root") &
                (validated["source"] == "all"),
                "path",
            ].values[0]
        )
        project_root = Path(
            validated.loc[
                (validated["scope"] == "project") &
                (validated["level"] == "root") &
                (validated["source"] == "all"),
                "path",
            ].values[0]
        )

        def resolve_path(scope: str, path_str: str, absolute: bool) -> Path:
            if absolute:
                return Path(path_str)
            return (shared_root if scope == "shared" else project_root) / path_str

        paths: dict = {}
        for _, row in validated.iterrows():
            src = row["source"]
            lvl = row["level"]
            paths.setdefault(src, {})[lvl] = resolve_path(row["scope"], row["path"], row["absolute"])

        return cls(
            prism=RawCandidateStagedRegistry(
                raw=paths["prism"]["raw"],
                staged=paths["prism"]["staged"],
                candidate=paths["prism"]["candidate"]
            ),
            cimis=RawStagedRegistry(raw=paths["cimis"]["raw"], staged=paths["cimis"]["staged"]),
            nldas=RawStagedRegistry(raw=paths["nldas"]["raw"], staged=paths["nldas"]["staged"]),
            gage=GageStorageRegistry(
                noaa=RawRegistry(raw=paths["noaa"]["raw"]),
                lcd=RawRegistry(raw=paths["lcd"]["raw"]),
                raws=RawRegistry(raw=paths["raws"]["raw"]),
                cdec=RawRegistry(raw=paths["cdec"]["raw"],),
                candidate=paths["gage"]["candidate"],
                staged=paths["gage"]["staged"]
            ),
            pre=CuratedRegistry(curated=paths["pre"]["curated"]),
            air=CuratedRegistry(curated=paths["air"]["curated"]),
        )

@dataclass(frozen=True)
class PrismTargets:
    
    data: DataFrame[PrismTargetsSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "PrismTargets":
        validated = PrismTargetsSchema.validate(df, lazy=True)
        return cls(validated)

@dataclass(frozen=True)
class CimisTargets:
    
    data: DataFrame[CimisTargetsSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "CimisTargets":
        validated = CimisTargetsSchema.validate(df, lazy=True)
        return cls(validated)

@dataclass(frozen=True)
class NldasTargets:
    
    data: DataFrame[NldasTargetsSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "NldasTargets":
        validated = NldasTargetsSchema.validate(df, lazy=True)
        return cls(validated)

@dataclass(frozen=True)
class GageTargets:
    
    data: DataFrame[GageTargetsSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "GageTargets":
        validated = GageTargetsSchema.validate(df, lazy=True)
        return cls(validated)

@dataclass(frozen=True)
class AirFileMapping:

    data: DataFrame[AirFileMappingSchema]
    
    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "AirFileMapping":
        validated = AirFileMappingSchema.validate(df, lazy=True)
        return AirFileMapping(validated)

@dataclass(frozen=True)
class PrismMapping:

    data: DataFrame[PrismMappingSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "PrismMapping":
        validated = PrismMappingSchema.validate(df, lazy=True)
        return PrismMapping(validated)

@dataclass(frozen=True)
class GageMapping:

    data: DataFrame[GageMappingSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "GageMapping":
        validated = GageMappingSchema.validate(df, lazy=True)
        return GageMapping(validated)

@dataclass(frozen=True)
class RequestControl: # TODO: Remove this, it is unecessary
    project_id: int
    project_name: str
    start_date: date
    end_date: date
    overwrite: bool
    project_control_file: Path

@dataclass(frozen=True)
class ProjectControl:
    request_control: RequestControl
    storage: StorageRegistry
    prism: PrismTargets
    cimis: CimisTargets
    nldas: NldasTargets
    gage: GageTargets
    airMap: AirFileMapping
    prismMap: PrismMapping
    gageMap: GageMapping

@dataclass(frozen=True)
class MasterControlTable:

    data: DataFrame[MasterControlTableSchema]

    @classmethod
    def from_df(cls, df: pd.DataFrame) -> "MasterControlTable":
        validated = MasterControlTableSchema.validate(df, lazy=True)
        return cls(validated)
    
@dataclass(frozen=True)
class DataRequest:
    start_datetime: datetime # NOTE: Assumes Pacific Time Zone (America/Los_Angeles)
    end_datetime: datetime   # NOTE: Assumes Pacific Time Zone (America/Los_Angeles)
    overwrite: bool
    output_dir: Path