import pandas as pd
import pandera.pandas as pa
from pandera.pandas import (Field)
from pandera.api.pandas.model_config import (BaseConfig)
from pandera.typing import (DataFrame, Series)
from pandera.errors import (SchemaError, SchemaErrors)
from pathlib import Path

# NOTE pd.Int64Dtype object is a nullable integer where as other int dtypes are not.
# TODO: Replace hard coded nested literals with injected objects/registries
class MasterControlTableSchema(pa.DataFrameModel):
    project_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)
    project_name: Series[pa.String] = Field(nullable=False)
    start_date: Series[pa.DateTime] = Field(nullable=False)
    end_date: Series[pa.DateTime] = Field(nullable=False)
    overwrite: Series[pa.Bool] = Field(nullable=False)
    project_control_file: Series[pa.String] = Field(nullable=False)

    class Config(BaseConfig):
        strict = True
        coerce = True

    @pa.dataframe_check
    def date_range_valid(cls, df: pd.DataFrame):
        """Validate start_date < end_date for all records."""
        return (df['start_date'] <= df['end_date']).all()

    @pa.dataframe_check
    def project_control_exists(cls, df: pd.DataFrame):
        """Validates that all project_control_files exist."""
        return df['project_control_file'].apply(
            lambda p: Path(p).exists()
        ).all()
    

class ProjectFlagsSchema(pa.DataFrameModel):
    flag: Series[pa.String] = Field(unique=True, nullable=False)
    value: Series[pa.Bool] = Field(unique=False, nullable=False)
    description: Series[pa.String] = Field(unique=False, nullable=True)

    class Config(BaseConfig):
        strict = True
        coerce = True

    @pa.dataframe_check
    def flag_in_registry(cls, df: pd.DataFrame):
        VALID = {"snow"}
        return df["flag"].isin(VALID).all()
    
class StorageRegistrySchema(pa.DataFrameModel):
    scope: Series[pa.String] = Field(unique=False, nullable=False)
    level: Series[pa.String] = Field(unique=False, nullable=False)
    source: Series[pa.String] = Field(unique=False, nullable=False)
    path: Series[pa.String] = Field(unique=False, nullable=False)
    absolute: Series[pa.Bool] = Field(unique=False, nullable=False)
    class Config(BaseConfig):
        strict = True
        coerce = True

    @pa.dataframe_check
    def correct_shape(cls, df: pd.DataFrame):
        return df.shape == (18,5)

    @pa.dataframe_check
    def scope_in_registry(cls, df: pd.DataFrame):
        VALID = {"shared","project"}
        return df["scope"].isin(VALID).all()

    @pa.dataframe_check
    def level_in_registry(cls, df: pd.DataFrame):
        VALID = {"root","raw","staged","candidate","curated"}
        return df["level"].isin(VALID).all()

    @pa.dataframe_check
    def source_in_registry(cls, df: pd.DataFrame):
        VALID = {"all","cimis","prism","nldas","gage","noaa","lcd","cdec","raws", "other","air","pre"}
        return df["source"].isin(VALID).all() # TODO: Expand for each data type of gage data
    
    @pa.dataframe_check
    def absolute_paths_exist(cls, df: pd.DataFrame):
        path_exists = df['path'].apply(lambda p: Path(p).exists())
        path_absolute = df['absolute']
        return path_exists[path_absolute].all()
    
    @pa.dataframe_check
    def required_pk_exist(cls, df: pd.DataFrame): # TODO: Validate that all the required records exist | these are the unique combos of level and source
        REQUIRED = {          
            ("shared", "root", "all"),
            ("shared", "raw", "prism"),
            ("shared", "raw", "nldas"),
            ("shared", "raw", "cimis"),
            ("project", "root", "all"),
            ("project", "raw", "noaa"),
            ("project", "raw", "cdec"),
            ("project", "raw", "lcd"),
            ("project", "raw", "raws"),
            ("project", "raw", "other"),
            ("project", "staged", "prism"),
            ("project", "staged", "cimis"),
            ("project", "staged", "nldas"),
            ("project", "candidate", "gage"),
            ("project", "candidate", "prism"),
            ("project", "staged", "gage"),
            ("project", "curated", "pre"),
            ("project", "curated", "air")
        }

        observed = set(df[["scope","level","source"]].itertuples(index=False, name=None))

        return observed == REQUIRED
    
    @pa.dataframe_check
    def root_paths_exist_and_absolute(cls, df: pd.DataFrame):
        """Validates that root directory for shared and project storage are defined and exist at specified paths"""

        REQUIRED = {
            ("shared", "root", "all", True),
            ("project", "root", "all", True)
        }

        observed = set(df[["scope","level", "source", "absolute"]].itertuples(index=False, name=None))

        return observed.issuperset(REQUIRED)

class TargetsSchema(pa.DataFrameModel):
    lat: Series[pa.Float] = Field(unique=False, nullable=False)
    lon: Series[pa.Float] = Field(unique=False, nullable=False)

    class Config(BaseConfig):
        strict = True
        coerce = True
    
    @pa.dataframe_check
    def unique_coordinates(cls, df: pd.DataFrame):
        return ~df.duplicated(subset=["lat","lon"]).any()

class PrismTargetsSchema(TargetsSchema):
    prism_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)

class CimisTargetsSchema(TargetsSchema):
    cimis_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)

class NldasTargetsSchema(TargetsSchema):
    nldas_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)

class GageTargetsSchema(TargetsSchema):
    gage_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)
    agency_id: Series[pa.String] = Field(unique=False, nullable=False)
    station_id: Series[pa.String] = Field(unique=True, nullable=False)
    elv_m: Series[pa.Float] = Field(unique=False, nullable=True)
    name: Series[pa.String] = Field(unique=False, nullable=False)
    start_date: Series[pa.DateTime] = Field(unique=False, nullable=False)
    end_date: Series[pa.DateTime] = Field(unique=False, nullable=True)
    resolution: Series[pa.String] = Field(unique=False, nullable=False)

    @pa.dataframe_check
    def agency_id_in_registry(cls, df: pd.DataFrame):
        """All agency_id (source) field values must be in the registry of avaiable sources."""
        VALID = {"noaa","lcd","raws","cdec","other"}
        return df["agency_id"].isin(VALID).all()
    
    @pa.dataframe_check
    def date_range_valid(cls, df: pd.DataFrame):
        """Validate start_date < end_date for all records, unless still active (empty end_date)"""
        
        inactive = ~df["end_date"].isna()
        return (df.loc[inactive, 'start_date'] <= df.loc[inactive, 'end_date']).all()

    @pa.dataframe_check
    def resolution_in_registry(cls, df: pd.DataFrame):
        """resolution must be either daily or hourly"""
        VALID = {"daily", "hourly"}
        return df["resolution"].isin(VALID).all()

class AirFileMappingSchema(pa.DataFrameModel):
    prism_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)
    cimis_id: Series[pd.Int64Dtype] = Field(unique=False, nullable=False)
    nldas_id: Series[pd.Int64Dtype] = Field(unique=False, nullable=False)
    
    class Config(BaseConfig):
        strict = True
        coerce = True

class PrismMappingSchema(pa.DataFrameModel):
    prism_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)
    nldas_id: Series[pd.Int64Dtype] = Field(unique=False, nullable=False)
    gage_id: Series[pd.Int64Dtype] = Field(unique=False, nullable=True)
    
    class Config(BaseConfig):
        strict = True
        coerce = True

class GageMappingSchema(pa.DataFrameModel):
    gage_id: Series[pd.Int64Dtype] = Field(unique=True, nullable=False)
    nldas_id: Series[pd.Int64Dtype] = Field(unique=False, nullable=False)
    noaa_100: Series[pa.Float] = Field(unique=False, nullable=False)
    
    class Config(BaseConfig):
        strict = True
        coerce = True

class GageRawDataSchema(pa.DataFrameModel):
    col0: Series[pa.DateTime] = Field(unique=True, nullable=False)
    col1: Series[pa.Float] = Field(unique=False, nullable=True)
    
    class Config(BaseConfig):
        strict = True
        coerce = True
