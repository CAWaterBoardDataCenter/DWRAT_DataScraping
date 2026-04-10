from typing import (Callable, TypeVar, List, Tuple, Any)
import xarray as xr
import pandas as pd
import numpy as np

from pathlib import Path

class CimisSpatialSubsetter:

    def extract_point_position_indexes(self, dataset: xr.Dataset, cimis_id: List[int]):
        """Reduces spatial dimensions of dataset from 2D (lat, lon) -> 1D (point [lat,lon])
        and extracts the positional indicies of the 1D spatial point array that correspond
        to the request cimis_ids in `cimis_ids`
        """
        ids = np.asarray(cimis_id, dtype=np.int64)
        ds_points = dataset.stack(point=("lat", "lon"))
        mask = ds_points["cimis_id"].isin(ids)

        points_pos_id = np.flatnonzero(mask.values)  # integer positions in stacked point dim
        points_cimis_id = ds_points["cimis_id"].isel(point=points_pos_id).values.astype(np.int64)

        # optional validation: ensure all requested IDs were found
        found = np.unique(points_cimis_id)
        missing = np.setdiff1d(np.unique(ids), found)
        if missing.size:
            raise KeyError(f"{missing.size} requested cimis_id(s) not found. Example: {missing[:10].tolist()}")

        return points_pos_id.astype(np.int64)

    def create_subset_preprocess(self, cimis_netcdf_file: Path, cimis_ids: List[int]) -> Callable[[xr.Dataset], xr.Dataset]:
        ds = xr.load_dataset(cimis_netcdf_file)
        points_pos_id = self.extract_point_position_indexes(dataset=ds, cimis_id=cimis_ids)
        
        def subset_preprocess(ds: xr.Dataset) -> xr.Dataset:
            ds_points = ds.stack(point=("lat", "lon"))
            ds_points_subset = ds_points.isel(point=points_pos_id)
            lat_da = ds_points_subset['lat']
            lon_da = ds_points_subset['lon']
            ds_points_subset = ds_points_subset.reset_index("point", drop=True)
            ds_points_subset = ds_points_subset.assign_coords(
                {
                     # 'point': ('point', np.arange(ds_points_subset.sizes['point'])),
                     'point': ds_points_subset['cimis_id'],
                    'lat': lat_da,
                    'lon': lon_da,
                })

            # promote variables `cimis_id` to coordinates
            # ds_points_subset = ds_points_subset.set_coords(["cimis_id"])
            ds_points_subset = ds_points_subset.drop_vars("cimis_id")
            ds_points_subset = ds_points_subset.rename({'point':'cimis_id'})
            
            return ds_points_subset
        
        return subset_preprocess

class CimisVariableTransformer:

    def __init__(self):
        self.convert = CimisVariableConverter()
        # self.calculate = CimisVariableCalculator()

class CimisVariableConverter:

    def eto_mm_to_in(self, ds: xr.Dataset) -> xr.Dataset:
        """Converts Cimis potential evapotranspiration data (Eto)
        stored in xr.Dataset from [mm] -> [in] for Lspc.
        """
        eto = ds['Eto'] / 25.4
        return ds.assign(Eto=eto)
