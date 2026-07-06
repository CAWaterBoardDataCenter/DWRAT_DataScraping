from pathlib import Path
from typing import Optional, Tuple, Union

import numpy as np
import pandas as pd
import xarray as xr


class CimisTransformer:
    """
    Transformer class for CIMIS data processing pipeline.

    Handles transformation of raw CIMIS raster time series into analysis-ready
    NetCDF files through a series of quality control and gap-filling steps:
        1. Express no-data values as NaN
        2. Pad time dimension for missing dates
        3. Apply spatial boundary mask
        4. Fill remaining NaN with no-data value
        5. Export to NetCDF

    Attributes:
        data_array (xr.DataArray): Main data container with time/lat/lon coordinates
        crs (str): Coordinate reference system (e.g., 'EPSG:3310')
        no_data_value (float|int): Value representing missing/invalid data
    """

    def __init__(self, data: np.ndarray, time: np.ndarray, lat: np.ndarray,
                 lon: np.ndarray, dims: Tuple[str, str, str], crs: str):
        """
        Initialize transformer with data and coordinates.

        Creates an xarray DataArray from numpy arrays with proper coordinate
        labeling for time series analysis and NetCDF export.

        Args:
            data: 3D numpy array with shape
            time: 1D array of pandas Timestamps for temporal dimension
            lat: 1D array of latitude coordinates
            lon: 1D array of longitude coordinates
            dims: Tuple of dimension names, typically
            crs: Coordinate reference system string
        """
        self.data_array = xr.DataArray(
            data=data,
            dims=dims,
            coords={
                'time': time,
                'lat': lat,
                'lon': lon
            },
            name='Eto'
        )
        self.crs = crs
        self.no_data_value = None

        if crs:
            self.data_array.attrs['crs'] = crs

    def _express_no_data_values(self, no_data_value: Union[float, int] = -9999):
        """
        Replace no-data values with NaN for processing.

        Args:
            no_data_value: Sentinel value representing missing/invalid data
        """
        self.no_data_value = no_data_value
        self.data_array = self.data_array.where(self.data_array != no_data_value)

    def apply_mask(self, mask: xr.DataArray) -> None:
        """
        Apply spatial boundary mask to data.

        Args:
            mask: 2D boolean DataArray with dims (lat, lon)
        """
        self.data_array = self.data_array.where(mask)

    def pad_on_time_dim(self, start_date: pd.Timestamp, end_date: pd.Timestamp) -> None:
        """
        Pad time dimension to include all dates in range.

        Args:
            start_date: First date in desired time range
            end_date: Last date in desired time range
        """
        full_range = pd.date_range(start=start_date, end=end_date, freq='D')
        self.data_array = self.data_array.reindex(time=full_range)

    def define_no_data_value(self, no_data_value: Union[float, int] = -9999) -> None:
        """
        Convert remaining NaN back to numeric no-data value.

        Args:
            no_data_value: Sentinel value for missing/invalid data in NetCDF
        """
        self.data_array = self.data_array.fillna(no_data_value)
        self.data_array.attrs['nodata'] = no_data_value
        self.data_array.encoding["_FillValue"] = no_data_value

    def add_spatial_indexes(self, da: xr.DataArray) -> xr.Dataset:
        """
        Assigns a unique spatial index (cimis_id) for each unique lat/lon coordinate. spatial indexes are
            assigned by counting row-wise from the NW Corner to the SE corner of the coordinate
            grid created by lat/lon.

        Args:
            da: data array to assign spatial indexes to

        Returns:
            dataset with spatial indexes included.
        """

        n_ax0 = da.sizes['lat']
        n_ax1 = da.sizes['lon']
        idx_da = xr.DataArray(
            np.arange(0,n_ax0*n_ax1).reshape((n_ax0,n_ax1)),
            dims=('lat','lon'),
            coords={'lat': da['lat'], 'lon': da['lon']},
            name='cimis_id'
        )
        
        return xr.Dataset({'Eto': da,'cimis_id': idx_da})


    def to_dataset(self) -> xr.Dataset:
        """Convert DataArray to xarray Dataset."""
        return self.data_array.to_dataset()

    def to_netcdf(self, dataset: Union[xr.DataArray, xr.Dataset], filepath: Path) -> None:
        """
        Export DataDataset to NetCDF file.

        Args:
            filepath: Output path for NetCDF file
        """
        dataset.to_netcdf(filepath)

    def run(self, outfilepath: Path, startDate: pd.Timestamp, endDate: pd.Timestamp,
            no_data_value: Union[float, int] = -9999,
            boundary_mask: Optional[xr.DataArray] = None) -> None:
        """
        Run complete transformation pipeline and save to NetCDF.

        Pipeline steps:
            1. Express no-data values as NaN
            2. Pad time dimension for missing dates
            3. Apply boundary mask (if provided)
            4. Define no-data value for NetCDF
            5. Add spatial index grid (0-N)
            6. Export to NetCDF file

        Args:
            outfilepath: Destination path for NetCDF file
            startDate: First date in time series
            endDate: Last date in time series
            no_data_value: Sentinel value for missing/invalid data (default: -9999)
            boundary_mask: Optional 2D boolean mask (lat, lon) for spatial bounds
        """
        self._express_no_data_values(no_data_value)

        self.pad_on_time_dim(startDate, endDate)

        if boundary_mask is not None:
            self.apply_mask(boundary_mask)

        self.define_no_data_value(no_data_value)
        
        ds = self.add_spatial_indexes(self.data_array)

        self.to_netcdf(ds, filepath=outfilepath)
        print(f"Saved transformed NetCDF: {outfilepath}")