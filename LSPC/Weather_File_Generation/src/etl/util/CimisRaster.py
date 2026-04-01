from pathlib import Path
from typing import Union
from rasterio.warp import reproject
from rasterio.enums import Resampling
import rasterio

import numpy as np
import xarray as xr


class CimisRaster:
    """
    Raster data handler for CIMIS ASC files with geospatial capabilities.

    Attributes:
        data (np.ndarray): 2D raster data array
        filepath (Path): Source file path
        width (int): Number of columns
        height (int): Number of rows
        dtype (np.dtype): Data type of raster values
        transform (Affine): Affine transformation matrix for georeferencing
        crs (rasterio.crs.CRS): Coordinate reference system
        nodata (float|int): No-data sentinel value
        bounds (BoundingBox): Spatial extent
    """
    def __init__(self):
        """
        Initialize empty CimisRaster.
        """
        self.data = None
        self.filepath = None
        self.width = None
        self.height = None
        self.dtype = None
        self.transform = None
        self.crs = None
        self.nodata = None
        self.bounds = None

    @classmethod
    def from_file(cls, filepath: Path):
        """
        Create CimisRaster from ASC file using rasterio.

        Args:
            filepath: Path to .asc raster file

        Returns:
            CimisRaster: Instance with loaded data and metadata
        """
        instance = cls()
        instance.filepath = filepath
        instance._load_from_rasterio()
        return instance

    @classmethod
    def from_array(cls, data: np.ndarray, **kwargs):
        """
        Create CimisRaster from numpy array with metadata.

        Args:
            data: 2D numpy array of raster values
            **kwargs: Metadata attributes

        Returns:
            CimisRaster: Instance with provided data and metadata
        """
        instance = cls()
        instance.data = data
        for attr, value in kwargs.items():
            setattr(instance, attr, value)
        return instance

    def __repr__(self):
        """
        String representation of CimisRaster.

        Returns:
            str: Summary of raster dimensions and CRS
        """
        return (f"CimisRaster(width={self.width}, height={self.height}, "
                f"dtype={self.dtype}, crs={self.crs})")

    def _load_from_rasterio(self):
        """Load raster data and metadata using rasterio."""
        with rasterio.open(self.filepath, 'r') as src:
            self.data = src.read(1)

            profile = src.profile
            self.width = profile['width']
            self.height = profile['height']
            self.dtype = profile['dtype']
            self.transform = profile['transform']
            self.nodata = profile.get('nodata', -9999)
            self.bounds = src.bounds

    def set_crs(self, crs_epsg_code: int = 3310):
        """
        Set coordinate reference system from EPSG code.

        Args:
            crs_epsg_code: EPSG code for CRS
        """
        from rasterio.crs import CRS
        self.crs = CRS.from_epsg(crs_epsg_code)

    def resample(self, template: "CimisRaster"):
        """
        Resample this raster to match template raster schema.

        Args:
            template: CimisRaster defining target spatial schema

        Raises:
            ValueError: If source or template CRS not set
        """
        if not self.crs:
            raise ValueError("Source CRS not set. Call set_crs() first.")
        if not template.crs:
            raise ValueError("Template CRS not set.")

        resampled_data = np.empty((template.height, template.width), dtype=self.dtype)

        reproject(
            source=self.data,
            destination=resampled_data,
            src_transform=self.transform,
            src_crs=self.crs,
            src_nodata=self.nodata,
            dst_transform=template.transform,
            dst_crs=template.crs,
            dst_nodata=template.nodata,
            resampling=Resampling.average
        )

        self.data = resampled_data
        self.transform = template.transform
        self.width = template.width
        self.height = template.height

    def lat(self):
        """
        Get latitude coordinates for raster rows.

        Returns:
            np.ndarray: 1D array of latitude coordinates (length = height)

        Raises:
            ValueError: If transform or height not set
        """
        if not self.transform or not self.height:
            raise ValueError("Transform and height required")

        from rasterio.transform import xy

        rows = np.arange(self.height)
        cols = np.zeros(self.height)

        _, y_coords = xy(self.transform, rows, cols, offset='center')

        return np.array(y_coords)

    def lon(self):
        """
        Get longitude coordinates for raster columns.

        Returns:
            np.ndarray: 1D array of longitude coordinates (length = width)

        Raises:
            ValueError: If transform or width not set
        """
        if not self.transform or not self.width:
            raise ValueError("Transform and width required")

        from rasterio.transform import xy

        rows = np.zeros(self.width)
        cols = np.arange(self.width)

        x_coords, _ = xy(self.transform, rows, cols, offset='center')

        return np.array(x_coords)