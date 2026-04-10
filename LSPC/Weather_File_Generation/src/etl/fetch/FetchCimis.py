import gzip
import shutil
from datetime import datetime, timedelta
from pathlib import Path
from time import sleep
from typing import List, Tuple, Optional, Union
from dataclasses import dataclass
from zoneinfo import ZoneInfo

import pandas as pd
import numpy as np
import xarray as xr
import requests
from tqdm import tqdm
from dask import delayed, compute
from dask.diagnostics import ProgressBar

import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

from src.etl.util.CimisRaster import CimisRaster
from src.etl.util.CimisTransformer import CimisTransformer
from src.etl.util.helpers import FetchCimisHelpers

from ..base import DataFetcher
from src.core import ProjectControl
from src.core.models import DataRequest

@dataclass(frozen=True)
class FetchCimisRequest(DataRequest):
    """
    Extends the base DataRequest with CIMIS-specific parameters for:
    - Template raster path for spatial resampling
    - Parallel processing settings
    - Coordinate reference system configuration
    """
    template_raster_path: Optional[Path] = None
    enable_resampling: bool = True
    enable_gap_filling: bool = True
    boundary_mask_path: Optional[Path] = None
    num_workers: int = 1
    crs_epsg: int = 3310
    enable_checkpointing: bool = False


def compare_raster_schema(raster1: CimisRaster, raster2: CimisRaster) -> bool:
    """
    Compare if two rasters have identical spatial schemas.

    Checks that two CimisRaster objects share the same:
    - Dimensions (width and height)
    - Affine transformation (geolocation)
    - Coordinate reference system

    Args:
        raster1: First raster to compare
        raster2: Second raster to compare

    Returns:
        bool: True if rasters have matching schemas, False otherwise
    """
    return (
        raster1.width == raster2.width and
        raster1.height == raster2.height and
        raster1.transform == raster2.transform and
        raster1.crs == raster2.crs
    )


def convert_to_mask(data: xr.DataArray, no_data_value: Union[float, int] = -9999) -> xr.DataArray:
    """
    Convert data array to boolean mask identifying valid data locations.

    Creates a mask where True indicates valid data (not equal to no_data_value)
    and False indicates no-data regions. Used for spatial boundary masking.

    Args:
        data: Input xarray DataArray
        no_data_value: Value representing missing/invalid data

    Returns:
        xr.DataArray: Boolean mask (True = valid data, False = no data)
    """
    return data != no_data_value

class FetchCimisValidators:

    def validate_datetime_range(
        self,
        start_datetime: datetime,
        end_datetime: datetime
    ) -> None:
        """
        Validate that requested date range is within CIMIS data availability.

        Args:
            start_datetime: Beginning of requested date range
            end_datetime: End of requested date range

        Raises:
            ValueError: If date range is invalid or outside CIMIS availability
        """
        min_datetime = datetime(2003, 10, 1)
        max_datetime = datetime.now(ZoneInfo("Etc/GMT+8"))
        max_datetime = datetime(max_datetime.year, max_datetime.month, max_datetime.day, 23)
        max_datetime = max_datetime - timedelta(days=3)

        if (start_datetime > end_datetime):
            raise ValueError(f"Start date {start_datetime} cannot be after end date {end_datetime}.")

        if (start_datetime < min_datetime) or (end_datetime > max_datetime):
            raise ValueError(f"Date range must be between {min_datetime} and {max_datetime}. Provided: {start_datetime} to {end_datetime}.")

class FetchCimis(DataFetcher):
    """
    Fetcher for CIMIS data.

    Processing pipeline:
        1. Download daily ETo.asc.gz files from spatialcimis.water.ca.gov
        2. Unzip to ASC raster format
        3. Resample to match template raster schema
        4. Stack into time series arrays
        6. Apply spatial boundary mask
        7. Export to NetCDF format
    """

    def __init__(self):
        """Initialize validators and helper utilities."""
        self._validators = FetchCimisValidators()
        self._helpers = FetchCimisHelpers()

    def fetch(self, project: ProjectControl) -> None:
        """
        Main entry point for fetching CIMIS data.

        Creates monthly batch requests from project settings and executes
        each request sequentially. Provides high-level orchestration of
        the download-process-save pipeline.

        Args:
            project: ProjectControl object containing date range, output paths,
            and processing configuration
        """
        print(f"\tFetching CIMIS data for project: {project.request_control.project_name}")

        cimis_requests = self.create_fetch_requests(project)

        for request in cimis_requests:
            self.execute_fetch_request(request)

    def create_fetch_requests(self, project: ProjectControl) -> List[FetchCimisRequest]:
        """
        Generate monthly CIMIS fetch requests from project configuration.

        Splits the overall date range into monthly intervals to enable:
        - Memory-efficient batch processing
        - Parallel processing opportunities

        Each request includes:
        - Monthly date boundaries
        - Template raster path for spatial alignment
        - Output directory for NetCDF files
        - Processing configuration (workers, gap filling, etc.)

        Args:
            project: ProjectControl with date range and storage configuration

        Returns:
            List[FetchCimisRequest]: Monthly batch requests ready for execution

        Raises:
            ValueError: If date range is invalid or outside CIMIS availability
        """
        request_control = project.request_control
        validators = self._validators
        helpers = self._helpers

        start_date = request_control.start_date
        end_date = request_control.end_date

        validators.validate_datetime_range(start_date, end_date)

        output_dir = project.storage.cimis.raw

        template_path = Path(__file__).parent.parent / "util" / "templates" / "2004-01-05_ETo.asc"

        date_ranges = helpers.split_date_range_into_monthly_intervals(
            start_date,
            end_date,
            force_boundaries=True
        )

        requests = []
        for date_range in date_ranges:
            if date_range:
                requests.append(
                    FetchCimisRequest(
                        start_datetime=date_range[0],
                        end_datetime=date_range[1],
                        overwrite=request_control.overwrite,
                        output_dir=output_dir,
                        template_raster_path=template_path,
                        num_workers=1 # passing in one worker for now
                    )
                )

        return requests

    def download_cimis_files(self, request: FetchCimisRequest) -> List[Path]:
        """
        Download CIMIS ETo.asc.gz files for the request date range.

        URL structure: https://spatialcimis.water.ca.gov/cimis/{year}/{month}/{day}/ETo.asc.gz

        Failed downloads are logged but don't stop the process - allows partial
        downloads to be processed.

        Args:
            request: FetchCimisRequest with date range and output directory

        Returns:
            List[Path]: Paths to successfully downloaded .gz files

        Notes:
            - Uses 0.01s sleep between requests to avoid overwhelming server
            - Disables SSL verification (spatialcimis cert issues)
            - Creates download_log.txt for failed attempts
        """
        tmp_dir = request.output_dir / "tmp"
        tmp_dir.mkdir(exist_ok=True, parents=True)

        zipped_dir = tmp_dir / "zipped"
        zipped_dir.mkdir(exist_ok=True, parents=True)

        dates = self._helpers.build_date_table(request.start_datetime, request.end_datetime)

        session = requests.Session()
        session.headers.update({"User-Agent": "cimis-downloader/1.0"})

        rturl = "https://spatialcimis.water.ca.gov/cimis"
        downloaded_files = []

        logfile_path = tmp_dir / "download_log.txt"
        with open(logfile_path, "w") as logfile:
            for _, row in tqdm(dates.iterrows(), desc=f"Downloading CIMIS data ({request.start_datetime} to {request.end_datetime})", total=dates.shape[0]):
                dtm = row["date"].strftime("%Y-%m-%d")
                url = "/".join([rturl, row["year"], row["month"], row["day"], "ETo.asc.gz"])

                filename = f"{dtm}_ETo"
                gz_path = zipped_dir / f"{filename}.gz"

                sleep(0.01)

                try:
                    self._download_file(url, gz_path, session, verify_ssl=False)
                    downloaded_files.append(gz_path)

                except Exception as e:
                    logfile.write(f"Failed | {dtm} | {url} | {type(e).__name__}: {e}\n")
                    logfile.flush()

        return downloaded_files

    def unzip_cimis_files(
        self,
        gz_files: List[Path],
        request: FetchCimisRequest
    ) -> List[Path]:
        """
        Decompress downloaded .gz files to ASC raster format.

        Args:
            gz_files: List of paths to .gz compressed files
            request: FetchCimisRequest containing output directory configuration

        Returns:
            List[Path]: Paths to unzipped .asc files

        Notes:
            - Unzipped files are stored in {output_dir}/tmp/unzipped/
            - Failed unzips are logged but don't stop the process
            - Uses shutil.copyfileobj for efficient streaming decompression
        """
        tmp_dir = request.output_dir / "tmp"
        unzipped_dir = tmp_dir / "unzipped"
        unzipped_dir.mkdir(exist_ok=True, parents=True)

        unzipped_files = []
        for gz_path in tqdm(gz_files, desc="Unzipping CIMIS files"):
            asc_path = unzipped_dir / gz_path.stem

            try:
                with gzip.open(gz_path, "rb") as f_in, open(asc_path, "wb") as f_out:
                    shutil.copyfileobj(f_in, f_out)
                unzipped_files.append(asc_path)
            except Exception as e:
                print(f"Error unzipping {gz_path}: {e}")

        return unzipped_files

    def process_raster_file(
        self,
        asc_file: Path,
        template_raster: CimisRaster,
        crs_epsg_code: int
    ) -> Tuple[np.ndarray, pd.Timestamp]:
        """
        Process a single ASC raster file into numpy array with metadata.

        Args:
            asc_file: Path to ASC raster file (format: YYYY-MM-DD_ETo.asc)
            template_raster: Reference raster defining target spatial schema
            crs_epsg_code: EPSG code for coordinate reference system (default 3310)

        Returns:
            Tuple[np.ndarray, pd.Timestamp]: Raster data array and associated date

        Notes:
            - Uses average resampling if spatial schemas don't match
        """
        date = pd.to_datetime(asc_file.stem.rstrip("_ETo"), format='%Y-%m-%d', errors='coerce')
        raster = CimisRaster.from_file(asc_file)
        raster.set_crs(crs_epsg_code)

        if not compare_raster_schema(raster, template_raster):
            raster.resample(template=template_raster)

        return raster.data, date

    def transform_cimis_data(
        self,
        unzipped_files: List[Path],
        request: FetchCimisRequest
    ) -> None:
        """
        Transform ASC rasters into processed NetCDF time series.

        Complete processing pipeline:
            1. Load template raster for spatial reference
            2. Process all ASC files (parallel or serial)
            3. Stack into 3D array (time, lat, lon)
            4. Create boundary mask from template
            5. Initialize CimisTransformer with coordinates
            6. Run transformation pipeline:
               - Express no-data values as NaN
               - Pad time dimension for missing dates
               - Apply boundary mask
               - Fill remaining NaN with no-data value
            7. Export to NetCDF with compression

        Args:
            unzipped_files: List of paths to unzipped ASC files
            request: FetchCimisRequest with processing configuration

        Outputs:
            NetCDF file: {output_dir}/cimis_eto_{YYYY-MM}.nc

        Notes:
            - Supports checkpointing for resuming interrupted processing
            - Can use parallel processing with num_workers > 1
            - Boundary mask ensures data only in valid spatial domain
        """
        if len(unzipped_files) == 0:
            print(f"No files to transform for {request.start_datetime} to {request.end_datetime}")
            return

        tmp_dir = request.output_dir / "tmp"
        checkpoint_dir = tmp_dir / "checkpoint"

        if not request.template_raster_path:
            raise ValueError("Template raster path is required but was not provided in request")

        if not request.template_raster_path.exists():
            raise FileNotFoundError(f"Template raster not found at: {request.template_raster_path}")

        template_raster = CimisRaster.from_file(request.template_raster_path)
        template_raster.set_crs(request.crs_epsg)

        if request.enable_checkpointing:
            checkpoint_dir.mkdir(exist_ok=True, parents=True)
            arrays_checkpoint = checkpoint_dir / 'stacked_eto_arrays.npy'
            dates_checkpoint = checkpoint_dir / 'dates_array.npy'

            if arrays_checkpoint.exists() and dates_checkpoint.exists():
                print("Loading from checkpoint...")
                arrays = np.load(arrays_checkpoint, allow_pickle=True)
                dates = np.load(dates_checkpoint, allow_pickle=True)
                print(f"Loaded {len(dates)} timesteps from checkpoint")
            else:
                arrays, dates = self._process_rasters(
                    unzipped_files,
                    template_raster,
                    request
                )
                np.save(arrays_checkpoint, arrays)
                np.save(dates_checkpoint, dates)
        else:
            arrays, dates = self._process_rasters(
                unzipped_files, template_raster, request
            )

        boundary_mask = xr.DataArray(
            data=template_raster.data,
            dims=('lat', 'lon')
        )

        no_data_value = template_raster.nodata if template_raster.nodata is not None else -9999
        boundary_mask = convert_to_mask(boundary_mask, no_data_value=no_data_value)

        transform = CimisTransformer(
            data=arrays,
            time=dates,
            lat=template_raster.lat(),
            lon=template_raster.lon(),
            dims=('time', 'lat', 'lon'),
            crs=f'EPSG:{request.crs_epsg}'
        )

        _fmt = "%Y-%m"
        month_str = pd.to_datetime(request.start_datetime).strftime(_fmt)
        output_file = request.output_dir / f"cimis_eto_{month_str}.nc"

        transform.run(
            outfilepath=output_file,
            startDate=pd.Timestamp(request.start_datetime),
            endDate=pd.Timestamp(request.end_datetime),
            no_data_value=no_data_value,
            boundary_mask=boundary_mask if request.enable_resampling else None
        )

    def _process_rasters(
        self,
        unzipped_files: List[Path],
        template_raster: CimisRaster,
        request: FetchCimisRequest
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Process all raster files either serially or in parallel.

        Handles batch processing of ASC files with two execution modes:
        - Serial (num_workers=1): Simple loop with progress bar
        - Parallel (num_workers>1): Dask delayed/compute with multiprocessing

        Args:
            unzipped_files: List of paths to ASC raster files
            template_raster: Reference raster for spatial alignment
            request: FetchCimisRequest with processing configuration

        Returns:
            Tuple containing:
                - arrays: 3D numpy array (time, lat, lon)
                - dates: 1D array of pandas Timestamps
        """
        arrays = []
        dates = []

        if request.num_workers == 1:
            t1 = pd.Timestamp.now()
            for asc_file in tqdm(sorted(unzipped_files), desc="Processing ASC files"):
                arr, date = self.process_raster_file(asc_file, template_raster, request.crs_epsg)
                arrays.append(arr)
                dates.append(date)
            print(f'Elapsed time: {pd.Timestamp.now() - t1}')
        else:
            print(f"Processing with {request.num_workers} workers...")
            tasks = [
                delayed(self.process_raster_file)(asc_file, template_raster, request.crs_epsg) 
                for asc_file in sorted(unzipped_files)
            ]

            with ProgressBar():
                results = compute(*tasks, scheduler='processes', num_workers=request.num_workers)

            arrays, dates = zip(*results)
            arrays = list(arrays)
            dates = list(dates)

        arrays = np.stack(arrays, axis=0)
        dates = np.array(dates)

        return arrays, dates

    def purge_request_temporary_storage(self, request: FetchCimisRequest):
        """
        Remove all temporary files created during processing.

        Args:
            request: FetchCimisRequest with output directory containing tmp folder
        """
        tmp_dir = request.output_dir / "tmp"
        if tmp_dir.exists():
            shutil.rmtree(tmp_dir)

    def execute_fetch_request(self, request: FetchCimisRequest) -> None:
        """
        Execute a single CIMIS fetch request from download to NetCDF export.

        Args:
            request: FetchCimisRequest with all configuration parameters

        Raises:
            ValueError: If output directory doesn't exist
        """
        if not request.output_dir.exists():
            raise ValueError(f"Output directory does not exist at {request.output_dir}")

        request.output_dir.mkdir(exist_ok=True, parents=True)

        downloaded_files = self.download_cimis_files(request)

        if len(downloaded_files) == 0:
            print(f"Warning: No files downloaded for {request.start_datetime} to {request.end_datetime}")
            return

        unzipped_files = self.unzip_cimis_files(downloaded_files, request)

        self.transform_cimis_data(unzipped_files, request)

        self.purge_request_temporary_storage(request)

    def _download_file(
        self,
        url: str,
        outpath: Path,
        session: requests.Session,
        verify_ssl: bool
    ) -> None:
        """
        Download a single file from URL using streaming.

        Args:
            url: Full URL to download from
            outpath: Destination file path
            session: Requests session with headers/auth configured
            verify_ssl: Whether to verify SSL certificates

        Raises:
            FileNotFoundError: If server returns 404
            requests.HTTPError: For other HTTP errors
        """
        with session.get(url, stream=True, timeout=60, verify=verify_ssl) as r:
            if r.status_code == 404:
                raise FileNotFoundError(f"404 Not Found: {url}")
            r.raise_for_status()

            tmp = outpath.with_suffix(outpath.suffix + ".part")
            with open(tmp, "wb") as f:
                for chunk in r.iter_content(chunk_size=1024 * 1024):
                    if chunk:
                        f.write(chunk)
            tmp.replace(outpath)