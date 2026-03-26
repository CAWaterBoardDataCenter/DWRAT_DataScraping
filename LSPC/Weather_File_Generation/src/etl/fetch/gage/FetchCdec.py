from src.etl.base import DataFetcher
from src.core.cdecClient import CdecClient
import pandas as pd
from pathlib import Path
from datetime import date
from typing import List, Tuple, Optional
from src.core.models import ProjectControl
from src.etl.util.helpers import StationNameHelpers

class FetchCdec(DataFetcher):

    # sensor id constants
    SENSOR_PRECIP_INCREMENTAL = 45
    SENSOR_PRECIP_ACCUMULATED = 2
    SENSOR_TEMP_HOURLY = 4
    SENSOR_TEMP_MIN = 31
    SENSOR_TEMP_MAX = 32
    SENSOR_SNOW_SWE = 3
    SENSOR_SNOW_DEPTH = 73
    SENSOR_SNOW_PILLOW = 82

    # final output columns
    OUTPUT_COLUMNS = [
        'DATE TIME', 'station_id', 'duration', 'sensor_number',
        'sensor_type', 'obs_date', 'value', 'data_flag', 'units'
    ]

    def __init__(self):
        self.station_name_helpers = StationNameHelpers()


    def fetch(self, project: ProjectControl) -> None:
        """
        Fetch CDEC gage data based on project control settings.

        This function retrieves climate data from CDEC for all stations in the project
        where agency_id is 'cdec'.

        The function:
        - Filters gage data to CDEC stations only
        - Creates output directories for raw data and sensor metadata
        - Initializes a CdecClient for API communication
        - Retrieves sensor metadata for each station
        - Selects appropriate sensors based on availability and priority
        - Downloads data for selected sensors
        - Saves formatted CSV files without headers

        Notes:
        - Sensor metadata is saved separately in Sensor_Metadata directory
        - RAIN sensors provide cumulative data, converted to incremental
        - PPT INC sensors provide incremental data directly
        - Files are saved as: {station_name}.csv
        - API client is properly closed after all requests

        Parameters
        ----------
        project : ProjectControl
            Project configuration containing gage stations, date range, and output paths.

        Returns
        -------
        None
        """
        print(f"\tFetching CDEC gage data for project: {project.request_control.project_name}")

        cdec_stations: pd.DataFrame = project.gage.data[
            project.gage.data['agency_id'] == 'cdec'
        ]

        if cdec_stations.empty:
            print("\tNo CDEC stations to fetch.")
            return

        output_dir: Path = Path(project.storage.gage.cdec.raw)
        sensor_dir: Path = output_dir / "Sensor_Metadata"
        sensor_dir.mkdir(parents=True, exist_ok=True)

        client: CdecClient = CdecClient()

        for _, station in cdec_stations.iterrows():
            self._fetch_station(
                client=client,
                station_id=str(station['station_id']),
                name=str(station['name']),
                start_date=pd.to_datetime(project.request_control.start_date).date(),
                end_date=pd.to_datetime(project.request_control.end_date).date(),
                output_dir=output_dir,
                sensor_dir=sensor_dir
            )

        client.close()

    def _fetch_station(
        self,
        client: CdecClient,
        station_id: str,
        name: str,
        start_date: date,
        end_date: date,
        output_dir: Path,
        sensor_dir: Path
    ) -> None:
        print(f"\t\tFetching CDEC station: {station_id} ({name})")

        clean_id: str = self.station_name_helpers.clean_station_id(station_id)

        sensors = self._get_sensor_metadata(client, station_id, clean_id, sensor_dir)
        if sensors is None:
            return

        selected_sensor_ids, selected_resolutions = self._select_sensors(sensors)
        if not selected_sensor_ids:
            print(f"\t\tNo valid sensors found for {station_id}")
            return

        print(f"\t\tSelected sensors: {selected_sensor_ids} at {selected_resolutions}")

        try:
            data = client.get_data(
                station_ids=[station_id],
                sensor_ids=selected_sensor_ids,
                resolutions=selected_resolutions,
                start=start_date.strftime('%Y-%m-%d'),
                end=end_date.strftime('%Y-%m-%d') if pd.notna(end_date) else pd.Timestamp.now().strftime('%Y-%m-%d')
            )

            sensor_dataframes = []
            for param in data[station_id].keys():
                temp_df = data[station_id][param].loc[start_date:end_date]
                temp_df = temp_df.reset_index()
                sensor_dataframes.append(temp_df)

            if sensor_dataframes:
                station_data = pd.concat(sensor_dataframes, axis=0, ignore_index=True)

                # format output
                station_data = self._format_output(station_data)

                resolution_suffix = selected_resolutions[0] if selected_resolutions else 'unknown'

                # save without headers
                station_data.to_csv(output_dir / f"{clean_id}_CDEC_{resolution_suffix}.csv", index=False, header=False)
                print(f"\t\tSaved data for {station_id}")

        except Exception as e:
            print(f"\t\tError fetching data for {station_id}: {e}")
            import traceback
            traceback.print_exc()

    def _format_output(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Normalize and format a sensor data DataFrame for CSV output.

        This function performs the following transformations:
        - Drops known unused metadata columns if they exist.
        - Filters rows to supported sensor types ('RAIN', 'PPT INC').
        - For RAIN-only datasets, converts cumulative values to per-interval
        deltas by differencing consecutive rows.
        - Replaces negative rainfall values with the string "NA".
        - Removes the sensor_type column after filtering.
        - Formats the 'DATE TIME' column as 'MM/DD/YYYY HH:MM' if present.

        Notes:
        - The input DataFrame is mutated in place.
        - Some operations (e.g., replacing negatives with "NA") may change
        column dtypes from numeric to object.

        Parameters
        ----------
        df : pd.DataFrame
            Raw sensor data to be formatted.

        Returns
        -------
        pd.DataFrame
            The formatted DataFrame.
        """
        # remove unused columns
        columns_to_remove = ['station_id', 'duration', 'sensor_number', 'units', 'obs_date', 'flag']
        df.drop(columns=[col for col in columns_to_remove if col in df.columns], inplace=True, errors='ignore')

        # filter sensors
        if 'sensor_type' in df.columns:
            df = df[df['sensor_type'].isin(['RAIN', 'PPT INC'])]

            if len(df['sensor_type'].unique()) == 1 and df['sensor_type'].unique()[0] == 'RAIN':
                df['prev'] = df['value'].shift(1)
                df['prev'] = df['prev'].fillna(0)
                df['delta'] = df['value'] - df['prev']
                df.drop(columns=['prev', 'value', 'data_flag'], inplace=True, errors='ignore')
                df.rename(columns={'delta': 'value'}, inplace=True)

                # set negative values to "NA"
                lt = df['value'] < 0
                df.loc[lt, 'value'] = "NA"

            # remove sensor column
            df.drop(columns=['sensor_type'], inplace=True)

        # format datetime as MM/DD/YYYY HH:MM
        if 'DATE TIME' in df.columns:
            df['DATE TIME'] = pd.to_datetime(df['DATE TIME'], errors='coerce').dt.strftime('%m/%d/%Y %H:%M')

        return df

    def _get_sensor_metadata(
        self,
        client: CdecClient,
        station_id: str,
        clean_id: str,
        sensor_dir: Path
    ) -> Optional[pd.DataFrame]:
        """Fetch and save sensor metadata, return indexed dataframe."""
        try:
            sensors_dict = client.get_station_sensors([station_id])
            sensors = sensors_dict[station_id]

            if sensors.empty:
                print(f"\t\tNo sensors found for {station_id}")
                return None

            sensors.set_index('sensor_id', inplace=True)

            sensors_copy = sensors.copy()
            sensors_copy['STAID'] = station_id
            sensors_copy.to_csv(sensor_dir / f"{clean_id}_metadata.csv", index=True)

            return sensors

        except Exception as e:
            print(f"\t\tError getting sensors for {station_id}: {e}")
            return None

    def _select_sensors(
        self,
        sensors: pd.DataFrame
    ) -> Tuple[List[int], List[str]]:
        """Select appropriate sensors based on priority logic."""
        selected_sensor_ids: List[int] = []
        selected_resolutions: List[str] = []

        def has_resolution(sensor_id: int, res: str) -> bool:
            """Check if a sensor supports a specific resolution."""
            if sensor_id not in sensors.index:
                return False
            sensor_res = sensors.loc[sensor_id, 'resolution']

            if isinstance(sensor_res, list):
                return res in sensor_res or res.lower() in [r.lower() for r in sensor_res]
            elif isinstance(sensor_res, str):
                res_list = [r.strip() for r in sensor_res.split(',')]
                return res in res_list or res.lower() in [r.lower() for r in res_list]
            return False

        def try_add_sensor(sensor_id: int, preferred_res: str, fallback_res: str = None) -> bool:
            """Try to add a sensor with preferred resolution, optionally falling back."""
            if has_resolution(sensor_id, preferred_res):
                selected_sensor_ids.append(sensor_id)
                selected_resolutions.append(preferred_res)
                return True
            if fallback_res and has_resolution(sensor_id, fallback_res):
                selected_sensor_ids.append(sensor_id)
                selected_resolutions.append(fallback_res)
                return True
            return False

        # prioritize daily
        if not (
            try_add_sensor(self.SENSOR_PRECIP_INCREMENTAL, 'daily', 'hourly') or
            try_add_sensor(self.SENSOR_PRECIP_ACCUMULATED, 'daily', 'hourly')
        ):
            print('\t\tNo precip sensor at this station')

        return selected_sensor_ids, selected_resolutions