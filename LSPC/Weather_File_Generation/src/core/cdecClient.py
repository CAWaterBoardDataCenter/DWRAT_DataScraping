import pandas as pd
from typing import List, Dict
import requests
import io

class CdecClient:
    BASE_URL: str = "https://cdec.water.ca.gov"

    def __init__(self, timeout: int = 30) -> None:
        self.timeout: int = timeout
        self.session: requests.Session = requests.Session()

    def get_station_sensors(self, station_ids: List[str]) -> Dict[str, pd.DataFrame]:
        result: Dict[str, pd.DataFrame] = {}

        for station_id in station_ids:
            url: str = f"{self.BASE_URL}/dynamicapp/staMeta?station_id={station_id}"

            try:
                sensor_list = pd.read_html(url, match='Sensor Description')[0]

                sensor_list.columns = ['variable', 'sensor_id', 'resolution', 'varcode', 'method', 'timerange']

                sensor_list[['variable', 'units']] = sensor_list['variable'].str.split(',', n=1, expand=True)

                sensor_list['resolution'] = sensor_list['resolution'].str.strip('()')

                sensor_list['sensor_id'] = pd.to_numeric(sensor_list['sensor_id'], errors='coerce')
                sensor_list = sensor_list.dropna(subset=['sensor_id'])
                sensor_list['sensor_id'] = sensor_list['sensor_id'].astype(int)

                result[station_id] = sensor_list

            except Exception as e:
                print(f"Error fetching sensors for {station_id}: {e}")
                result[station_id] = pd.DataFrame()

        return result

    def get_data(
        self,
        station_ids: List[str],
        sensor_ids: List[int],
        resolutions: List[str],
        start: str,
        end: str
    ) -> Dict[str, Dict[str, pd.DataFrame]]:

        result: Dict[str, Dict[str, pd.DataFrame]] = {}

        for station_id in station_ids:
            sensors_df = self.get_station_sensors([station_id])[station_id]

            station_data: Dict[str, pd.DataFrame] = {}

            for sensor_id, resolution in zip(sensor_ids, resolutions):
                sensor_row = sensors_df[sensors_df['sensor_id'] == sensor_id]
                if sensor_row.empty:
                    continue

                variable_name = sensor_row.iloc[0]['variable']

                df: pd.DataFrame = self._fetch_sensor_data(
                    station_id=station_id,
                    sensor_id=sensor_id,
                    resolution=resolution,
                    start_date=start,
                    end_date=end
                )

                if not df.empty:
                    station_data[variable_name] = df

            result[station_id] = station_data

        return result

    def _get_duration_code(self, resolution: str) -> str:
        resolution_map: Dict[str, str] = {
            'event': 'E',
            'hourly': 'H',
            'daily': 'D',
            'monthly': 'M'
        }
        return resolution_map.get(resolution.lower(), 'D')

    def _fetch_sensor_data(
        self,
        station_id: str,
        sensor_id: int,
        resolution: str,
        start_date: str,
        end_date: str
    ) -> pd.DataFrame:
        dur_code: str = self._get_duration_code(resolution)

        start_dt: pd.Timestamp = pd.to_datetime(start_date)
        end_dt: pd.Timestamp = pd.to_datetime(end_date)

        url: str = f"{self.BASE_URL}/dynamicapp/req/CSVDataServlet"
        params: Dict[str, str] = {
            'Stations': station_id,
            'SensorNums': str(sensor_id),
            'dur_code': dur_code,
            'Start': start_dt.strftime('%Y-%m-%d'),
            'End': end_dt.strftime('%Y-%m-%d')
        }

        try:
            response: requests.Response = self.session.get(url, params=params, timeout=self.timeout)
            response.raise_for_status()

            df: pd.DataFrame = pd.read_csv(
                io.StringIO(response.text),
                parse_dates=['DATE TIME'],
                index_col='DATE TIME',
                na_values='---'
            )

            df.columns = ['station_id', 'duration', 'sensor_number', 'sensor_type', 'obs_date', 'value', 'data_flag', 'units']

            df = df.rename(columns={'data_flag': 'flag'})

            df['value'] = pd.to_numeric(df['value'], errors='coerce')

            return df

        except Exception as e:
            print(f"Error fetching {station_id} sensor {sensor_id} at {resolution}: {e}")
            return pd.DataFrame()

    def close(self) -> None:
        """Close the requests session."""
        self.session.close()

    def __enter__(self) -> 'CdecClient':
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.close()