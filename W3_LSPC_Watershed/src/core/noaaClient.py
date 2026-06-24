import pandas as pd
from typing import List, Dict, Optional, Union
import requests
import io


class NoaaClient:
    BASE_URL: str = "https://www.ncei.noaa.gov/pub/data/ghcn/daily/all"

    def __init__(self, timeout: int = 30) -> None:
        self.timeout: int = timeout
        self.session: requests.Session = requests.Session()

    def get_data(
        self,
        station_id: str,
        elements: Optional[List[str]] = None,
        start_date: Optional[str] = None,
        end_date: Optional[str] = None,
        as_dataframe: bool = True
    ) -> Union[pd.DataFrame, Dict[str, pd.DataFrame]]:
        url = f"{self.BASE_URL}/{station_id}.dly"

        try:
            response = self.session.get(url, timeout=self.timeout)
            response.raise_for_status()

            data = self._parse_dly_file(response.text, elements)

            if start_date:
                start_dt = pd.to_datetime(start_date)
                for element in data:
                    data[element] = data[element][data[element].index >= start_dt]

            if end_date:
                end_dt = pd.to_datetime(end_date)
                for element in data:
                    data[element] = data[element][data[element].index <= end_dt]

            return data

        except Exception as e:
            print(f"Error fetching GHCN data for {station_id}: {e}")
            return {}

    def _parse_dly_file(
        self,
        content: str,
        elements: Optional[List[str]] = None
    ) -> Dict[str, pd.DataFrame]:
        data_by_element = {}

        for line in content.split('\n'):
            if not line.strip():
                continue

            station_id = line[0:11].strip()
            year = int(line[11:15])
            month = int(line[15:17])
            element = line[17:21].strip()

            if elements and element not in elements:
                continue

            if element not in data_by_element:
                data_by_element[element] = []

            for day in range(1, 32):
                pos = 21 + (day - 1) * 8

                value_str = line[pos:pos+5].strip()
                mflag = line[pos+5:pos+6].strip()
                qflag = line[pos+6:pos+7].strip()
                sflag = line[pos+7:pos+8].strip()

                if value_str == '-9999' or not value_str:
                    continue

                try:
                    date = pd.Timestamp(year=year, month=month, day=day)
                except ValueError:
                    continue

                value = int(value_str)

                data_by_element[element].append({
                    'month_period': date,
                    'value': value,
                    'mflag': mflag,
                    'qflag': qflag,
                    'sflag': sflag
                })

        result = {}
        for element, records in data_by_element.items():
            if records:
                df = pd.DataFrame(records)
                df.set_index('month_period', inplace=True)
                df.sort_index(inplace=True)
                result[element] = df

        return result

    def get_stations(
        self,
        country: Optional[str] = None,
        state: Optional[str] = None
    ) -> pd.DataFrame:
        url = "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"

        try:
            response = self.session.get(url, timeout=self.timeout)
            response.raise_for_status()

            stations = []
            for line in response.text.split('\n'):
                if not line.strip():
                    continue

                station = {
                    'id': line[0:11].strip(),
                    'latitude': float(line[12:20].strip()),
                    'longitude': float(line[21:30].strip()),
                    'elevation': float(line[31:37].strip()),
                    'state': line[38:40].strip(),
                    'name': line[41:71].strip(),
                    'gsn_flag': line[72:75].strip(),
                    'hcn_crn_flag': line[76:79].strip(),
                    'wmo_id': line[80:85].strip()
                }

                if country and not station['id'].startswith(country):
                    continue
                if state and station['state'] != state:
                    continue

                stations.append(station)

            return pd.DataFrame(stations)

        except Exception as e:
            print(f"Error fetching station list: {e}")
            return pd.DataFrame()

    def close(self) -> None:
        self.session.close()

    def __enter__(self) -> 'NoaaClient':
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.close()