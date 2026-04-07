import io
import shutil
import pandas as pd
import requests
import time
from pathlib import Path

from src.etl.util.helpers import StationNameHelpers
from ...base import DataFetcher
from ...base import ProjectControl

class FetchLcd(DataFetcher):

    def __init__(self):
        self.station_name_helpers = StationNameHelpers()

    def fetch(self, project: ProjectControl) -> None:
        """
        Fetch LCD gage data based on project control settings.

        This function downloads annual CSV files from NOAA's LCD database for each
        station in the project's gage data.

        The function:
        - Iterates through all LCD stations in the project
        - Downloads data for each year in the project date range
        - Saves raw CSV files to the project's LCD raw data directory
        - Implements a 1.2 second delay between requests to avoid rate limiting
        - Skips stations/years that fail to download with a warning message

        Parameters
        ----------
        project : ProjectControl
            Project configuration containing gage stations, date range, and output paths.

        Returns
        -------
        None
        """
        print(f"\tFetching LCD gage data for project: {project.request_control.project_name}")

        start_date = project.request_control.start_date
        end_date = project.request_control.end_date

        year_range = range(start_date.year, end_date.year + 1)

        gage_data = project.gage.data
        lcd_stations = gage_data[gage_data.agency_id == "lcd"]

        if lcd_stations.empty:
            print(f"\t\tNo LCD stations found in project. Skipping.")
            return

        output_dir = project.storage.gage.lcd.raw
        output_dir.mkdir(parents=True, exist_ok=True)

        for _, station in lcd_stations.iterrows():
            station_id = station.station_id
            print(f"\t\tFetching data for station: {station_id}")

            temp_dir = output_dir / "temp"
            temp_dir.mkdir(parents=True, exist_ok=True)

            yearly_files = []

            for year in year_range:
                try:
                    output_file = self._fetch_station_year(
                        station_id=station_id,
                        year=year,
                        output_dir=temp_dir
                    )
                    if output_file:
                        yearly_files.append(output_file)
                except Exception as e:
                    print(f"\t\t\tWarning: Failed to fetch {station_id} for {year}: {str(e)}")
                    continue

                time.sleep(1.2)

            if yearly_files:
                self._combine_yearly_files(station_id, yearly_files, output_dir)

            if temp_dir.exists():
                shutil.rmtree(temp_dir)


    def _fetch_station_year(self, station_id: str, year: int, output_dir: Path) -> None:
        """Download LCD CSV file for a given station and year."""
        formatted_station_id = self._format_station_id(station_id)

        url = f"https://www.ncei.noaa.gov/oa/local-climatological-data/v2/access/{year}/LCD_{formatted_station_id}_{year}.csv"

        print(f"\t\t\tDownloading: {year}")
        response = requests.get(url)

        if response.status_code != 200:
            raise ValueError(f"HTTP {response.status_code}")

        df = pd.read_csv(io.StringIO(response.text), low_memory=False)

        required_cols = ['DATE', 'HourlyPrecipitation', 'DailyPrecipitation', 'REPORT_TYPE']
        missing_cols = [col for col in required_cols if col not in df.columns]
        if missing_cols:
            raise ValueError(f"Missing required columns: {missing_cols}")

        df['DATE'] = pd.to_datetime(df['DATE'], errors='coerce')

        df['HourlyPrecipitation'] = df['HourlyPrecipitation'].replace('T', 0.0)
        df['HourlyPrecipitation'] = pd.to_numeric(df['HourlyPrecipitation'], errors='coerce')

        df['DailyPrecipitation'] = df['DailyPrecipitation'].replace('T', 0.0)
        df['DailyPrecipitation'] = pd.to_numeric(df['DailyPrecipitation'], errors='coerce')

        df = df[df['REPORT_TYPE'] == 'FM-15'].copy() # filter to fm-15 report type
        if df.empty:
            print(f"\t\t\t\tNo FM-15 data found; skipping.")
            return

        hourly_has_data = df['HourlyPrecipitation'].notna().any()

        if hourly_has_data:
            hourly_daily = (
                df.assign(DATE=df['DATE'].dt.floor('D'))
                .groupby('DATE', as_index=False)['HourlyPrecipitation']
                .sum(min_count=1)
                .rename(columns={'HourlyPrecipitation': 'DailyPrecipitation'})
            )
            hourly_count = len(hourly_daily.dropna(subset=['DailyPrecipitation']))
        else:
            hourly_daily = None
            hourly_count = 0

        daily_direct = (
            df.assign(DATE=df['DATE'].dt.floor('D'))
            .groupby('DATE', as_index=False)['DailyPrecipitation']
            .first()
        )
        daily_count = len(daily_direct.dropna(subset=['DailyPrecipitation']))

        if daily_count > 0:
            daily_df = daily_direct
            print(f"\t\t\t\tUsing daily data: {daily_count} days")
        if hourly_count > daily_count: # use whichever data has less null entries 
            daily_df = hourly_daily
            print(f"\t\t\t\tUsing hourly data (aggregated): {hourly_count} days")
        else:
            print(f"\t\t\t\tNo precipitation data found; skipping.")
            return

        daily_df['DATE'] = pd.to_datetime(daily_df['DATE'])
        daily_df['DATE'] = daily_df['DATE'].dt.strftime('%m/%d/%Y %H:%M')

        clean_station_id = self.station_name_helpers.clean_station_id(station_id)
        MM_TO_INCHES = 1 / 25.4
        output_filename = output_dir / f"{clean_station_id}_LCD_{year}_daily.csv"
        daily_df['DailyPrecipitation'] = daily_df['DailyPrecipitation']*MM_TO_INCHES
        daily_df[['DATE', 'DailyPrecipitation']].to_csv(output_filename, index=False, header=False)
        print(f"\t\t\t\tSaved to: {output_filename}")

        return output_filename

    def _combine_yearly_files(self, station_id: str, yearly_files: list[Path], output_dir: Path) -> None:
        """Combine multiple yearly files into a single consolidated file."""
        clean_station_id = self.station_name_helpers.clean_station_id(station_id)

        print(f"\t\t\tCombining {len(yearly_files)} files for station {station_id}")

        dfs = []
        for file in sorted(yearly_files):
            df = pd.read_csv(file, header=None, names=['DATE', 'DailyPrecipitation'])
            dfs.append(df)

        combined_df = pd.concat(dfs, ignore_index=True)

        combined_df['DATE'] = pd.to_datetime(combined_df['DATE'])
        combined_df = combined_df.sort_values('DATE').drop_duplicates(subset=['DATE'])
        combined_df['DATE'] = combined_df['DATE'].dt.strftime('%m/%d/%Y %H:%M')

        output_filename = output_dir / f"{clean_station_id}_LCD_daily.csv"
        combined_df.to_csv(output_filename, index=False, header=False)

        print(f"\t\t\tCombined file saved to: {output_filename}")

    def _format_station_id(self, station_id: str) -> str:
        """Format station ID for LCD API requests."""
        if station_id.startswith("WBAN:"):
            # remove "WBAN:" prefix and replace with "USW000"
            return station_id.replace("WBAN:", "USW000")
        elif station_id.startswith("GHCND:"):
            # remove "GHCND:" prefix
            return station_id.replace("GHCND:", "")
        else:
            return station_id