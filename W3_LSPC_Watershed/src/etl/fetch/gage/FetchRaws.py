from datetime import date
from io import StringIO
from pathlib import Path
import requests
import pandas as pd
from bs4 import BeautifulSoup

from src.etl.util.helpers import StationNameHelpers
from ...base import DataFetcher
from ...base import ProjectControl

class FetchRaws(DataFetcher):

    def __init__(self):
        super().__init__()
        # load the RAWS ID mapping table
        mapping_file = Path(__file__).parent / "station_id_to_raws_id_mapping.csv"
        self.raws_mapping = pd.read_csv(mapping_file)

        self.id_lookup = dict(zip(
            self.raws_mapping['MesoWest Station ID'],
            self.raws_mapping['RAWS_ID']
        ))

        self.station_name_helpers = StationNameHelpers()

    def fetch(self, project: ProjectControl) -> None:
        """
        Fetch RAWS gage data based on project control settings.

        This function retrieves climate data from the RAWS network for all stations in the project
        where agency_id is 'raws'.

        The function:
        - Filters gage data to RAWS stations only
        - Converts MesoWest Station IDs to RAWS IDs using lookup table
        - Parses HTML table responses into DataFrames
        - Saves formatted CSV files without headers

        Station ID Conversion:
        - Uses station_id_to_raws_id_mapping.csv for ID translation
        - Skips stations with no matching RAWS_ID in lookup table

        Parameters
        ----------
        project : ProjectControl
            Project configuration containing gage stations, date range, and output paths.

        Returns
        -------
        None
        """
        print(f"\tFetching RAWS gage data for project: {project.request_control.project_name}")

        project_start_date = project.request_control.start_date
        end_date = project.request_control.end_date

        gage_data = project.gage.data
        raws_stations = gage_data[gage_data.agency_id == "raws"]

        if raws_stations.empty:
            print(f"\t\tNo RAWS stations found in project. Skipping.")
            return

        print(f"\t\tFound {len(raws_stations)} RAWS stations")

        for _, station in raws_stations.iterrows():
            station_id = station.station_id
            raws_start_date = station.start_date
            delta = (raws_start_date - project_start_date).days
            if delta > 5:
                start_date = raws_start_date    
            else:
                start_date = project_start_date       

            print(f"\t\tFetching data for station: {station_id}")

            raws_id = self._get_raws_id(station_id)

            if raws_id is None:
                print(f"\t\t\tWarning: No RAWS ID found for station ID: {station_id}. Skipping.")
                continue

            try:
                self._fetch_station_data(
                    raws_id=raws_id,
                    station_id=station_id,
                    start_date=start_date,
                    end_date=end_date,
                    project=project
                )
            except ValueError as e:
                print(f"\t\t\tWarning: {str(e)}")
                continue

    def _get_raws_id(self, station_id: str) -> str:
        """Convert MesoWest Station ID to RAWS ID using lookup table."""
        raws_id = self.id_lookup.get(station_id)

        if pd.isna(raws_id) or raws_id == '':
            return None

        return raws_id

    def _fetch_station_data(
        self,
        raws_id: str,
        station_id: str,
        start_date: date,
        end_date: date,
        project: ProjectControl
    ) -> pd.DataFrame:
        """Fetch WRCC RAWS weather data for a given station and date range."""

        url = "https://wrcc.dri.edu/cgi-bin/wea_dysimts2.pl"

        start_month = f"{start_date.month:02d}"
        start_day = f"{start_date.day:02d}"
        start_year = f"{start_date.year % 100:02d}"  # convert to two-digit year
        end_month = f"{end_date.month:02d}"
        end_day = f"{end_date.day:02d}"
        end_year = f"{end_date.year % 100:02d}"  # convert to two-digit year

        body = {
            "stn": raws_id,
            "smon": start_month,
            "sday": start_day,
            "syea": start_year,
            "emon": end_month,
            "eday": end_day,
            "eyea": end_year,
            "qPR": "ON",
            "Ofor": "H",
            "Datareq": "C",
            "qc": "Y",
            "miss": "07",
            "obs": "N",
            "WsMon": "01",
            "WsDay": "01",
            "WeMon": "12",
            "WeDay": "31"
        }

        headers = {
            "Host": "wrcc.dri.edu",
            "User-Agent": "libcurl/8.10.1 r-curl/6.2.3 httr/1.4.7",
            "Accept-Encoding": "deflate, gzip",
            "Accept": "application/json, text/xml, application/xml, */*",
            "Content-Type": "application/x-www-form-urlencoded"
        }

        response = requests.post(url, data=body, headers=headers)

        if response.status_code != 200:
            raise ValueError(f"HTTP {response.status_code}")

        soup = BeautifulSoup(response.content, "html.parser")
        table = soup.find("table")

        if table is None:
            raise ValueError(f"No data table found for '{station_id}'")

        df = pd.read_html(StringIO(str(table)))[0]
        df = self._format_output(df)

        clean_station_id = self.station_name_helpers.clean_station_id(station_id)

        output_dir = project.storage.gage.raws.raw
        output_filename = output_dir / f"{clean_station_id}_RAWS_daily.csv"
        output_filename.parent.mkdir(parents=True, exist_ok=True)
        df.to_csv(output_filename, index=False, header=False)

        print(f"\t\t\tSaved to: {output_filename}")
        return df

    def _format_output(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Normalize and format RAWS data DataFrame for CSV output.

        This function performs the following transformations:
        - Skips the first row
        - Drops columns 1-3
        - Formats the datetime column as 'MM/DD/YYYY HH:MM'

        Notes:
        - The input DataFrame is mutated in place
        - After dropping columns, keeps date and all remaining data columns

        Parameters
        ----------
        df : pd.DataFrame
            Raw RAWS data parsed from HTML table to be formatted

        Returns
        -------
        pd.DataFrame
            The formatted DataFrame with datetime as first column and data columns following
        """
        if len(df) > 0:
            df = df.iloc[1:].reset_index(drop=True)

        if len(df.columns) > 4:
            df = df.drop(df.columns[1:4], axis=1)

        if len(df.columns) > 0:
            df.iloc[:, 0] = pd.to_datetime(df.iloc[:, 0], errors='coerce').dt.strftime('%m/%d/%Y %H:%M')

        return df