from src.etl.base import DataFetcher
from src.core.noaaClient import NoaaClient
import pandas as pd
from pathlib import Path
from datetime import date
from typing import Optional
from src.core.models import ProjectControl
from src.etl.util.helpers import StationNameHelpers

class FetchNoaa(DataFetcher):
    # NOAA params
    NOAA_PARAMS = ['PRCP', 'SNOW', 'TMIN', 'TMAX']

    # param groups
    PRECIP_PARAMS = ['PRCP', 'SNOW']
    TEMP_PARAMS = ['TMIN', 'TMAX']

    # unit conversions
    TENTHS_DIVISOR = 10
    MM_TO_INCHES = 1 / 25.4
    CELSIUS_TO_FAHRENHEIT_SLOPE = 1.8
    FAHRENHEIT_OFFSET = 32

    # precision constants
    PRECIP_DECIMALS = 2
    TEMP_DECIMALS = 0

    def __init__(self):
        self.station_name_helpers = StationNameHelpers()

    def fetch(self, project: ProjectControl) -> None:
        """
        Fetch NOAA gage data based on project control settings.

        This function retrieves daily climate data from NOAA's GHCND database for all
        stations in the project where agency_id is 'noaa'.

        The function:
        - Filters gage data to NOAA stations only
        - Creates output directory for raw data storage
        - Initializes a NoaaClient for API communication
        - Iterates through each station and downloads data
        - Saves formatted CSV files without headers

        Data Processing:
        - Converts WBAN station IDs to GHCND format if needed
        - Fetches PRCP, SNOW, TMIN, TMAX parameters
        - Converts units
        - Filters to PRCP only and formats datetime as MM/DD/YYYY HH:MM
        - Removes quality flag columns (mflag, sflag, qflag)

        Parameters
        ----------
        project : ProjectControl
            Project configuration containing gage stations, date range, and output paths.

        Returns
        -------
        None
        """
        print(f"\tFetching NOAA gage data for project: {project.request_control.project_name}")

        noaa_stations: pd.DataFrame = project.gage.data[
            project.gage.data['agency_id'] == 'noaa'
        ]

        if noaa_stations.empty:
            print("\tNo NOAA stations to fetch.")
            return

        output_dir: Path = Path(project.storage.gage.noaa.raw)
        output_dir.mkdir(parents=True, exist_ok=True)

        client: NoaaClient = NoaaClient()

        for _, station in noaa_stations.iterrows():
            self._fetch_station(
                client=client,
                station_id=str(station['station_id']),
                name=str(station['name']),
                start_date=pd.to_datetime(project.request_control.start_date).date(),
                end_date=pd.to_datetime(project.request_control.end_date).date(),
                output_dir=output_dir
            )

        client.close()

    def _fetch_station(
        self,
        client: NoaaClient,
        station_id: str,
        name: str,
        start_date: date,
        end_date: date,
        output_dir: Path
    ) -> None:
        """Fetch data for a single NOAA station."""
        print(f"\t\tFetching NOAA station: {station_id} ({name})")

        station_id = self._convert_station_id(station_id)

        if not station_id.startswith('GHCND:'):
            print(f"\t\tSkipping non-GHCND station: {station_id}")
            return

        clean_station_id = self.station_name_helpers.clean_station_id(station_id)
        ghcnd_id = station_id.split(':')[1]

        try:
            data = self._fetch_data(client, ghcnd_id, start_date, end_date)

            if not data:
                print(f"\t\tNo data returned for {station_id}")
                return

            dfs = [
                self._process_parameter(data, param, station_id, start_date, end_date)
                for param in self.NOAA_PARAMS
            ]

            dfs = [df for df in dfs if df is not None]

            if dfs:
                sta_df = pd.concat(dfs, axis=0)
                sta_df = self._format_output(sta_df) # filter and format
                sta_df.to_csv(output_dir / f"{clean_station_id}_NOAA_daily.csv", index=False, header=False) # save without headers
                print(f"\t\tSaved data for {station_id}")
            else:
                print(f"\t\tNo valid parameters found for {station_id}")

        except Exception as e:
            print(f"\t\tError fetching data for {station_id}: {e}")
            import traceback
            traceback.print_exc()

    def _format_output(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Normalize and format NOAA data DataFrame for CSV output.

        This function performs the following transformations:
        - Drops quality flag columns (mflag, sflag, qflag) if they exist.
        - Filters rows to only precipitation parameter ('PRCP').
        - Removes the param column after filtering.
        - Converts the datetime index to a regular column.
        - Formats the datetime column as 'MM/DD/YYYY HH:MM'.

        Notes:
        - The input DataFrame is mutated in place.
        - NOAA data is daily, so time component is set to 00:00.
        - Returns empty DataFrame if no PRCP data exists.

        Parameters
        ----------
        df : pd.DataFrame
            Raw NOAA data with datetime index to be formatted.

        Returns
        -------
        pd.DataFrame
            The formatted DataFrame with datetime as first column.
        """
        # drop all flag columns if they exist
        columns_to_drop = ['mflag', 'sflag', 'qflag']
        df.drop(columns=[col for col in columns_to_drop if col in df.columns], inplace=True)

        # filter to only PRCP parameter
        df = df[df['param'].isin(['PRCP'])].copy()

        # drop param column
        df.drop(columns=['param'], inplace=True)

        df = df.reset_index()

        # format datetime as MM/DD/YYYY HH:MM
        if not df.empty and len(df.columns) > 0:
            first_col = df.columns[0]
            df[first_col] = pd.to_datetime(df[first_col], errors='coerce').dt.strftime('%m/%d/%Y %H:%M')

        return df

    def _convert_station_id(self, station_id: str) -> str:
        """Convert WBAN station IDs to GHCND format."""
        if station_id.startswith('WBAN:'):
            wban_id = station_id.split(':')[1]
            converted_id = f"GHCND:USW000{wban_id}"
            print(f"\t\tConverted WBAN to GHCND: {converted_id}")
            return converted_id
        return station_id

    def _fetch_data(
        self,
        client: NoaaClient,
        ghcnd_id: str,
        start_date: date,
        end_date: date
    ) -> dict:
        """Fetch data from NOAA API with date validation."""
        end_date_str = (
            end_date.strftime('%Y-%m-%d')
            if pd.notna(end_date)
            else pd.Timestamp.now().strftime('%Y-%m-%d')
        )

        return client.get_data(
            station_id=ghcnd_id,
            elements=self.NOAA_PARAMS,
            start_date=start_date.strftime('%Y-%m-%d'),
            end_date=end_date_str,
            as_dataframe=True
        )

    def _process_parameter(
        self,
        data: dict,
        param: str,
        station_id: str,
        start_date: date,
        end_date: date
    ) -> Optional[pd.DataFrame]:
        """Process a single parameter's data with unit conversion."""
        try:
            end_dt = end_date if pd.notna(end_date) else pd.Timestamp.now().date()

            temp_df = data[param].loc[start_date:end_dt].copy()
            temp_df.dropna(subset=['value'], inplace=True)

            if temp_df.empty:
                return None

            temp_df['value'] = self._convert_units(temp_df['value'], param)

            temp_df['param'] = param

            return temp_df

        except KeyError:
            print(f"\t\t{param} not available at {station_id}")
            return None
        except Exception as e:
            print(f"\t\tError processing {param} for {station_id}: {e}")
            return None

    def _convert_units(self, values: pd.Series, param: str) -> pd.Series:
        """Convert units based on parameter type."""
        values = values / self.TENTHS_DIVISOR

        if param in self.PRECIP_PARAMS:
            # mm -> in
            values = values * self.MM_TO_INCHES
            return pd.to_numeric(values).round(self.PRECIP_DECIMALS)

        elif param in self.TEMP_PARAMS:
            # c -> f
            values = (values * self.CELSIUS_TO_FAHRENHEIT_SLOPE) + self.FAHRENHEIT_OFFSET
            return pd.to_numeric(values).round(self.TEMP_DECIMALS)

        return values