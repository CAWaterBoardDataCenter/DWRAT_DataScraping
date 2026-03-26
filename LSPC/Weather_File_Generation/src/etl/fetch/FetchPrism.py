import os
from pathlib import Path
import shutil
from typing import List, Tuple
from urllib.request import urlopen
from datetime import (date, datetime, timedelta, timezone)
from dateutil.relativedelta import relativedelta
from time import sleep
from http.client import RemoteDisconnected
import requests
from requests import Session
import zipfile

# Package imports
from src.core.models import ProjectControl
from ..base import DataFetcher
from src.core.miscellaneous import DateRanger

class FetchPrism(DataFetcher):

    def fetch(self, project) -> None:
        """Fetch PRISM data based on project control settings."""
        # Implementation for fetching PRISM data goes here
        print(f"\tFetching PRISM data for project: {project.request_control.project_name}")

        # Creating Temporary folder to download Zipped PRISM data
        temp_folder = Path(project.storage.prism.raw) / "temp"
        temp_folder.mkdir(parents=True, exist_ok=True)

        # Create fetch request
        prism_request_collections = self.create_fetch_requests(project)   

        MAX_RETRIES = 3           # how many times to retry each collection
        RETRY_DELAY = 10           # seconds between retries for the same collection
        BETWEEN_COLLECTION_DELAY = 5  # seconds between different collections

        # Start a session to fetch raw PRISM data
        with Session() as session:
            for collection in prism_request_collections: # date range grouping (network request level)
                filename = r"prism_ppt_us_25m_" + str(collection[-16:-10]) +".zip"          # Extracting out the YYYYMM date format from "collection"
                zip_path = temp_folder / filename
                for attempt in range(1, MAX_RETRIES + 1):
                    try:
                        zipped_prism_downloads = session.get(collection,timeout=(10, 60),stream=True)
                        zipped_prism_downloads.raise_for_status()  
                        
                        # Save the ZIP file in temporary folder
                        with open(zip_path, "wb") as f:
                            for chunk in zipped_prism_downloads.iter_content(chunk_size=8192):
                                f.write(chunk)
                        
                        # Unzip the Zip file content in raw PRISM folder
                        with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                            zip_ref.extractall((Path(project.storage.prism.raw))) 
                        break      
                    # Catch network related errors         
                    except requests.exceptions.ConnectionError as e:
                        cause = getattr(e, "__cause__", None)
                        print(f"ConnectionError for {collection} on attempt {attempt}: {repr(e)}")

                        if isinstance(cause, RemoteDisconnected):
                            print("Underlying cause: RemoteDisconnected (remote end closed connection without response).")
                            
                        if attempt == MAX_RETRIES:
                            print(f"Failed to fetch PRISM data for {collection} after {MAX_RETRIES} attempts. Skipping.")
                        else:
                            print(f"Retrying {collection} in {RETRY_DELAY} seconds...")
                            sleep(RETRY_DELAY)

                    except requests.exceptions.Timeout as e:
                        print(f"Timeout for {collection} on attempt {attempt}: {repr(e)}")

                        if attempt == MAX_RETRIES:
                            print(f"Failed due to repeated timeouts for {collection}. Skipping.")
                        else:
                            print(f"Retrying {collection} in {RETRY_DELAY} seconds...")
                            sleep(RETRY_DELAY)

                    except requests.exceptions.HTTPError as e:
                        status = e.response.status_code if e.response is not None else "unknown"
                        print(f"HTTPError for {collection} (status {status}): {repr(e)}")
                        print(f"Not retrying {collection} due to HTTP error.")
                        break  # exit retry loop
                    # Catch all other unexpected errors 
                    except Exception as e:
                        print(f"Error fetching PRISM data for request {collection}: {e}")
                        if attempt == MAX_RETRIES:
                            print(f"Failed to fetch PRISM data for {collection} after {MAX_RETRIES} attempts. Skipping.")
                        else:
                            # Wait before next retry for this same collection
                            print(f"Retrying {collection} in {RETRY_DELAY} seconds...")
                            sleep(RETRY_DELAY)
                sleep(BETWEEN_COLLECTION_DELAY)    

        # Delete temporary folder at the end of fetch execution       
        if temp_folder.exists() and temp_folder.is_dir():
                shutil.rmtree(temp_folder)  
        #self.write_prism_grid_csv(project)
                         
    def create_fetch_requests(self, project: ProjectControl):
        """Create PRISM data requests based on project control settings   """
        # Extract request variable from 
        request_control = project.request_control           # Get global (i.e. project level) request control                  
        start_datetime = request_control.start_date
        end_datetime = request_control.end_date
        prism_rawfolder = Path(project.storage.prism.raw)
        # Generate full list of datetime range between start and end datetimes in YYYYMM format to insert in PRISM fetching url.
        date_ranges = DateRanger(start_datetime,end_datetime)
        datetime_ranges = FetchPrism.check_files_by_date(prism_rawfolder,date_ranges)
        # Create list of GET requests
        request_collections = []
        for date_range in datetime_ranges:
             request_collections.append(str(r"https://services.nacse.org/prism/data/get/us/4km/ppt/"+ str(date_range) + "?format=nc"))

        return request_collections

    @staticmethod
    def check_files_by_date(filedirectory:Path, daterange:List) -> List:
        List_DateRange = []
        current_date = datetime.now()
        for date_instance in daterange:
            try:
                file_name = r"prism_ppt_us_25m_" + str(date_instance) +".nc"
                nc_filepath = filedirectory/file_name
                file_date = datetime.strptime(date_instance, "%Y%m")
            except ValueError:
                continue

            # Calculate difference in months
            months_diff = (current_date.year - file_date.year) * 12 + (current_date.month - file_date.month)
            # If file doesn't exist in the raw directory, add it to the fetch list
            if not os.path.exists(nc_filepath):
                # File does not exist, add to new_request_list
                List_DateRange.append(date_instance)
            else:
                # Check when the file is modified i.e. downloaded
                modified_time = datetime.fromtimestamp(os.path.getmtime(nc_filepath))
                last_download = current_date - modified_time    # Calculate when the file was downloaded relative to today
                if (months_diff <= 7) and (last_download.days >= 2):  # Redownload the files for last seven months (PRISM provisional data to stable version) if its not downloaded in last 2 days
                    List_DateRange.append(date_instance)
                else:
                    pass
        return List_DateRange


