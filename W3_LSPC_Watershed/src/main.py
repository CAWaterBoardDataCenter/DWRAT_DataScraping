from pathlib import Path
from src import WeatherDataEtl

if __name__ == "__main__":

    # Define user inputs
    master_control_file = Path(r'C:\Users\jon.gendron\Projects\lspc\lspc-climate-processing-restructure\test\test_inputs\Master_Control.xlsx')
    
    # Wire application and builder
    app = WeatherDataEtl()
    #builder = MasterControlBuilder()

    # Build projects
    projects = app.builder.build_projects(master_control_file=master_control_file)

    # Build project storage repositories
    for project in projects:
        app.builder.build_storage_respository(project)

    # User logic for running etl fetching
    for project in projects:
        app.run.fetch_weather_data(project)

    # User logic for running etl data transformation / staging
    for project in projects:
        app.run.stage_weather_data(project)

    # NOTE: Natural break for manual qaqc

    # User logic for running etl transformation / staging of gage data after manual qaqc
    for project in projects:
        app.run.stage.gage(project)

    # User logic for running etl loading / writing lspc files
    for project in projects:
        app.run.write_lspc_files(project)
        

    
