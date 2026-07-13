# Import Library Dependencies
import os, sys
from pathlib import Path

# Add workspace root to Python path to enable imports
workspace_root = Path(__file__).parent.parent
sys.path.insert(0, str(workspace_root))
print(f"Added to sys.path: {workspace_root}")

from src import WeatherDataEtl

print(f'System Paths: {sys.path}\n\n')
print(f'cwd: {os.getcwd()}')

# Path to master control file
master_control_file = Path(r'C:\Users\jon.gendron\Projects\lspc\lspc-climate-processing-restructure\test\test_inputs\Master_Control.xlsx')

# Instantiate application
app = WeatherDataEtl()

# Build/config project objects
projects = app.builder.build_projects(master_control_file=master_control_file)
print(f'Found {len(projects)} project(s)')


# Builds storage repository for projects
# for project in projects:
#     app.builder.build_storage_respository(project)

# Perform stage cimis by project
for project in projects:
    app.run.qc_remake_timeseries_automation(project)


