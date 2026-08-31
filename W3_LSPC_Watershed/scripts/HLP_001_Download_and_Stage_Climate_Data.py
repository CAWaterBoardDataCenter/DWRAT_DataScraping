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
print("Hello")
#master_control_file = Path(r'C:\Users\jon.gendron\Projects\lspc\lspc-climate-processing-restructure\test\test_inputs\Master_Control.xlsx')

# Instantiate application
#app = WeatherDataEtl()



