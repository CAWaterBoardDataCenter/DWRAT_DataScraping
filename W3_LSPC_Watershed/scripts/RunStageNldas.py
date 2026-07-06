# %% [markdown]
# # Weather Data Processing ETL Application (Interactive Notebook)
# - This notebook reproduces the procedure from `src/main.py`, but allows the user
# to run each modules individually in series.

# %% [markdown]
# ## Import Library Dependencies

# %%
import os, sys
from pathlib import Path

# Add workspace root to Python path to enable imports
workspace_root = Path(__file__).parent.parent
sys.path.insert(0, str(workspace_root))
print(f"Added to sys.path: {workspace_root}")

from src import WeatherDataEtl

print(f'System Paths: {sys.path}\n\n')
print(f'cwd: {os.getcwd()}')
# %% [markdown]
# ## Define User Inputs
# - **Master Control** workbook path.

# %%
# master_control_file = Path(r'C:\Users\jon.gendron\Projects\lspc\lspc-climate-processing-restructure-main\test\test_inputs\Master_Control.xlsx')
master_control_file = Path(r'test\test_inputs\Master_Control.xlsx')

# %% [markdown]
# ## Instantiate the System 
# - Wire application components

# %%
app = WeatherDataEtl()


# %% [markdown]
# ## Configure Projects
# - ingesting -> validating -> objectifying user-input control workbooks
# - create storage repository

# %% [markdown]
# ### Ingest -> Validate -> Store (as Objects)
# 1. **Ingests** user-input data from master control workbook and project control workbooks
# 2. **Validates** data against predefined schemas and performs cross-validate checks for dependencies.
# 3. **Stores** data is a standard `ProjectControl` object that can be provided as input to run any module.

# %%
projects = app.builder.build_projects(master_control_file=master_control_file)
print(f'Found {len(projects)} project(s)')

# %% [markdown]
# ### Builds storage repository for projects
# - Based on Storage Sheet in project respoitory
# - Does not overwrite existing respositories.

# %%
for project in projects:
    app.builder.build_storage_respository(project)

# %% [markdown]
# ## Run ETL Fetching (by source)

# # %% [markdown]
# # ### Fetch Prism

# # %%
# for project in projects:
#     app.run.fetch.prism(project)

# # %% [markdown]
# # ### Fetch Cimis

# # %%
# for project in projects:
#     app.run.fetch.cimis(project)

# # %% [markdown]
# # ### Fetch Nldas

# # %%
# for project in projects:
#     app.run.fetch.nldas(project)

# # %% [markdown]
# # ### Fetch Gage

# # %% [markdown]
# # *Fetch NOAA*:

# # %%
# for project in projects:
#     app.run.fetch.gage.noaa(project)

# # %% [markdown]
# # *Fetch CDEC*:

# # %%
# for project in projects:
#     app.run.fetch.gage.cdec(project)

# # %% [markdown]
# # *Fetch LCD*:

# # %%
# for project in projects:
#     app.run.fetch.gage.lcd(project)

# # %% [markdown]
# # *Fetch RAWS*:

# # %%
# for project in projects:
#     app.run.fetch.gage.raws(project)

# # %% [markdown]
# # ## Run ETL Staging (by source)

# %% [markdown]
# ### Stage NLDAS (must be first)

# %%
for project in projects:
    app.run.stage.nldas(project)

# # %% [markdown]
# # ### Stage Prism

# # %%
# for project in projects:
#     app.run.stage.prism(project)

# # %% [markdown]
# # ### Stage Cimis

# # %%
# for project in projects:
#     app.run.stage.cimis(project)

# # %% [markdown]
# # ### **Natural break for manual qaqc**
# # > Inspect staged data and perform manual QA/QC before continuing.

# # %% [markdown]
# # ### Stage Gage

# # %%
# for project in projects:
#     app.run.stage.gage(project)

# # %% [markdown]
# # ## Run ETL Writing (by LSPC file type)

# # %% [markdown]
# # ### Write Prefiles

# # %%
# for project in projects:
#     app.run.write.pre(project)

# # %% [markdown]
# # ### Write Airfiles

# # %%
# for project in projects:
#     app.run.write.air(project)


