# -*- coding: utf-8 -*-
"""
Created on Wed Feb 19 10:16:51 2025

@author: vohararhi
"""
import os
import pandas as pd

from src.core.models import ProjectControl

def qc_remake_timeseries_automation(project: ProjectControl):
    input_workbook = project.storage.gage.candidate / "QCSpreadsheets" / "Ground_stations_Flagged_1_2_3.xlsx"
    input_path = project.storage.gage.candidate / "IntermediateFiles"
    output_path = project.storage.gage.candidate / "QCIntermediateFiles"
    output_path.mkdir(parents=True, exist_ok=True)

    df_master = pd.read_excel(input_workbook, sheet_name='Sheet1')
    df_master.iloc[:, 0] = pd.to_datetime(df_master.iloc[:, 0])

    pre_files = [f for f in os.listdir(input_path) if f.endswith('.pre')] #filtering only for files ending with .pre in the IntemediateFiles folder

    for filename in pre_files: 
        file_path = os.path.join(input_path, filename)
        df_temp = pd.read_csv(file_path, delimiter=",", header=None)
        df_temp[0] = pd.to_datetime(df_temp[0], errors="coerce")

        # Find the corresponding column in df_master that matches the filename
        matched_column = None
        for col in df_master.columns:
            if filename.rsplit('.', 1)[0] in col:  # Adjust condition as needed (e.g., `col == match_value`)
                matched_column = col
                break

        if matched_column:
            # Merge the matched column onto df_temp
            df_new = df_temp.merge(df_master[['Date', matched_column]], left_on=0, right_on="Date", how="inner")
            nan_count_og = df_new[1].isnull().sum()
            nan_count_new = df_new[filename.rsplit('.', 1)[0]].isnull().sum()
            change = nan_count_new-nan_count_og
            print(f"{change} records converted to NAN.")

            # Replace the second column in df_temp with the matched column from df_master
            df_new.iloc[:, 1] = df_new[matched_column]

            # Drop the matched column since we only need to replace the data, not add a new column
            df_new.drop(columns=[matched_column], inplace=True)
            df_new.drop(columns="Date", inplace=True)

            # Save updated file with the same filename in the output directory
            output_file_path = os.path.join(output_path, filename)
            df_new.to_csv(output_file_path, index=False, header=False)
        else:
            print(f"Warning: No matching column found in df_master for {filename}. Skipping.")
