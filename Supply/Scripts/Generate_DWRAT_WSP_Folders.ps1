# Define the base directory (adjust this manually as needed)
$base_directory = "C:\Users\palemi"

# Define the SharePoint directory (common path for everyone)
$sharepoint_directory = "Water Boards\Supply and Demand Assessment - Documents\DWRAT\SDU_Runs\Full_DWRAT\WSP"

# Combine the base directory and SharePoint directory to get the target directory
$target_directory = Join-Path $base_directory $sharepoint_directory

# Define the DWRAT_WSP folders
$DWRAT_WSP_folders = @("2008-07_PVP_20", "2008-07_PVP_30", "2008-07_PVP_40", "2008-07_PVP_50", 
"2008-08_PVP_20", "2008-08_PVP_30", "2008-08_PVP_40", "2008-08_PVP_50", "2013-07_PVP_0", 
"2013-07_PVP_10", "2013-07_PVP_20", "2013-07_PVP_30", "2013-07_PVP_40", "2013-07_PVP_50", 
"2013-08_PVP_20", "2013-08_PVP_30", "2013-08_PVP_40", "2013-08_PVP_50", "2015-07_PVP_10",
 "2015-07_PVP_20", "2015-07_PVP_30", "2015-07_PVP_40", "2015-07_PVP_50", "2015-08_PVP_20",
  "2015-08_PVP_30", "2015-08_PVP_40", "2015-08_PVP_50", "2021-07_PVP_20", "2021-07_PVP_30",
   "2021-07_PVP_40", "2021-07_PVP_50", "2021-08_PVP_20", "2021-08_PVP_30", "2021-08_PVP_40",
    "2021-08_PVP_50", "2024-07_PVP_10", "2024-07_PVP_20", "2024-07_PVP_30", "2024-07_PVP_40", 
    "2024-07_PVP_50", "2024-08_PVP_10", "2024-08_PVP_20", "2024-08_PVP_30", "2024-08_PVP_40", 
    "2024-08_PVP_50")


# Create each folder
foreach ($i in $DWRAT_WSP_folders) {
    # Construct the folder path
    $folder_path= Join-Path -Path $target_directory -ChildPath $i

    # Create the child folder
    New-Item -Path $folder_path -ItemType Directory -Force | Out-Null
}

Write-Host "The folder structure was created successfully at $target_directory."
