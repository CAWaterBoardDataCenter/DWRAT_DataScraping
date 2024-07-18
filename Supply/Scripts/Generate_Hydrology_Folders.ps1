# Define the base directory (adjust this manually as needed)
$base_directory = "C:\Users\palemi"

# Define the SharePoint directory (common path for everyone)
$sharepoint_directory = "Water Boards\Supply and Demand Assessment - Documents\DWRAT\SDU_Runs\Hydrology"

# Combine the base directory and SharePoint directory to get the target directory
$target_directory = Join-Path $base_directory $sharepoint_directory

# Change the active directory
Set-Location -Path $target_directory

# Define the date prefix and base folder name
$date_prefix = "2024-07-30"
$base_folder_name = "$date_prefix"

# Define the paths
$base_folder_path = Join-Path $target_directory $base_folder_name

# Define the subfolders
$sub_folders = @("PRMS\InputData", "PRMS\OutputData", "SRP\InputData", "SRP\OutputData")

# Create the base folder
New-Item -Path $base_folder_path -ItemType Directory -Force

# Create the subfolders
foreach ($sub_folder in $sub_folders) {
    $full_path = Join-Path $base_folder_path $sub_folder
    New-Item -Path $full_path -ItemType Directory -Force
}

# Create the README file
$readme_file_name = "${date_prefix}_README.docx"
$readme_file_path = Join-Path $base_folder_path $readme_file_name
New-Item -Path $readme_file_path -ItemType File -Force

# Inform the user
Write-Output "Folder structure created at: $base_folder_path"
