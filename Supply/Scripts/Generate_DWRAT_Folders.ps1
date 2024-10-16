# Define the base directory (adjust this manually as needed)
$base_directory = "C:\Users\palemi"

# Define the base directory (adjust this manually as needed)
$base_directory = "C:\Users\palemi"

# Define the SharePoint directory (common path for everyone)
$sharepoint_directory = "Water Boards\Supply and Demand Assessment - Documents\DWRAT\SDU_Runs\Full_DWRAT\2017-2020_DD_Runs"

# Combine the base directory and SharePoint directory to get the target directory
$target_directory = Join-Path $base_directory $sharepoint_directory

# Change the active directory
Set-Location -Path $target_directory

# Define the date prefix and base folder name
$date_prefix = "2024-08"
$base_folder_name = "$date_prefix"

# Define the paths
$base_folder_path = Join-Path $target_directory $base_folder_name

# Define the subfolders for the 'Disconnected' folder
$sub_folders = @(
    "Disconnected\Historic\InputData",
    "Disconnected\Historic\OutputData",
    "Disconnected\Forecast\InputData",
    "Disconnected\Forecast\OutputData"
)

# Create the base folder
New-Item -Path $base_folder_path -ItemType Directory -Force

# Create the subfolders and README files
foreach ($sub_folder in $sub_folders) {
    $full_path = Join-Path $base_folder_path $sub_folder
    New-Item -Path $full_path -ItemType Directory -Force
    
    # Create README files in Historic and Forecast folders
    if ($sub_folder -match "Historic" -or $sub_folder -match "Forecast") {
        $readme_file_name = "${date_prefix}_README.docx"
        $readme_folder_path = (Split-Path $full_path -Parent)
        $readme_file_path = Join-Path $readme_folder_path $readme_file_name
        if (-not (Test-Path $readme_file_path)) {
            New-Item -Path $readme_file_path -ItemType File -Force
        }
    }
}

# Inform the user
Write-Output "Folder structure created at: $base_folder_path"
