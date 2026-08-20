# When running models, there are often input and output files that should be preserved

# These functions help with managing file archives


#' @title Copy a File to a Model Archive Sub-Folder
#' 
#' @description
#' Given the path to a file, this function copies that file to an archive directory. 
#' The same filename will be used, but the path will become 
#' `outputDirectory/model/subDir/filename`. 
#' 
#' @details
#' `outputDirectory` contains the path to the archive folder. Within this folder,
#' there should be a folder with the same name as `model`. Inside the model folder,
#' there should be folders with names like "Input" and "Output". `subDir` should contain
#' the name of the planned destination folder under the model folder. 
#' 
#' These three parameters help construct the planned destination path for the 
#' input file. Using [extract_filename()], the final piece is pulled from 
#' `inputPath`. After that, `copyFile` (a function still contained within 
#' the "Shared_Scripts" folder) is called to perform the file copy procedure.
#' 
#' Please note that the function is vectorized, so `inputPath` can contain one or
#' more filepaths. The destination folder will be the same in all cases. 
#' 
#' @usage copy_file_to_archive(inputPath, outputDirectory, 
#'                      model = "PRMS", subDir = "Input") 
#' 
#' @param inputPath A [character] vector containing one or more file paths.
#' 
#' @param outputDirectory A [character] string that contains the path to the 
#' archive folder.
#' 
#' @param model A [character] string for the name of the model folder 
#' (exactly as it appears in the archive folder). 
#' 
#' @param subDir A [character] string that contains the name of the sub-folder 
#' under `model` that will be the archive destination of the file. 
#' 
#' @returns NULL (invisibly)
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Example archive directory path
#' archiveDir <- "C:/Users/person/Archive"
#' 
#' # Example file
#' myFilePath <- "W2_Russian_River/Input/my_file.csv"
#' 
#' 
#' # Save the file to the "Output" folder under "PRMS" in 'archiveDir'
#' copy_file_to_archive(myFilePath, archiveDir, "PRMS", "Output")
#' 
#' # The copy's path will be "C:/Users/person/Archive/PRMS/Output/my_file.csv"
#' 
#' 
#' # Altneratively, save the file to the "Example" folder under "SRP" in 'archiveDir'
#' copy_file_to_archive(myFilePath, archiveDir, "SRP", "Example")
#' 
#' # The copy's path will be "C:/Users/person/Archive/SRP/Example/my_file.csv"
#' }
copy_file_to_archive <- function (inputPath, outputDirectory, model = "PRMS",
                                  subDir = "Input") {
  
  # Given a path to a file, copy it to a sub-folder of 'outputDirectory' 
  # with a similar filename
  
  
  # If 'inputPath' is NULL, end the function without doing anything
  if (is.null(inputPath)) {
    return(invisible(NULL))
  }
  
  
  # Otherwise, start by setting the output path
  
  # Modify 'inputPath' into a location within the model's sub-folder
  # in 'outputDirectory'
  outputPath <- paste0(outputDirectory, "/", model, "/", subDir, "/",
                       extract_filename(inputPath))
  
  
  # Copy the file
  copyFile(inputPath, outputPath, quietly = TRUE)
  
  
  # Return nothing
  return(invisible(NULL))
  
}
