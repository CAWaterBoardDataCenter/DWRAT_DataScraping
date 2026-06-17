# Before running through the procedure, 
# ensure that the working directory is correct

# The Russian River Workflow procedure is intended to be run from 
# SDA's DWRAT_DataScraping repository


if (!grepl("DWRAT_DataScraping$",
           getwd() |> normalizePath(winslash = "/"))) {
  
  paste0("Working Directory Issue\n\n",
         "The Russian River workflow is intended to be run through ",
         "the Supply R Project in SDA's \"DWRAT_DataScraping\" repository. ",
         "Please correct the working directory before proceeding.") |>
    strwrap(width = 0.99 * getOption("width")) |>
    paste0(collapse = "\n") |>
    stop()
  
}
