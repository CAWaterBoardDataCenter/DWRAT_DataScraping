#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



#### SETUP ####



# Required libraries
require(shiny)
require(shinyjs)
require(data.table)
require(tidyverse)
require(readxl)
require(openxlsx)
require(sf)
require(mapview)
require(leaflet)
require(httr)
require(shinyFiles)
require(cli)
require(DT)



cat(col_green("Setting up app...\n\n\n"))



# Set options and clean environment 
options(viewer = NULL)
options(shiny.autoload.r = FALSE)
remove(list = ls())



# Load useful variables
# Import relevant functions and 'ws'
source("../[HELPER]_1_Shared_Functions.R")
source("../[HELPER]_3_Watershed_Selection.R")
source("../[HELPER]_5_GIS_Analysis_Functions.R")



# Read in the selected watershed's polygon 
Watershed_Polygon <- getGIS(ws, 
                            "WATERSHED_BOUNDARY_DATABASE_SHAREPOINT_PATH", 
                            "WATERSHED_BOUNDARY_LAYER_NAME") %>%
  st_transform("epsg:4326") %>%
  mutate(WATERSHED = names(ws)[2]) %>%
  select(WATERSHED)



# Using 'Watershed_Polygon', get the points that make up the boundary of this polygon
Watershed_Boundary_Points <- Watershed_Polygon %>%
  st_cast("POINT") %>%
  mutate(INDEX = row_number())



# Load in a statewide list of all PODs
# (From the eWRIMS POD table)
statewidePODList <- "Program Watersheds/7. Snowflake Demand Data Downloads/eWRIMS Flat File POD Subset/" %>% 
  makeSharePointPath() %>% 
  list.files(full.names = TRUE) %>% sort() %>% tail(1) %>%
  fileRead("read_csv") %>%
  mutate(LONGITUDE2 = as.numeric(LONGITUDE), LATITUDE2 = as.numeric(LATITUDE)) %>%
  filter(!is.na(LONGITUDE2)) %>% 
  unique() %>%
  st_as_sf(coords = c("LONGITUDE2", "LATITUDE2"), crs = "NAD83")



# Read in the flag table as well
flagDF <- readFlagTable()



cat(col_green("\n\n\nThe app will open in your default web browser shortly...\n\n\n"))



#### SHINY APP CODE ####


# UI variable
ui <- fluidPage(
  
  # External CSS File
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
  
  
  
  # Important header information
  tags$html(lang="en"), 
  tags$head(tags$title("GIS Manual Review")),
  
  
  
  useShinyjs(),
  
  
  
  # Application Title
  titlePanel(textOutput("ws_name")),
  
  
  
  # PART A
  div(id = "PART_A_FILE_UPLOAD", 
      
      HTML("<h3>Part 1: File Upload</h3>"),
      
      
      div(class = "PART_A_BOXES",
          HTML(paste0("<p id = 'intro1'>Hello, this tool will help you perform the GIS ",
                      "Manual Review for your watershed</p>")),
          HTML(paste0("<p id = 'intro2'>You'll follow a guided path for assessing ",
                      "whether a POD diverts from the watershed</p>")),), 
      
      
      div(class = "PART_A_BOXES",
          HTML(paste0("<p id = 'fileInstr_1'>To begin, please specify the filepath to ", 
                      "your manual review spreadsheet</p>")),
          HTML("<p id = 'fileInstr_2'>(Make sure you don't have that file open!)</p>")),
      
      
      HTML("<p class = 'bold'>Path to Manual Review Spreadsheet</p>"),
      shinyFilesButton("reviewSpreadsheet1", title = "Path to Manual Review Spreadsheet", label = "Select File...",
                       accept = ".xlsx", width = "40%", multiple = FALSE)),
  #fileInput("reviewSpreadsheet", "Path to Manual Review Spreadsheet", accept = ".xlsx", width = "40%")),
  
  
  
  # PART B
  div(id = "PART_B_EXIT_POINT",
      
      HTML("<h3>Part 2: Watershed Exit Point</h3>"),
      
      
      div(class = "PART_A_BOXES",
          
          HTML(paste0("<p>The 'Exit Point' is the point on the watershed boundary line ",
                      "that is closest to the main outlet of the watershed</p>")),
          
          HTML(paste0("<p>For this analysis, we distinguish between different points based ",
                      "on the numerical index ('INDEX') assigned to each point</p>"))),
      
      
      div(id = "ExitPointInfo", class = "PART_A_BOXES"),
          #textOutput("ExitPointDecision1"),
          
          #textOutput("ExitPointDecision2"),
          
          #textOutput("ExitPointDecision3")),
          
      
      
      
      div(class = "PART_A_BOXES",
          leafletOutput("exitPointPlot", width = "60%")),
      
      
      
      uiOutput("ExitPointSelectOut"),
      
      
      uiOutput("ExitPointSubmitOut"),
      
  ),
  
  
  
  div(id = "PART_C_MANUAL_REVIEW",
      
      HTML("<h3>Part 3: Manual Review</h3>"),
      
      
      div(class = "PART_A_BOXES",
          HTML(paste0("<div>",
                      "<p id = 'PART_C_INFO'>",
                      "This procedure involves reviewing data and documentation related to ",
                      "a water right and its points of diversion (PODs)</p>",
                      
                      "<p>We want to identify PODs that:</p>",
                      "<p>(1) Are located within this watershed AND</p>",
                      "<p>(2) Divert water from the watershed</p>",
                      
                      "<p id = 'PART_C_PURPOSE'>By reviewing all of these flagged PODs, we will determine ",
                      "which water rights are relevant to this watershed (and whose demand data we ",
                      "will consider in later steps)</p>",
                      "</div>"))
      ),
      
      
      div(id = "PART_C_LARGE_REVIEW_OCCURRED", class = "bold"),
      
      
      div(id = "PART_C_LARGE_REVIEW",
          
          HTML("<h4 class = 'redText bold'>Large Review Detected!</h4>"),
          
          
          div(id = "LargeReviewInfo", class = "PART_A_BOXES"),
          
          
          actionButton("LargeYes", "Yes", width = "20%", inline = TRUE),
          actionButton("LargeNo", "No", width = "20%", inline = TRUE),
          
          
          div(id = "LargeYesInfo", class = "PART_A_BOXES")
          
          
      ),
      
      
      div(id = "PART_C_MAIN_REVIEW",
          
          div(class = "PART_A_BOXES",
              HTML(paste0("<p id = 'PART_C_START'>",
                          "To get started with the review, choose a water right:</p>")),
              
              
              uiOutput("WaterRightSelectOut"),
              
              
              DTOutput("ReviewProgress", width = "40%"),
              
              
              div(id = "wrSectionHeader", class = "PART_A_BOXES slightPadding"),
              
              
              textOutput("rightPODs"),
              
              
              textOutput("relatedRights")),
          
          
          
          div(id = "PART_C_POD_MAP", class = "PART_A_BOXES",
              
              HTML(paste0("<h4>[1] POD Plot</h4>")),
              
              
              leafletOutput("podPlot", width = "60%"),
              
              
              div(id = "podPlotText"),
              
              
              DTOutput("podPlotTable", width = "80%")
              
              
          ),
          
          div(id = "PART_C_POD_DB_FIELDS", class = "slightPadding",
              
              HTML(paste0("<h4>[2] POD eWRIMS Database Fields</h4>")),
              
              
              div(id = "podDBInfo", class = "PART_A_BOXES"),
              
              
              DTOutput("podDBTable", width = "80%"),
              
              
              #uiOutput("podDBResOut"),
              
              checkboxInput("dbErrors", 
                            paste0("Check this box if the database values ",
                                   "require corrections"),
                            width = "70%")
              
          ),
          
          
          div(id = "PART_C_POD_WR_DOC", class = c("PART_A_BOXES", "slightPadding"),
              
              HTML(paste0("<h4>[3] 
                          
                          Water Right Documentation</h4>")),
              
              
              div(id = "wrDocumentationInfo"),
              
              checkboxInput("noDocumentation", 
                            paste0("Check this box if no documentation is ",
                                   "available"),
                            width = "60%")
              
              
          )
          
      ),
      
  ),
  
  
  
  HTML("<div id = 'ErrorDiv'></div>"),
  
  # 
  
  
  
  HTML("<p id = 'LoadingText'>Loading...</p>"),
  
  
  
  # Empty string that forces updates to UI
  textOutput("forceUpdates"),
  
  
  
  # External JS File
  tags$head(tags$script(src = "app.js"))
  
  
)



# Define server logic
server <- function(input, output, session) {
  
  
  # Hide all later stages of the application
  # (The user begins with "PART_A_FILE_UPLOAD")
  shinyjs::hide("PART_B_EXIT_POINT")
  shinyjs::hide("PART_C_MANUAL_REVIEW")
  shinyjs::hide("PART_C_LARGE_REVIEW")
  shinyjs::hide("PART_C_MAIN_REVIEW")
  shinyjs::hide("PART_C_POD_MAP")
  shinyjs::hide("PART_C_POD_DB_FIELDS")
  shinyjs::hide("PART_C_POD_WR_DOC")
  
  
  
  # Title text for the app
  output$ws_name <- renderText({
    paste0("GIS Manual Review for ", names(ws)[2])
  })
  
  
  
  # Initialize the review spreadsheet filepath as a reactive value
  filePath <- reactiveVal(NULL)
  
  
  
  # Next, perform additional variable setup
  
  
  # In a separate function, add in "PARTY_ID" information from the flag table
  # to the eWRIMS POD table
  statewidePODList <- statewidePODList %>%
    addPartyInfo(flagDF)
  
  
  
  shinyjs::hide("LoadingText")
  
  
  
  #### PART A ####
  
  
  # Review Spreadsheet File Input
  
  
  
  # List the paths from which a file can be specified
  rootVec <- c("OutputData" = "../../../OutputData/",
               "SharePoint" = makeSharePointPath(""),
               "Root" = "C:/")
  
  
  
  # Set up the file choosing dialog box
  shinyFileChoose(input, 'reviewSpreadsheet1', session = session, 
                  roots = rootVec,
                  filetypes = "xlsx")
  
  
  
  # Wait for input
  observeEvent(input$reviewSpreadsheet1, {
    
    # As the value of 'reviewSpreadsheet1' changes, update two reactive variables:
    # 'filePath' and 'fileData'
    
    
    
    # Parse the path given in 'reviewSpreadsheet1'
    path <- parseFilePaths(rootVec, input$reviewSpreadsheet1)[["datapath"]][1]
    
    
    
    # Update the reactive variables
    filePath(path)
    
    fileData()
    
  })
  
  
  
  # filePath <- eventReactive(input$reviewSpreadsheet1, {
  #   
  #   # 'filePath' is NULL while a user has not specified a file path
  #   # ('reviewSpreadsheet1' will have a length of 2 when a path is specified)
  #   
  #   if (length(input$reviewSpreadsheet1) < 2) {
  #     return(NULL)
  #   }
  #   
  #   
  #   
  #   # Build the filepath
  #   # paste0(rootVec[which(names(rootVec) ==
  #   #                        input$reviewSpreadsheet1[2]$root[1])[1]],
  #   #        input$reviewSpreadsheet1[1]$files$`0` %>% 
  #   #          unlist() %>% paste0(collapse = "/") %>%
  #   #          str_remove("^/"))
  #   parseFilePaths(rootVec, input$reviewSpreadsheet1)[["datapath"]][1]
  #   
  # })
  
  
  
  # File Input (PART A)
  fileData <- reactive({
    
    # Check for user input of a file
    #filePath <- input$reviewSpreadsheet
    
    # No input = NULL
    if (is.null(filePath()) || is.na(filePath())){
      return(NULL)      
    }
    
    
    
    shinyjs::show("LoadingText")
    
    
    
    # Check if the provided spreadsheet has a "REVIEW_SHEET" path
    if (!("REVIEW_SHEET" %in% sheets(loadWorkbook(filePath())))) {
      
      stopApp()
      
      stop(paste0("The spreadsheet is missing a 'REVIEW_SHEET' worksheet. ",
                  "Manual review spreadsheets output by '[WS]_1_GIS_Preprocessing.R' ",
                  "should contain this worksheet. Please close the app and try again ",
                  "with a proper spreadsheet."))
      
    }
    
    
    
    # Read in the "REVIEW_SHEET" table
    spreadsheetDF <- read_xlsx(filePath(), sheet = "REVIEW_SHEET")
    
    
    
    # Check for all required columns in 'spreadsheetDF'
    # Throw an error otherwise
    desiredColumns <- c("APPLICATION_NUMBER", "POD_ID", "URL", 
                        "LATITUDE", "LONGITUDE", "MTRS", "FFMTRS", 
                        "HUC_8_NAME", "HUC_12_NAME", 
                        "WATERSHED", "SOURCE_NAME", "TRIB_DESC", 
                        "FLAG_PLSS_MATCH_MTRS_OR_FFMTRS", 
                        "FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY", 
                        "FLAG_LESS_THAN_ONE_MILE_WITHIN_WATERSHED_BOUNDARY", 
                        "FLAG_MENTIONS_WATERSHED_IN_SOURCE_INFORMATION", 
                        "REPORT_LAT_LON_COORDINATES", "REPORT_NOR_EAS_COORDINATES", 
                        "REPORT_PLSS_DISPLACEMENT", 
                        "NOTES", "KEEP_OR_REMOVE_POD", 
                        "REPORT_NOR_EAS_AS_LAT_LON_COORDINATES", 
                        "REPORT_PLSS_DISPLACEMENT_AS_LAT_LON_COORDINATES", 
                        "CORRECTED_POD_COORDINATES")

    
    
    # Error Check
    if (sum(desiredColumns %in% names(spreadsheetDF)) != length(desiredColumns)) {
      
      stopApp()
      
      
      stop(paste0("Missing column(s) in the 'REVIEW_SHEET' worksheet of this spreadsheet\n\n",
                  "Please rerun '[WS]_1_GIS_Preprocessing.R' and ensure that the following ",
                  "column(s) appear in the output:\n  ",
                  desiredColumns[!(desiredColumns %in% names(spreadsheetDF))] %>% 
                    sort() %>% paste0(collapse = "\n  ")))
      
    }
    
    
    
    # Also verify that there are rows to review in the spreadsheet
    if (nrow(spreadsheetDF) == 0) {
      
      stopApp()
      
      
      stop(paste0("The 'REVIEW_SHEET' appears to be empty. Are there no flagged PODs?",
                  "\n\nPlease investigate '[WS]_1_GIS_Preprocessing.R' and the ", 
                  "spreadsheet"))
      
    }
    
    
    
    
    # if (spreadsheetDF %>% filter(is.na(KEEP_OR_REMOVE_POD)) %>% nrow() == 0) {
    #   
    #   stopApp()
    #   
    #   
    #   stop(paste0("The manual review already seems to be complete ",
    #               "for this spreadsheet. (All rows in 'KEEP_OR_REMOVE_POD' ",
    #               "have been filled in)\n\nIs this the wrong file?"))
    #   
    # }
    
    
    
    # If there's no error, proceed to the next step
    # (Part B is related to the exit point)
    if (is.null(input$ExitPointSelect)) {
      
      shinyjs::hide("PART_A_FILE_UPLOAD")
      shinyjs::show("PART_B_EXIT_POINT")
      
    }

    
    
    # Hide 'Loading..." text
    shinyjs::hide("LoadingText")
    
    
    
    # The value of 'fileData' will now be the table in 'spreadsheetDF'
    spreadsheetDF
    
  })
  
  
  
  #### PART B ####
  
  
  
  # Get a preliminary exit point for the watershed
  initExitPoint <- observe({
    
    # Check if a filepath was specified (Part A)
    reviewPath <- filePath()
    df <- fileData()
    
    
    
    if (is.null(df) || is.null(reviewPath) || !is.null(input$ExitPointSelect)) {
      return(NULL)
    }
    
    
    
    shinyjs::show("LoadingText")
    
    
    
    # Check if an exit point was already specified by the user
    # It would be located in the review spreadsheet
    if ("WS_EXIT_POINT" %in% sheets(loadWorkbook(reviewPath))) {
      
      
      # Read in the exit point worksheet
      exitDF <- read_xlsx(reviewPath, sheet = "WS_EXIT_POINT")
      
      
      
      # Check to ensure that all required columns appear in 'exitDF'
      desiredColumns <- c("POINT_INDEX", "DATABASE_PATH", "LAYER_PATH")
      
      
      
      # Error Check
      if (sum(desiredColumns %in% names(exitDF)) != length(desiredColumns)) {
        
        stopApp()
        
        stop(paste0("Missing column(s) in the 'WS_EXIT_POINT' worksheet of this spreadsheet\n\n",
                    "Please correct the worksheet and ensure that the following ",
                    "column(s) appear in the output:\n  ",
                    desiredColumns[!(desiredColumns %in% names(exitDF))] %>% 
                      sort() %>% paste0(collapse = "\n  ")))
        
      }
      
      
      
      # Filter to the current watershed
      exitDF <- read_xlsx(reviewPath, sheet = "WS_EXIT_POINT") %>%
        filter(DATABASE_PATH == getPath(ws, "WATERSHED_BOUNDARY_DATABASE_SHAREPOINT_PATH")) %>%
        filter(is.na(LAYER_PATH) | LAYER_PATH == getPath(ws, "WATERSHED_BOUNDARY_LAYER_NAME"))
      
      
      
      # If more than one match was found, throw an error
      # (This should not happen)
      if (nrow(exitDF) > 1) {
        
        stopApp()
        
        stop(paste0("The manual review spreadsheet's 'WS_EXIT_POINT' worksheet contains a critical error.\n\n",
                    "There should only be *ONE* row in this table that corresponds to the watershed's ",
                    "specified GIS boundary polygon ('WATERSHED_BOUNDARY_DATABASE_SHAREPOINT_PATH' and ",
                    "'WATERSHED_BOUNDARY_LAYER_NAME').\n\nPlease remove extra rows before running this ",
                    "application again for this watershed."))
        
        
        # If a match was found, proceed as normal
        # Map the exit point for the user and ask them to confirm the choice
        # Then wait for them to click on "Submit"
      } else if (nrow(exitDF) == 1) {
        
        
        # Get the exit point defined in 'exitDF'
        Exit_Point <- Watershed_Boundary_Points[exitDF$POINT_INDEX, ] %>%
          mutate(CHOSEN_INDEX = exitDF$POINT_INDEX)
        
        
        
        # Prepare the UI for selecting and confirming an exit point
        output$ExitPointSelectOut <- renderUI({
          selectInput("ExitPointSelect", "Select an Exit Point Index", 
                      1:nrow(Watershed_Boundary_Points), 
                      selected = exitDF$POINT_INDEX)
        })
        
        
        
        output$ExitPointSubmitOut <- renderUI({
          actionButton("ExitPointSubmit", "Submit", inline = TRUE)
        })
        
        
        
        # Output the text as well
        html("ExitPointInfo",
             paste0("<p id = 'ExitPointDecision1'>",
                    "An approximate exit point has already been specified for ",
                    "this watershed (Index ", exitDF$POINT_INDEX, ")</p>",
                    "<p id = 'ExitPointDecision2'>",
                    "Please double-check whether it is accurate and click on the ",
                    "'Submit' button to continue with the manual review</p>"))
        
        
        
        shinyjs::hide("LoadingText")
        
        
        
        # Finally, prepare a plot
        exitPlot <- mapview(Watershed_Polygon, col.regions = "darkgray", popup = NULL) + 
          mapview(Watershed_Boundary_Points, popup = "INDEX", 
                  col.regions = "black", color = "black", burst = FALSE) + 
          mapview(Exit_Point, col.regions = "green", popup = "CHOSEN_INDEX", label = "CHOSEN_INDEX")
        
        
        
        # Output the plot
        output$exitPointPlot <- renderLeaflet({
          exitPlot@map
        })
        
        
        
        # Hide the loading text
        shinyjs::hide("LoadingText")
        
        
        
        # Return the exit point index value as 'initExitPoint'
        return(exitDF$POINT_INDEX[1])
        
      }
      
    }
    
    
    
    # Two different scenarios can reach this point
    # (1) There is no "WS_EXIT_POINT" sheet in the spreadsheet
    # (2) "WS_EXIT_POINT" exists, but the specified watershed boundary
    #       is not mentioned there
    
    
    
    # In both cases, the user MUST specify an exit point for this watershed boundary
    
    
    
    # Use USGS StreamStats to try and help them find the exit point
    if (is.null(input$ExitPointSelect)) {
      
      
      withProgress(message = "Trying to guess the exit point location...Please wait...", value = 0,
                   {
                     # Start by trying to help them identify the exit point
                     # Do this automatically by checking three PODs' flow paths
                     # using USGS StreamStats and comparing where they exit
                     
                     
                     
                     incProgress(0.05, detail = "Getting ready...")
                     
                     
                     
                     # Get a spatial object based on 'df'
                     # Filter to PODs that are at least one mile within the watershed boundary
                     spatialDF <- df %>%
                       filter(FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY == TRUE) %>%
                       mutate(LAT2 = as.numeric(LATITUDE),
                              LON2 = as.numeric(LONGITUDE)) %>%
                       st_as_sf(coords = c("LON2", "LAT2"), crs = "NAD83")
                     
                     
                     
                     # If no rows are available, use the centroid of 'Watershed_Polygon'
                     if (is.null(nrow(spatialDF)) || nrow(spatialDF) == 0) {
                       
                       spatialDF <- st_centroid(Watershed_Polygon)
                       
                       # Otherwise , select 3 random POD rows
                     } else {
                       
                       # (If fewer than 3 rows are available, use as many rows as available)
                       spatialDF <- spatialDF[sample(nrow(spatialDF), 
                                                     min(3, nrow(spatialDF)), 
                                                     replace = FALSE), ]
                       
                     }
                     
                     
                     
                     # Prepare a vector of chosen indices
                     chosenIndex <- vector(mode = "list", length = nrow(spatialDF))
                     
                     
                     
                     # Iterate through each of the selected points
                     for (i in 1:nrow(spatialDF)) {
                       
                       
                       
                       incProgress(0.25, detail = paste0("Querying USGS StreamStats...[", 
                                                         i, "/", nrow(spatialDF), "]"))
                       
                       
                       
                       # Send a query to USGS StreamStats about the POD
                       # Get its flowpath
                       flowRes <- requestFlowPath(spatialDF[i, ])
                       
                       
                       
                       # Calculate the distances between every point of 
                       # 'Watershed_Boundary_Points' and 'flowRes'
                       distMat <- st_distance(Watershed_Boundary_Points, flowRes)
                       
                       
                       
                       # Get the row index containing the minimum distance in 'distMat'
                       # (The row indices correspond to 'Watershed_Boundary_Points')
                       # The POD flow path and watershed boundary point likely intersect
                       # at the location with the minimum distance in 'distMat'
                       # The boundary point identified here is likely the outlet of the watershed
                       chosenIndex[[i]] <- which(distMat == min(distMat), arr.ind = TRUE)[1, 1]
                       
                       
                       
                       Sys.sleep(0.2)
                       
                     }
                     
                     
                     
                     incProgress(0.05, detail = "Performing final calculations...")
                     
                     
                     
                     # Get the index of the likely exit point
                     exitIndex <- names(table(unlist(chosenIndex)))[table(unlist(chosenIndex)) == 
                                                                      max(table(unlist(chosenIndex)))][1] %>%
                       as.numeric()
                     
                     
                     
                     incProgress(0.05, detail = "Performing final calculations...")
                     
                     
                     
                     Possible_Exit_Point <- Watershed_Boundary_Points[exitIndex, ] %>%
                       mutate(POSSIBLE_INDEX = exitIndex)
                     
                     
                     
                     setProgress(1, detail = "Done!")
                     
                   })
      
      
      
      # Prepare the UI for selecting and confirming an exit point
      output$ExitPointSelectOut <- renderUI({
        selectInput("ExitPointSelect", "Select an Exit Point Index", 
                    1:nrow(Watershed_Boundary_Points), 
                    selected = Possible_Exit_Point)
      })
      
      
      
      output$ExitPointSubmitOut <- renderUI({
        actionButton("ExitPointSubmit", "Submit", inline = TRUE)
      })
      
      
      
      # Include accompanying text as well
      html("ExitPointInfo",
           paste0("<p id = 'ExitPointDecision1'>", 
                  "Please examine the map and locate the watershed outlet. ",
                  "Note the index of the point closest to the outlet in the ",
                  "input box provided below.</p>",
                  "<p id = 'ExitPointDecision2'>",
                  "This tool tried to locate the exit point and identified Point ", 
                  exitIndex, ". Please verify if this is accurate and correct the ",  
                  "value if needed.</p>",
                  "<p id = 'ExitPointDecision3'>",
                  "Once you are ready to proceed with the review, please click ",
                  "the 'Submit' button.</p>"))
      
      
      
      # Finally, prepare a map for the user
      exitPlot <- mapview(Watershed_Polygon, col.regions = "darkgray", popup = NULL) + 
        mapview(Watershed_Boundary_Points, popup = "INDEX", 
                col.regions = "black", color = "black", burst = FALSE) + 
        mapview(Possible_Exit_Point, col.regions = "orange", 
                popup = "POSSIBLE_INDEX", label = "POSSIBLE_INDEX")
      
      
      
      output$exitPointPlot <- renderLeaflet({exitPlot@map})
      
      
      
      # Hide the "Loading..." text
      shinyjs::hide("LoadingText")
      
      
      
      # Set the exit point guess as 'initExitPoint'
      return(exitIndex)
      
    }
    
    
    
    shinyjs::hide("LoadingText")
    
  })
  
  
  
  # Submission of Exit Point
  # Update the manual review spreadsheet and proceed to PART C
  observeEvent(input$ExitPointSubmit, {
    
    
    
    shinyjs::show("LoadingText")
    
    
    
    # Check the user's selection for the Exit Point
    selectVal <- input$ExitPointSelect
    
    
    
    # Do nothing if 'selectVal' is empty
    if (is.null(selectVal) || selectVal == "" | is.null(filePath())) {
      return()
    }
    
    
    
    # Update the manual review sheet's "WS_EXIT_POINT" worksheet with this selection
    # (But if the sheet doesn't exist, create it first)
    
    
    
    # Load in the workbook
    wb <- loadWorkbook(filePath())
    
    
    
    # Prepare a data frame with the relevant exit point information
    exitDF <- data.frame(POINT_INDEX = as.numeric(selectVal),
                         DATABASE_PATH = getPath(ws, "WATERSHED_BOUNDARY_DATABASE_SHAREPOINT_PATH"),
                         LAYER_PATH = if_else(is.na(getPath(ws, "WATERSHED_BOUNDARY_LAYER_NAME")),
                                              NA_character_,
                                              getPath(ws, "WATERSHED_BOUNDARY_LAYER_NAME")))
    
    
    
    # Check if "WS_EXIT_POINT" exists in the spreadsheet
    if ("WS_EXIT_POINT" %in% sheets(wb)) {
      
      # If the worksheet DOES exist, read in the existing table
      # Append those rows to 'exitDF'
      # (But do not include rows whose "DATABASE_PATH" and "LAYER_PATH" match
      # the current record in 'exitDF')
      existingDF <- read_xlsx(filePath(),
                              sheet = "WS_EXIT_POINT") %>%
        select(POINT_INDEX, DATABASE_PATH, LAYER_PATH) %>%
        filter(!(DATABASE_PATH %in% exitDF$DATABASE_PATH[1] &
                   LAYER_PATH %in% exitDF$LAYER_PATH[1]))
      
      
      
      exitDF <- exitDF %>%
        bind_rows(existingDF)
      
      
      
      # Then, remove the worksheet
      removeWorksheet(wb, "WS_EXIT_POINT")
      
    }
    
    
    
    # Add a blank worksheet titled "WS_EXIT_POINT"
    addWorksheet(wb, "WS_EXIT_POINT")
    
    
    
    # Write 'exitDF' to this new worksheet
    writeData(wb, "WS_EXIT_POINT", exitDF)
    
    
    
    # Save the workbook
    updateReviewSpreadsheet(wb, filePath())
    
    
    
    # Include additional checks for failure
    if (!("WS_EXIT_POINT" %in% sheets(loadWorkbook(filePath()))) ||
        nrow(exitDF) != nrow(read_xlsx(filePath(), sheet = "WS_EXIT_POINT")) ||
        ncol(exitDF) != ncol(read_xlsx(filePath(), sheet = "WS_EXIT_POINT")) ||
        sum(exitDF == read_xlsx(filePath(), sheet = "WS_EXIT_POINT")) != 
        nrow(exitDF) * ncol(exitDF)) {
      
      
      # Output an error message
      html("ErrorDiv",
           paste0("<p><span class = 'bold'>Error: </span>Failed to update the ",
                  "manual review spreadsheet</p>",
                  "<p>Do you have the file open? Alternatively, another program ", 
                  "could be using the file.</p>",
                  "<p>Please ensure that the spreadsheet is completely closed ", 
                  "and try again.</p>"))
      
      
      # Stop the app
      stopApp()
      
      
      
      # Output the same error message above to the console
      stop(paste0("Failed to update the manual review spreadsheet\n\n",
                  "Do you have the file open? Alternatively, another program could be using ",
                  "the file. Please ensure that the spreadsheet is completely closed and try again."))
      
    }
    
    
    
    # Prepare for the next operations (PART C + a special check for large reviews)
    # Get the review spreadsheet's data
    sheetDF <- fileData()
    
    
    
    shinyjs::hide("LoadingText")
    
    
    
    # Hide PART B and begin PART C
    shinyjs::hide("PART_B_EXIT_POINT")
    shinyjs::show("PART_C_MANUAL_REVIEW")
    
    
    
    #### PART C SPECIAL ####
    
    # For large reviews, offer the user the ability to mark every POD
    # that is one mile or more within the watershed as "KEEP"
    if (sheetDF %>%
        filter(is.na(KEEP_OR_REMOVE_POD)) %>%
        select(APPLICATION_NUMBER) %>% unique() %>%
        nrow() > 300) {
      
      shinyjs::show("PART_C_LARGE_REVIEW")
      
      
      html("LargeReviewInfo",
           paste0("<p>This manual review spreadsheet contains ",
                  sheetDF %>% filter(is.na(KEEP_OR_REMOVE_POD)) %>%
                    select(APPLICATION_NUMBER) %>% unique() %>% nrow(),
                  " water rights with PODs that need to be reviewed</p>",
                  "<p>Ideally, we would want to review every POD for accuracy, ",
                  "but a significant time investment would be required in this case</p>",
                  "<p id = 'BULK_QUESTION'>Would you like to <span class = 'redText bold'>'bulk approve'",
                  "</b></span> all PODs whose eWRIMS coordinates place them <i>one mile or ",
                  "more</i> within the watershed boundaries?</p>",
                  "<p>This would affect ", 
                  sheetDF %>% filter(is.na(KEEP_OR_REMOVE_POD)) %>% 
                    filter(FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY) %>% nrow(),
                  " PODs (across ", 
                  sheetDF %>% filter(is.na(KEEP_OR_REMOVE_POD)) %>% 
                    filter(FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY) %>%
                    select(APPLICATION_NUMBER) %>% unique() %>% nrow(),
                  " water rights)</p>"))
      
      
      
      html("LargeYesInfo",
           paste0("<p>If you click 'Yes', you are making the following assumptions:</p>",
                  "<p>(1) These PODs divert from the watershed</p>",
                  "<p>(2) These PODs are all correctly plotted in eWRIMS</p>"
           ))
      
      
      
    # If no large review option is offered, proceed straight to the main review
    } else {
      
      #### PART C (MAIN REVIEW START PATH 1) ####
      partC_MainReview_Start(sheetDF, output)
      
    }
    
  })
  
  
  
  # Large Review Button Click - "YES"
  observeEvent(input$LargeYes, {
    
    # The user clicked "Yes"
    # Bulk approve the PODs that are:
    #   (1) unreviewed AND 
    #   (2) one mile or more within the watershed
    
    
    
    shinyjs::show("LoadingText")
    
    
    
    # Get the review spreadsheet 
    reviewDF <- fileData()
    
    
    
    # Approve all PODs that are unreviewed and one mile or more within the watershed
    # (Update the "NOTES" column as well)
    reviewDF$NOTES[is.na(reviewDF$KEEP_OR_REMOVE_POD) & 
                                  reviewDF$FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY] <- paste0("Assumed that the POD diverts from ",
                                                                                                      "the watershed and that its database ",
                                                                                                      "coordinates are correct")
    
    
    
    reviewDF$KEEP_OR_REMOVE_POD[is.na(reviewDF$KEEP_OR_REMOVE_POD) & 
                                  reviewDF$FLAG_ONE_MILE_OR_MORE_WITHIN_WATERSHED_BOUNDARY] <- "KEEP"
    
    
    
    # Load in the entire workbook
    wb <- loadWorkbook(filePath())
    
    
    
    # Update the "REVIEW_SHEET" worksheet with these new changes
    writeData(wb, "REVIEW_SHEET", reviewDF)
    
    
    
    # Save the spreadsheet
    updateReviewSpreadsheet(wb, filePath())
    
    
    
    # 'fileData' needs to be updated now
    # This script's approach is kinda hacky
    
    
    # Temporarily save the path in 'filePath' elsewhere
    # And set 'filePath' equal to 'NULL'
    # Then, assign the original path back to 'filePath'
    tempVal <- filePath()
    filePath(NULL)
    filePath(tempVal)
    
    
    
    # This will trigger an update in 'fileData'
    
    
    
    # Obtain an updated version of the review spreadsheet
    reviewDF <- fileData()
    
    
    
    # Hide the "Loading..." text that appeared while reading in 'reviewDF'
    shinyjs::hide("LoadingText")
    
    
    
    html("PART_C_LARGE_REVIEW_OCCURRED",
         paste0("<p>PODs that are at least one mile within the watershed ",
                "were successfully marked with 'KEEP'!\n\nPlease review any ",
                "remaining PODs</p>"))
    
    
    #### PART C (MAIN REVIEW START PATH 2) ####
    
    
    
    # Hide the "Large Review" section and move forward with the main review
    shinyjs::hide("LoadingText")
    shinyjs::hide("PART_C_LARGE_REVIEW")
    
    partC_MainReview_Start(reviewDF, output)
    
  })
  
  
  
  observeEvent(input$LargeNo, {
    
    # The user clicked "No"
    # No bulk approval of PODs will occur
    # Therefore, proceed straight to the main review
    
    
    #### PART C (MAIN REVIEW START PATH 3) ####
    
    
    # Hide the "Large Review" section and move forward with the main review
    shinyjs::hide("PART_C_LARGE_REVIEW")
    
    partC_MainReview_Start(fileData(), output)
    
  })
  
  
  
  # State the number of PODs owned by the selected right
  output$rightPODs <- renderText({
    
    if (is.null(input$WaterRightSelect) || 
        length(input$WaterRightSelect) == 0 ||
        input$WaterRightSelect == "") {
      
      return("")
      
    }
    
    
    
    reviewDF <- fileData()
    
    
    
    # Set the current section's header
    html("wrSectionHeader",
         paste0("<h4 class = 'bold'>Reviewing ", 
                input$WaterRightSelect, "</h4>"))
    
    
    
    # Show the "PART_C_POD_MAP" div to get that portion started too
    shinyjs::show("PART_C_POD_MAP")
    
    
    
    # Return a simple string if the water right has only one POD
    if (statewidePODList %>%
        filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
        nrow() == 1) {
      
      return("This water right has only 1 associated POD")
      
    }
    
    
    
    # For water rights with more than one POD, use a somewhat different string
    # There will be two sentences related to the number of PODs
    # Initiate the first one here:
    finalStr <- paste0("This water right has ",
                       statewidePODList %>%
                         filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
                         nrow(),
                       " PODs. ")
    
    
    
    # Compare the number of PODs in 'reviewDF' and 'statewidePODList'
    # This will affect the rest of the message
    if (reviewDF %>% 
        filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
        nrow() != statewidePODList %>%
        filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
        nrow()) {
      
      
      return(paste0(finalStr,
                    reviewDF %>% 
                      filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
                      nrow(),
                    if_else(reviewDF %>% 
                              filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
                              nrow() == 1, " POD (", " PODs ("),
                    reviewDF %>% 
                      filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
                      select(POD_ID) %>% unique() %>% unlist(use.names = FALSE) %>%
                      sort() %>%
                      paste0(collapse = ", ") %>%
                      if_else(str_count(., ",") == 1, 
                              str_replace(", ", " and "), 
                              if_else(str_count(., ",") > 1,
                                      str_replace(", (\\w+)$", ", and \\1"),
                                      .)),
                    ") ",
                    if_else(reviewDF %>% 
                              filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
                              nrow() == 1, "was", "were"),
                    " flagged for this watershed by the scripts."))
      
    } else {
      
      return(paste0(finalStr,
                    "All of them were flagged for this watershed by the scripts."))
      
    }
    
  })
  
  
  
  # State information about related water rights as well
  output$relatedRights <- renderText({
    
    if (is.null(input$WaterRightSelect) || 
        length(input$WaterRightSelect) == 0 ||
        input$WaterRightSelect == "") {
      
      return("")
      
    }
    
    
    
    reviewDF <- fileData()
    
    
    
    # Get the "PARTY_ID" of the selected water right
    selectedParty <- statewidePODList %>%
      st_drop_geometry() %>%
      filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
      select(PARTY_ID) %>%
      unique() %>% unlist(use.names = FALSE) %>%
      str_split(";") %>% unlist() %>%
      sort()
    
    
    
    # Get the water right(s) related to this "PARTY_ID"
    relevantRights <- statewidePODList %>%
      st_drop_geometry() %>%
      filter(PARTY_ID %in% selectedParty) %>%
      filter(APPLICATION_NUMBER != input$WaterRightSelect) %>%
      select(APPLICATION_NUMBER) %>% unique() %>%
      unlist(use.names = FALSE) %>% 
      sort()
    
    
    
    # If 'relevantRights' is empty, output no information
    if (length(relevantRights) == 0) {
      return("")
    }
    
    
    
    # Otherwise, prepare a text string related to the related rights
    if (length(relevantRights) == 1) {
      
      finalStr <- paste0("There is 1 other ",
                         "water right related to ", input$WaterRightSelect, 
                         " (same owner):\n      ",
                         relevantRights[1])
      
    } else {
      
      finalStr <- paste0("There are ", length(relevantRights), " other ",
                         "water rights related to ", input$WaterRightSelect, 
                         " (same owner):\n      ",
                         relevantRights %>% paste0(collapse = "\n      "))
      
    }
    
    
    
    # Check if any of 'relevantRights' appear in the review sheet
    # If so, add another sentence related to that
    if (sum(relevantRights %in% reviewDF[["APPLICATION_NUMBER"]]) ==
        length(relevantRights)) {
      
      if (length(relevantRights) == 1) {
        
        finalStr <- paste0(finalStr,
                           "\n(That right was also flagged ", 
                           "for this manual review)")
        
      } else {
        
        finalStr <- paste0(finalStr,
                           "\n(All of these rights were also flagged ", 
                           "for this manual review)")
        
      }
      
      
    } else if (sum(relevantRights %in% reviewDF[["APPLICATION_NUMBER"]]) == 1) {
      
      finalStr <- paste0(finalStr,
                         "\n(", 
                         sum(relevantRights %in% reviewDF[["APPLICATION_NUMBER"]]),
                         " of these rights was also flagged for this manual review)")
      
    } else if (sum(relevantRights %in% reviewDF[["APPLICATION_NUMBER"]]) > 0) {
      
      finalStr <- paste0(finalStr,
                         "\n(", 
                         sum(relevantRights %in% reviewDF[["APPLICATION_NUMBER"]]),
                         " of these rights were also flagged for this manual review)")
      
    } else {
      
      finalStr <- paste0(finalStr,
                         "\n(None of these ", 
                         "rights were flagged in this manual review)")
      
    }
    
    
    
    return(finalStr)
    
  })
  
  
  
  # Map them
  output$podPlot <- renderLeaflet({
    
    if (is.null(input$WaterRightSelect) || 
        length(input$WaterRightSelect) == 0 ||
        input$WaterRightSelect == "") {
      
      return(NULL)
      
    }
    
    
    
    shinyjs::show("LoadingText")
    
    
    
    # Get the PODs associated with the chosen water right
    podDF <- statewidePODList %>%
      filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
      arrange(POD_ID)
    
    
    
    # Set up a progress bar for the query process
    barMessage <- paste0("Getting the flow path",
                         if_else(nrow(podDF) > 1, "s", ""),
                         " for this right's POD",
                         if_else(nrow(podDF) > 1, "s", ""),
                         " (according to the eWRIMS coordinates)")
    
    
    
    withProgress(message = barMessage, value = 0,
                 {
                   
                   # Prepare a variable to hold the flow paths for each POD
                   flowLines <- vector(mode = "list", length = nrow(podDF))
                   
                   
                   
                   incProgress(0.05, detail = "Getting started...")
                   
                   
                   
                   # Send queries to USGS StreamStats about their flowpaths
                   for (i in 1:nrow(podDF)) {
                     
                     
                     incProgress(0, detail = paste0("Querying USGS StreamStats...[", 
                                                       i, "/", nrow(podDF), "]"))
                     
                     
                     
                     # Obtain flowpath points from USGS StreamStats for each POD
                     flowPoints <- requestFlowPath(podDF[i, ])
                     
                     
                     
                     # If "LINEID" is not in the output,
                     # there was an issue while querying StreamStats
                     while (!("LINEID" %in% names(flowPoints))) {
                       
                       html("ErrorDiv",
                            paste0("<p><span class = 'bold'>Error: </span>Failed to get a response ",
                                   "from USGS StreamStats</p>",
                                   "<p>This tool will automatically wait 3 minutes before trying ", 
                                   "again.</p>",
                                   "<p>Otherwise, you may quit the app and try again later.</p>", 
                                   "<p>(This issue can sometimes happen due to problems with ",
                                   "USGS's server.</p>",
                                   "<p>Alternatively, if they receive too many requests ",
                                   "in a period of time, a cooldown period is enforced.)</p>"))
                       
                       
                       
                       incProgress(0, detail = paste0("Query failed...See error message at ",
                                                      "the bottom of the page...Will retry ",
                                                      "around ", 
                                                      format(Sys.time() + 3, format = "%I:%M"),
                                                      "..."))
                       
                       
                       
                       # Wait 3 minutes
                       Sys.sleep(60 * 3)
                       
                       
                       
                       # Try again to obtain flowpath points from USGS StreamStats
                       flowPoints <- requestFlowPath(podDF[i, ])
                       
                       
                       
                       # Clear out the message the error message
                       html("ErrorDiv", "")
                       
                       
                       
                       # Update the progress bar too
                       incProgress(0, detail = paste0("Querying USGS StreamStats...[", 
                                                      i, "/", nrow(podDF), "]"))
                       
                     }
                     
                     
                     
                     # Convert these points into linestrings and save them in 'flowLines'
                     flowLines[[i]] <- flowPoints %>%
                       group_by(LINEID) %>%
                       summarize(COUNTS = n(), do_union = FALSE) %>%
                       st_cast("LINESTRING")
                     
                     
                     
                     # Wait a little extra
                     Sys.sleep(0.2)
                     
                     
                     
                     # Increment progress
                     # (This loop will add at most 80% of the progress bar)
                     incProgress(0.8 * 1/nrow(podDF), detail = paste0("Querying USGS StreamStats...[", 
                                                                      i, "/", nrow(podDF), "]"))
                     
                   }
                   
                   
                   
                   setProgress(value = 1, detail = "Preparing map...")
                   
                 })
    
    
    
    # Prepare the layers of the output plot
    Water_Right_PODs <- podDF %>%
      select(APPLICATION_NUMBER, POD_ID)
    
    
    
    # Make a custom popup table for the PODs and flow paths
    popupHTML <- Water_Right_PODs %>% 
      st_drop_geometry() %>% 
      mutate(TEMP = paste0("<table class='mapview-popup'>", 
                           "<tbody>",
                           "<tr><th>APPLICATION_NUMBER&emsp;</th>",
                           "<td align='right'>", APPLICATION_NUMBER, "&emsp;</td></tr>",
                           "<tr><th>POD_ID&emsp;</th>",
                           "<td align='right'>", POD_ID, "&emsp;</td></tr>",
                           "</tbody></table>")) %>% 
      select(TEMP) %>% unlist(use.names = FALSE)
    
    
    
    # Add the watershed boundary and the PODs
    plot <- mapview(Watershed_Polygon, col.regions = "darkgray", popup = NULL) +
      mapview(Water_Right_PODs, col.regions = "orange", popup = popupHTML, label = "POD_ID") +
      mapview(Watershed_Boundary_Points[as.numeric(input$ExitPointSelect), ] %>%
                mutate(INDEX = as.numeric(input$ExitPointSelect)) %>%
                select(INDEX), 
              layer.name = "Exit_Point",
              col.regions = "green", popup = "INDEX", label = "INDEX")
    
    
    
    for (i in 1:length(flowLines)) {
      
      plot <- plot +
        mapview(flowLines[[i]], 
                color = "#0000ff",
                popup = popupHTML %>%
                  str_subset(paste0(">", podDF$POD_ID[i], "&")) %>%
                  head(1),
                label = paste0(podDF$POD_ID[i], " (Segment ", flowLines[[i]]$LINEID, ")"),
                layer.name = paste0("POD_Flow_Path_", podDF$POD_ID[i]))
      
    }
    
    
    
    # Check if any of the POD flow paths are within 100 meters of the watershed's exit point
    exitOverlap <- flowLines %>%
      map_lgl(exitPointCheck, 
              exitPoint = Watershed_Boundary_Points[as.numeric(input$ExitPointSelect), ])
    
    
    
    # Output information about the results to 'podPlotText'
    output$podPlotText <- html("podPlotText",
                               paste0("<p>The map above contains the POD",
                                      if_else(nrow(podDF) > 1, "s", ""),
                                      " associated with ",
                                      input$WaterRightSelect, ". The flow path",
                                      if_else(nrow(podDF) > 1, "s", ""),
                                      " (according to the eWRIMS ",
                                      "coordinates and USGS StreamStats) ",
                                      if_else(nrow(podDF) > 1, "are", "is"),
                                      " also included.</p>",
                                      "<p>The table below summarizes whether a POD flow path contains ",
                                      "a segment that is within 100 meters of the watershed exit point.</p>",
                                      "<p>Wherever 'YES' is listed, this <i>may</i> indicate that the POD ",
                                      "pulls water from the watershed. However, if more information about ",
                                      "the POD is available, we should first consider that as well.</p>"))
    
    
    
    # Add a table that contains 'exitOverlap'
    output$podPlotTable <- renderDT({
      
      Water_Right_PODs %>%
        st_drop_geometry() %>%
        mutate(WITHIN_100_METERS_OF_WS_EXIT_POINT = 
                 if_else(exitOverlap, "YES", "NO")) %>%
        datatable(options = list("searching" = FALSE, "pageLength" = nrow(Water_Right_PODs), 
                                 "lengthChange" = FALSE, "paging" = FALSE, "info" = FALSE),
                  rownames = FALSE) %>%
        formatStyle('WITHIN_100_METERS_OF_WS_EXIT_POINT',
                    color = styleEqual(c("YES", "NO"), c("#006100", "#9E0000")))
      
    })
    
    
    
    # Remove "Loading..."
    shinyjs::hide("LoadingText")
    
    
    
    # Unhide the next part of the review process as well to get that started
    shinyjs::show("PART_C_POD_DB_FIELDS")
    
    
    
    # Finally, render the map
    return(plot@map)
    
  })
  
  
  
  
  # Output a table containing the "WATERSHED", "TRIB_DESC", and "SOURCE_NAME" fields
  output$podDBTable <- renderDT({
    
    
    if (is.null(input$WaterRightSelect) || 
        length(input$WaterRightSelect) == 0 ||
        input$WaterRightSelect == "") {
      
      return(NULL)
      
    }
    
    
    
    # Unhide the next section
    shinyjs::show("PART_C_POD_WR_DOC")
    
    
    
    # Output information relevant to the section
    html("podDBInfo",
         paste0("<p>These are eWRIMS database fields related to the water ",
                "sources of PODs.</p>",
                "<p>Ideally, the watershed, its sub-basins, and/or its ",
                "tributaries should be mentioned here if the POD diverts ",
                "from that watershed.</p>",
                "<p>There are inaccuracies and missing data in the database, ",
                "but this information may still be useful.</p>",
                "<p>(Also, if you notice any errors, please submit a correction)</p>"))
    
    
    
    # Include information about the next section as well
    docURL <- fileData() %>%
      filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
      select(URL) %>% unique() %>% head(1) %>%
      unlist(use.names = FALSE)
    
    
    
    html("wrDocumentationInfo",
         paste0("<p>Most water rights' paper records have been digitized. ",
                "Click on the link below to download it:</p>",
                "<a href = '", docURL, "'>", docURL, "</a>",
                "<p>If no file is downloaded, the documentation has not yet been scanned</p>",
                "<p>In that case, mark the checkbox below</p>")) 
                #"<p>By heading to the Records Room, you can request to view these materials</p>"))
    
    
    
    # Finally, output a table with database information
    statewidePODList %>%
      st_drop_geometry() %>%
      filter(APPLICATION_NUMBER == input$WaterRightSelect) %>%
      select(APPLICATION_NUMBER, POD_ID, 
             WATERSHED, SOURCE_NAME, TRIB_DESC, HUC_8_NAME, HUC_12_NAME)
    
  },
  options = list("searching" = FALSE, #"pageLength" = 3, 
                 "lengthChange" = FALSE, "paging" = FALSE, "info" = FALSE),
  rownames = FALSE)
  
  
  
  
  
  # Add a disclaimer that the fields can be incorrect
  
  # User can select a "Yes"/"No" 
  # Based on these database fields, 
  # does the POD seem to divert from the watershed
  
  
  # Link to the URL for downloading records
  
  # Have a box to check if no records are available
  # (If marked, use only the eWRIMS coordinates)
  # (Extra text should appear about that)
  
  
  # Provide options for what appears in the report:
  # Lat/Lon
  # DMS
  # Northing/Easting
  # PLSS Displacement
  # Other (Not Handled by the App) (like Parcel Number)
  
  
  
  # Each checkmark reveals a particular section
  
  
  # Specify one or more of each of the coordinate methods
  # (Option to add more of each check type)
  
  
  # Section for verifying that eWRIMS coordinates are correct
  
  
  
  
  # output$forceUpdates <- renderText({
  #   
  #   
  #   df <- fileData()
  #   
  #   
  #   if (is.null(df)) {
  #     return("")
  #   }
  #   
  #   
  #   print("Update")
  #   
  #   
  #   ""
  #   
  # })
  
  
  
  # Automatically stop running the app when the browser is closed
  # https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
  session$onSessionEnded(function() {
    
    stopApp()
    
  })
  
  
  
}



addPartyInfo <- function (podDF, flagDF) {
  
  # Add "PARTY_ID" information to the variable containing eWRIMS POD information
  
  
  
  # The "PARTY_ID" column is present in the flag table
  # Isolate that column
  flagDF <- flagDF %>%
    select(APPLICATION_NUMBER, PARTY_ID) %>%
    unique()
  
  
  
  # If multiple "PARTY_ID" values are associated with a water right,
  # combine them into one string per "APPLICATION_NUMBER"
  if (length(flagDF$APPLICATION_NUMBER) != length(unique(flagDF$APPLICATION_NUMBER))) {
    
    flagDF <- flagDF %>%
      group_by(APPLICATION_NUMBER) %>%
      summarize(PARTY_ID = paste0(PARTY_ID, collapse = ";"), .groups = "drop")
    
  }
  
  
  
  return(podDF %>%
           left_join(flagDF, 
                     by = "APPLICATION_NUMBER",
                     relationship = "many-to-one"))
  
}



updateReviewSpreadsheet <- function (wb, path) {
  
  # Save the entire manual review workbook 
  # (Overwriting the previous version)
  
  
  
  # Perform the operation
  checkRes <- saveWorkbook(wb, path, overwrite = TRUE, return = TRUE)
  
  
  
  # If 'checkRes' is FALSE, saving the workbook failed
  if (!checkRes) {
    
    
    # Output an error message in the app too
    html("ErrorDiv",
         paste0("<p><span class = 'bold'>Error: </span>Failed to update the ",
                "manual review spreadsheet</p>",
                "<p>Do you have the file open? Alternatively, another program ", 
                "could be using the file.</p>",
                "<p>Please ensure that the spreadsheet is completely closed ", 
                "and try again.</p>"))
    
    
    
    # Stop the app
    stopApp()
    
    
    
    # Output the same error message above to the console
    stop(paste0("Failed to update the manual review spreadsheet\n\n",
                "Do you have the file open? Alternatively, another program could be using ",
                "the file. Please ensure that the spreadsheet is completely closed and try again."))
    
  }
  
}



partC_MainReview_Start <- function (sheetDF, output) {
  
  
  # Start Part C of the review process
  
  
  shinyjs::show("PART_C_MAIN_REVIEW")
  
  
  # (Part C will begin within the Part B 'Submit' button's reactive context)
  #reviewDF <- fileData() %>%
  #  filter(is.na(KEEP_OR_REMOVE_POD))
  reviewDF <- sheetDF %>%
    arrange(APPLICATION_NUMBER, POD_ID) %>%
    mutate(REVIEWED = !is.na(KEEP_OR_REMOVE_POD))
  
  
  
  # Get the water rights that have been partially reviewed
  partialReviewRights <- reviewDF %>%
    select(APPLICATION_NUMBER, REVIEWED) %>%
    unique() %>%
    group_by(APPLICATION_NUMBER) %>%
    summarize(COUNT = n()) %>%
    filter(COUNT > 1) %>%
    select(APPLICATION_NUMBER) %>% unlist(use.names = FALSE)
  
  
  
  # Make a select input with different water rights as options
  # (Make labels for already reviewed rows different)
  output$WaterRightSelectOut <- renderUI({
    selectInput("WaterRightSelect", label = "Application Number", 
                choices = list("PARTIALLY_REVIEWED" = c("", "", partialReviewRights) %>%
                                 sort(),
                               "UNREVIEWED" = c("", "", reviewDF %>%
                                                  filter(!REVIEWED) %>%
                                                  filter(!(APPLICATION_NUMBER %in% partialReviewRights)) %>%
                                                  select(APPLICATION_NUMBER) %>% unlist(use.names = FALSE)) %>%
                                 sort(),
                               "REVIEWED" = c("", "", reviewDF %>%
                                                filter(REVIEWED) %>%
                                                filter(!(APPLICATION_NUMBER %in% partialReviewRights)) %>%
                                                select(APPLICATION_NUMBER) %>% unlist(use.names = FALSE)) %>%
                                 sort()), 
                multiple = FALSE,
                width = "40%")
  })
  
  
  
  # Also include a table showing the review progress
  output$ReviewProgress <- renderDT({
    
    tibble("REVIEWED" = reviewDF %>%
             filter(REVIEWED) %>%
             filter(!(APPLICATION_NUMBER %in% partialReviewRights)) %>%
             select(APPLICATION_NUMBER) %>% nrow(),
           "PARTIALLY_REVIEWED" = length(partialReviewRights),
           "UNREVIEWED" = reviewDF %>%
             filter(!REVIEWED) %>%
             filter(!(APPLICATION_NUMBER %in% partialReviewRights)) %>%
             select(APPLICATION_NUMBER) %>% nrow())
    
  },
  options = list("searching" = FALSE, #"pageLength" = 3, 
                 "lengthChange" = FALSE, "paging" = FALSE, "info" = FALSE),
  rownames = FALSE)
  
  
}



# Run the application 
shinyApp(ui = ui, server = server, options = list("launch.browser" = TRUE))


