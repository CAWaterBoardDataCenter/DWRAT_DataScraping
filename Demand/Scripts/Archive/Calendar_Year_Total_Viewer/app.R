#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#### Dependencies ####


library(shiny)
library(tidyverse)
library(readxl)


#### Functions ####

fileCheck <- function () {
  
  # Verify that the required input file is present
  # The relative filepath is "OutputData/Calendar_Year_Totals_AF.xlsx"

  
  if (!file.exists("../../OutputData/Calendar_Year_Totals_AF.xlsx")) {
    
    stop("The required spreadsheet 'Calendar_Year_Totals_AF.xlsx' is not present in the 'OutputData' folder. Please run the 'Expected_Demand.R' script to generate this file.")
    
  }
  
  
  # Return nothing
  return(invisible(NULL))
  
}


prepareMainTable <- function (calendarData, selectedAppNums) {
  
  # Prepare a data frame containing values for "CALENDAR_YEAR_TOTAL"
  # It will be limited to rights specified in 'selectedAppNums'
  
  
  # For consistency, make sure 'selectedAppNums' is sorted
  selectedAppNums <- sort(selectedAppNums)
  
  
  
  # Create a filtered dataset for the "APPLICATION_NUMBER" values specified in 'selectedAppNums'
  filteredDF <- calendarData %>%
    filter(APPLICATION_NUMBER %in% selectedAppNums)
  
  
  
  # Identify the years for which data is contained within 'filteredDF'
  years <- filteredDF$YEAR %>%
    unique() %>% sort()
  
  
  
  # Define a matrix (that will later be converted into a data frame)
  # Each row will be a unique application number and each column will be a unique year
  mainDF <- matrix(NA_real_, nrow = length(selectedAppNums), ncol = length(years))
  
  
  
  # Iterate through the years in 'years'
  # Assign values to columns in 'mainDF' for each year
  for (i in 1:length(years)) {
    
    # Error Check
    # There should not be more than one row per application number
    # Each "APPLICATION_NUMBER" value should appear at most once
    stopifnot(sum(table(filteredDF$APPLICATION_NUMBER[filteredDF$YEAR == years[i]]) > 1) == 0)
    
    
    
    # Iterate through the values in 'selectedAppNums' next
    # (So this is a nested for loop)
    for (j in 1:length(selectedAppNums)) {
      
      # Extract the individual "CALENDAR_YEAR_TOTAL" value from 'filteredDF'
      afVal <- filteredDF %>%
        filter(YEAR == years[i] & APPLICATION_NUMBER == selectedAppNums[j]) %>%
        select(CALENDAR_YEAR_TOTAL) %>%
        unlist() %>% as.vector()
      
      
      # If the value is empty, leave "NA" in 'mainDF'
      # Otherwise, replace the entry with 'afVal'
      if (length(afVal) > 0) {
        mainDF[j, i] <- afVal
      }
      
    } # End of 'j' loop
    
  } # End of 'i' loop
  
  
  # Convert 'mainDF' into a true data frame
  # Set the column names equal to 'years'
  # Then, add a new column for the application numbers (and move it to the front)
  mainDF <- mainDF %>%
    data.frame() %>%
    set_names(years) %>%
    mutate(APPLICATION_NUMBER = selectedAppNums) %>%
    relocate(APPLICATION_NUMBER)
  
  
  # Return 'mainDF'
  return(mainDF)
  
}


prepareSummaryStats <- function (mainDF) {
  
  # Using the primary data table 'mainDF',
  # Produce a table of summary statistics that contains, for each year:
  #   Sum
  #   Median
  #   Average
  #   Standard Deviation
  
  # NA values will be excluded from these statistics
  
  
  # The returned data frames will be modifications of 'mainDF'
  
  
  # If 'mainDF' only has one year's worth of data, use a simpler procedure
  if (ncol(mainDF) == 2) {
    
    return(c("TOTAL", sum(mainDF[, -1], na.rm = TRUE),
             "MEDIAN", median(mainDF[, -1], na.rm = TRUE),
             "MEAN", mean(mainDF[, -1], na.rm = TRUE),
             "ST_DEV", sd(mainDF[, -1], na.rm = TRUE)) %>%
             matrix(ncol = 2, byrow = TRUE) %>%
             data.frame() %>% tibble() %>%
             set_names(c("STATISTIC", names(mainDF)[2])))
    
  }
  
  
  
  # colSums() and colMeans() can be used to calculate the totals and means for each numeric column
  # apply() with median() and sd() must be used for the median and standard deviation
  # For all four statistics, NA values are ignored (na.rm = TRUE)
  # The last four rows of 'mainDF' are extracted (these four summary statistics that were calculated)
  # Finally, "APPLICATION_NUMBER" is renamed to a more appropriate column name
  return(mainDF %>%
           rbind(c("TOTAL", colSums(mainDF[, -1], na.rm = TRUE)),
                 c("MEDIAN", apply(mainDF[, -1], 2, median, na.rm = TRUE)),
                 c("MEAN", colMeans(mainDF[, -1], na.rm = TRUE)),
                 c("ST_DEV", apply(mainDF[, -1], 2, sd, na.rm = TRUE))) %>%
           tail(4) %>%
           rename(STATISTIC = APPLICATION_NUMBER))
  
}


#### App Components ####


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calendar Year Total Viewer"),
    
    
    mainPanel(
      fluidRow(selectizeInput(inputId = "AppNum_Selector", label = "Please select one or more Application Numbers",
                              choices = NULL, multiple = TRUE)),
      
      fluidRow(textOutput(outputId = "Main_CYT_Table_Text")),
      fluidRow(tableOutput("Main_CYT_Table")),
      
      fluidRow(textOutput(outputId = "Summary_Stats_Table_Text")),
      fluidRow(tableOutput("Summary_Stats_Table"))
    )

)



# Define server logic required to draw a histogram
server <- function(input, output) {

  
  # Read in the input file "Calendar_Year_Totals_AF.xlsx"
  calendarData <- read_xlsx("../../OutputData/Calendar_Year_Totals_AF.xlsx")
  
  
  # Update the selectable APPLICATION_NUMBER values in "AppNum_Selector"
  updateSelectizeInput(inputId = "AppNum_Selector", 
                    choices = sort(unique(calendarData$APPLICATION_NUMBER)), 
                    server = TRUE)
  
  
  output$Main_CYT_Table_Text <- renderText("Awaiting user input")
  
  
  observe({
    
    if (length(input$AppNum_Selector) > 0) {
      
      # Update the label text
      output$Main_CYT_Table_Text <- renderText("Calendar Year Total (AF)")
      
      
      
      # Use another function to produce the main table for this app
      mainTable <- prepareMainTable(calendarData, input$AppNum_Selector)
      
      
      # Render 'mainTable'
      output$Main_CYT_Table <- renderTable(mainTable)
      
      
      
      # Also, use 'mainTable' to compute summary statistics
      
      # First, output text related to that table
      output$Summary_Stats_Table_Text <- renderText("Summary Statistics (AF)")
      
      
      # Then, render that table using 'mainTable' and another function
      output$Summary_Stats_Table <- renderTable(prepareSummaryStats(mainTable))
      
    }
    
  })
  
  
  
  
  
  
}



#### Run the Application ####


# Check for the input file
fileCheck()


# Then, start the application
shinyApp(ui = ui, server = server)
