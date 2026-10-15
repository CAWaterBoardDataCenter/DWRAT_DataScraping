# These functions are related to error messages and warnings



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' ...
#' }
error_if <- function (condition, message) {
  
  # If a 'condition' resolves to TRUE, 
  # output an error message (contained in 'message')
  
  if (condition == TRUE) {
    
    message |>
      stop_script()
    
  }
  
  
  # Return nothing otherwise
  return(invisible(NULL))
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' ...
#' }
errWrap <- function (message, widthRatio = 0.99) {
  
  # Modify the wrapping of an error message 
  # This reduces the need for horizontal scrolling in the console
  return(message |>
           strwrap(width = widthRatio * getOption("width")) |>
           paste0(collapse = "\n"))
  
}



#' @title ...
#' 
#' @description
#' ...
#' 
#' @details
#' ...
#' 
#' @usage ...
#' 
#' @param ... ...
#' 
#' @returns ...
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' ...
#' }
stop_script <- function (message) {
  
  # Stop running a script
  
  # For a given string 'message', adjust its formatting before displaying the error
  
  message |>
    errWrap() |>
    stop()
  
}
