# These functions help calculate various statistical metrics


#### Metric Functions ####

#' @title Calculate the Nash-Sutcliffe Efficiency Coefficient
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
#' # ...
calc_nse <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate the Nash Sutcliffe Model Efficiency Coefficient 
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # 1 - sum[ (obs - sim)^2 ] / sum[ (obs - mean_obs)^2 ]
  
  
  return(calculate_stat_metric("NSE", obs, sim, na.rm))
  
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
#' # ...
calc_pbias <- function (obs, sim, na.rm = FALSE, asPercent = TRUE) {
  
  # Calculate the Percent Bias Coefficient 
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # sum[ (sim - obs) ] / sum[ obs ]
  
  
  # NOTE
  
  # For this equation, we are using the formula that is posted by 
  # the HEC-HMS Technical Reference Manual
  
  # (Moriasi 2007) has a different version of this formula ("obs - sim" instead)
  
  # With the HEC-HMS formulation, positive P-Bias values indicate model overestimation,
  # while negative values represent model underestimation
  
  
  # Calculate P-Bias
  pbias <- calculate_stat_metric("PBIAS", obs, sim, na.rm)
  
  
  # If 'asPercent' is TRUE, return the coefficient as a percent
  if (asPercent == TRUE) {
    pbias <- 100 * pbias
  }
  
  
  # Add an attribute to 'pbias' to indicate whether overestimation or 
  # underestimation has occurred
  if (pbias > 0) {
    
    attr(pbias, "simulation") <- "overestimation"
    
  } else if (pbias < 0) {
    
    attr(pbias, "simulation") <- "underestimation"
    
  }
  
  
  # Return 'pbias'
  return(pbias)
  
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
#' # ...
calc_rsr <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate the "Ratio of the Root Mean Square Error (RMSE) to the 
  # Standard Deviation Ratio (RSR)"
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # sqrt[ sum[ (obs - sim)^2 ] ] / sqrt[ sum[ (obs - mean_obs)^2 ] ]
  
  
  return(calculate_stat_metric("RSR", obs, sim, na.rm))
  
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
#' # ...
calc_mkge <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate the "Modified Kling Gupta Efficiency"
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # 1 - sqrt[ (R - 1)^2 + (B - 1)^2 + (G - 1)^2 ]
  
  # Where 
  # R = Pearson Correlation Coefficient (between 'obs' and 'sim')
  # B = mean_sim / mean_obs
  # G = (st_dev_sim / mean_sim) / (st_dev_obs / mean_obs)
  
  
  # Perform these calculations in another function
  return(calculate_stat_metric("MKGE", obs, sim, na.rm))
  
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
#' # ...
calc_r_sqrd <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate R^2, the "Coefficient of Determination"
  
  # Given observed and simulated values, this coefficient is simply 
  # the square of the Pearson Correlation Coefficient (R)
  
  
  return(calculate_stat_metric("RSQ", obs, sim, na.rm))
  
}



#### Helper Functions ####

# (These functions are an important part of the procedure,
#  but they will not be exported into the package)

calculate_stat_metric <- function (metric, obs, sim, na.rm) {
  
  # The statistical metric functions mostly follow the same procedure
  
  # The main thing that differs is the actual calculation steps
  
  # To reduce redundancy, those functions all call this one to perform 
  # the initial setup steps and apply the metric formulas
  
  
  # Validate the input arguments first
  check_input_vectors(obs, sim)
  
  
  # If 'na.rm' is TRUE, remove entries from 'obs' and 'sim' wherever 
  # either of them are NA
  if (na.rm == TRUE) {
    
    # Get the index values where either 'obs' or 'sim' contains NA
    naIndices <- which(is.na(obs) | is.na(sim))
    
    
    # If any NA values are found, remove them from both vectors
    if (length(naIndices) > 0) {
      
      obs <- obs[-naIndices]
      
      sim <- sim[-naIndices]
      
    }
    
  }
  
  
  # Next, based on the value of 'metric', calculate a result
  
  
  if (metric == "NSE") {
    
    # Nash-Sutcliffe Formula:
    # # 1 - sum[ (obs - sim)^2 ] / sum[ (obs - mean_obs)^2 ]
    
    
    # Calculate the numerator and denominator of the fraction
    numerator <- sum((obs - sim)^2)
    denominator <- sum((obs - mean(obs))^2)
    
    
    # Return 1 minus 'numerator' / 'denominator'
    return(1 - (numerator / denominator))
    
    
  } else if (metric == "PBIAS") {
    
    # P-Bias Formula:
    # sum[ (sim - obs) ] / sum[ obs ]
    
    # (HEC-HMS Technical Reference Manual Version)
    
    
    return(sum(sim - obs) / sum(obs))
    
    
  } else if (metric == "RSR") {
    
    # RSR Formula:
    # sqrt[ sum[ (obs - sim)^2 ] ] / sqrt[ sum[ (obs - mean_obs)^2 ] ]
    
    
    # Calculate the numerator and denominator separately first
    numerator <- sqrt(sum((obs - sim)^2))
    denominator <- sqrt(sum((obs - mean(obs))^2))
    
    
    # Return 'numerator' / 'denominator'
    return(numerator / denominator)
    
    
  } else if (metric == "MKGE") {
    
    # Modified Kling-Gupta Efficiency Coefficient Formula:
    # 1 - sqrt[ (R - 1)^2 + (B - 1)^2 + (G - 1)^2 ]
    
    # Where 
    # R = Pearson Correlation Coefficient (between 'obs' and 'sim')
    # B = mean_sim / mean_obs
    # G = (st_dev_sim / mean_sim) / (st_dev_obs / mean_obs)
    
    
    # Calculate 'R' first
    r <- cor(obs, sim, method = "pearson")
    
    
    # Next, calculate 'B' (beta)
    b <- mean(sim) / mean(obs)
    
    
    # Then, determine 'G' (gamma)
    g <- (sd(sim) / mean(sim)) /
      (sd(obs) / mean(obs))
    
    
    # Finally, calculate and return the MKGE
    return(1 - sqrt((r - 1)^2 + (b - 1)^2 + (g - 1)^2))
    
    
  } else if (metric == "RSQ") {
    
    # R^2 Formula:
    # The square of the Pearson Correlation Coefficient (r)
    
    
    return(cor(obs, sim, method = "pearson")^2)
    
    
  } else {
    
    paste0("Unknown Metric \"", metric, "\" Specified\n\n",
           "Please investigate and revise the function as needed") |>
      stop_script()
    
  }
  
}



input_checks <- function (obs, sim) {
  
  # Make sure 'obs' and 'sim' are equivalent in size and type
  
  
  # Both variables should be numeric vectors
  error_if(!is.vector(obs) || !is.vector(sim) ||
             !is.numeric(obs) || !is.numeric(sim),
           paste0("Input arguments must be numeric vectors"))
  
  
  # Make sure 'obs' and 'sim' have the same length too
  error_if(length(obs) != length(sim),
           paste0("Input arguments have different lenghts\n\n",
                  "Vector of Observed Values: ", length(obs), " Element(s)\n\n",
                  "Vector of Simulated Values: ", length(sim), " Element(s)\n\n"))
  
  
  # Return nothing if there are no issues
  return(invisible(NULL))
  
}




