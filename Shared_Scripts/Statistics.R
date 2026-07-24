# These functions help calculate various statistical metrics


#### Dependencies ####


# This script DOES NOT call all required packages and dependencies

# Please use "!Shared_Functions_Importer.R"


#### Functions ####

calcNSE <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate the Nash Sutcliffe Model Efficiency Coefficient 
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # 1 - sum[ (obs - sim)^2 ] / sum[ (obs - mean_obs)^2 ]
  
  numerator <- sum((obs - sim)^2, na.rm = na.rm)
  denominator <- sum((obs - mean(obs, na.rm = na.rm))^2, na.rm = na.rm)
  
  
  # Return 1 minus 'numerator' / 'denominator'
  return(1 - (numerator / denominator))
  
}



calcPBias <- function (obs, sim, na.rm = FALSE, asPercent = TRUE) {
  
  # Calculate the Percent Bias Coefficient 
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # sum[ (sim - obs) ] / sum[ obs ]
  
  
  # NOTE
  
  # For this equation, we are using the formula that is posted by 
  # the HEC-HMS Technical Reference Manual
  
  # (Moriasi 2007) has a different version of this formula ("obs - sim" instead)
  
  # With the HEC-HMS formulation, positive P-Bias values indicate model 
  # overestimation, while negative values represent model underestimation
  
  
  # Calculate P-Bias
  pbias <- sum(sim - obs, na.rm = na.rm) / sum(obs, na.rm = na.rm)
  
  
  # If 'asPercent' is TRUE, return the coefficient as a percent
  if (asPercent) {
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



calcRSR <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate the "Ratio of the Root Mean Square Error (RMSE) to the 
  # Standard Deviation Ratio (RSR)"
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # sqrt[ sum[ (obs - sim)^2 ] ] / sqrt[ sum[ (obs - mean_obs)^2 ] ]
  
  
  numerator <- sqrt(sum((obs - sim)^2, na.rm = na.rm))
  denominator <- sqrt(sum((obs - mean(obs, na.rm = na.rm))^2, na.rm = na.rm))
  
  
  # Return 'numerator' / 'denominator'
  return(numerator / denominator)
  
}



calcMKGE <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate the "Modified Kling Gupta Efficiency"
  
  # Given observed and simulated values, 
  # apply this formula to determine the coefficient:
  
  # 1 - sqrt[ (R - 1)^2 + (B - 1)^2 + (G - 1)^2 ]
  
  # Where 
  # R = Pearson Correlation Coefficient (between 'obs' and 'sim')
  # B = mean_sim / mean_obs
  # G = (st_dev_sim / mean_sim) / (st_dev_obs / mean_obs)
  
  
  # Calculate 'R' first
  
  # `cor` does not handle NA values, so that must be addressed first
  if (na.rm) {
    
    # Find where 'obs' or 'sim' contains NA
    naIndices <- which(is.na(obs) | is.na(sim)) |>
      unique() |> sort()
    
    
    # Remove entries from 'obs' and 'sim' wherever NA was detected
    if (length(naIndices) > 0) {
      
      obs <- obs[-naIndices]
      sim <- sim[-naIndices]
      
    }
    
  }
  
  
  r <- cor(obs, sim, method = "pearson")
  
  
  # Next, calculate 'B' (beta)
  b <- mean(sim, na.rm = na.rm) / mean(obs, na.rm = na.rm)
  
  
  # Then, determine 'G' (gamma)
  g <- (sd(sim, na.rm = na.rm) / mean(sim, na.rm = na.rm)) /
    (sd(obs, na.rm = na.rm) / mean(obs, na.rm = na.rm))
  
  
  # Finally, calculate and return the MKGE
  return(1 - sqrt((r - 1)^2 + (b - 1)^2 + (g - 1)^2))
  
}



calcRSqrd <- function (obs, sim, na.rm = FALSE) {
  
  # Calculate R^2, the "Coefficient of Determination"
  
  # Given observed and simulated values, this coefficient is simply 
  # the square of the Pearson Correlation Coefficient (R)
  
  
  # `cor` does not handle NA values, so that must be addressed first
  if (na.rm) {
    
    # Find where 'obs' or 'sim' contains NA
    naIndices <- which(is.na(obs) | is.na(sim)) |>
      unique() |> sort()
    
    
    # Remove entries from 'obs' and 'sim' wherever NA was detected
    if (length(naIndices) > 0) {
      
      obs <- obs[-naIndices]
      sim <- sim[-naIndices]
      
    }
    
  }
  
  
  return(cor(obs, sim, method = "pearson")^2)
  
}
