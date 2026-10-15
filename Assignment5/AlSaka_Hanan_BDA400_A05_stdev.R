# Assignment 5 - Standard Deviation (stdev)
# Measures how much values vary around their mean
stdev <- function(data) {
  
  # Calculate the mean
  mean_value <- sum(data) / length(data)
  
  # Calculate differences from the mean
  diff_values <- data - mean_value
  
  # Square the differences
  squared_diff <- diff_values^2
  
  # Calculate variance
  variance <- sum(squared_diff) / length(data)
  
  # Calculate standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}