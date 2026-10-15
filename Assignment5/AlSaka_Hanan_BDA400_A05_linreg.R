# Assignment 5 - Linear Regression (linreg)
# Calculate the slope, intercept, and predicted values
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  # Calculate the number of data points
  n <- length(regressionSource)
  
  # Check the input values
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the data length")
  }
  
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # Find the starting and ending positions
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  # Select the data for regression
  source_subset <- regressionSource[start_index:end_index]
  
  # Create index values
  index_values <- seq_along(source_subset)
  
  # Calculate averages
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  
  # Calculate slope and intercept
  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return the results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}