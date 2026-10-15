# Assignment 5 - Crossover
# Detect when the first line crosses above the second line
crossover <- function(arr1, arr2) {
  
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize crossover signals
  crossover_signals <- rep("None", length(arr1))
  
  # Check whether the first line crosses the second
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      
      if (arr1[i] > arr2[i] &&
          arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
        
      } else if (arr1[i] < arr2[i] &&
                 arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      }
    }
  }
  
  return(crossover_signals)
}