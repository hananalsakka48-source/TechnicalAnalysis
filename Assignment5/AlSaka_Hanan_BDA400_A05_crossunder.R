# Assignment 5 - Crossunder
# Detect when the first line crosses below the second line
crossunder <- function(arr1, arr2) {
  
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize crossunder signals
  crossunder_signals <- rep("None", length(arr1))
  
  # Check for downward crossings
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] < arr2[i] &&
          arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }
  
  return(crossunder_signals)
}