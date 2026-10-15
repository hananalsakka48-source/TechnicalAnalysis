# Assignment 5 - Simple Moving Average (SMA)
# Calculate the average of values over a selected period
sma <- function(data, period) {
  if (period < 1 || period > length(data)) {
    stop("Data length should be greater than or equal to the period")
  }
  
  sma_values <- numeric(length(data) - period + 1)
  
  for (i in 1:length(sma_values)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}