# Assignment 5 - Moving Average Convergence Divergence (MACD)
# Calculates the MACD line, signal line, and histogram
macd <- function(data, short_period, long_period, signal_period) {
  
  # Calculate short-term and long-term EMA
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the histogram
  histogram <- macd_line - signal_line
  
  # Return all three results
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}