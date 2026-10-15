# Assignment 5 - Stochastic RSI
# Calculate the Stochastic RSI using RSI and SMA
stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Calculate RSI
  rsi_values <- rsi(data, period)
  
  # Normalize RSI values between 0 and 1
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  if (length(valid_rsi) < k_period + d_period - 1) {
    stop("Not enough data to calculate Stochastic RSI")
  }
  
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  
  if (max_rsi == min_rsi) {
    stop("Cannot normalize RSI when all values are equal")
  }
  
  k_values <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  
  # Calculate the %K line
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line
  d_line <- sma(k_line, d_period)
  
  # Return both lines
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}