# Assignment 5 - Relative Strength Index (RSI)
# Measures price momentum using gains and losses
rsi <- function(data, period) {
  
  # Check inputs
  if (period < 1 || period != floor(period) ||
      length(data) <= period) {
    stop("Invalid period or insufficient data")
  }
  
  # Calculate price changes
  diff_values <- diff(data)
  
  # Separate gains and losses
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  
  # Calculate initial averages
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  # Initialize RSI values
  rsi_values <- rep(NA_real_, length(data))
  
  # First RSI value
  if (avg_loss == 0) {
    rsi_values[period + 1] <- if (avg_gain == 0) 50 else 100
  } else {
    rs <- avg_gain / avg_loss
    rsi_values[period + 1] <- 100 - 100 / (1 + rs)
  }
  
  # Apply Wilder's smoothing
  if (length(data) > period + 1) {
    for (i in (period + 2):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      
      if (avg_loss == 0) {
        rsi_values[i] <- if (avg_gain == 0) 50 else 100
      } else {
        rs <- avg_gain / avg_loss
        rsi_values[i] <- 100 - 100 / (1 + rs)
      }
    }
  }
  
  return(rsi_values)
}