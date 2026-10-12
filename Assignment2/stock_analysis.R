
# BDA400 Assignment 2
# Technical Analysis using R

library(quantmod)
library(TTR)

# Read stock symbols
symbols <- readLines("portfolio.txt")

# Function to load stock data
load_stock_data <- function(symbols) {
  stock_data <- list()
  
  for (symbol in symbols) {
    tryCatch({
      data <- getSymbols(
        symbol,
        src = "yahoo",
        from = "2025-01-01",
        auto.assign = FALSE
      )
      
      stock_data[[symbol]] <- data.frame(
        Date = as.Date(index(data)),
        coredata(data)
      )
      
      cat("Loaded:", symbol, "\n")
    }, error = function(e) {
      message("Failed to load ", symbol, ": ", e$message)
    })
  }
  
  return(stock_data)
}

# Load all stocks
stocks <- load_stock_data(symbols)

# Display imported data
lapply(stocks, head)



# Statistical Analysis

# Mode function
calculate_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate statistics for each stock
results <- lapply(names(stocks), function(symbol) {
  data <- stocks[[symbol]]
  
  close_col <- grep("\\.Close$", names(data), value = TRUE)
  prices <- as.numeric(data[[close_col]])
  
  data.frame(
    Stock = symbol,
    Mean = mean(prices, na.rm = TRUE),
    Median = median(prices, na.rm = TRUE),
    Mode = calculate_mode(prices),
    SD = sd(prices, na.rm = TRUE),
    SMA20 = as.numeric(tail(TTR::SMA(prices, n = 20), 1))
  )
})

# Combine and display results
statistics <- do.call(rbind, results)
print(statistics)



# Stock Price Charts with 20-Day Moving Average

for (symbol in names(stocks)) {
  data <- stocks[[symbol]]
  
  close_col <- grep("\\.Close$", names(data), value = TRUE)
  prices <- as.numeric(data[[close_col]])
  dates <- as.Date(data$Date)
  
  sma20 <- TTR::SMA(prices, n = 20)
  
  png(paste0("Chart_", symbol, ".png"),
      width = 1000, height = 600)
  
  plot(dates, prices, type = "l",
       col = "blue", lwd = 2,
       main = paste(symbol, "Stock Price and SMA20"),
       xlab = "Date", ylab = "Closing Price (USD)")
  
  lines(dates, sma20, col = "red", lwd = 2)
  
  legend("topleft",
         legend = c("Closing Price", "20-Day SMA"),
         col = c("blue", "red"),
         lty = 1, lwd = 2)
  
  dev.off()
}

print("All stock charts saved successfully!")
