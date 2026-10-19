# BDA400 - Assignment 6
# Technical Analysis Using R - Visualization Phase
# Interactive Stock Dashboard with R Shiny

# Install the required packages
#install.packages(c("shiny", "ggplot2", "quantmod"))
# Load the required libraries
library(shiny)
#library(ggplot2)
library(quantmod)

# Step 1 - Fetch historical stock data
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Check the first rows of the data
head(stock_data)
# Step 2 - Create the Shiny dashboard interface
ui <- fluidPage(
  titlePanel("Stock Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-06-30"
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
    checkboxGroupInput(
      "technical_indicators",
      "Select Technical Indicators:",
      choices = c(
        "Moving Averages",
        "RSI",
        "MACD"
      )
    ),
    ),
    mainPanel(
      plotOutput("stock_chart"),
      h4("Trading Signal"),
      textOutput("trading_signal"),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') !== -1",
        plotOutput("rsi_chart", height = "220px")
      )
    )
  )
)
# Step 3 - Create the Shiny server
server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    
    # Filter stock data by the selected dates
    filtered_data <- stock_data[
      paste0(input$date_range[1], "/", input$date_range[2])
    ]
    
    # Check that data is available
    validate(
      need(NROW(filtered_data) > 0,
           "No stock data available for these dates.")
    )
    
    # Apply the selected time frame
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data)
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data)
    }
    
    # Draw the stock chart
    chartSeries(
      filtered_data,
      theme = chartTheme("white"),
      name = "Apple Stock Prices"
    )
    # Annotate the latest candle with the RSI trading signal
    rsi_values <- TTR::RSI(Cl(filtered_data), n = 14)
    latest_rsi <- as.numeric(tail(na.omit(rsi_values), 1))
    
    if (length(latest_rsi) > 0) {
      signal <- if (latest_rsi < 30) {
        "BUY"
      } else if (latest_rsi > 70) {
        "SELL"
      } else {
        "HOLD"
      }
      
      signal_labels <- xts::xts(
        rep(NA_real_, NROW(filtered_data)),
        order.by = zoo::index(filtered_data)
      )
      
      signal_labels[NROW(signal_labels)] <- as.numeric(tail(Cl(filtered_data), 1))
      
      addTA(
        signal_labels,
        on = 1,
        type = "p",
        pch = 19,
        col = "blue"
      )
      
      mtext(paste("Latest Signal:", signal), side = 3, line = 0)
    }
    # Overlay Moving Averages when selected
    if ("Moving Averages" %in% input$technical_indicators) {
      addSMA(n = 20, col = "blue")
      addSMA(n = 50, col = "red")
    }
    # RSI will be displayed in a separate plot
    # Show MACD indicator when selected
    if ("MACD" %in% input$technical_indicators) {
      addMACD()
    }
  })
  # Draw RSI in a separate chart
  output$rsi_chart <- renderPlot({
    
    filtered_data <- stock_data[
      paste0(input$date_range[1], "/", input$date_range[2])
    ]
    
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data)
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data)
    }
    
    validate(
      need(NROW(filtered_data) > 14,
           "Select a longer date range for RSI.")
    )
    
    rsi_values <- TTR::RSI(Cl(filtered_data), n = 14)
    
    plot(
      index(filtered_data),
      as.numeric(rsi_values),
      type = "l",
      col = "purple",
      ylim = c(0, 100),
      xlab = "Date",
      ylab = "RSI",
      main = "Relative Strength Index (14)"
    )
    
    abline(h = c(30, 70), col = "red", lty = 2)
  })
  # Generate Buy, Sell, or Hold signal using RSI
  output$trading_signal <- renderText({
    
    filtered_data <- stock_data[
      paste0(input$date_range[1], "/", input$date_range[2])
    ]
    
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data)
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data)
    }
    
    validate(
      need(NROW(filtered_data) > 14,
           "Select a longer date range.")
    )
    
    rsi_values <- TTR::RSI(Cl(filtered_data), n = 14)
    latest_rsi <- as.numeric(tail(na.omit(rsi_values), 1))
    
    if (length(latest_rsi) == 0) {
      return("Not enough data")
    }
    
    if (latest_rsi < 30) {
      "BUY"
    } else if (latest_rsi > 70) {
      "SELL"
    } else {
      "HOLD"
    }
  })
}

# Launch the Shiny application
shinyApp(ui, server)



