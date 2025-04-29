# Load necessary libraries
library(tseries)  # For adf.test()
library(forecast)
library(zoo)  # For NA handling

# List of stock files
stock_files <- c("Aselsan_stock_data.csv", "Broadcom_stock_data.csv", "Intel_stock_data.csv", 
                 "Microsoft_stock_data.csv", "Northrop Grumman_stock_data.csv", 
                 "Oracle_stock_data.csv", "Qualcomm_stock_data.csv", 
                 "Saab AB_stock_data.csv", "Suncore Energy_stock_data.csv", 
                 "TechnipFMC_stock_data.csv", "Woodside Energy_stock_data.csv")

# Base directory for stock files
base_dir <- "C:/Users/Baron/Downloads/stock_data/content/stock_data/"

# Read the first stock file to extract dates
first_stock <- read.csv(paste0(base_dir, stock_files[1]))
first_stock$Date <- as.Date(first_stock[, 1], format = "%Y-%m-%d")  # Assuming Date is in the first column
dates <- tail(first_stock$Date, 3947)  # Extract the last 3947 dates

# Create a matrix to store all log returns with dates as column names
# +1 column for stock names
all_log_returns_matrix <- matrix(NA, nrow = length(stock_files), ncol = length(dates))
rownames(all_log_returns_matrix) <- sapply(stock_files, function(x) sub("_stock_data.csv", "", x))
colnames(all_log_returns_matrix) <- as.character(dates)

# Loop through each stock file and process it
for (i in 1:length(stock_files)) {
  stock_file <- stock_files[i]
  cat("\nProcessing", stock_file, "...\n")
  
  # Read the stock data
  stock <- read.csv(paste0(base_dir, stock_file))
  
  # Extract the 'Close' column
  closing_prices <- stock$Close
  
  # Convert to numeric
  closing_prices_numeric <- as.numeric(closing_prices)
  
  # Handle any NA values with linear interpolation
  closing_prices_interpolated <- na.approx(closing_prices_numeric, na.rm = FALSE)
  
  # Compute log returns
  log_returns <- diff(log(closing_prices_interpolated))
  
  # Take the last 3947 log returns to match our date range
  log_returns <- tail(log_returns, length(dates))
  
  # Store in the matrix
  all_log_returns_matrix[i, ] <- log_returns
  
  # Perform ADF test on original closing prices
  tryCatch({
    adf_result_original <- adf.test(closing_prices_interpolated, alternative = "stationary")
    
    # Extract relevant ADF test results for the original data
    df_stat_original <- adf_result_original$statistic
    p_value_original <- adf_result_original$p.value
    lag_order_original <- adf_result_original$lags
    
    # Print the ADF test result for the original data
    cat("\nADF test result for", stock_file, "on original data:\n")
    cat("Dickey-Fuller = ", df_stat_original, ", Lag order = ", lag_order_original, ", p-value = ", p_value_original, "\n")
  }, error = function(e) {
    cat("ADF test failed on original data for", stock_file, ":", e$message, "\n")
  })
  
  # Perform ADF test on the log-differenced prices
  tryCatch({
    adf_result_log_diff <- adf.test(log_returns, alternative = "stationary")
    
    # Extract relevant ADF test results for the log-differenced data
    df_stat_log_diff <- adf_result_log_diff$statistic
    p_value_log_diff <- adf_result_log_diff$p.value
    lag_order_log_diff <- adf_result_log_diff$lags
    
    # Print the ADF test result for the log-differenced data
    cat("\nADF test result for", stock_file, "on log-differenced data:\n")
    cat("Dickey-Fuller = ", df_stat_log_diff, ", Lag order = ", lag_order_log_diff, ", p-value = ", p_value_log_diff, "\n")
  }, error = function(e) {
    cat("ADF test failed on log returns for", stock_file, ":", e$message, "\n")
  })
  
  cat("\n----------------------------------------------------\n")
}

# Convert the matrix to a data frame with proper structure
all_log_returns_df <- as.data.frame(all_log_returns_matrix)

# Add a column name for the stock names when we export
output_df <- cbind(Stock = rownames(all_log_returns_df), all_log_returns_df)

# Check dimensions
cat("\nDimensions of the output data frame:", dim(output_df), "\n")
cat("Number of dates:", length(dates), "\n")
cat("Number of stocks:", length(stock_files), "\n")

# Print sample of the data frame to verify format
cat("\nSample of the output data frame:\n")
print(head(output_df[, 1:min(6, ncol(output_df))]))  # Print first few columns only

# Write to CSV
write.csv(output_df, "all_log_returns_wide_format_fixed.csv", row.names = FALSE)
cat("\nData successfully written to 'all_log_returns_wide_format.csv'\n")

# Optional: Create plots for visual inspection
if (FALSE) {
  # Example plots for the first stock
  stock_idx <- 1
  stock_name <- rownames(all_log_returns_matrix)[stock_idx]
  
  # Plot log returns
  dev.new()
  plot(as.numeric(all_log_returns_matrix[stock_idx, ]), type = "l", col = "blue",
       xlab = "Time", ylab = "Log Returns", 
       main = paste("Log Returns for", stock_name))
  
  # ACF and PACF plots
  dev.new()
  acf(as.numeric(all_log_returns_matrix[stock_idx, ]), 
      main = paste("ACF for", stock_name), col = "blue", lag.max = 50)
  
  dev.new()
  pacf(as.numeric(all_log_returns_matrix[stock_idx, ]), 
       main = paste("PACF for", stock_name), col = "red", lag.max = 50)
}