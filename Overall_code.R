
setwd("C:/Users/Hon. GAB/Documents/Research_support/New folder")
# Load necessary libraries
library(tidyverse)
library(readxl)
library(urca)
library(vars)
library(dplyr)
library(openxlsx)
library(seastests)

Monthly_Averages <- read_excel("Monthly Averages.xlsx")

# Read the data
overall_data <- Monthly_Averages %>%
  mutate(
    Month = factor(
      Month,
      levels = c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December"),
      ordered = TRUE
    ) # Ensure the months follow the natural order
  ) %>%
  group_by(Year, Month) %>%
  summarise(
    malaria = round(mean(TotalMalariacases), 0),
    rainfall = round(mean(Totalrianfallamounts), 3),
    min_temp = round(mean(Mini), 3),
    max_temp = round(mean(Max), 3),
    NDVI = round(mean(NDVI), 3)
  ) %>%
  ungroup() %>%
  arrange(Year, Month) # Order by Year and natural Month order

##Seasonality
stest_m<- fried(overall_data$malaria, freq = 12, residuals =F,  diff = T, autoarima = T)
stest_r<- fried(overall_data$rainfall, freq = 12, residuals =F,  diff = T, autoarima = T)
stest_mn<- fried(overall_data$min_temp, freq = 12, residuals =F,  diff = T, autoarima = T)
stest_mx<- fried(overall_data$max_temp, freq = 12, residuals =F,  diff = T, autoarima = T)
stest_nd<- fried(overall_data$NDVI, freq = 12, residuals =F,  diff = T, autoarima = T)


# Initialize result containers
lag_selection_results <- list()
cointegration_results <- list()
vecm_results <- list()
vecm_summary <- data.frame()
summary_results <- data.frame()

# Perform analysis on the overall_data
cat("Performing analysis on overall_data...\n")

# Convert overall_data to a time series object
ts_data <- ts(
  overall_data[, c("malaria", "rainfall", "min_temp", "max_temp", "NDVI")],
  start = c(min(overall_data$Year), 1),  # Start from the earliest year
  frequency = 12  # Monthly data
)

# Step 1: Seasonal adjustment using STL decomposition
if (nrow(overall_data) >= 24) {
  tryCatch({
    ts_data_deseasonalized <- ts(apply(ts_data, 2, function(x) {
      decomposition <- stl(x, s.window = "periodic")
      residuals <- decomposition$time.series[, "remainder"]
      return(residuals)
    }), frequency = 12)
  }, error = function(e) {
    cat("Error in STL decomposition: ", e$message, "\n")
    ts_data_deseasonalized <- ts_data  # Fallback to raw data
  })
} else {
  ts_data_deseasonalized <- ts_data
}

# Step 2: Select optimal lag length using VAR
lag_selection <- VARselect(ts_data, lag.max = 10, type = "const")
aic_lag <- lag_selection$selection["AIC(n)"]
sc_lag <- lag_selection$selection["SC(n)"]
cat("Optimal lags: AIC =", aic_lag, ", SC =", sc_lag, "\n")

# Step 3: Perform Johansen cointegration test
optimal_lag <- max(sc_lag, 2)
johansen_test <- ca.jo(ts_data, type = "trace", ecdet = "const", K = optimal_lag)

# Extract eigenvalues, trace statistics, and critical values
eigen_values <- johansen_test@lambda
trace_statistics <- johansen_test@teststat
critical_values <- johansen_test@cval
num_ranks <- min(length(trace_statistics), length(eigen_values), nrow(critical_values))
eigen_values <- eigen_values[1:num_ranks]
trace_statistics <- trace_statistics[1:num_ranks]
critical_values <- critical_values[1:num_ranks, , drop = FALSE]

# Determine number of cointegration equations
num_cointegrations <- sum(trace_statistics > critical_values[, 2])  # Compare with 5% critical value

# Create rank hypothesis data frame
rank_hypothesis <- data.frame(
  Rank = 0:(num_ranks - 1),
  Null_Hypothesis = paste0("r <= ", 0:(num_ranks - 1)),
  Alternative_Hypothesis = paste0("r > ", 0:(num_ranks - 1)),
  Eigenvalue = eigen_values,
  TraceStatistic = trace_statistics,
  Critical_10_Percent = critical_values[, 3],
  Critical_5_Percent = critical_values[, 2],
  Critical_1_Percent = critical_values[, 1],
  Cointegration_Result = ifelse(trace_statistics > critical_values[, 2], "Rejected", "Not Rejected")
)

# Save Johansen test results
cointegration_results <- list(
  johansen_test = johansen_test,
  rank_hypothesis = rank_hypothesis,
  num_cointegrations = num_cointegrations
)

# Step 4: Check for cointegration and estimate VECM
if (num_cointegrations > 0) {
  cat("Cointegration detected. Estimating VECM...\n")
  vecm <- cajorls(johansen_test, r = 1)  # Estimate VECM with 1 cointegrating relationship
  
  if (!is.null(vecm)) {
    vecm_results <- vecm
    
    # Summarize VECM results
    vecm_summary <- data.frame(
      Coefficients = toString(vecm$rlm$coefficients),
      Num_Cointegrations = num_cointegrations
    )
    
    cat("VECM successfully estimated.\n")
  }
} else {
  cat("No cointegration found. Skipping VECM estimation.\n")
}


# Save results to files
write.csv(lag_selection$criteria, "Lag_selection_for_Overall_Data.csv", row.names = FALSE)
cat("\nLag selection results are to 'Lag_selection_for_Overall_Data.csv'.\n")

write.csv(rank_hypothesis, "cointegration_rank_hypothesis.csv", row.names = FALSE)
cat("\nJohansen test results saved to 'cointegration_rank_hypothesis.csv'.\n")

if (!is.null(vecm_summary)) {
  write.csv(vecm_summary, "vecm_results_summary.csv", row.names = FALSE)
  cat("\nVECM results saved to 'vecm_results_summary.csv'.\n")
}


# Step 5: Forecasting malaria cases
if (num_cointegrations > 0) {
  cat("\nGenerating forecasts for malaria cases...\n")
  
  # Convert Johansen test result to VAR representation for forecasting
  vecm_var <- vec2var(johansen_test, r = 1)  # Assume 1 cointegrating equation
  
  # Generate forecasts for the next 12 months
  forecast <- predict(vecm_var, n.ahead = 12, ci = 0.95)
  
  # Extract forecasts for malaria cases (assuming malaria is the first variable)
  malaria_forecast <- forecast$fcst[["malaria"]][, 1:3]  # Mean, lower, and upper bounds
  
  # Create a data frame for the forecasts
  malaria_forecast_df <- data.frame(
    Month = 1:12,
    Forecast = malaria_forecast[, 1],
    Lower = malaria_forecast[, 2],
    Upper = malaria_forecast[, 3]
  )
  
  # Save malaria forecasts to a CSV file
  write.csv(malaria_forecast_df, "malaria_forecasts1.csv", row.names = FALSE)
  cat("\nMalaria forecasts saved to 'malaria_forecasts.csv'.\n")
} else {
  cat("\nNo cointegration found. Forecasting skipped.\n")
}




