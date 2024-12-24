
setwd("C:/Users/Hon. GAB/Documents/Research_support/New folder")
# Load necessary libraries
library(tidyverse)
library(readxl)
library(urca)
library(vars)
library(dplyr)
library(openxlsx)

# Districts list
dist <- c("Adjuman", "Bukomansimbi", "Gomba", "Isingiro", "Kiruhura ", "Koboko",
          "Kotido", "Lwengo", "Nabilatuk ", "Napak", "Rwampara", "Wakiso", "Yumbe")

# Initialize lists to store results
lags<- list()
lag_selection_results <- list()
cointegration_results <- list()
vecm_results <- list()
summary_results <- data.frame()
vecm_summary <- data.frame()

# Loop over each district to perform analysis
for (i in dist) {
  cat("\nProcessing district:", i, "\n")
  
  # Read data for the current district
  data <- read_excel("Monthly Series.xlsx", sheet = i)
  data <- data[, 3:7] # Remove month and year
  
  # Skip processing if data couldn't be loaded or has insufficient rows
  if (is.null(data) || nrow(data) < 10) {
    cat("Data for district", i, "is missing or insufficient. Skipping...\n")
    next
  }
  
  # Convert data to a time series object
  ts_data <- ts(data)
  
  # mod<- VAR(ts_data, lag.max=10, type = "both")
  
 
  # Step 1: Select optimal lag length using VAR
  lag_selection <- VARselect(ts_data, lag.max = 10, type = "const")
  lags[[i]]<- lag_selection
  
  # ?VARselect
  # Extract lag lengths for AIC and SC criteria
  aic_lag <- lag_selection$selection["AIC(n)"]
  sc_lag <- lag_selection$selection["SC(n)"]
  
  lag_selection_results[[i]] <- list(AIC = aic_lag, SC = sc_lag)
  
  cat("Optimal lags for district", i, ": AIC =", aic_lag, ", SC =", sc_lag, "\n")
  
  # Use SC-selected lag for Johansen test (minimum lag of 2 for stability)
  optimal_lag <- max(sc_lag, 2)
  
  # Step 2: Perform Johansen cointegration test
  johansen_test <- ca.jo(ts_data, type = "trace", ecdet = "const", K = optimal_lag)
  
  # Extract eigenvalues, eigenvectors, trace statistics, and critical values
  eigen_values <- johansen_test@lambda  # Eigenvalues
  trace_statistics <- johansen_test@teststat  # Trace statistics
  critical_values <- johansen_test@cval  # Critical values
  
  # Align lengths of eigenvalues, trace statistics, and critical values
  num_ranks <- min(length(trace_statistics), length(eigen_values), nrow(critical_values))
  eigen_values <- eigen_values[1:num_ranks]
  trace_statistics <- trace_statistics[1:num_ranks]
  critical_values <- critical_values[1:num_ranks, , drop = FALSE]
  
  # Determine number of cointegration equations
  num_cointegrations <- sum(trace_statistics > critical_values[, 2]) # Compare with 5% critical value
  
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
  
  cointegration_results[[i]] <- list(
    johansen_test = johansen_test,
    rank_hypothesis = rank_hypothesis,
    num_cointegrations = num_cointegrations
  )
  
  # Print hypothesis details for each rank
  cat("\nCointegration test results for district:", i, "\n")
  print(rank_hypothesis)
  cat("Number of cointegration equations for district", i, ":", num_cointegrations, "\n")
  
  # Step 3: Check for cointegration and estimate VECM
  if (num_cointegrations > 0) {
    cat("Cointegration detected for district", i, ". Estimating VECM...\n")
    
    vecm <- cajorls(johansen_test, r = 1) # Estimate VECM with 1 cointegrating relationship
    
    if (!is.null(vecm)) {
      vecm_results[[i]] <- vecm
      
      # Summarize VECM results (coefficients and diagnostic stats)
      vecm_summary <- rbind(
        vecm_summary,
        data.frame(
          District = i,
          Coefficients = toString(vecm$rlm$coefficients), # Save coefficients as string
          Num_Cointegrations = num_cointegrations
        )
      )
      cat("VECM successfully estimated for district", i, "\n")
    }
  } else {
    cat("No cointegration found for district", i, ". Skipping VECM estimation.\n")
  }
  
  # Append summary results for this district
  summary_results <- rbind(
    summary_results,
    data.frame(
      District = i,
      AIC_Lag = aic_lag,
      SBC_Lag = sc_lag,
      Num_Cointegrations = num_cointegrations
    )
  )
}

# Save eigenvalues, trace statistics, hypotheses, and critical values as a CSV file
eigen_data <- lapply(names(cointegration_results), function(i) {
  if (!is.null(cointegration_results[[i]])) {
    cointegration_results[[i]]$rank_hypothesis %>%
      mutate(District = i)
  } else {
    NULL
  }
}) %>% bind_rows()

write.csv(eigen_data, "eigenvalues_trace_statistics_critical_values.csv", row.names = FALSE)
cat("\nEigenvalues, trace statistics, and critical values saved to 'eigenvalues_trace_statistics_critical_values.csv'.\n")

# Save lag selection and number of cointegration equations as a summary Excel file
write.xlsx(summary_results, "district_lag_cointegration_summary.xlsx", row.names = FALSE)
cat("\nSummary results including AIC and SBC lags saved to 'district_lag_cointegration_summary.xlsx'.\n")

# Save VECM summary results to a CSV file
write.csv(vecm_summary, "vecm_results_summary.csv", row.names = FALSE)
cat("\nVECM summary results saved to 'vecm_results_summary.csv'.\n")

# Save detailed results for future reference
results <- list(
  lag_selection_results = lag_selection_results,
  cointegration_results = cointegration_results,
  vecm_results = vecm_results
)

saveRDS(results, file = "cointegration_vecm_results.rds")
cat("\nAnalysis complete. Results saved to 'cointegration_vecm_results.rds'.\n")

results$vecm_results$Adjuman$rlm

results$vecm_results$Yumbe$rlm
