setwd("C:/Users/Hon. GAB/Documents/Research_support/New folder")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define a function to process and plot the data for each year
plot_rain <- function(year) {
  # Read and prepare data
  Monthly_Averages <- read_excel("Monthly Averages1.xlsx") %>%
    mutate(
      Month = as.Date(Date, format = "%d/%B/%Y"),  # Convert Date column to Date format
      Year = as.factor(format(Month, "%Y"))       # Extract year as a factor
    )
  
  # Check if the file has data
  if (nrow(Monthly_Averages) == 0) {
    message("No data found in the Excel file.")
    return(NULL)
  }
  
  # Filter data for the specified year
  Monthly_Averages <- Monthly_Averages %>% filter(Year == as.character(year))
  
  # If no data for the year, skip to the next year
  if (nrow(Monthly_Averages) == 0) {
    message(paste("No data for year", year))
    return(NULL)
  }
  
  # Check the column names and data structure
  message("Columns in the dataset: ", paste(colnames(Monthly_Averages), collapse = ", "))
  
  # Ensure required columns exist
  required_cols <- c("Date", "TotalMalariacases", "Totalrianfallamounts", "Mini", "Max", "NDVI", "District")
  missing_cols <- setdiff(required_cols, colnames(Monthly_Averages))
  
  if (length(missing_cols) > 0) {
    message("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  # Reshape data to long format for multiple variables
  Monthly_Averages_long <- Monthly_Averages %>%
    pivot_longer(cols = c(TotalMalariacases, Totalrianfallamounts, Mini, Max, NDVI), 
                 names_to = "Variable", 
                 values_to = "Value")
  
  # Check if reshaping worked correctly
  if (nrow(Monthly_Averages_long) == 0) {
    message("No data after reshaping.")
    return(NULL)
  }
  
  # Plot the data
  plot <- ggplot(Monthly_Averages_long, aes(x = Month, y = Value, color = Variable)) +
    geom_line(size = 1) +  # Lines representing the variables over time
    geom_point(size = 1.5) +  # Points for better visibility of data points
    labs(
      title = paste("Monthly series", year),
      x = "Month",
      y = "Values",
      color = "Variable"
    ) +
    facet_wrap(~District, scales = "free_y") +  # Adjust y-axis scales per district if needed
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Format the x-axis as months and years
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Save the plot to a file
  ggsave(paste0("plot_rain_", year, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
  
  # Also print the plot to the screen
  print(plot)
}

# Loop through the years and generate plots for each year
for (year in 2015:2022) {
  plot_rain(year)
}


##**Overall data**
overall_data <- read.csv("overall_data.csv") %>%
  mutate(
    Month = as.Date(Date, format = "%d/%B/%Y"),  # Convert Date column to Date format
    Year = as.factor(format(Month, "%Y"))       # Extract year as a factor
  )

overall_data<- read.csv("overall_data.csv")%>%
  pivot_longer(cols = c(malaria, rainfall, min_temp, max_temp, NDVI), 
               names_to = "Variable", 
               values_to = "Value")%>%
  ggplot(aes(x = Month, y = Value, color = Variable)) +
  geom_line(size = 1) +  # Lines representing the variables over time
  geom_point(size = 1.5) +  # Points for better visibility of data points
  labs(
    title = paste("Monthly series", year),
    x = "Month",
    y = "Values",
    color = "Variable"
  ) +# Adjust y-axis scales per district if needed
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Format the x-axis as months and years
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
