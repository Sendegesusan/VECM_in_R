# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Function to plot data for a given year
plot_yearly_data <- function(year) {
  # Read and process the overall data
  overall_data <- read.csv("overall_data.csv") %>%
    mutate(
      Month = as.Date(Date, format = "%d/%B/%Y"),  # Convert Date column to Date format
      Year = as.factor(format(Month, "%Y"))       # Extract year as a factor
    )
  
  # Filter the data for the specific year
  yearly_data <- overall_data %>%
    filter(Year == as.character(year))
  
  # Reshape the data to long format and create the plot
  plot <- yearly_data %>%
    pivot_longer(cols = c(malaria, rainfall, min_temp, max_temp, NDVI), 
                 names_to = "Variable", 
                 values_to = "Value") %>%
    ggplot(aes(x = Month, y = Value, color = Variable)) +
    geom_line(size = 1) +  # Lines representing the variables over time
    geom_point(size = 1.5) +  # Points for better visibility of data points
    labs(
      title = paste("Monthly Series for Year", year),
      x = "Month",
      y = "Values",
      color = "Variable"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = c(0, 0)) +  # Format the x-axis as months and years
    theme_classic() +
    theme(
      # plot.title = element_text(size = 14, face = "bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),  # Set axis text x only once
       legend.position = "bottom",
      # strip.text = element_text(size = 12),  # Increase the size of facet labels
      # axis.text.x = element_text(size = 10),  # Set text size for x-axis labels
      # axis.title.x = element_text(size = 12)  # Set text size for x-axis title
    )
  
  # Save the plot to a file
  ggsave(paste0("plot_", year, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
  
}

# Loop through the years and generate plots for each year
for (year in 2015:2022) {
  plot_yearly_data(year)
}
