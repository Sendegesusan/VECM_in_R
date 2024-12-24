# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(officer)

# Function to plot data for a given year with two y-axes
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
  
  # Reshape the data into long format
  yearly_long <- yearly_data %>%
    pivot_longer(cols = c(malaria, rainfall, min_temp, max_temp, NDVI), 
                 names_to = "Variable", 
                 values_to = "Value")
  
  # Define scaling factors for secondary y-axis
  max_primary <- max(yearly_data$malaria, yearly_data$rainfall, na.rm = TRUE)
  max_secondary <- max(yearly_data$min_temp, yearly_data$max_temp, yearly_data$NDVI, na.rm = TRUE)
  scale_factor <- max_primary / max_secondary
  
  # Plot with two y-axes
  plot <- ggplot() +
    # Primary axis: Malaria cases and Rainfall
    geom_line(data = yearly_long %>% filter(Variable %in% c("malaria", "rainfall")),
              aes(x = Month, y = Value, color = Variable), size = 1) +
    
    # Secondary axis: Temperature and NDVI
    geom_line(data = yearly_long %>% filter(Variable %in% c("min_temp", "max_temp", "NDVI")),
              aes(x = Month, y = Value * scale_factor, color = Variable), size = 1) +
    
    # Scale the y-axes
    scale_y_continuous(
      name = "Malaria Cases and Rainfall",
      sec.axis = sec_axis(~ . / scale_factor, name = "Temperature and NDVI")
    ) +
    
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = c(0, 0)) +
    
    # Labels and theme
    labs(
      title = paste("Monthly Series for Year", year),
      x = "Month",
      color = "Variable"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Save the plot to a file
  ggsave(paste0("plot_", year, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
}

# Loop through the years and generate plots for each year
for (year in 2015:2022) {
  plot_yearly_data(year)
}

# Create a Word document to store all plots
doc <- read_docx()

for (year in 2015:2022) {
  plot_path <- paste0("plot_", year, ".png")
  doc <- doc %>%
    body_add_par(value = paste("Monthly Series for Year", year), style = "heading 2") %>%
    body_add_img(src = plot_path, width = 6, height = 4)
}

# Save the Word document
print(doc, target = "yearly_plots_with_dual_axes.docx")
