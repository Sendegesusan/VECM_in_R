
setwd("C:/Users/Hon. GAB/Documents/Research_support/New folder")
library(readxl)
library(tidyverse)
library(gridExtra)

# Set working directory and load data

Forecasts <- read.csv("malaria_forecasts.csv")
head(Forecasts)
# # Ensure 'Months' and 'Districts' columns are factors
Forecasts$Months <- factor(Forecasts$Month, levels = c("Jan", "Feb", "Mar", "Apr",
                                                        "May", "Jun", "Jul", "Aug",
                                                        "Sep", "Oct", "Nov", "Dec"))

# List of districts
districts <- c("Adjuman", "Bukomansimbi", "Gomba", "Isingiro", "Kiruhura ", 
               "Koboko", "Kotido", "Lwengo", "Nabilatuk ", "Napak", 
               "Rwampara", "Wakiso", "Yumbe")

# Create a function to generate smaller plots for each district
create_plot <- function(district) {
  ggplot(Forecasts %>% filter(District == district), aes(x = Months, group = 1)) +
    geom_line(aes(y = Forecast), color = "blue", linewidth = 0.5) +
    geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 0.6) +
    geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 0.6) +
    labs(
      title = paste(district),
      x = " ",
      y = "Forecats"
    ) +
    theme_classic(base_size = 8) +  # Reduce base text size
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Generate plots for all districts
plots <- lapply(districts, create_plot)

# Arrange plots in a grid
final_plot <- grid.arrange(grobs = plots, ncol = 3)

# Save the output to a file that fits on one page
ggsave("Forecasts_All_Districts.png", plot = final_plot, width = 12, height = 8)

#Overall forecast
Malaria_forecasts_Overall <- read_csv("Malaria_forecasts Overall.csv")%>%
  mutate(Months = factor(Month, levels = c("January", "February", "March", "April",
                                                                      "May", "June", "July", "August","September", "October", 
                                                                      "November", "December")))%>%
  ggplot( aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 0.5) +
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 0.6) +
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 0.6) +
  labs(
    title = "Overall Malaria Forecasts",
    x = " ",
    y = "Forecats"
  ) +
  theme_classic(base_size = 8) +  # Reduce base text size
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
