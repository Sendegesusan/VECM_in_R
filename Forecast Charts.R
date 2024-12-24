
setwd("C:/Users/Hon. GAB/Documents/Research_support/New folder")
rm(list = ls())
library(readxl)
library(tidyverse)
library(gridExtra)

Forecasts <- read_excel("Forecasts.xlsx", sheet = "Yumbe")

Forecasts$Months <- factor(Forecasts$Months, levels = c("Jan", "Feb", "Mar", "Apr", 
                                                        "May", "Jun", "Jul", "Aug", 
                                                        "Sep", "Oct", "Nov", "Dec"))
dist<- c("Adjuman", "Bukomansimbi", "Gomba","Isingiro","Kiruhura ","Koboko",
         "Kotido","Lwengo","Nabilatuk ","Napak","Rwampara","Wakiso", "Yumbe")

# Plot the Forecast data with fixed aesthetics
Adjuman<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot the Forecast data with fixed aesthetics
Bukomansimbi<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Gomba<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Isingiro<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Kiruhura <-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Koboko<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Kotido<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Lwengo<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Nabilatuk<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Napak<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Rwampara<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Wakiso<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the Forecast data with fixed aesthetics
Yumbe<-ggplot(Forecasts, aes(x = Months, group = 1)) +
  geom_line(aes(y = Forecast), color = "blue", linewidth = 1) +  # Forecast line
  geom_line(aes(y = Lower), color = "orange", linetype = "dashed", linewidth = 1) +  # Lower bound
  geom_line(aes(y = Upper), color = "gray", linetype = "dashed", linewidth = 1) +  # Upper bound
  labs(
    title = "Forecast with Confidence Interval",
    x = "Months",
    y = "Values"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dist<- c("Adjuman", "Bukomansimbi", "Gomba","Isingiro","Kiruhura ","Koboko",
         "Kotido","Lwengo","Nabilatuk ","Napak","Rwampara","Wakiso", "Yumbe")

grid.arrange(Adjuman,Bukomansimbi,Gomba, Isingiro,Kiruhura,
             Koboko,Kotido,Lwengo,Nabilatuk,Napak,Rwampara,
             Wakiso,Yumbe, ncol = 2)

grid.arrange(Adjuman,Bukomansimbi,Gomba, Isingiro,Kiruhura,
             Koboko,Kotido,Lwengo,Nabilatuk,Napak,Rwampara,
             Wakiso,Yumbe, ncol = 2,
  widths = c(0.8, 0.8),  # Adjust the relative widths of columns
  heights = c(0.6, 0.6)  # Adjust the relative heights of rows
)

