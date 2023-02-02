# Load packages
library(readxl)
library(tidyverse)
library(summarytools)

# Load data
data <- read_excel("Datasets.xlsx")

# Explore and transform data
head(data)
tail(data)
summary(data)
summary(is.na(data))
VNM$Date <- as.Date(data$Date, format = "%d/%m/%Y")


# Descriptive statistics
descr(data$Return, round.digits = 6, 
      stats = c("mean", "med", "sd", "min", "max", "skewness", "kurtosis"))

## VISUALIZE DATA ##
# Price plot
ggplot(data, aes(x = Date, y = Price)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = "Philippines (PSEi)")

# Return plot
ggplot(data, aes(x = Date, y = Return)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = "Philippines (PSEi)")

# Return distribution plot
ggplot(data, aes(x = Return)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "#3366ff", linewidth = 1) + 
  labs(title = "Philippines (PSEi)")

## OLS regression ##
# Day-of-the-week
DOW <- lm(Return ~ as.factor(DOW), data = data)
summary(DOW)

# Month-of-the-year
MOY <- lm(Return ~ as.factor(MOY), data = data)
summary(MOY)