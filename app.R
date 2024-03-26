library(dplyr)
library(ggplot2)
library(plotly)

setwd("/Users/nareshkumar/MS5130-3/MS5130")
data1 <- read.csv("dataset/Depression.csv")
data2 <- read.csv("dataset/Life expectancy.csv")
data3 <- read.csv("dataset/Suicide Rate.csv")

merged_data <- inner_join(data2, data3, by = "Country")

# Write the merged dataset to a new CSV file
write.csv(merged_data, "dataset/merged_data.csv", row.names = FALSE)

head(merged_data)
str(merged_data)

merged_data$GDP.per.capita <- as.numeric(gsub(",", "", merged_data$GDP.per.capita))

# Perform linear regression
model <- lm(Suicide.rate ~ Life.Expectancy..years....Men + Life.Expectancy..years....Women +
              Happiness.Score + Fertility.Rate..births.per.woman. + GDP.per.capita, data = merged_data)

# Summary of the regression model
summary(model)

# Visualize the regression line along with the actual data points
library(ggplot2)

ggplot(merged_data, aes(x = GDP.per.capita, y = Suicide.rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: Suicide Rate Prediction",
       x = "GDP per capita",
       y = "Suicide Rate") +
  theme_minimal()