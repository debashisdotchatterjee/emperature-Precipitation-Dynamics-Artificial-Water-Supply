# Install and load required libraries
install.packages(c("tidyverse", "forecast", "rstanarm", "devtools"))
library(devtools)
library(tidyverse)
library(forecast)
library(rstanarm)
# Install and load required libraries
install.packages(c("tidyverse", "forecast", "rstanarm", "devtools", "png"))
library(devtools)
library(tidyverse)
library(forecast)
library(rstanarm)
library(png)
# Install and load rnoaa from GitHub
devtools::install_github("ropensci/rnoaa")
library(rnoaa)

# Set NOAA API key (Replace 'API_KEY' with your actual key)
options(noaakey = "API_KEY")



####################


# Example: Daily precipitation data for a specific station
precip_data <- ncdc(datasetid = 'GHCND', stationid = 'GHCND:USW00094728',
                    startdate = '2020-01-01', enddate = '2020-12-31',
                    datatypeid = 'PRCP', limit = 1000)$data

# Example: Daily maximum temperature data for the same station
tmax_data <- ncdc(datasetid = 'GHCND', stationid = 'GHCND:USW00094728',
                  startdate = '2020-01-01', enddate = '2020-12-31',
                  datatypeid = 'TMAX', limit = 1000)$data

# Example: Daily minimum temperature data for the same station
tmin_data <- ncdc(datasetid = 'GHCND', stationid = 'GHCND:USW00094728',
                  startdate = '2020-01-01', enddate = '2020-12-31',
                  datatypeid = 'TMIN', limit = 1000)$data

# Inspect the structure of the data
str(tmax_data)
str(tmin_data)

# Convert the date columns from character to Date format
precip_data <- precip_data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%dT%H:%M:%S"))

tmax_data <- tmax_data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%dT%H:%M:%S"))

tmin_data <- tmin_data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%dT%H:%M:%S"))

# Data preparation and cleaning based on the correct column names
precip_data <- precip_data %>%
  select(date, value) %>%
  mutate(precipitation = value / 10) %>%  # Convert to mm
  select(date, precipitation)

tmax_data <- tmax_data %>%
  select(date, value) %>%
  mutate(tmax = value / 10) %>%  # Convert to °C
  select(date, tmax)

tmin_data <- tmin_data %>%
  select(date, value) %>%
  mutate(tmin = value / 10) %>%  # Convert to °C
  select(date, tmin)

# Calculate average temperature
temp_data <- tmax_data %>%
  inner_join(tmin_data, by = "date") %>%
  mutate(temperature = (tmax + tmin) / 2) %>%
  select(date, temperature)

# Merge datasets to form a complete climate data frame
climate_data <- precip_data %>%
  left_join(temp_data, by = "date")

# Plotting the climate data
climate_plot <- ggplot(climate_data, aes(x = date)) +
  geom_line(aes(y = precipitation, color = "Precipitation")) +
  geom_line(aes(y = temperature, color = "Temperature")) +
  labs(title = "Climate Data Over Time", y = "Values", color = "Variables") +
  theme_minimal()

# Save the plot
ggsave("climate_data_plot.png", plot = climate_plot)

# Multiple Linear Regression Model
mlr_model <- lm(precipitation ~ temperature, data = climate_data)
summary(mlr_model)

# Time Series Analysis with ARIMA
climate_ts <- ts(climate_data$precipitation, start = c(2020, 1), frequency = 12)
arima_model <- auto.arima(climate_ts)
summary(arima_model)
forecast_data <- forecast(arima_model, h = 24)
arima_forecast_plot <- autoplot(forecast_data) +
  labs(title = "ARIMA Model Forecast", y = "Precipitation")

# Save the ARIMA forecast plot
ggsave("arima_forecast_plot.png", plot = arima_forecast_plot)

# Bayesian Hierarchical Model using rstanarm
climate_data$month <- as.factor(format(climate_data$date, "%m"))

# Modify priors and increase the number of iterations
bayesian_model <- stan_glmer(
  precipitation ~ temperature + (1 | month),
  data = climate_data,
  family = gaussian,
  prior = normal(0, 1),
  prior_intercept = normal(0, 5),
  adapt_delta = 0.95,
  iter = 4000,
  warmup = 1000,
  chains = 4
)
summary(bayesian_model)

# Plotting the Bayesian Hierarchical Model results
bayesian_model_plot <- plot(bayesian_model, "areas") +
  labs(title = "Bayesian Hierarchical Model Coefficients")

# Save the Bayesian model plot
ggsave("bayesian_model_plot.png", plot = bayesian_model_plot)

# Display saved plots
library(grid)
plots <- list.files(pattern = "*.png")
for (plot in plots) {
  img <- readPNG(plot)
  grid.raster(img)
}

# Display plots within R environment
print(climate_plot)
print(arima_forecast_plot)
print(bayesian_model_plot)
