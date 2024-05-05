library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(purrr)
library(modelsummary)
library(skimr)
library(xtable)
library(knitr)
library(stargazer)
library(tidyr)
library(kableExtra)

# GRABBING DATA
county_temperature = 
  county_temperature = read_csv("county_temperature.csv")

tornadoData = 
  read_csv("1950-2021_all_tornadoes.csv")

# DATA CLEANING
# Renaming variables
colnames(county_temperature)[1] = "fips"
colnames(tornadoData)[2] = "year"

# Creating a fips variable in the tornado data set
# Pad the variable 'f1' to three digits
tornadoData$f1 = sprintf("%03d", tornadoData$f1)

# Combining 'stf' and padded 'f1'
tornadoData$fips <- paste(tornadoData$stf, tornadoData$f1, sep = "")

# Convert the 'fips' variable to numeric type
tornadoData <- mutate(tornadoData, fips = as.numeric(fips))

# Merging the two data sets on the fips variablE
# without matching tornado data
data <- left_join(tornadoData, county_temperature, by = c("fips", "year"))

# Taking out observations past 2019, due to lack of temperature data
data = data %>%
  filter(year <= 2019)

# Removing data that does not have a match in county temperatures data set
data <- data[!is.na(data$mean_lat), ]

# selecting only the variables plan to use 
data <- select(data, year, mag, slon, slat, mean_tmax_jan, mean_tmax_jul,
               mean_tmin_jan, mean_tmin_jul, mean_prec_jan, mean_prec_jul)


# SUMMARY STATISTICS
# Creating a summary statistics table to be exported to a latex
summ_tbl <- data.frame(t(sapply(data, function(x) {
  x <- x[!is.na(x)]
  c(n = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})))

rownames(summ_tbl) <- names(data)

# Convert the summary table to an xtable object
xtable_obj <- xtable(summ_tbl, caption = "Summary Statistics", digits = 2)

# Define the folder path
folder_path <- "~/Documents/datasciencefinalproject/"

# Define the file name
file_name <- "summary_table.tex"

# Define the full file path
file_path <- paste0(folder_path, file_name)

# Print the LaTeX code to the specified file
print(xtable_obj, caption.placement = "top", include.rownames = TRUE, file = file_path)


# creating latex of summary statistics for sub data sets
# Summary table for EF1plus dataset
summ_tbl_EF1plus <- data.frame(t(sapply(EF1plus, function(x) {
  x <- x[!is.na(x)]
  c(n = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})))

rownames(summ_tbl_EF1plus) <- names(EF1plus)

# Convert the summary table to an xtable object
xtable_obj_EF1plus <- xtable(summ_tbl_EF1plus, caption = "Summary Statistics for EF1plus dataset", digits = 2)

# Define the folder path and file name for EF1plus dataset
folder_path_EF1plus <- "~/Documents/datasciencefinalproject/"
file_name_EF1plus <- "summary_table_EF1plus.tex"
file_path_EF1plus <- paste0(folder_path_EF1plus, file_name_EF1plus)

# Print the LaTeX code to the specified file for EF1plus dataset
print(xtable_obj_EF1plus, caption.placement = "top", include.rownames = TRUE, file = file_path_EF1plus)

# Summary table for EF1plus_recent dataset
summ_tbl_EF1plus_recent <- data.frame(t(sapply(EF1plus_recent, function(x) {
  x <- x[!is.na(x)]
  c(n = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})))

rownames(summ_tbl_EF1plus_recent) <- names(EF1plus_recent)

# Convert the summary table to an xtable object
xtable_obj_EF1plus_recent <- xtable(summ_tbl_EF1plus_recent, caption = "Summary Statistics for EF1plus_recent dataset", digits = 2)

# Define the folder path and file name for EF1plus_recent dataset
folder_path_EF1plus_recent <- "~/Documents/datasciencefinalproject/"
file_name_EF1plus_recent <- "summary_table_EF1plus_recent.tex"
file_path_EF1plus_recent <- paste0(folder_path_EF1plus_recent, file_name_EF1plus_recent)

# Print the LaTeX code to the specified file for EF1plus_recent dataset
print(xtable_obj_EF1plus_recent, caption.placement = "top", include.rownames = TRUE, file = file_path_EF1plus_recent)



# GRAPHS
# creating the mean for lat and lon
data <- data %>%
  group_by(year) %>%
  mutate(mean_elon = mean(slon, na.rm = TRUE),
         mean_elat = mean(slat, na.rm = TRUE))

# Load a US map data
us_map <- map_data("state")

# Plot the mean data over the US map
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(data = data, aes(x = mean_elon, y = mean_elat, color = factor(year)),
             size = 3, alpha = 0.7) +
  scale_color_discrete(name = "Year") +
  labs(x = "Mean Longitude", y = "Mean Latitude", title = "Mean Locations in the US by Year") +
  theme_minimal()


# Line plot of mean longitude and year
ggplot(data = data, aes(x = year, y = mean_elon)) +
  geom_line() +
  labs(x = "Year", y = "Mean Longitude", 
       title = "Mean Longitude of Tornado Occurrences Over Time") +
  theme_minimal()

# Line plot of mean latitude and year
ggplot(data = data, aes(x = year, y = mean_elat)) +
  geom_line() +
  labs(x = "Year", y = "Mean Latitude", 
       title = "Mean Latitude of Tornado Occurrences Over Time") +
  theme_minimal()


# LINEAR REGRESSIONS
# Convert the 'year' variable to a factor
data$year <- as.factor(data$year)
EF1plus$year <- as.factor(EF1plus$year)
EF1plus_recent$year <- as.factor(EF1plus_recent$year)

# whole data
lon_est = lm(slon ~ mean_tmax_jul + mean_prec_jul + year, data = data)
lat_est = lm(slat ~ mean_tmax_jul + mean_prec_jul + year, data = data)
modelsummary(list(lon_est, lat_est), stars = TRUE)

# EF1 sub data
lon_est.2 = lm(slon ~ mean_tmax_jul + mean_prec_jul + year, data = EF1plus)
lat_est.2 = lm(slat ~ mean_tmax_jul + mean_prec_jul + year, data = EF1plus)

# EF1 pre 1970 sub data
lon_est.3 = lm(slon ~ mean_tmax_jul + mean_prec_jul + year, data = EF1plus_recent)
lat_est.3 = lm(slat ~ mean_tmax_jul + mean_prec_jul + year, data = EF1plus_recent)
modelsummary(list(lon_est, lat_est, lon_est.2, lat_est.2, lon_est.3, lat_est.3), 
             stars = TRUE)

# exporting regression results as latex
# Generate table using stargazer
stargazer(lon_est, lat_est, type = "latex", 
          title = "Regression Results Whole Data",
          align = TRUE)

stargazer(lon_est.3, lat_est.3, type = "latex", 
          title = "Regression Results Whole Data",
          align = TRUE)


# exporting regression results of all models
# Define variable labels
covariate_labels <- c("Mean Temp July", "Mean Precip July")

# Define the variables to keep (excluding year variables)
keep_vars <- c("mean_tmax_jul", "mean_prec_jul")

# Generate table using stargazer
stargazer(lon_est, lat_est, lon_est.2, lat_est.2, lon_est.3, lat_est.3, 
          type = "latex", title = "Regression Results", align = TRUE,
          covariate.labels = covariate_labels, keep = keep_vars)


# plotting the predictions of the year variables using 3rd model
# Extract coefficients from the regression model, excluding intercept and non-year variables
coefficients_lon <- coef(lon_est.3)[-c(1, 2, 3)]  
coefficients_lat <- coef(lat_est.3)[-c(1, 2, 3)]  

# Define the years
years <- 1971:2019

# creating a new false intercept 
# Extract coefficients from the regression model
coef_temp <- coef(lon_est.3)["mean_tmax_jul"]
coef_prec <- coef(lon_est.3)["mean_prec_jul"]
intercept <- coef(lon_est.3)["(Intercept)"]

# Mean values of temperature and precipitation
mean_temp <- mean(data$mean_tmax_jul)
mean_prec <- mean(data$mean_prec_jul)

# Calculate the new intercept
new_intercept <- intercept + (coef_temp * mean_temp) + (coef_prec * mean_prec)

# Extract coefficients from the regression model
coef_temp_lat <- coef(lat_est.3)["mean_tmax_jul"]
coef_prec_lat <- coef(lat_est.3)["mean_prec_jul"]
intercept_lat <- coef(lat_est.3)["(Intercept)"]

# Mean values of temperature and precipitation
mean_temp_lat <- mean(data$mean_tmax_jul)
mean_prec_lat <- mean(data$mean_prec_jul)

# Calculate the new intercept for latitude
new_intercept_lat <- intercept_lat + (coef_temp_lat * mean_temp_lat) + (coef_prec_lat * mean_prec_lat)


# Create empty vectors to store predicted values
predicted_values_lon <- numeric(length(years))
predicted_values_lat <- numeric(length(years))

# Calculate predicted values for longitude and latitude
for (i in seq_along(years)) {
  predicted_values_lon[i] <- new_intercept + coefficients_lon[i]
  predicted_values_lat[i] <- new_intercept_lat + coefficients_lat[i]
}

# Create data frames for longitude and latitude predictions
predictions_lon <- data.frame(year = years, predicted_value_lon = predicted_values_lon)
predictions_lat <- data.frame(year = years, predicted_value_lat = predicted_values_lat)

# Merge longitude and latitude predictions
predictions <- merge(predictions_lon, predictions_lat, by = "year")

# Plot longitude predictions
ggplot(predictions, aes(x = year)) +
  geom_line(aes(y = predicted_value_lon), color = "blue") +
  geom_line(aes(y = predicted_value_lat), color = "red") +
  labs(x = "Year", y = "Predicted Value", 
       title = "Predicted Longitude and Latitude Over Time")


# plotting predictions together over a US map
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_path(data = predictions, aes(x = predicted_value_lon, y = predicted_value_lat),
            color = "black") +
  geom_point(data = filter(predictions, year == 1971),
             aes(x = predicted_value_lon, y = predicted_value_lat, color = "blue"), size = 3) +
  geom_point(data = filter(predictions, year == 2019),
             aes(x = predicted_value_lon, y = predicted_value_lat, color = "red"), size = 3) +
  labs(x = "Predicted Longitude", y = "Predicted Latitude", 
       title = "Predicted Locations Over US Map") +
  scale_color_manual(values = c("blue" = "blue", "red" = "red"),
                     labels = c("1971", "2019"),
                     name = "Year")
