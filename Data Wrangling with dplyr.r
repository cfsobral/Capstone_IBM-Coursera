# Check if you need to install the `tidyverse` library
require("tidyverse")
library(tidyverse)

bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv")

# Or you may read it from here again
# url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# Notice some column names in the raw datasets are not standalized if you haven't done them properly in the previous lab

summary(bike_sharing_df)
dim(bike_sharing_df)

# Drop rows with `RENTED_BIKE_COUNT` column == NA
bike_sharing_df <- bike_sharing_df[!is.na(bike_sharing_df$RENTED_BIKE_COUNT), ]



# Print the dataset dimension again after those rows are dropped
bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))


bike_sharing_df %>% 
                filter(is.na(TEMPERATURE))

# Calculate the summer average temperature
tapply(bike_sharing_df$TEMPERATURE, bike_sharing_df$SEASONS == "Summer", mean)

# Impute missing values for TEMPERATURE column with summer average temperature
bike_sharing_df <- bike_sharing_df %>%
  replace_na(list(TEMPERATURE = 7.89705))

# Print the summary of the dataset again to make sure no missing values in all columns
summary(bike_sharing_df)

# Save the dataset as `seoul_bike_sharing.csv`
write.csv(bike_sharing_df, "seoul_bike_sharing.csv", row.names = FALSE)

# Using mutate() function to convert HOUR column into character type
bike_sharing_df_2 <- bike_sharing_df

# Convert SEASONS, HOLIDAY, FUNCTIONING_DAY, and HOUR columns into indicator columns.
## HOUR

bike_sharing_df_2 <- bike_sharing_df_2 %>%
  mutate(dummy = 1) %>%
  spread(
    key = HOUR,
    value = dummy,
    fill = 0
  )
  
 ## SEASONS
season <- c("Spring", 1)
bike_sharing_df_2 <- bike_sharing_df_2 %>%
  mutate(dummy = 1) %>%
  spread(
    key = SEASONS,
    value = dummy,
    fill = 0
  )

## HOLIDAY

bike_sharing_df_2 <- bike_sharing_df_2 %>%
  mutate(dummy = 1) %>%
  spread(
    key = HOLIDAY,
    value = dummy,
    fill = 0
  )

# Print the dataset summary again to make sure the indicator columns are created properly
summary(bike_sharing_df_2)

# Save the dataset as `seoul_bike_sharing_converted.csv`
# write_csv(dataframe, "seoul_bike_sharing_converted.csv")

 write_csv(bike_sharing_df_2, "seoul_bike_sharing_converted.csv")

# Use the `mutate()` function to apply min-max normalization on columns 
# `RENTED_BIKE_COUNT`, `TEMPERATURE`, `HUMIDITY`, `WIND_SPEED`, `VISIBILITY`, `DEW_POINT_TEMPERATURE`, `SOLAR_RADIATION`, `RAINFALL`, `SNOWFALL`
 ## RENTED_BIKE_COUNT
 bike_sharing_df_2$RENTED_BIKE_COUNT <- (bike_sharing_df_2$RENTED_BIKE_COUNT - min(bike_sharing_df_2$RENTED_BIKE_COUNT)) / 
   (max(bike_sharing_df_2$RENTED_BIKE_COUNT) - min(bike_sharing_df_2$RENTED_BIKE_COUNT))
 
 ## TEMPERATURE
 bike_sharing_df_2$TEMPERATURE <- (bike_sharing_df_2$TEMPERATURE - min(bike_sharing_df_2$TEMPERATURE)) / 
   (max(bike_sharing_df_2$TEMPERATURE) - min(bike_sharing_df_2$TEMPERATURE))
 
 ## HUMIDITY
 bike_sharing_df_2$HUMIDITY <- (bike_sharing_df_2$HUMIDITY - min(bike_sharing_df_2$HUMIDITY)) / 
   (max(bike_sharing_df_2$HUMIDITY) - min(bike_sharing_df_2$HUMIDITY))
 
 ## WIND_SPEED
 bike_sharing_df_2$WIND_SPEED <- (bike_sharing_df_2$WIND_SPEED - min(bike_sharing_df_2$WIND_SPEED)) / 
   (max(bike_sharing_df_2$WIND_SPEED) - min(bike_sharing_df_2$WIND_SPEED))
 
 ## VISIBILITY
 bike_sharing_df_2$VISIBILITY <- (bike_sharing_df_2$VISIBILITY - min(bike_sharing_df_2$VISIBILITY)) / 
   (max(bike_sharing_df_2$VISIBILITY) - min(bike_sharing_df_2$VISIBILITY))
 
 ## DEW_POINT_TEMPERATURE
 bike_sharing_df_2$DEW_POINT_TEMPERATURE <- (bike_sharing_df_2$DEW_POINT_TEMPERATURE - min(bike_sharing_df_2$DEW_POINT_TEMPERATURE)) / 
   (max(bike_sharing_df_2$DEW_POINT_TEMPERATURE) - min(bike_sharing_df_2$DEW_POINT_TEMPERATURE))
 
 ## SOLAR_RADIATION
 bike_sharing_df_2$SOLAR_RADIATION <- (bike_sharing_df_2$SOLAR_RADIATION - min(bike_sharing_df_2$SOLAR_RADIATION)) / 
   (max(bike_sharing_df_2$SOLAR_RADIATION) - min(bike_sharing_df_2$SOLAR_RADIATION))
 
 ## RAINFALL
 bike_sharing_df_2$RAINFALL <- (bike_sharing_df_2$RAINFALL - min(bike_sharing_df_2$RAINFALL)) / 
   (max(bike_sharing_df_2$RAINFALL) - min(bike_sharing_df_2$RAINFALL))
 
 ## SNOWFALL
 bike_sharing_df_2$SNOWFALL <- (bike_sharing_df_2$SNOWFALL - min(bike_sharing_df_2$SNOWFALL)) / 
   (max(bike_sharing_df_2$SNOWFALL) - min(bike_sharing_df_2$SNOWFALL))


# Print the summary of the dataset again to make sure the numeric columns range between 0 and 1
 summary(bike_sharing_df_2)

# Save the dataset as `seoul_bike_sharing_converted_normalized.csv`
# write_csv(dataframe, "seoul_bike_sharing_converted_normalized.csv")

write_csv(bike_sharing_df_2, "seoul_bike_sharing_converted_normalized.csv")

# Dataset list
dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')

for (dataset_name in dataset_list){
    # Read dataset
    dataset <- read_csv(dataset_name)
    # Standardized its columns:
    # Convert all columns names to uppercase
    names(dataset) <- toupper(names(dataset))
    # Replace any white space separators by underscore, using str_replace_all function
    names(dataset) <- str_replace_all(names(dataset), " ", "_")
    # Save the dataset back
    write.csv(dataset, dataset_name, row.names=FALSE)
}
