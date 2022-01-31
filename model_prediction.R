# Loading the libraries
library(tidyverse)
library(httr)
library(shiny)
library(ggplot2)
library(leaflet)
library(httr)
library(scales)
library(ggthemes)
library(htmltools)
library(crosstalk)
library(lubridate)

# Create some empty vectors to hold data temporarily
city <- c()
weather <- c()
temperature <- c()
visibility <- c()
humidity <- c()
wind_speed <- c()
hours <- c()
forecast_date <-c()
weather_labels<-c()
weather_details_labels<-c()
seasons <- c()

# Get weather forecast data by cities
get_weather_forecaset_by_cities <- function(city_names){
  
  # Get 5-days forecast data for each city
  for (city_name in city_names){
    url_get='https://api.openweathermap.org/data/2.5/forecast'
    # Replace the OpenWeatherAPI api_key with your own
    api_key <- "6989b155391e8b656141043f220de220"
    forecast_query <- list(q = city_name, appid = api_key, units="metric")
    response <- GET(url_get, query=forecast_query)
    json_list <-content(response, as="parsed")
    results <- json_list$list
    
    for(result in results) {
      # Get weather data and append them to vectors
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      
      # Get predictor variables
      temperature <- c(temperature, result$main$temp)
      visibility <- c(visibility, result$visibility)
      humidity <- c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      
      forecast_datetime <- result$dt_txt
      hour <- as.numeric(strftime(forecast_datetime, format="%H"))
      month <- as.numeric(strftime(forecast_datetime, format="%m"))
      forecast_date <- as.POSIXct(c(forecast_date, forecast_datetime), tz = "Europe/Paris")
      seasons <- "Spring"
      # Simple rule to determine season
      if (month >= 3 && month <= 5)
        season <- "SPRING"
      else if(month >= 6  &&  month <= 8)
        season <- "SUMMER"
      else if (month >= 9  && month <= 11)
        season <- "AUTUMN"
      else
        season <- "WINTER"
      # Add a HTML label to be shown on Leaflet
      weather_label <- paste(sep = "",
                             "<b><a href=''>",
                             city_name, 
                             "</a></b>", "</br>", 
                             "<b>", result$weather[[1]]$main, "</b></br>")
      # Add a detailed HTML label to be shown on Leaflet
      weather_detail_label <- paste(sep = "",
                                    "<b><a href=''>",
                                    city_name, 
                                    "</a></b>", "</br>", 
                                    "<b>", result$weather[[1]]$main, "</b></br>",
                                    "Temperature: ", result$main$temp, " C </br>",
                                    "Visibility: ", result$visibility, " m </br>",
                                    "Humidity: ", result$main$humidity, " % </br>", 
                                    "Wind Speed: ", result$wind$speed, " m/s </br>", 
                                    "Datetime: ", forecast_datetime, " </br>")
      weather_labels <- c(weather_labels, weather_label)
      weather_details_labels <- c(weather_details_labels, weather_detail_label)
      
      seasons <- c(seasons, season)
      hours <- c(hours, hour)
    }
  }
  # Create and return a tibble
  weather_df <- tibble(CITY_ASCII=city, WEATHER=weather, 
                       TEMPERATURE=temperature,
                       VISIBILITY=visibility, 
                       HUMIDITY=humidity, 
                       WIND_SPEED=wind_speed, SEASONS=season, HOURS=hours, FORECASTDATETIME=forecast_date,
                       DATE = as.Date(FORECASTDATETIME, tz="",format = "%y-%m-%d"),
                       LABEL=weather_labels, DETAILED_LABEL=weather_details_labels)
                     
  
  return(weather_df)
  
}

cities <- c("Seoul", "New York", "Paris", "Suzhou", "London")
city_weather_bike_df <- get_weather_forecaset_by_cities(cities)

# Load a saved regression model (variables and coefficients) from csv
load_saved_model <- function(model_name){
  model <- read_csv(model_name)
  model <- model %>% 
    mutate(Variable = gsub('"', '', Variable))
  coefs <- setNames(model$Coef, as.list(model$Variable))
  return(coefs)
}

# Predict bike-sharing demand using a saved regression model
predict_bike_demand <- function(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS){
  model <- load_saved_model("model.csv")
  weather_terms <- model['Intercept'] + TEMPERATURE*model['TEMPERATURE'] + HUMIDITY*model['HUMIDITY'] +
    WIND_SPEED*model['WIND_SPEED'] + VISIBILITY*model['VISIBILITY'] 
  season_terms <- c()
  hour_terms <- c()
  # Calculate season related regression terms
  for(season in SEASONS) {
    season_term <- switch(season, 'SPRING'=model['SPRING'],'SUMMER'=model['SUMMER'],
                          'AUTUMN'=model['AUTUMN'], 'WINTER'=model['WINTER'])
    season_terms <- c(season_terms, season_term)
  }
  # Calculate hour related regression terms
  for(hour in HOURS){
    hour_term<- switch(as.character(hour),'0'=model['0'],'1'=model['1'],'2'=model['2'],'3'=model['3'],
                       '4'=model['4'],'5'=model['5'],'6'=model['6'],'7'=model['7'],
                       '8'=model['8'],'9'=model['9'],'10'=model['10'],'11'=model['11'],
                       '12'=model['12'],'13'=model['13'],'14'=model['14'],'15'=model['15'],'16'=model['16'],
                       '17'=model['17'],'18'=model['18'],'19'=model['19'],'20'=model['20'],
                       '21'=model['21'],'22'=model['22'],'23'=model['23'])
    hour_terms <- c(hour_terms, hour_term)
    
  }
  
  return(as.integer(weather_terms + season_terms + hour_terms))     
  
}

# Define a bike-sharing demand level, used for leaflet visualization
calculate_bike_prediction_level<- function(predictions) {
  levels <- c()
  for(prediction in predictions){
    if(prediction <= 1000 && prediction > 0)
      levels <- c(levels, 'small')
    else if (prediction > 1000 && prediction < 3000)
      levels <- c(levels, 'medium')
    else  
      levels <- c(levels, 'large')
  }
  return(levels)
}

# Generate a data frame containing weather forecasting and bike prediction data
generate_city_weather_bike_data <- function (){
  cities_df <- read_csv("selected_cities.csv")
  weather_df <- get_weather_forecaset_by_cities(cities_df$CITY_ASCII)
  results <- weather_df %>% 
    mutate(BIKE_PREDICTION=predict_bike_demand(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS)) %>%
    mutate(BIKE_PREDICTION_LEVEL=calculate_bike_prediction_level(BIKE_PREDICTION))
  
  cities_bike_pred <- cities_df %>% left_join(results) %>% 
    select(CITY_ASCII, LNG, LAT, TEMPERATURE, HUMIDITY, BIKE_PREDICTION, BIKE_PREDICTION_LEVEL, LABEL, DETAILED_LABEL, HOURS, DATE, FORECASTDATETIME)
    
   
    return(cities_bike_pred)
}

city_weather_bike_df <- generate_city_weather_bike_data()
city_weather_bike_df






## Separating DATE and HOURS from FORECASTDATETIME
# Creating a example dataframe
## ADD 3 HOURS IN DATAFRAME
#city_weather_bike_df$FORECASTDATETIME <- city_weather_bike_df$FORECASTDATETIME + hours(3)


#cities_bike_pred$BIKE_PREDICTION <- (cities_bike_pred$BIKE_PREDICTION - min(cities_bike_pred$BIKE_PREDICTION)) / 
#  (max(cities_bike_pred$BIKE_PREDICTION) - min(cities_bike_pred$BIKE_PREDICTION))  

#cities_max_bike <- city_weather_bike_df%>%
#  group_by(CITY_ASCII,LAT,LNG,BIKE_PREDICTION,BIKE_PREDICTION_LEVEL,
#           LABEL,DETAILED_LABEL,FORECASTDATETIME,TEMPERATURE)%>%
#  summarize(count = n(), max = max(BIKE_PREDICTION, na.rm = T))
#cities_max_bike <- data.frame(cities_max_bike)


#xTest <-  tidyr::separate(xTest, FORECASTDATETIME, c("DATE", "HOURS"), sep = " ", remove = F)
#xTest
## Backuping all datasets

#bkp_city_weather_bike_df <- city_weather_bike_df
#bkp_predict_city_weather_bike_df <- cities_max_bike 

## Converting cities_max_bike to city_weather_bike_df

#city_weather_bike_df <- cities_max_bike





### VERIFYING THE DATASETS

#xDataset <- read.csv("selected_cities.csv", header = T, fill = T)
#modeldataset <- read.csv("model.csv", header = T, fill = T)

#head(xDataset)

#head(modeldataset)

## Creating My Model
## LM Model

#xTest_df <- stack(lm_poly$fit$coefficients) # These values come from Refine Baseline Regression Models.R 

#xTest_df <- xTest_df%>%replace(is.na(.), 0)
#xTest_df <- rename(xTest_df, Coef = values)
#xTest_df <- rename(xTest_df, Variable = ind)

#xTest_df <- xTest_df[ ,c(2,1)]
#xTest2_df <- xTest_df[ , c(2, 1)]

#write.csv(xTest_df, "my_model.csv", )

#city_weather_bike_df$TIME <- as.Date(city_weather_bike_df$FORECASTDATETIME)
#city_weather_bike_df$HOUR <- format(as.POSIXct(city_weather_bike_df$FORECASTDATETIME), format = "%H:%M:%S")

#bkp_city_weather_bike_df <- city_weather_bike_df

# Write city_weather_bike_df to `city_weather_bike_df.csv`
#write.csv(city_weather_bike_df, "raw_city_weather_bike_df.csv", row.names=FALSE)

