# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
require(ggthemes)
require(htmltools)
require(crosstalk)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  city_dropdown = c("All", "Seoul", "New York", "Suzhou", "London")
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  
  cities_max_bike <- city_weather_bike_df%>%
    group_by(CITY_ASCII,LAT,LNG,HUMIDITY,BIKE_PREDICTION,BIKE_PREDICTION_LEVEL,
             LABEL,DETAILED_LABEL,HOURS,DATE,FORECASTDATETIME,TEMPERATURE)%>%
    summarize(count = n(), MAX = max(BIKE_PREDICTION, na.rm = T, .groups = "drop"))
  cities_max_bike <- data.frame(cities_max_bike)
  

  # Observe drop-down event
  
  observeEvent(input$city_dropdown,
               if(input$city_dropdown != "All"){
                 filteredData <- cities_max_bike%>%
                   filter(CITY_ASCII == input$city_dropdown)
      
 # Then render output plots with an id defined in ui.R
 
 # If just one specific city was selected, then render a leaflet map with one marker
 # on the map and a popup with DETAILED_LABEL displayed
  
   output$city_bike_map <- renderLeaflet({
       leaflet(data = filteredData)%>%
       addTiles()%>%
       addPopups(~LNG, ~LAT, ~DETAILED_LABEL)%>%
       addMarkers(data = filteredData,
                  lng = filteredData$LNG, 
                  lat = ~filteredData$LAT,
                  popup = ~filteredData$DETAILED_LABEL,
                  label = ~htmlEscape(filteredData$DETAILED_LABEL),
                  labelOptions = labelOptions(noHide = F, textOnly = T,
                                              direction = "bottom", 
                                              style = list( "color" = "black", 
                                                            "font-family" = "serif",
                                                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)", 
                                                            "font-size" = "12px")))
     
     }) 
  
   observeEvent(input$city_dropdown,
                if(input$city_dropdown != "All"){
                  filteredData <- city_weather_bike_df%>%
                    filter(CITY_ASCII == input$city_dropdown)
                     #format(as.Date(TIME, format = "%B %d %Y"), "%m-%d-%Y")
                  
                  output$bike_line <- renderPlot({
                    ggplot(data = filteredData, aes(x=FORECASTDATETIME, y= BIKE_PREDICTION, label = BIKE_PREDICTION)) + 
                      geom_text(check_overlap = T, hjust = "inward") + geom_point() + geom_line(linetype = "dashed", color = "darkblue") +
                      labs(x = "Time(3 hours ahead)", y = "Predicated Bike Count")  
                    
                  })
                    
                
                   
      }) 
                
                  
   
  })
                  
     observeEvent(input$city_dropdown, 
                  if(input$city_dropdown != "All"){ 
                    filteredData2 <- cities_max_bike%>% 
                      filter(CITY_ASCII == input$city_dropdown)
                      
                      
                    
                                 output$temp_line <- renderPlot({
                                   ggplot(data = filteredData2, aes(x = HOURS, y= TEMPERATURE, label = TEMPERATURE, group = 1)) + 
                                     geom_text(aes(label = paste(TEMPERATURE,"°C")), check_overlap = T, hjust = "inward") + geom_point() + 
                                     geom_line(color = "yellow") + labs(x = "Time (3 hours ahead)", y = "TEMPERATURE (°C)")
                                   
                                   
                                   
      })
 
      observeEvent(input$city_dropdown, 
                   if(input$city_dropdown != "All"){
                     filteredData <- cities_max_bike%>% 
                       filter(CITY_ASCII == input$city_dropdown)
                       
                       
                     output$bike_date_output <- renderText({
                       paste0("Time = ", as.POSIXct(input$plot_click$x, origin = "1970-01-01"),"\n","BikeCountPred = ", input$plot_click$y)    
                                                       
              })
      
      observeEvent(input$city_dropdown, 
                   if(input$city_dropdown != "All"){
                     filteredData3 <- cities_max_bike%>%
                       filter(CITY_ASCII == input$city_dropdown)
                     
                     output$humidty_pred_chart <- renderPlot({
                       ggplot(filteredData3, aes(x = HUMIDITY, y = BIKE_PREDICTION)) + geom_point() + 
                         geom_smooth(method = "lm", formula = y ~ poly(x, 4))
                     })
                     
                   })
   
   })}
 
 # If All was selected from dropdown, then render a leaflet map with circle markers
 # and popup weather LABEL for all five cities
 
  else{
   output$city_bike_map <- renderLeaflet({
     leaflet(cities_max_bike)%>%
       clearMarkers()%>%
       addTiles()%>%
       addPopups(~LNG, ~LAT, ~LABEL, options = popupOptions(closeButton = F))%>%
       addCircleMarkers(lng = ~LNG, 
                        lat = ~LAT,
                        radius = ~ifelse(BIKE_PREDICTION_LEVEL == 'small', 6, 12),
                        color = ~color_levels(BIKE_PREDICTION_LEVEL),
                        fill = TRUE,
                        weight = 2,
                        stroke = FALSE,
                        fillOpacity = 0.8
                        )
                 
   })
   output$temp_line <- renderPlot({
     p <-  ggplot(data = city_weather_bike_df, aes(x=HOURS, y=TEMPERATURE, label = TEMPERATURE)) + 
       geom_text(aes(label = paste(TEMPERATURE,"°C")),check_overlap = T, hjust = "inward") + geom_point() + 
       geom_line(linetype = "dashed", color = "blue") +
       labs(x = "Time (3 hours ahead)", y = "TEMPERATURE (°C)")
     p + theme_solarized()  
   
   
   })
   
   
   
   output$bike_line <- renderPlot({
     q <- ggplot(data = city_weather_bike_df, aes(x=FORECASTDATETIME, y=BIKE_PREDICTION, label = BIKE_PREDICTION)) + 
       geom_text(check_overlap = T) + geom_point() + geom_line(color = "yellow") + labs(x = "Time(3 hours ahead)", y = "Predicated Bike Count")
     q + theme_economist() 
  
  })
   
  
    output$humidty_pred_chart <- renderPlot({
    s <- ggplot(data = city_weather_bike_df, aes(x = HUMIDITY, y = BIKE_PREDICTION)) + geom_point() + 
      geom_smooth(method = "lm", formula = y ~ poly(x, 4))
    
    s + theme_light()
  })
  
    
  })

  
 
})
