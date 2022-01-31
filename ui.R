# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"),
  
  # Create a side-bar layout
  sidebarLayout(
    position = "right",
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput("city_bike_map"), #, width = "100%", height = 1000)
      leafletOutput("city_bike_map_2"),
      #absolutePanel(top = 10, right = 10),
     
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
     
      # select drop down list to select city
      selectInput(inputId = "city_dropdown", "Choose City", c("All", "Seoul", "New York", "Suzhou", "London")),
      h6("Select city dropdown to show its bike prediction details:", style = "color:blue"),
      plotOutput("temp_line"),
      plotOutput("bike_line", click = "plot_click" ),
      verbatimTextOutput("bike_date_output"),
      plotOutput("humidty_pred_chart")
      
      
    ))
))