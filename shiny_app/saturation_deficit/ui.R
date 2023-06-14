library(shiny)
library(leaflet)
library(ggplot2)

ui <- fluidPage(
  fluidRow( 
    column(1),
    column(5, selectInput("season", "Select season", c("Spring", "Summer", "Autumn", "Winter"))),
    column(5, selectInput("scenario", "Select scenario", c("2023", "2085, RCP 4.5", "2085, RCP 8.5"))),
    column(1)),        
  fluidRow(leafletOutput("map"))
)
