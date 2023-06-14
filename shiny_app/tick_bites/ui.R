library(shiny)
library(leaflet)
library(ggplot2)

ui <- fluidPage(
  fluidRow( 
    column(1),
    column(5, selectInput("season", "Select season", c("Spring", "Summer", "Autumn", "Winter"))),
    column(6)),        
  fluidRow(leafletOutput("map"))
)
