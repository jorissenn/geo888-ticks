library(shiny)
library(leaflet)
library(sf)
library(ggplot2)

if (!interactive()){
  fishnet <- st_transform(read_sf("processed_data/validation/validation.shp"), crs=4326)
} else {
  fishnet <- st_transform(read_sf("../../processed_data/validation/validation.shp"), crs=4326)
}

# defining the color scale
min_bites <- min(fishnet$bites_w,
                 fishnet$bites_sp,
                 fishnet$bites_su,
                 fishnet$bites_a)
max_bites <- max(fishnet$bites_w,
                 fishnet$bites_sp,
                 fishnet$bites_su,
                 fishnet$bites_a)

pal <- colorNumeric("YlOrRd", c(min_bites, max_bites))
  
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.1,
                                     minZoom = 7.8)) %>%
      setView(lng=7.671679, lat=46.96051, zoom = 7.8) %>%
      setMaxBounds(lng1=4.857424, lat1=46.09225, lng2=10.49224, lat2=47.81489) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addLegend(pal = pal, 
                values = c(min_bites, max_bites),
                opacity = 1, 
                title=HTML("Number of<br>reported tick<br>bites and<br>sightings"),
                position="bottomright"
      )
    })
  
  observeEvent(input$season, {
    # remove current display from the map
    leafletProxy("map") %>%
      clearGroup("tick_bites")
    
    # cover every combination of season and scenario to adjust plots and map
    if (input$season == "Spring") {
      labels_bites <- sprintf(
        "<strong>Reported tick bites</strong><br/>%s",
        round(fishnet$bites_sp,0)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "tick_bites",
                    fillColor = ~pal(bites_sp),
                    fillOpacity = 0.7,
                    color = "white",
                    dashArray = "3",
                    weight = 1,
                    highlightOptions = highlightOptions(
                      weight = 3,
                      color = "black",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels_bites,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    } else if (input$season == "Summer"){
      labels_bites <- sprintf(
        "<strong>Reported tick bites</strong><br/>%s",
        round(fishnet$bites_su,0)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "tick_bites",
                    fillColor = ~pal(bites_su),
                    fillOpacity = 0.7,
                    color = "white",
                    dashArray = "3",
                    weight = 1,
                    highlightOptions = highlightOptions(
                      weight = 3,
                      color = "black",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels_bites,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))   
    } else if (input$season == "Autumn"){
      labels_bites <- sprintf(
        "<strong>Reported tick bites</strong><br/>%s",
        round(fishnet$bites_a,0)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "tick_bites",
                    fillColor = ~pal(bites_a),
                    fillOpacity = 0.7,
                    color = "white",
                    dashArray = "3",
                    weight = 1,
                    highlightOptions = highlightOptions(
                      weight = 3,
                      color = "black",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels_bites,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))      
    } else if (input$season == "Winter"){
      labels_bites <- sprintf(
        "<strong>Reported tick bites</strong><br/>%s",
        round(fishnet$bites_w,0)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "tick_bites",
                    fillColor = ~pal(bites_w),
                    fillOpacity = 0.7,
                    color = "white",
                    dashArray = "3",
                    weight = 1,
                    highlightOptions = highlightOptions(
                      weight = 3,
                      color = "black",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels_bites,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))     
    } 
  })
}