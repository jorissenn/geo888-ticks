library(shiny)
library(leaflet)
library(sf)
library(ggplot2)

if (!interactive()){
  fishnet <- st_transform(read_sf("processed_data/fishnet/fishnet.shp"), crs=4326)
} else {
  fishnet <- st_transform(read_sf("../../processed_data/fishnet/fishnet.shp"), crs=4326)
}

# defining the color scale
min_sd <- min(fishnet$sd_w_2023,
              fishnet$sd_sp_2023,
              fishnet$sd_su_2023,
              fishnet$sd_a_2023,
              fishnet$sd_w_45,
              fishnet$sd_sp_45,
              fishnet$sd_su_45,
              fishnet$sd_a_45,
              fishnet$sd_w_85,
              fishnet$sd_sp_85,
              fishnet$sd_su_85,
              fishnet$sd_a_85)
max_sd <- max(fishnet$sd_w_2023,
              fishnet$sd_sp_2023,
              fishnet$sd_su_2023,
              fishnet$sd_a_2023,
              fishnet$sd_w_45,
              fishnet$sd_sp_45,
              fishnet$sd_su_45,
              fishnet$sd_a_45,
              fishnet$sd_w_85,
              fishnet$sd_sp_85,
              fishnet$sd_su_85,
              fishnet$sd_a_85)

pal <- colorNumeric("Reds", c(min_sd, max_sd))
  
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
                values = c(min_sd, max_sd),
                opacity = 1, 
                title=HTML("Saturation<br>deficit"),
                position="bottomright"
      )
    })
  
  observeEvent(list(input$season, input$scenario), {
    # remove current display from the map
    leafletProxy("map") %>%
      clearGroup("saturation_deficit")
    
    # cover every combination of season and scenario to adjust plots and map
    if (input$season == "Spring" & input$scenario == "2023") {
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_sp_2023,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_sp_2023),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    } else if (input$season == "Summer" & input$scenario == "2023"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_su_2023,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_su_2023),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))    
    } else if (input$season == "Autumn" & input$scenario == "2023"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_a_2023,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_a_2023),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))     
    } else if (input$season == "Winter" & input$scenario == "2023"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_w_2023,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_w_2023),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))     
    } else if (input$season == "Spring" & input$scenario == "2085, RCP 4.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_sp_45,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_sp_45),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))        
    } else if (input$season == "Summer" & input$scenario == "2085, RCP 4.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_su_45,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_su_45),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))      
    } else if (input$season == "Autumn" & input$scenario == "2085, RCP 4.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_a_45,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_a_45),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))      
    } else if (input$season == "Winter" & input$scenario == "2085, RCP 4.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_w_45,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_w_45),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))     
    } else if (input$season == "Spring" & input$scenario == "2085, RCP 8.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_sp_85,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_sp_85),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))      
    } else if (input$season == "Summer" & input$scenario == "2085, RCP 8.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_su_85,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_su_85),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))    
    } else if (input$season == "Autumn" & input$scenario == "2085, RCP 8.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_a_85,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_a_85),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))     
    } else if (input$season == "Winter" & input$scenario == "2085, RCP 8.5"){
      labels_sd <- sprintf(
        "<strong>Saturation deficit</strong><br/>%s",
        round(fishnet$sd_w_85,1)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = fishnet,
                    group = "saturation_deficit",
                    fillColor = ~pal(sd_w_85),
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
                    label = labels_sd,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))      
    }
  })
}