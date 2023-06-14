library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(dplyr)

if (!interactive()){
  tick_bite_risk <- st_transform(read_sf("processed_data/tick_bite_risk/tick_bite_risk.shp"), crs=4326)
} else {
  tick_bite_risk <- st_transform(read_sf("../../processed_data/tick_bite_risk/tick_bite_risk.shp"), crs=4326)
}

# 12 individual dataframes, containing all the values
risk_spring_23 <- data.frame(values = tick_bite_risk$risk_sp_23, season ="Spring", scenario = "2023")
risk_summer_23 <- data.frame(values = tick_bite_risk$risk_su_23, season ="Summer", scenario = "2023")
risk_autumn_23 <- data.frame(values = tick_bite_risk$risk_a_23, season ="Autumn", scenario = "2023")
risk_winter_23 <- data.frame(values = tick_bite_risk$risk_w_23, season ="Winter", scenario = "2023")

risk_spring_45 <- data.frame(values = tick_bite_risk$risk_sp_45, season ="Spring", scenario = "2085, RCP 4.5")
risk_summer_45 <- data.frame(values = tick_bite_risk$risk_su_45, season ="Summer", scenario = "2085, RCP 4.5")
risk_autumn_45 <- data.frame(values = tick_bite_risk$risk_a_45, season ="Autumn", scenario = "2085, RCP 4.5")
risk_winter_45 <- data.frame(values = tick_bite_risk$risk_w_45, season ="Winter", scenario = "2085, RCP 4.5")

risk_spring_85 <- data.frame(values = tick_bite_risk$risk_sp_85, season ="Spring", scenario = "2085, RCP 8.5")
risk_summer_85 <- data.frame(values = tick_bite_risk$risk_su_85, season ="Summer", scenario = "2085, RCP 8.5")
risk_autumn_85 <- data.frame(values = tick_bite_risk$risk_a_85, season ="Autumn", scenario = "2085, RCP 8.5")
risk_winter_85 <- data.frame(values = tick_bite_risk$risk_w_85, season ="Winter", scenario = "2085, RCP 8.5")

risk_values <- rbind(risk_spring_23,
                     risk_summer_23,
                     risk_autumn_23,
                     risk_winter_23,
                     risk_spring_45,
                     risk_summer_45,
                     risk_autumn_45,
                     risk_winter_45,
                     risk_spring_85,
                     risk_summer_85,
                     risk_autumn_85,
                     risk_winter_85)

# defining seasons as factors
risk_values$season <- factor(risk_values$season,
                             levels = c("Winter",
                                        "Spring",
                                        "Summer",
                                        "Autumn"))

seasons <- c("Spring", "Summer", "Autumn", "Winter")
scenarios <- c("2023", "2085, RCP 4.5", "2085, RCP 8.5")

# functions for generating the plots
plotAcrossScenarios <- function(cur_season, cur_scenario){
  alphas_scenarios <- rep(0.35, 3)
  alphas_scenarios[cur_scenario == scenarios] <- 1
  
  returnedPlot <- ggplot() +
    geom_histogram(data = risk_values %>% filter(season == cur_season),
                   mapping = aes(x = values, fill=scenario), binwidth = 0.05, position="dodge") +
    scale_fill_manual(values = c("2023" = alpha("yellow", alphas_scenarios[1]), 
                                 "2085, RCP 4.5" = alpha("orange", alphas_scenarios[2]), 
                                 "2085, RCP 8.5" = alpha("red", alphas_scenarios[3]))) +
    labs(fill = "", x="tick bite risk", y="number of hexagonal cells",
         title = paste("Tick bite risk distribution for", tolower(cur_season), "across scenarios")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face="bold", size=10))
  
  return(returnedPlot)
}

plotAcrossSeasons <- function(cur_season, cur_scenario){
  alphas_seasons <- rep(0.35, 4)
  alphas_seasons[cur_season == seasons] <- 1
  
  returnedPlot <- ggplot() +
    geom_histogram(data = risk_values %>% filter(scenario == cur_scenario),
                   mapping = aes(x = values, fill=season), binwidth = 0.05, position="dodge") +
    scale_fill_manual(values = c("Spring" = alpha("springgreen3", alphas_seasons[1]), 
                                 "Summer" = alpha("red", alphas_seasons[2]), 
                                 "Autumn" = alpha("orange", alphas_seasons[3]),
                                 "Winter" = alpha("lightblue", alphas_seasons[4]))) +
    labs(fill = "", x="tick bite risk", y="number of hexagonal cells",
         title = paste("Tick bite risk distribution for", cur_scenario, "across seasons")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face="bold", size=10))
  
  return(returnedPlot)
}

# defining the color scale
min_tick_risk <- 0
max_tick_risk <- max(tick_bite_risk$risk_sp_23,
                     tick_bite_risk$risk_su_23,
                     tick_bite_risk$risk_a_23,
                     tick_bite_risk$risk_w_23,
                     tick_bite_risk$risk_sp_45,
                     tick_bite_risk$risk_su_45,
                     tick_bite_risk$risk_a_45,
                     tick_bite_risk$risk_w_45,
                     tick_bite_risk$risk_sp_85,
                     tick_bite_risk$risk_su_85,
                     tick_bite_risk$risk_a_85,
                     tick_bite_risk$risk_w_85)

pal <- colorNumeric(palette="YlOrRd", c(min_tick_risk, max_tick_risk))
  
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
                values = c(min_tick_risk, max_tick_risk),
                opacity = 1, 
                title="Tick bite risk",
                position="bottomright"
      )
    })
  
  observeEvent(list(input$season, input$scenario), {
    # remove current display from the map
    leafletProxy("map") %>%
      clearGroup("tick_risk")
    
    # cover every combination of season and scenario to adjust plots and map
    if (input$season == "Spring" & input$scenario == "2023") {
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_sp_23,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_sp_23),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Summer" & input$scenario == "2023"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_su_23,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_su_23),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))    
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Autumn" & input$scenario == "2023"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_a_23,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_a_23),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))  
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Winter" & input$scenario == "2023"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_w_23,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_w_23),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))  
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Spring" & input$scenario == "2085, RCP 4.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_sp_45,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_sp_45),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))   
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Summer" & input$scenario == "2085, RCP 4.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_su_45,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_su_45),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))    
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Autumn" & input$scenario == "2085, RCP 4.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_a_45,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_a_45),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))   
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Winter" & input$scenario == "2085, RCP 4.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_w_45,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_w_45),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))      
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Spring" & input$scenario == "2085, RCP 8.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_sp_85,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_sp_85),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))    
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Summer" & input$scenario == "2085, RCP 8.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_su_85,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_su_85),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))      
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Autumn" & input$scenario == "2085, RCP 8.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_a_85,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_a_85),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))   
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    } else if (input$season == "Winter" & input$scenario == "2085, RCP 8.5"){
      labels_risk <- sprintf(
        "<strong>Tick bite risk</strong><br/>%s",
        round(tick_bite_risk$risk_w_85,2)
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        addPolygons(data = tick_bite_risk,
                    group = "tick_risk",
                    fillColor = ~pal(risk_w_85),
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
                    label = labels_risk,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))    
      
      output$leftplot <- renderPlot({plotAcrossScenarios(input$season, input$scenario)})
      output$rightplot <- renderPlot({plotAcrossSeasons(input$season, input$scenario)})
    }
  })
}