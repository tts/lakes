library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(lwgeom) # shinyapps.io asks for this

# Lakes
my_files <- list.files(pattern = "lakes[0-9]\\.RDS$")
lakes <- list()
lakes <- lapply(my_files, readRDS)
sizes <- c("-10.000", "10.000-100.000", "100.000-1.000.000", "1.000.000-")
# Municipality borders
area <- readRDS("area.RDS")

ui <- fluidPage(
  
  tags$h2(
    HTML("Closest lake(s) in the Helsinki area for cross-country skiing and other winter sports")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #333333;
        color: white;
      },
      .shiny-input-container {
        color: snow;
      }
      label.control-label {
        color: #5f9ea0;
      }
      .leaflet-container {
       cursor: pointer !important;
      }"
    ))
  ),
  
  sidebarPanel(
    selectInput(inputId = "size",
                label = "Size (m2)",
                choices = sizes),
    selectInput(inputId = "nr",
                label = "Number",
                choices = seq(1:5)),
    HTML("<p></p>
          <span style='color:black;font-size:12px'
          <p>
            Pick your location by clicking/tapping the map. Select the category of the lake size, and the number of lakes to show.
          </p>
          <p></p>
          <p>
              -10.000 m2 <= 100x100 m<br/>
              10.000-100.000 <~ 300x300 m<br/>
              100.000-1.000.000 <~ 1x1 km
          </p>
          <p>
          </p>
          <p><a href='https://github.com/tts/lakes'>R code</a> by <a href='https://twitter.com/ttso'>@ttso</a>.</p>
          <p></p>
          <p>Data: <a href='https://hri.fi/data/en_GB/dataset/seutukartta'>Helsinki Region Map</a>.</p>
          </span>"),
    width = 3
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Map", 
               leafletOutput("map", height = 800, width = "100%"))
    ),
    width = 9
  )
)

server <- function(input, output, session) {
  
  size_chosen <- reactive({
    
    if (input$size == "-10.000") {
      lakes_from_here <- lakes[[1]] 
    }
    if (input$size == "10.000-100.000") {
      lakes_from_here <- lakes[[2]]
    }
    if (input$size == "100.000-1.000.000") {
      lakes_from_here <- lakes[[3]]
    }
    if (input$size == "1.000.000-") {
      lakes_from_here <- lakes[[4]]
    }
    
    lakes_from_here
    
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(minZoom = 9, maxZoom = 15)) %>% 
      addPolylines(data = area, color = "#919c94", weight = 2) %>% 
      # Martinlaakso
      setView(lng = 24.869128, lat = 60.277815, zoom = 9)
  })
  
  
  observe({
    
    click = input$map_click
    
    if(is.null(click))
      return()
    
    here <- data.frame(latitude = click$lat, longitude = click$lng)
    here_sf <- st_as_sf(here, coords = c("longitude", "latitude"), crs = 4326)
    
    # Closests ones
    lakes_from_here <- size_chosen() %>%
      rowwise() %>%
      mutate(d_from_here = st_distance(here_sf, geometry)) %>%
      arrange(d_from_here)
    
    # Coordinates for labels
    centers <- lakes_from_here[1:input$nr, ] %>% 
      st_centroid() %>% 
      data.frame() %>% 
      st_as_sf() %>% 
      mutate(lon = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2]) 
    
    map_proxy = leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>% 
      addPolylines(data = area, color = "#919c94", weight = 2) %>% 
      addCircleMarkers(lat = click$lat, lng = click$lng, 
                       color = "orange", stroke = FALSE, weight = 3) %>%
      addPolygons(data = lakes_from_here[1:input$nr, ], stroke = FALSE) %>% 
      addLabelOnlyMarkers(data = centers,
                          lng = ~lon, lat = ~lat, 
                          label = ~ifelse(!is.na(vesisto_nimi_s), vesisto_nimi_s, vesisto_nimi_r),
                          labelOptions = labelOptions(noHide = TRUE, 
                                                      textOnly = TRUE,
                                                      offset = c(0,0),
                                                      style = list(
                                                        "color" = "black", 
                                                        "font-family" = "serif",
                                                        "font-style" = "normal",
                                                        "font-size" = "12px",
                                                        "padding" = "10px"))) %>% 
      setView(lng = 24.869128, lat = 60.277815, zoom = 9)
    
  })
  
  
}

shinyApp(ui, server)
