library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(lwgeom)

my_files <- list.files(pattern = "\\.RDS$")
lakes <- list()
lakes <- lapply(my_files, readRDS)

sizes <- c("-10.000", "10.000-100.000", "100.000-1.000.000", "1.000.000-")

ui <- fluidPage(
  
  tags$h2(
    HTML("In the Helsinki area, find your closest lake(s) for cross-country skiing, skating etc.")
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
            Choose the lake size category, the number of lakes to show - and click the map.
          </p>
          <p></p>
          <p>
              -10.000 m2 <= 100x100 m<br/>
              10.000-100.000 <= 300x300<br/>
              100.000-1.000.000 <= 1000x1000
          </p>
          <p>
          </p>
          <p><a href='https://github.com/tts/lakes/app.R'>R code</a></p>
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
      # Center for Martinlaakso
      setView(lng = 24.869128, lat = 60.277815, zoom = 9)
  })
  
  
  observe({
    
    click = input$map_click
    
    if(is.null(click))
      return()
    
    here <- data.frame(latitude = click$lat, longitude = click$lng)
    here_sf <- st_as_sf(here, coords = c("longitude", "latitude"), crs = 4326)
    
    lake_category <- input$size
    
    lakes_from_here <- size_chosen() %>%
      rowwise() %>%
      mutate(d_from_here = st_distance(here_sf, geometry)) %>%
      arrange(d_from_here)
    
    map_proxy = leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = lakes_from_here[1:input$nr, ]) %>% 
      setView(lng = 24.869128, lat = 60.277815, zoom = 9)
    
  })
  
  
}

shinyApp(ui, server)
