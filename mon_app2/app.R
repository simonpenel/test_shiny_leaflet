library(shiny)
library(leaflet)
library(RColorBrewer)

# Masting data
masting <- read.csv("../MASTREEplus_2024-06-26_V2.csv")


# UI
ui <- bootstrapPage(

  tags$style(type = "text/css",
    "html, body {width:100%;height:100%}
    #controls { background-color: #ddd; opacity: 0.5;"
  ),

  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10, class = "panel panel-default",
    sliderInput("range", "Year", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),
    selectInput("colors", "Color Scheme",
      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    ),
    checkboxInput("legend", "Show legend", TRUE),

    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",

      plotOutput("histCentile", height = 250),
      plotOutput("scatterCollegeIncome", height = 250)
    )
  )
)

# SERVER
server <- function(input, output, session) {

  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(masting) %>% addTiles() %>%
    fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })

  # Incremental changes to the map  should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.

  filteredData <- reactive({
    masting[masting$Year >= input$range[1] & masting$Year <= input$range[2],]
  })

  observe({
    pal<-colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      addMarkers(label = ~paste(Species," ", Year),clusterOptions = markerClusterOptions())
  })
}

shinyApp(ui, server)
