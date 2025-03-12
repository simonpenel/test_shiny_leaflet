library(shiny)
library(leaflet)
library(RColorBrewer)

# Masting data
options(encoding="latin1")
#masting <- read.csv("../MASTREEplus_2024-06-26_V2.csv")
masting <- read.csv("https://github.com/JJFoest/MASTREEplus/raw/refs/heads/main/Data/MASTREEplus_2024-06-26_V2.csv")

# UI
ui <- bootstrapPage(

  tags$style(type = "text/css",
    "html, body {width:100%;height:100%}
    #controls { background-color: #ddd; opacity: 0.85;"
  ),

  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10, width = 600, class = "panel panel-default", draggable = TRUE,
    sliderInput("range", "Year", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),
    selectInput("colors", "Color Scheme",
      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    ),
    #checkboxInput("legend", "Show legend", TRUE),

    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",

      plotOutput("histCentile", height = 250),
      #plotOutput("scatterCollegeIncome", height = 250)
      tableOutput("data_masting_variable"),
      dataTableOutput("data_masting_species"),      
    )
  )
)

# SERVER
server <- function(input, output, session) {

  colorpal <- reactive({
    colorNumeric(input$colors, masting$Value)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(masting) %>% addTiles(options = tileOptions(minZoom = 0, maxZoom = 25)) %>%
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
      addMarkers(label = ~paste(Species," ", Year, " Value=", Value, "Type=",VarType,"[",Variable,"] (in " , Site ,")" ),
      clusterOptions = markerClusterOptions())
  })



    output$histCentile <- renderPlot({  
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)


    data_plot<-subset(masting,
      Latitude >= latRng[1] & Latitude <= latRng[2] &
      Longitude >= lngRng[1] & Longitude <= lngRng[2] &
      Year >= input$range[1] &
      Year <= input$range[2] )

    plot(data_plot$Year,data_plot$Value,type="b")
  })

 output$data_masting_variable <- renderTable(
  {    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)


    data_plot<-subset(masting,
      Latitude >= latRng[1] & Latitude <= latRng[2] &
      Longitude >= lngRng[1] & Longitude <= lngRng[2] &
      Year >= input$range[1] &
      Year <= input$range[2] )
  unique(sort(data_plot$Variable))
  })


 output$data_masting_species <- renderDataTable(
  {    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)


    data_plot<-subset(masting,
      Latitude >= latRng[1] & Latitude <= latRng[2] &
      Longitude >= lngRng[1] & Longitude <= lngRng[2] &
      Year >= input$range[1] &
      Year <= input$range[2] )    
  as.data.frame(unique(sort(data_plot$Species)))
  })

}

shinyApp(ui, server)
