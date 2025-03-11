library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
    sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
      value = range(quakes$mag), step = 0.1
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

server <- function(input, output, session) {

  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.

  data <- reactive({
    file <- "../MASTREEplus_2022-02-03_V1.csv"
    read.csv(file)
  })


  data <- read.csv("../MASTREEplus_2022-02-03_V1.csv")
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })


    filteredData <- reactive({
    data
  })
  

  observe({
    pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      #addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
      #  fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      #)
           addCircles(radius = ~log(Value)*100, weight = 1, color = "#777777",
        fillColor = "black", fillOpacity = 0.7, popup = ~paste(Value)
      ) 
  })
}

shinyApp(ui, server)