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




  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({  
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)


    toto<-subset(quakes,
      lat >= latRng[1] & lat <= latRng[2] &
        long >= lngRng[1] & long <= lngRng[2])


  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })

  tata <- subset(toto, mag >= input$range[1] & mag <= input$range[2])

    test <- tata$mag
    hist(test)
    # If no zipcodes are in view, don't plot
    #if (nrow(zipsInBounds()) == 0)
    #  return(NULL)

 #   hist(zipsInBounds()$centile,
 #     breaks = centileBreaks,
 #     main = "SuperZIP score (visible zips)",
 #     xlab = "Percentile",
 #     xlim = range(allzips$centile),
 #     col = '#00DD00',
 #     border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({

     hist(quakes$mag)
     })



  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
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
  observe({
    pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
        fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
        pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)