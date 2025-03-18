library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

# Masting data
options(encoding="latin1")
masting <- read.csv("https://github.com/JJFoest/MASTREEplus/raw/refs/heads/main/Data/MASTREEplus_2024-06-26_V2.csv")
variables =  unique(sort(masting$Variable))
species =  unique(sort(masting$Species))

test <- masting 
test$Year <- NULL
test$Value <- NULL
test <- test[!duplicated(test), ]
print(nrow(test))
print(nrow(masting))
# UI
ui <- bootstrapPage(
  useShinyjs(),
  tags$style(type = "text/css",
    "html, body {width:100%;height:100%;overflow-y:scroll}
    #controls { background-color: #ddd; opacity: 0.85;}"
  ),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(top = 10, left = 60, width = 400, class = "panel panel-default", draggable = TRUE,

    downloadButton("download"),
    
    sliderInput("range", "Year", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),
    
    selectInput("colors", "Color Scheme",
      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    ),
    
    prettyCheckboxGroup("select_variable", "Variables", choices = variables, selected=variables,status="primary"),
    
    fluidRow(
      column(2, offset = 0,
            actionButton(inputId = "button", label = "Species selection"),
      ),
    ),
    
    box(id = "myBox",collapsed=TRUE,
      checkboxGroupInput("select_species", "Species in the current view for the selected variables")
    ),
    
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 10, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("histCentile", height = 250),
      uiOutput("titre_table"),
      tableOutput("data_masting_variable"),
    )
  )
)


# Select the data on the area, the year, the variable and the species 
select_in_map <- function(input) {
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  selected_data<-subset(masting,
    Latitude >= latRng[1] & Latitude <= latRng[2] &
    Longitude >= lngRng[1] & Longitude <= lngRng[2] &
    Year >= input$range[1] &
    Year <= input$range[2] &
    Variable %in% input$select_variable &
    Species %in% input$select_species
    )
  selected_data  
}


# Select the data on the area, the year and the variable
select_in_map_all_species <- function(input) {
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  selected_data<-subset(masting,
    Latitude >= latRng[1] & Latitude <= latRng[2] &
    Longitude >= lngRng[1] & Longitude <= lngRng[2] &
    Year >= input$range[1] &
    Year <= input$range[2] &
    Variable %in% input$select_variable
  )
  selected_data  
}

# SERVER
server <- function(input, output, session) {
  shinyjs::hide("myBox")
  colorpal <- reactive({
    colorNumeric(input$colors, masting$Value)
  })

  # observe the button being pressed
  observeEvent(input$button, {
    shinyjs::toggle("myBox")
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(masting) %>% addTiles(options = tileOptions(minZoom = 0, maxZoom = 25)) %>%
      # Layers control
  addLayersControl(
    # baseGroups = c(
    #   "OSM (default)",
    #   "Positron (minimal)",
    #   "World Imagery (satellite)"
    # ),
    overlayGroups = c("mast", "Outline"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
    fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })

  # This will be used for the map.
  filteredData <- reactive({
    masting[masting$Year >= input$range[1] & masting$Year <= input$range[2] 
    & masting$Variable %in% input$select_variable  & masting$Species %in% input$select_species, ] 
  })
  

  # filteredCircle <- reactive({
  #           test[test$Start > input$range[1] & test$End < input$range[2] 
  #   & test$Variable %in% input$select_variable  & test$Species %in% input$select_species, ] 
  # })




  # Incremental changes to the map  should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.

  #  Update markers on the map 
  observe({
    pal<-colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      addMarkers(label = ~paste(Alpha_Number,":",Species," ", Year, " Value=", Value, "Type=",VarType,"[",Variable,"] (in " , Site ,")" ),
      group = "mast",
      clusterOptions = markerClusterOptions())
  })


  # observe({
  #   pal<-colorpal()
  #   leafletProxy("map", data = filteredCircle()) %>%
  #     clearShapes() %>%
  #     addMarkers(label = ~paste(Alpha_Number,":",Species," ", Start,  "Type=",VarType,"[",Variable,"] (in " , Site ,")" ),
  #     clusterOptions = markerClusterOptions())
  # })

  # Update the species checkbox
  observe({ 
    data_plot <- select_in_map_all_species(input)
    select_species <- unique(sort(data_plot$Species))
    updateCheckboxGroupInput(session, "select_species", 
      label = NULL,  
      choices = select_species, 
      selected = select_species
      ) 
  }) 

  # Output the plot
  output$histCentile <- renderPlot({
    data_plot <- select_in_map(input)
    plot(data_plot$Year,data_plot$Value,type="b")
  })

  # Output the title of the variables  table
  output$titre_table <- renderPrint({ HTML(paste0("<b>Values present in the current view:</b>"))})

  # Output the  variables  table
  output$data_masting_variable <- renderTable(
    {    
      data_plot <- select_in_map(input)
      unique(sort(data_plot$Variable))
    },
    colnames=FALSE)

  # Output for the download
  output$download <- downloadHandler(
    filename = function() {
      paste0("selected_data.csv")
    },
    content = function(file) {
      saved_data<- select_in_map(input)
      write.csv(saved_data, file)
    }
  )
}

shinyApp(ui, server)
