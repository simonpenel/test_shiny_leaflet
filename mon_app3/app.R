library(shiny)
library(leaflet)
library(RColorBrewer)
#library(shinydashboard)
library(shinyWidgets)
# Masting data
options(encoding="latin1")
masting <- read.csv("https://github.com/JJFoest/MASTREEplus/raw/refs/heads/main/Data/MASTREEplus_2024-06-26_V2.csv")
variables =  unique(sort(masting$Variable))
species =  unique(sort(masting$Species))
# UI
ui <- bootstrapPage(

  tags$style(type = "text/css",
    "html, body {width:100%;height:100%}
    #controls { background-color: #ddd; opacity: 0.85;"
  ),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(top = 10, right = 10, width = 600, class = "panel panel-default", draggable = TRUE,
    downloadButton("download"),
    sliderInput("range", "Year", min(masting$Year), max(masting$Year),
      value = range(masting$Year), step = 1, sep ="", width=600
    ),
    selectInput("colors", "Color Scheme",
      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    ),
    #checkboxGroupInput("select_variable", "Variables", choices = variables, selected=variables),
    prettyCheckboxGroup("select_variable", "Variables", choices = variables, selected=variables),
    checkboxGroupInput("select_species", "Species"),
    
    #prettyCheckboxGroup("select_species", "Species", choices =, selected=species),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      plotOutput("histCentile", height = 250),
      tableOutput("data_masting_variable"),
      dataTableOutput("data_masting_species"),
    )
  )
)

select_in_map <- function(input) {

  bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    print(input$variable)
    print(input$select_variable)
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

select_in_map_all_species <- function(input) {

  bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    print(input$variable)
    print(input$select_variable)
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


  # this will be used for the map only
  filteredData <- reactive({
    masting[masting$Year >= input$range[1] & masting$Year <= input$range[2] 
    & masting$Variable %in% input$select_variable  & masting$Species %in% input$select_species, ] 
  })
  

  observe({
    pal<-colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      addMarkers(label = ~paste(Alpha_Number,":",Species," ", Year, " Value=", Value, "Type=",VarType,"[",Variable,"] (in " , Site ,")" ),
      clusterOptions = markerClusterOptions())
  })


  observe({ 
    data_plot <- select_in_map_all_species(input)
    boxes <- data_plot$Species


    #  print(data_plot)
    #  print(unique(sort(data_plot$Species)))
    select_species = unique(sort(data_plot$Species))
    print(select_species)
    updateCheckboxGroupInput(session, "select_species", 
                             label = NULL,  
                             choices = select_species, 
                             selected = select_species) 


  }) 

    output$histCentile <- renderPlot({

    data_plot <- select_in_map(input)

    plot(data_plot$Year,data_plot$Value,type="b")
  })



 

 output$data_masting_variable <- renderTable(
  {    
    data_plot <- select_in_map(input)
  unique(sort(data_plot$Variable))
  })


 output$data_masting_species <- renderDataTable(
  {   
    data_plot <- select_in_map(input)
  as.data.frame(unique(sort(data_plot$Species)))
  })

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
