library(shiny)
library(rgdal)
library(leaflet)
library(geoshaper)
library(raster)
library(leafem)
library(leaflet.extras)
library(leaflet.mapboxgl)
library(leaflet.opacity)
library(htmltools)
library(htmlwidgets)
library(shinythemes)
library(DT)
library(modules)

m <- modules::use('dropdown.R')

shape <- readOGR(dsn="./GlobalLandslideCatalog")

precip <- raster("Nepal_Precip.gri")
raster_layer <- raster('Nepal_Elevations.gri')
raster_values = values(precip)
pal = leaflet::colorNumeric(rgb(0/555, 1:445/450, 5/100), raster_values)

landslides <- shape@data
landslides$population <- format(landslides$population, big.mark = ",")

months <- c("01" , "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

names(months) <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")

# Define UI
ui <- tagList(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    navbarPage(
        theme = shinytheme("journal"),
        title = "Nepal Landslide Risk",
        tabPanel("Map",
                 fluidRow(
                     column(3,
                            wellPanel(
                                dateRangeInput('dates', "Historical Landslide Date Range", start = "2007-03-21",
                                               end = "2015-03-03", min = "2007-03-21",
                                               max = "2015-03-03", format="yyyy/mm/dd"),
                                uiOutput("RainMonths"), 
                                sliderInput("fatalities", "Fatalities", min=0, max=174, value=c(0,174)),
                                numericInput("radius", "Multiply Radius (Equal to Distance) by: ", 50, min = 1)
                            ),
                            plotOutput("RainfallDistribution")
                            ),
                     column(9,
                            leafletOutput("NepalRain")
                            )
                 )
        ),
        tabPanel("Data",
                 fluidRow(
                     dataTableOutput("LandslideData")
                 )
            
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
    
  data_of_click <- reactiveValues(clickedMarker = list())
    
  observeEvent({input$dates
    input$rain_months
    input$fatalities
    input$radius}, {
          
          if(nrow(nepal_landslides()) == 0) {
            leafletProxy("NepalRain") %>%
              clearShapes() 
          } else {
            locationLabels <- sprintf(
              "<strong>Id: %s</strong><br/>
              Date: %s <sup></sup><br/>
              Near %s <sup></sup><br/>
              %g Fatalities, Population of %s <sup></sup>",
              nepal_landslides()$objectid, nepal_landslides()$date_date_,
              nepal_landslides()$nearest_pl, nepal_landslides()$fatalities, 
              nepal_landslides()$population
            ) %>% lapply(htmltools::HTML)
            
            leafletProxy("NepalRain") %>%
              clearShapes() %>%
              addCircles(data = nepal_landslides(),
                         radius = nepal_landslides()$distance * input$radius,
                         lat = nepal_landslides()$latitude,
                         lng = nepal_landslides()$longitude,
                         fillColor = "white",
                         fillOpacity = 1,
                         color = "wheat",
                         weight = 2,
                         stroke = T,
                         layerId = as.character(nepal_landslides()$objectid),
                         label = locationLabels,
                         highlightOptions = highlightOptions(color = "brown",
                                                             opacity = 1.0,
                                                             weight = 2,
                                                             bringToFront = TRUE))
          }
          
        })
    
    nepal_landslides <- eventReactive({input$dates
        input$fatalities
        input$rain_months}, {
           
            req(!is.null(input$rain_months))
          
            date1 <- input$dates[1]
            date2 <- input$dates[2]
            
            nepal_landslides <- subset(landslides, countrynam=='Nepal')
            nepal_landslides$month <- strsplit(as.character(nepal_landslides$date_date_), "/")
            nepal_landslides$month <- sapply(nepal_landslides$month, function(x) x[2])
            nepal_landslides <- subset(nepal_landslides, month %in% input$rain_months)
            nepal_landslides$date_date_ <- as.Date(nepal_landslides$date_date_)
            nepal_landslides <- subset(nepal_landslides, date_date_ >= input$dates[1] & date_date_ <= input$dates[2])
            nepal_landslides <- subset(nepal_landslides, fatalities >= input$fatalities[1] & fatalities <= input$fatalities[2])
            
            if(nrow(nepal_landslides) > 0) {
              nepal_landslides$objectid <- as.character(nepal_landslides$objectid)
              nepal_landslides$secondobjectid <- paste0(nepal_landslides$objectid, "_selected")
            } else {
              return(nepal_landslides)
            }
            
            nepal_landslides
        })
    
    output$RainMonths <- renderUI({
      
      m$dropdownButton(status = "default", label = "Months of Landslides", width = "100%",
                     tags$div(class="dropdown-btn-inner-btn-group"
                              , actionButton("all_months", "SELECT ALL", width = "80%")
                              , actionButton("clear_months", "CLEAR ALL", width="80%")
                     ),
                     tags$div(class="dropdown-btn-inner-radio-btn-group"
                              , checkboxGroupInput("rain_months", label = "",
                                                   choices = months,
                                                   selected = months
                              )
                     )
      )
      
    })
    
    observeEvent(input$all_months, {
      updateCheckboxGroupInput(session, "rain_months", choices = months, selected = months)
    })
    
    observeEvent(input$clear_months, {
      updateCheckboxGroupInput(session, "rain_months", choices = months, selected = character(0))
      
      leafletProxy("NepalRain") %>%
        clearShapes()
    })
    
    output$RainfallDistribution <- renderPlot({
        Precip.Distribution <- precip[!is.na(precip)]
       
        hist(Precip.Distribution, prob=TRUE, col='lightblue'
             , main="Annual Rainfall in Nepal, 1980-2000"
             , xlab="Rainfall (mm)")
        lines(density(Precip.Distribution, adjust=5), col="darkgreen", lwd=2, lty="dotted")
    })

    output$NepalRain <- renderLeaflet({
     
        leaflet() %>%
            addTiles("https://api.mapbox.com/styles/v1/dainaandries/cjxxfhzrvbztm1dtev60x044h/tiles/{z}/{x}/{y}?access_token=YOUR_ACCESS_TOKEN_HERE"
                     , attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%                                               
            addRasterImage(precip, layerId= "Rainfall"
                           , colors=rgb(0/555, 1:445/450, 5/100), opacity = 0.8) %>%
            addOpacitySlider("Rainfall") %>%
            addRasterImage(raster_layer, layerId= "Elevation"
                           , colors=rgb(375/555, 1:445/450, 5/100), opacity = 0) %>%
            addMouseCoordinates() %>%
            leafem::addImageQuery(raster_layer, type="mousemove", layerId = "Elevation"
                                  , position = "topright") %>%
            addLegend(pal = pal, values = raster_values[!is.na(raster_values)],
                      title = "Rainfall (mm)", position = 'bottomright') %>%
            addDrawToolbar(
                polylineOptions=FALSE,
                markerOptions = FALSE,
                circleMarkerOptions = FALSE,
                polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
                rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
                circleOptions = FALSE,
                editOptions = editToolbarOptions(edit = FALSE,
                                                 selectedPathOptions = selectedPathOptions()))
        })
    
    output$LandslideData <- renderDataTable({
        
        if(length(data_of_click$clickedMarker) > 0){
            points <- subset(nepal_landslides(), objectid %in% data_of_click$clickedMarker)
        } else {
            points <- nepal_landslides()
        }
            
        display <- subset(points,
                          select = c("objectid",
                                     "trigger",
                                     "population",
                                     "date_date_",
                                     "distance",
                                     "fatalities",
                                     "injuries",
                                     "landslide1",
                                     "landslide_",
                                     "location_a",
                                     "nearest_pl"))
        
        names(display) <- c(
            "Id",
            "Trigger",
            "Population",
            "Date",
            "Distance",
            "Fatalities",
            "Injuries",
            "Severity",
            "Type",
            "Location",
            "Near"
        )
        datatable(display, rownames = FALSE)
    })
    
    coordinates <- reactive({
        SpatialPointsDataFrame(nepal_landslides()[,c('longitude', 'latitude')] , nepal_landslides())
    })
    
    observeEvent(input$NepalRain_draw_new_feature,{
        # Only add new layers for bounded locations
   
        found_in_bounds <- findLocations(shape = input$NepalRain_draw_new_feature
                                         , location_coordinates = coordinates()
                                         , location_id_colname = "objectid")
        
        for(id in found_in_bounds){
            if(id %in% data_of_click$clickedMarker){
                # don't add id
              return()
            } else {
                # add id
                data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
            }
        }
        
        # look up airports by ids found
        selected <- subset(nepal_landslides(), objectid %in% data_of_click$clickedMarker)
        
        locationLabels <- sprintf(
          "<strong>Id: %s</strong><br/>
              Date: %s <sup></sup><br/>
              Near %s <sup></sup><br/>
              %g Fatalities, Population of %s <sup></sup>",
          selected$objectid, selected$date_date_,
          selected$nearest_pl, selected$fatalities, 
          selected$population
        ) %>% lapply(htmltools::HTML)
        
        proxy <- leafletProxy("NepalRain")
        proxy %>% addCircles(data = selected,
                             radius =  selected$distance * input$radius,
                             lat = selected$latitude,
                             lng = selected$longitude,
                             fillColor = "orange",
                             fillOpacity = 1,
                             color = "orange",
                             weight = 2,
                             stroke = T,
                             layerId = as.character(selected$secondobjectid),
                             label = locationLabels,
                             highlightOptions = highlightOptions(color = "wheat",
                                                                 opacity = 1.0,
                                                                 weight = 2,
                                                                 bringToFront = TRUE))
        
    })
    
    observeEvent(input$NepalRain_draw_deleted_features,{
        # loop through list of one or more deleted features/ polygons
        for(feature in input$NepalRain_draw_deleted_features$features){
            
            # get ids for locations within the bounding shape
            bounded_layer_ids <- findLocations(shape = feature
                                               , location_coordinates = coordinates()
                                               , location_id_colname = "secondobjectid")
            
            
            # remove second layer representing selected locations
            proxy <- leafletProxy("NepalRain")
            proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
            
            first_layer_ids <- subset(nepal_landslides(), secondobjectid %in% bounded_layer_ids)$objectid
            
            data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                       %in% first_layer_ids]
        }
    })

  
}

# Run the application 
shinyApp(ui = ui, server = server)