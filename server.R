
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(XML)
library(dismo)

DecauxKey <- "da879af595184f071c181408b837b7da636f924f"
DecauxContractName <- "Paris"
NoClickYet <- FALSE

UrlDecaux <- function(decaux,key) {
  if (grepl('\\?',decaux, perl = TRUE)) {
    delim <- '&'
  }
  else {
    delim <- '?'
  }
  sprintf("https://api.jcdecaux.com/vls/v1/%s%sapiKey=%s",decaux,delim,key)
}

GetJsonDecaux <- function(decaux, key = DecauxKey) {
  jsonlite::fromJSON(UrlDecaux(decaux,key), flatten = TRUE)
}

ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))

SelectNStats<-function(N=5,adresse,data){
  geocoord<-geocode(adresse)
  gpslon<-as.double(geocoord["longitude"])
  gpslat<-as.double(geocoord["latitude"])
  data$distanceGPS=abs(gpslat-data[['position.lat']])+abs(gpslon-data[['position.lng']])
  data<-data[order(data$distanceGPS),]
  
  SelectNStats<-data[1:5,]
  return(SelectNStats)
}

shinyServer(function(input, output, session) {

  Stations <- reactive({
    #invalidateLater(60000, session)
    input$refreshButton
    input$calculGeo
    isolate({
    Stations <- SelectNStats(5,input$adresse,GetJsonDecaux(sprintf("stations?contract=%s",
                          DecauxContractName)))
    Stations })
    
  })

  
  output$DecauxMap <- renderLeaflet({
    #isolate({
      LeafletDecaux <- leaflet(data = Stations()) %>%
      addTiles() %>% fitBounds(~min(position.lng), ~min(position.lat), ~max(position.lng), ~max(position.lat))
    #})
  })
  
  observe({
    leafletProxy("DecauxMap", session = session, data = Stations()) %>% 
        clearShapes() %>%
        addCircles(~ position.lng, ~ position.lat,
    popup = ~ sprintf("<b> Available bikes: %s</b>",as.character(available_bikes)),radius = ~bike_stands,
                                                   color = ~ ColorPal( available_bikes / bike_stands),
                                                    stroke = TRUE, fillOpacity = 0.75,
    layerId = 1:nrow(Stations()))
  })
  
  output$DecauxId <- renderUI({ 
    click <- input$DecauxMap_shape_click
    isolate({
      if (is.null(click)) { click = list(id = 1) }
     Station <- Stations()[click$id,]
      div(
        h1(Station$name[1]),
        p("Bike Stands:" , Station$bike_stands[1])
      )
      })
    }
  )
})
