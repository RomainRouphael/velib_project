
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

###-----Libraries---------------------------------------

library(ggmap)
library(dismo)
library(leaflet)
library(mongolite)
library(jsonlite)
library(dplyr)

library(shiny)
library(XML)

#install.packages('devtools')
#devtools::install_github('rstudio/shinyapps')
#shinyapps::setAccountInfo(name='dsspvelibproject',
#                          token='333521F6600A7DF88449A7E88F047631',
#                          secret='hNwWjJMGLuw76lYFowsN+BNX6ihHza0u5JqjoU6r')

#library(shinyapps)
#shinyapps::deployApp('/Users/romainrouphael/Documents/DSSP/Day6/Velib_Camp/Decaux_RR2')


###------Variables-------------------------------------

DecauxKey <- "74bdc322e2ef04dd9cff52d229b7688f28143b39"
DecauxContractName <- "Paris"
nb.top.stations <- 3
address <- "37 rue Ampere, 75017, France"
mdb <- mongo("Decaux")
fileName1 <- "data_all_Paris.jjson"
loadDataMongo <- FALSE

setwd("/Users/romainrouphael/Documents/DSSP/Day6/Velib_Camp")

NoClickYet <- FALSE

## ----Functions-----------------------------------------------------------

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

Stations <- GetJsonDecaux(sprintf("stations?contract=%s",
                                  DecauxContractName))

#Calcul des top stations les plus proches

top.stations <- function(address, nb.top.stations = 5){
  user.geo <- dismo::geocode(address, oneRecord = TRUE)
  user.lat <- user.geo$latitude
  user.lng <- user.geo$longitude
  user.lat.vect <- rep(user.lat, nrow(Stations))
  user.lng.vect <- rep(user.lng, nrow(Stations))
  Stations2 <- Stations
  Stations2$dist <- distance(user.lat.vect, user.lng.vect, Stations2$position.lat, Stations2$position.lng)
  Stations2 <- Stations2[order(Stations2$dist), ]
  stations.top <- Stations2[1:nb.top.stations, ]
  return (stations.top)
}

#Fonction pour calculer la distance entre deux points

distance <- function(latA, lngA, latB, lngB){
  r <- 6367445
  rad.latA <- latA * (pi/180)
  rad.lngA <- lngA * (pi/180)
  rad.latB <- latB * (pi/180)
  rad.lngB <- lngB * (pi/180)
  distance <- acos(sin(rad.latA)*sin(rad.latB) + cos(rad.latA)*cos(rad.latB)*cos(rad.lngB-rad.lngA))
  return (r*distance)
}

#top.stations(address, 5)
#top.stations(address, 5)$address

loadFileMongoDB <- function(fileName, mdb) {
  connection <- file(fileName)
  open(connection)
  i=0
  ok = TRUE
  while (ok) {
    i=i+1
    tmp <- readLines(connection, n = 1L)
    
    if((i%%100)==0) print(i)
    
    if (length(tmp)==0) {
      ok = FALSE
    } else
    {
      toInsert <- fromJSON(tmp, flatten = TRUE)
      mdb$insert(toInsert) 
    }
  }
  print(length(i))
  close(connection)
}


##Get all historical data from Mongo DB for the top stations

getAllHistory <- function(stations.top, mdb){
  data.station <- data.frame()
  for (i in 1:nrow(stations.top)){
    row <- stations.top[i, ]
    data.station <- rbind(data.station, mdb$find(sprintf('{"number": %s}', row$number)))
  }
  return (data.station)
}

getRangeDataStation <- function(data.station, station.id, nb.day.offset = 0, interval = 3600){
  time <- unclass(Sys.time())
  offset <- interval*nb.day.offset*24
  
  range.down <- (time - offset) * 1000
  range.up <- (time + interval - offset) * 1000
  
  range.station <- data.station[(data.station$last_update >= range.down & data.station$last_update <= range.up & data.station$number == station.id), ]
  return(range.station)
}


###------Prediction-----------------------------------

#If flag set, then open flag and load in Mango, can take some

if(loadDataMongo || mdb$count() == 0){
  loadFileMongoDB(fileName1, mdb)
}

#Calcul de prédiciton sur l'heure à venir basée sur le nombre d'available_bikes il y a 7 et 14 jours

predict.avail.bikes <- function (data.station, station.id, nb.day.offset, interval, nb.top.stations){
  
  mean.available.bikes.past.week <- rep(0, nb.top.stations)
  mean.available.bikes.past.2week <- rep(0, nb.top.stations)
  predict.available.bikes <- rep(0, nb.top.stations)
  
  for (i in 1:nb.top.stations){
    bikes.past.week <- getRangeDataStation(data.station, station.id$number[i], nb.day.offset, interval)
    bikes.past.2week <- getRangeDataStation(data.station, station.id$number[i], (nb.day.offset + 7), interval)
    
    mean.available.bikes.past.week[i] <- mean(bikes.past.week$available_bikes)
    mean.available.bikes.past.2week[i] <- mean(bikes.past.2week$available_bikes)
    
    predict.available.bikes[i] <- (mean.available.bikes.past.week[i] + mean.available.bikes.past.2week[i])/2
    names(predict.available.bikes) <- top.stations(address, nb.top.stations)$address
    
    return (predict.available.bikes)
  }
}


#Display the top stations 

ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,50))



##########Serveur Shiny##############################


shinyServer(function(input, output, session) {
  
  predict.available.bikes <- predict.avail.bikes(data.station = getAllHistory(top.stations(address, nb.top.stations), mdb), 
                                                 station.id = top.stations(address, nb.top.stations), 
                                                 nb.day.offset = 7, 
                                                 interval = 3600,
                                                 nb.top.stations = 3)
  
  Stations <- reactive({
    #invalidateLater(60000, session)
    input$refreshButton
    input$calculGeo
    isolate({
      Stations <- top.stations(input$adresse, 3)
      Stations$predict.available.bikes <- predict.avail.bikes(data.station = getAllHistory(top.stations(address, nb.top.stations), mdb), 
                                                              station.id = top.stations(address, nb.top.stations), 
                                                              nb.day.offset = 7, 
                                                              interval = 3600,
                                                              nb.top.stations = 3)
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
                 popup = ~ sprintf("<b> Predicted bikes next hour: %s</b>",as.character(round(predict.available.bikes, 2))),
                 radius = ~bike_stands,
                 color = ~ ColorPal(predict.available.bikes / bike_stands),
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



