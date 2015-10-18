## ----POC @ Polytechnique--------------------------------------------
## ----It does what it is suppose to do hopefully
## load velib data, filter around nearest, predic whihtin the coming hour how much of bikes availabilty
## ----Maud Lenfant-------------------------------------------
## ----Gael Richardd--------------------------------------------
## ---patrice david saha------------------------------------------
## ----Kim Podinarath-------------------------------------------
## ----Romain Rouphael-------------------------------------------
## ----Baes Olivier--------------------------------------------

## ----includes----------------------------------------------
library(leaflet)
library(jsonlite)
library(shiny)
library(XML)
library(mongolite)
library(ggmap)
library(dismo) 
## ----Variables-----------------------------------------------------------
DecauxKey <- "da879af595184f071c181408b837b7da636f924f"
DecauxContractName <- "Paris"
nbStations <- 5
PIO <- "19 rue de la vistule Paris, France"
fileName1 <- "data_all_Paris.jjson.gz"
mdb <- mongo("Decaux")
## ----Must do, set the path & prepare mongo server and put flag loadmongo a TRUE to force laoding
getwd()
setwd("/Users/olivierbaes/Documents/X - Polytechnique/Day6/velib_project/")
loadDatainMongo=FALSE
## ----Functions-----------------------------------------------------------
GetJsonDecaux <- function(decaux, key = DecauxKey) {
  jsonlite::fromJSON(UrlDecaux(decaux,key), flatten = TRUE)
}

UrlDecaux <- function(decaux,key) {
  if (grepl('\\?',decaux, perl = TRUE)) {
    delim <- '&'
  }
  else {
    delim <- '?'
  }
  sprintf("https://api.jcdecaux.com/vls/v1/%s%sapiKey=%s",decaux,delim,key)
}


GetNearestDistanceFromPIO<-function(Stations,address,number=5) {
  posi <- geocode(address, oneRecord=TRUE)
  # calculate manhattan distances
  Stations$distance <- abs(Stations$position.lat-posi$latitude)+abs(Stations$position.lng-posi$longitude)
  # order & filter 
  TOPStations<-Stations[order(Stations$distance),]
  TOPStations<-Stations2[1:5,]
  return(TOPStations)
}


loadfileinMongoDB<-function(fileName,mdb) {
  #connection <- file(fileName)
  connection <- gzcon(file(fileName, "rb"))
  #connection <- gzcon(fileName)
  
  open(connection)
  i=0
  ll = list()
  ok = TRUE
  while (ok) {
    i=i+1
    tmp <- readLines(connection, n = 1L)
   
     if((i%%100)==0) print(i)
    
    if (length(tmp)==0) {
      ok = FALSE
    } else
    {
      toinsert <- fromJSON(tmp, flatten = TRUE)
      mdb$insert(toinsert) 
    }
  }
  print(length(i))
  close(connection)
}

# get all historical data from mongo
getallhistory<-function(TOPStations,mdb) {
  theres <- data.frame()
  # for each station
  for(i in 1:nrow(TOPStations)) {
    row <- TOPStations[i,]
    #print(sprintf("load %s",row$number))
    #ajout au dataframe du resultat des historique de chaque stations
    theres<-rbind( theres, mdb$find( sprintf('{ "number": %s }',row$number) ) )
  }
  return(theres)
}

getarangeofdataforstation<-function(dataf,stationid,nbdayoffset=0,interval=3600,past=FALSE) {
  ofset <- 3600*24*ofsetday
  if( past==TRUE ) interval
  else
  interval <- rangeduration
  
  time <- unclass(Sys.time())
  #interval
  range1 <- (time-ofset) * 1000.0
  #range1
  
  range2 <- (time-ofset) * 1000.0
  #range2
  
  restult <- dataf[(dataf$last_update >= range2 & dataf$last_update <= range1 & dataf$number==stationid) , ]
  #nrow(restult)
  return(restult)
}

################################################################

#the hearth ot the code start here

# Get all stations
Stations <- GetJsonDecaux(sprintf("stations?contract=%s", DecauxContractName))

# Get top station in regards of distances
TOPStations<-GetNearestDistanceFromPIO(Stations,PIO,nbStations)

# display the top stations
ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))
LeafletDecaux <- leaflet(data = TOPStations) %>%
  addTiles() %>%
  addCircles(~ position.lng, ~ position.lat, popup = ~ sprintf("<b> Available bikes: %s id station %s</b>",as.character(available_bikes),as.character(number)),
             radius = ~bike_stands,
             color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
             stroke = TRUE, fillOpacity = 0.75)
LeafletDecaux


# if flag set then open file & load in mongo
# could take some time
if( loadDatainMongo || mdb$count()==0) {
  loadfileinMongoDB(fileName1,mdb)
}

# get all historical information for each stations
thehistory <- getallhistory<-function(TOPStations,mdb) {

stationids <- 13035
  




tututf<- rbind(getdataforstation(restult,stationids,7,3600),getdataforstation(restult,stationids,14,3600),getdataforstation(restult,stationids,21,3600))
#nrow(tututf)
#tututf
meanf <- mean(tututf$available_bike_stands)
meanf

tututn<-getdataforstation(restult,stationids,0,3600*6,TRUE)
meann<- mean(tututn$available_bike_stands)
meann

tututp<- rbind(getdataforstation(restult,stationids,7,3600*6,TRUE),getdataforstation(restult,stationids,14,3600*6,TRUE),getdataforstation(restult,stationids,21,3600*6,TRUE))
#nrow(tututp)
#tututp
meanp <- mean(tututp$available_bike_stands)
meanp
print(sprintf("station %s moyenne de l'heure à venir pour les memes jours dans le passé %s ",stationids,meanf))

#getdataforstation(restult,13035,14,3600)
#getdataforstation(restult,13035,14,3600,TRUE)

# prediction moyenne de l'ehure à venir il y a 7 14 21 jours (linear regression ????)
# le ratio de l'heure passée par rapport à la moyen de l'heure pass&e il y a 7 14 21 jours 
# ratio * prediction = prediction finale
meann
print(sprintf("station %s moyenne d'utilisation des 6 heure précédente du jour  %s ",stationids,meann))
print(sprintf("station %s moyenne d'utilisation des 6 heure précédente pour les memes jours dans le passé %s ",stationids,meanp))

meanp
ratio<-meanp/meann
#print(sprintf("station %s ratio d'utilisation du jour  %s ",stationids,ratio))


predict <- meanf/ratio
print(sprintf(" station %s predicted bike available %s ",stationids,predict))
#restult$last_update[2]
#as.POSIXct(restult$last_update[2], origin='1970-01-01')
#restult$olala<- as.POSIXct(restult$last_update, origin='1970-01-01')
#restult$olala[2]

#theres$test <-as.POSIXct(restult$download_date, origin='1970-01-01')

#test <- mdb$find( sprintf("{ $and: [ {number: %s }, {status:'OPEN' } ] }",Stations2$number)  )
#test <- lapply(sprintf("{ number: %s }",Stations2$number)  , mdb$find)

#head(test)