## ----DecauxAPI-----------------------------------------------------------
DecauxKey <- "da879af595184f071c181408b837b7da636f924f"
DecauxContractName <- "Paris"

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

library(ggmap)
library(dismo) 
testt <- geocode("19 rue de la vistule Paris, France", oneRecord=TRUE)
testt
Stations$distance <- abs(Stations$position.lat-testt$latitude)+abs(Stations$position.lng-testt$longitude)
nrow(Stations)
Stations2<-Stations[order(Stations$distance),]
Stations2<-Stations2[1:5,]
nrow(Stations2)
Stations2$number

## ----Leaflet, cache = FALSE----------------------------------------------
library(leaflet)

ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab"), domain = c(0,1))
LeafletDecaux <- leaflet(data = Stations2) %>%
  addTiles() %>%
  addCircles(~ position.lng, ~ position.lat, popup = ~ sprintf("<b> Available bikes: %s id station %s</b>",as.character(available_bikes),as.character(number)),
             radius = ~bike_stands,
             color = ~ ColorPal( available_bikes / (available_bikes + available_bike_stands)),
             stroke = TRUE, fillOpacity = 0.75)
LeafletDecaux


#library(jsonlite)
library(shiny)

## ----Mongodb-------------------------------------------------------------
library(mongolite)

mdb <- mongo("Decaux3")

#mesdatas2 <- stream_in(file("/Users/olivierbaes/Downloads/data_all_Paris.jjson.json"))
#mesdatas2 <- stream_in(file("/Users/olivierbaes/Downloads/data_all_Paris2.jjson"))
#mesdatas2 <- stream_in(gzcon(file("/Users/olivierbaes/Downloads/data_all_Paris.jjson_2015-08-01-1438416396.gz")))
#mesdatas2 <- stream_in(file("/Users/olivierbaes/Downloads/data_all_Paris.jjson_2015-09-01-1441081722"))
#head(mesdatas2)
#summary(mesdatas2)

loaddd<-function() {
connection <- file("/Users/olivierbaes/Downloads/data_all_Paris.jjson.json")
open(connection)

ll = list()
ok = TRUE
while (ok) {
  tmp <- readLines(connection, n = 1L)
  print(length(tmp))
  if (length(tmp)==0) {
    ok = FALSE
  } else
  {
    mesdatas2 <- fromJSON(tmp, flatten = TRUE)
    mdb$insert(mesdatas2) 
  }
}

close(connection)
}

#loaddd()
count(mesdatas2)

#mdb$insert(mesdatas2)

#StationsAll <- mdb$find()
Stations2$number
summary(Stations2)
str(Stations2)
head(Stations2)
nrow(Stations2)

theres <- data.frame()
for(i in 1:nrow(Stations2)) {
  row <- Stations2[i,]
  # do stuff with row
  print(sprintf("load %s",row$number))
  #print(mdb$find( sprintf('{ "number": %s }',row$number) ) )
  theres<-rbind( theres, mdb$find( sprintf('{ "number": %s }',row$number) ) )
  #str(theres)
  nrow(theres)
}
nrow(theres)
head(theres)

stationids <- 13035

getdataforstation<-function(dataf,stationid,ofsetday=1,rangeduration=3600,past=FALSE) {
if( past==TRUE )
  ofset <- 3600*24*ofsetday+rangeduration
else
  ofset <- 3600*24*ofsetday
interval <- rangeduration
interval
nexttime <- (unclass(Sys.time())+(interval-ofset)) * 1000.0
nexttime

nowtime <- (unclass(Sys.time())+(-ofset)) * 1000.0
nowtime

restult <- theres[(theres$last_update >= nowtime & theres$last_update <= nexttime & theres$number==stationid) , ]
nrow(restult)
return(restult)
}

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