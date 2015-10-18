
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(
    fluidRow(
      column(3,
             actionButton("refreshButton","Refresh"),textInput("adresse","Entrez votre adresse ici :","11 boulevard Malesherbes Paris, France"),actionButton("calculGeo","Envoyer")),
      column(9,
           leafletOutput("DecauxMap"),
           htmlOutput("DecauxId")
  ))))
