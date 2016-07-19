#### BASIC INFO ####
# boxes global script
# stuff in here is loaded into both server.r and ui.r
# last edited: 14 jul 2016 (jy)

#### syntax libraries ####
# library(tidyr)
library(dplyr)

#### geospatial libraries ####
library(rgdal)
library(sp)
library(raster)
library(leaflet)

#### shiny libraries ####
library(shiny)
library(shinyBS)
library(shinythemes)

#### handy vectors ####
# Months
mths <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", 
    "nov", "dec")

#countries <- read.csv("data/Countries_ISO.csv", header = T)
#countries.codes <- countries[,2]

