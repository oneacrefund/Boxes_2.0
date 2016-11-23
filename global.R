#### BASIC INFO ####
# boxes global script
# stuff in here is loaded into both server.r and ui.r
# last edited: 01 nov 2016 (bk)

#### syntax libraries ####
# library(tidyr)
library(dplyr)

#### geospatial libraries ####
library(rgdal)
library(sp)
library(raster)
library(leaflet)
library(RColorBrewer)
library(webshot)
#webshot::install_phantomjs()
library(htmlwidgets)

#### shiny libraries ####
library(shiny)
library(shinyBS)
library(shinythemes)
library(DT)

#### handy vectors ####
# Months
mths <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", 
    "nov", "dec")

# Countries & regions
countries <- read.csv("data/Countries_ISO.csv", header = T, stringsAsFactors = F)
ctISO <- countries[,1]
ctName <- countries[,2]

regions <- c("East Africa", "South Africa", "West Africa", "All OAF countries",
 "Core OAF countries", "All of SSA", "Central Africa")

# Crops whose yield gap we have:
cropsYield <- c("Barley" = "barley", "Groundnut" = "groundnut", "Maize" = "maize",
 "Potato" = "potato", "Rice" = "rice", "Soybean" = "soybean")

#### define collapsable panel titles ####
panel1 <- "Step 1: choose what you want to visualize"
panel2 <- "Step 2: set your filters and map it!"
panel3 <- "Step 3: ward-level drilldowns"

