
## Prep - boundaries: script for creating regional borders
## Written by: Bernard Kiprop
## Written for: Boxes 2.0
## Date created: 4/19/16
## Last modified: 5/09/16

### Set up ###
libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet", 
          "dplyr", "spatial.tools", "ff", "shiny", "shinyBS", "maptools")
lapply (require, libs, character.only = T)

wd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny"
cd <- paste(wd, "Code files", sep = "/")
dd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny/data/resRasters"
cbd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny/data/countryBoundaries"

###  Getting the desired regions (border files) ###
# Loading border files (borrowed from Colin's previous Boxes!work)
# Creates a list of spatialpolygons dataframes (for each country)
countries <- dir(cbd, "*.rds")

loadfiles <- function(filename) {
    tempobj <- readRDS(paste(cbd, filename, sep = "/")) 
    assign(paste(tempobj$ISO[1], "Border", sep = ""), readRDS(paste(cbd, filename, sep = "/")))
}
borderList <- lapply(countries, loadfiles)

#all <- readOGR(paste(cbd, "countryBorders.shp", sep ="/")); not working


### Create regions out of the border files (SPDFS that we can use to crop the tifs) ###
# East Africa; west Africa; central Africa; south Africa; OAF core countries; all OAF ctries
# Regional grouping of countries is based on UNM49 classification
# (http://unstats.un.org/unsd/methods/m49/m49regin.htm)
# Missing some countries (border files we have are from Boxes! and don't have all countries)

### Get list of countries ###
ctries <- ""
length(ctries) <- length(borderList)
for (i in 1:length(borderList)) {
    ctries[i] <- borderList[[i]]$NAME_0[1]
}

# Manually picking out countries for each region (based on location on the ctries vector)
EA <- ctries[c(2,12,13,14,21,24,26,30,38,39,40,41)]
WA <- ctries[c(3,4,7,11,16,17,18,19,22,23,25,28,29,32,33,37)]
CA <- ctries[c(1,6,8,9,10,20,15)]
SA <- ctries[c(5,27)]
COAF <- c("Kenya", "Tanzania", "Rwanda", "Burundi") # Core OAF; check this
AOAF <- c(COAF, "Uganda", "Malawi", "Ethiopia", "Zambia") # All OAF; check this


### Function that creates the regional borders ###
# spRbind country polygons to create regions; also partly borrowed from Boxes!
createRegion <- function (cts) {
    b <- list()
    for(i in 1:length(cts)){
        for(j in 1:length(borderList)){
            if ( borderList[[j]]$NAME_0 == cts[i]){
                b[[length(b) + 1]] <- (borderList[[j]])
            }
            }
        }
    
    for(i in 1:length(b)){
        b[[i]] <- spChFIDs(b[[i]], paste(b[[i]]$ISO, b[[i]]$NAME_1, sep = "_"))
        if (i == 1) {
            region  <- b[[i]]
        } else {
            region <- spRbind(region, b[[i]])
        }
    }
    return (region)
}

### Creating the regions ###

suppressWarnings(createRegion)
south <- createRegion(SA)
west <- createRegion(WA)
east <- createRegion(EA)
OAFcore <- createRegion(COAF)
OAFall <- createRegion(AOAF)
all <- list(south, west, east, OAFcore, OAFall)

### Save the created boundaries ###
# Saved to the folder 'borders' within the restRasters folder
# Might need to create new borders without the regional boundaries

writeOGR(south, dsn = paste(dd, "borders", sep = "/"), layer = "SouthAfricaBorder",
         driver = "ESRI Shapefile", overwrite_layer = T, verbose = T)

writeOGR(west, dsn = paste(dd, "borders", sep = "/"), layer = "WestAfricaBorder",
         driver = "ESRI Shapefile", overwrite_layer = T, verbose = F)

writeOGR(east, dsn = paste(dd, "borders", sep = "/"), layer = "EastAfricaBorder",
         driver = "ESRI Shapefile", overwrite_layer = T, verbose = F)

writeOGR(OAFcore, dsn = paste(dd, "borders", sep = "/"), layer = "OAF_core",
         driver = "ESRI Shapefile", overwrite_layer = T, verbose = F)

writeOGR(OAFall, dsn = paste(dd, "borders", sep = "/"), layer = "OAF_all",
         driver = "ESRI Shapefile", overwrite_layer = T, verbose = F)
