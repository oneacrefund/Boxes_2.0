# Boxes 2.0: Server script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: Back-end server script for Shiny app
# Date created: 29 Mar 2016
# Date modified: 6 Apr 2016


# *****************************************************************************
## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 
# *****************************************************************************

#### Set up ####
## A). libraries & directories ##
# Using a local 'copy' of the Google Drive for now. Change these back when commiting

libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet", 
          "dplyr", "spatial.tools", "ff", "shiny", "shinyBS", "maptools")
lapply(libs, require, character.only = TRUE)
rm(libs)

# ~ not working for rgdal, so using the full path name
wd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny"
cd <- paste(wd, "Code files", sep = "/")
dd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny/data/resRasters"
cbd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny/data/countryBoundaries"
#wd <- "~/Boxes_2.0/Shiny"
#cd <- paste("~/Boxes_2.0/Code files", sep = "/")

## source data prep file, which loads latest data ##
# source(paste(cd, "data_prep.r", sep = "/"))

## B). Loading the resRasters files (instead of sourcing from the data prep script)
# Using the 1km resolution rasters as the default
# Will change to loading from the ff/Rdata files once I figure out how to
#sdd <- paste(dd, "5km", sep = "/")
#load(paste(dd,"cleanData.Rdata", sep = "/"))
#load(paste(dd, "dat_mat.Rdata", sep = "/"))
#open("dat.mat")
# Function that creates a filepath for a chosen res
chooseRes <- function (res) {
    pathN <- paste(dd, res, sep = "/")
    return (pathN)
}

# Loading default res rasters (5km):
# start <- Sys.time()

pth <- chooseRes("5km")
toLoad <- list.files(pth, "*.tif")
for (i in 1:length(toLoad)) {
    assign(toLoad[i], raster(paste(pth, toLoad[i], sep = "/")))
}

# Loading border files:
bpath <- paste(dd, "borders", sep = "/")
east <- readOGR(dsn = bpath, layer = "EastAfricaBorder", verbose = T)
west <- readOGR(dsn = bpath, layer = "WestAfricaBorder", verbose = T)
south <- readOGR(dsn = bpath, layer = "SouthAfricaBorder", verbose = T)
allOAF <- readOGR(dsn = bpath, layer = "OAF_all", verbose = T)
coreOAF <- readOGR(dsn = bpath, layer = "OAF_core", verbose = T)
ssa <- readOGR(dsn = bpath, layer = "all_ssa", verbose = T)

#bpath <- paste(dd, "borders", sep = "/")
#borderload <- list.files(bpath,"*.shp")
#for (i in 1:length(borderload)) {
#   loc <- paste(bpath, borderload[i], sep = "/")
#  assign(borderload[i], readOGR(dsn = bpath,
#                               layer = borderload[i]))
#}

#### functions #### 
# Function for cropping tif files to selected geography:

# Function for switching out the raster files based on the resolution slider input:
changeResData <- function(newRes) {
    pth <- chooseRes(newRes)
    toLoad <- list.files(pth, "*.tif")
    for (i in 1:length(toLoad)) {
        assign(toLoad[i], raster(paste(pth, toLoad[i], sep = "/")))
    }
}

#### server logic ####

shinyServer(function(input, output, session) {
    # create baselayer map using leaflet and selected base map data set
    output$map <- renderLeaflet({
        
        # Create a Progress object to indicate map is being loaded
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit the reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Drawing Map -- this may take a while", value = 0)
        
        
        # Create a variable for the dataset that switches based on changed input
        data <- switch(input$base,
                       "Default" = NULL,
                       "Population" = mean_pop.dense.tif,
                       "Land Size" = av.size,
                       "Urban Areas" = sum_u.pop.tif)
        
        # create a variable for color schemes that fits base map (uses RColorBrewer)
        color <- switch(input$base,
                        "Default" = NULL,
                        "Population" = colorBin("YlOrRd",
                                                bins = c(-1, 100, 150, 200, 300, 
                                                         400, 500, 600, Inf),
                                                na.color = "transparent"),
                        "Land Size" = colorBin("Greens", bins = c(-1, 2, 5, 
                                                                  10, Inf)),
                        "Urban Areas" = c("darkgreen", "yellow", "red"))
        #"Landcover" = "YlGn",
        #"water" = c("transparent", "darkblue")
        
        # Create default map layer that opens instantaneously without data
        
        if(input$base == "Default"){
            leaflet() %>% addProviderTiles("MapQuestOpen.Aerial") %>% 
                setView(22.66, 7.961, 3)
            
            # render raster data sets for base map, send to UI
        } else{
            leaflet() %>% addRasterImage(data, color, project = FALSE, maxBytes = Inf)
        }
        
    })
    
    
    # Create a variable that listens to the slider input, and passes it to the 
    # changeResData function
    
    observe ({
        changeDat <- newRes <- paste(as.character(input$res), "km", sep = "")
        changeResData(newRes)
        print(newRes)
       # output$map
    })
    
})
