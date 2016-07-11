# Boxes 2.0: Server script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Apprentice: Bernard Kiprop (bernard.kiprop@oneacrefund.org)
# Description: Back-end server script for Shiny app
# Date last  modified: 11 Jul 2016


# *****************************************************************************
## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 

# *****************************************************************************
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#### SET-UP: (DON'T copy this to the upload script!!!!!!!!!)
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rm(list = ls()); cat("\014")

## libs -- in global.r

# libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet", 
#           "dplyr", "spatial.tools", "ff", "shiny", "shinyBS", "maptools", "shinythemes",
#           "devtools", "sp")
# lapply(libs, require, character.only = TRUE)
# rm(libs)

#wd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny"
#dd <- paste(wd, "data", sep = "/")
#dd <- paste(cd, "resRasters2", sep = "/")
#bpath <- paste(cd, "borders", sep = "/")

dd <- "data"; bpath <- "borders"

## Load shapefiles
# To-do: load border files reactively based on geo selection
ssa <- readOGR(dsn = bpath, layer = "all_ssa")
coreOAF <- readOGR(dsn = bpath, layer = "OAF_core")
#east <- readOGR(dsn = bpath, layer = "EastAfricaBorder")
#west <- readOGR(dsn = bpath, layer = "WestAfricaBorder")
#south <- readOGR(dsn = bpath, layer = "SouthAfricaBorder")
allOAF <- readOGR(dsn = bpath, layer = "OAF_all")

## Load filter ranges (now only 5km and 10 km to test):
f.range <- read.csv(paste(dd, "filterRanges_iqr.csv", sep = "/"), header = T)

# *****************************************************************************
#### FUNCTIONS:

## Function that creates a file path for laoding data
chooseRes <- function (n) {
  n <- as.character(n)
  pth <- paste(dd, paste(n, "km", sep = ""), sep = "/")
  return (pth)
}

## Test filter data here:

## Function that matches the checkboxgroup input to number values from the checkbox input:
dtList <- c("sum_pop.tif", "sum_hhs.tif", "sum_prod_maize.tif",
            "mean_gs.l.tif")

dtList.1 <- f.range[,2]

getNames <- function (ch){
  if(length(ch) == 0) {
    return()
  }
  d <- character()
  for(i in 1:length(ch)){
    d[(length(d)) + 1] <- dtList[as.numeric(ch[i])]
  }
  return(d)
}

## Function that crops data to chosen region, and returns a new list
# Takes in a default border but possible to specify border when calling
crpDat <- function (a, bd = coreOAF) {
  r.1 <- crop(a,bd)
  r.1 <- mask(r.1, bd)
  return(r.1)
}


#================================================================================
## Function that loads the data and puts in a list (for easy access
# and manipulation)
loadDat <- function (x, toLoad) {
  #pth <- chooseRes(x)
  #toLoad <- list.files(pth, "*.tif")
  a <- list()
  print("selected: "); print(length(toLoad))
  
  for (i in 1:length(toLoad)) {
    r <- raster(paste(x, toLoad[i], sep = "/"))
    r <- crpDat(r)
    #r <- mask(r, bd)
    a[[length(a) + 1]] <- assign(toLoad[i],r)
    #print("created: "); print(length(a))
  }
  print("created: "); print(length(a))
  return (a)
}

#================================================================================
## Functions for combining rasters and painting boxe:

## Function that takes in a raster and filters and returns an equivalent size
# raster with 1s and NAs
getBool <- function (rast, rastfil) {
  rast$filt <- (rast >= rastfil[1] & rast <= rastfil[2])
  rast$filt[rast$filt == T] <- 1
  rast$filt[rast$filt == F] <- NA
  return(rast$filt)
}

getStack <- function (rastlist, rastfil) {
  pr <- paste(length(rastlist), "rasters to stack"); print(pr)
  pr.1 <- paste(length(rastfil), "filters to use"); print (pr.1)
  bools <- list()
  for (i in 1:length(rastlist)){
    x <- getBool(rastlist[[i]], rastfil[[i]])
    bools[[length(bools) + 1]] <- x
  }
  pr.3 <- paste(length(bools), "is the # of rasters to stack"); print(pr.3)
  #return (bools)
  bool.1 <- lapply(bools, prod)
  #bool.1 <- apply(bools, "prod")
  pr.2 <- paste(length(bool.1), "- length of stack list"); print(pr.2)
  return(bool.1)
}

# Function that stacks 0/1 raster layers and returns a  single layer for painting
# Pass filter arguments here
# getStack <- function (rastlist, filtlist) {
#   a <- lapply(rastlist, getBool)
#   rsMerged <- lapply(a,)
#   return(rsMerged)
# }

#### ***************************************************************************


#### SERVER:
shinyServer(function(input, output, session) {
  
  ## Get selected border:
  bdr <- reactive({input$geo})
  
  ## Load selected data:
  loadDat1 <- reactive ({
    dt <- input$testdata
    dt <- getNames(dt)
    pth <- chooseRes(input$res)
    a <- loadDat(pth, dt)
    # pr <- paste(class(a), "Class of what laodDat1 returns"); print(pr)
    return(a)
    #w <- paste(pth, dt[length(dt)], sep = "/")
    #return (w)
  }) 
  
  ## Get filter values for selected datasets:
  
  ## Stack selected raster files, and set cells outside filter values to NA:
  
  #-----------------------------------------------------------------------------
  ### Dynamically generate filters based on the datasets selected- WIP:
  
  
  ## Might make sense to use a function to generate needed filters and put in a list
 output$filters1 <-  renderUI ({
    # Do it individually first, then wrap into a function
    # First get a list of data to generate sliders for then get their names:
    dt1 <- input$testdata
    #dt2 <- input$coredata 
    dt1.names <- getNames(dt1)
    loadDat1()
    
    # Now get their filter ranges:
    minRange <-  f.range[which(f.range$res == input$res & f.range$var == dt1.names[1]),]
    minRange <- subset(minRange, select = min.threshold)
    minRange <- minRange[,1]
    
    
    #minRange <-  f.range[which(f.range$res == "5km" & f.range$var == "sum_pop.tif"),]
    #minRange <- subset(minRange, select = min.threshold)
    
    
    #maxRange <-  f.range[which(f.range$res == "5km" & f.range$var == dt1.names[1]),]
    #f.range$max.threshold]
    
    maxRange <-  f.range[which(f.range$res == input$res & f.range$var == 
                                 dt1.names[1]),]
    maxRange <- subset(maxRange, select = max.threshold)
    maxRange <- maxRange[,1]
    
    ## Get step value
    stepV <- c(500, 100, 100, 100) # TO-DO
    minRange <- c(0,0,0,0)
    maxRange <- c(5e3, 5e3, 2e3, 3e3)
    # Use the filter ranges and names to generate the filters:
    
    print("Printing a bunch of stuff:: ...")
    print(str(maxRange)); print(" ");print(str(minRange)); print(" ");
    print(str(dt1.names)); print(" "); print(str(stepV))
    
    # Might be useful:
    #https://groups.google.com/forum/#!topic/shiny-discuss/xW8f5g5gm4s
    fl <- vector("list", length(dt1.names))
    for (i in 1:length(dt1.names)){
      fl[[i]] <- list(sliderInput(dt1.names[i], label = dt1.names[i],
                                value = c(0, 100), step = stepV[i], min = minRange,
                                max = maxRange))
      
    }
    print("--------------") ; print(length(fl))
    return(fl)
    
  })
  
#reactive({makeFilts()})
  
  #-----------------------------------------------------------------------------
  ## Paint a base map - TO-DO
  #isolate({
  #   output$map <- renderLeaflet({
  #     # Get the raster file
  #     dt <- loadDat1()
  #     print(length(dt)); print("avail for painting")
  #     r <- dt[[length(dt)]]
  #     
  #     # Set progress
  #     progress <- shiny::Progress$new()
  #     on.exit(progress$close())
  #     progress$set(message = "Drawing Map -- say yaay when you see it", value = 0)
  #     
  #     #pal1 <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
  #     #                    na.color = "transparent")
  #     
  #     # Draw leaflet map
  #     leaflet() %>% addTiles() %>%  addRasterImage(r, opacity = 0.5)
  #     
  #     # addLegend(pal = pal1, position = "bottomright", values = values(r),
  #     #          title = names(r))
  #     #  print("Done")
  #   })
  #})
  
  #-----------------------------------------------------------------------------
  
  ## Create filters for selected datasets
  # To-do: auto-generate the filters for selected datasets instead
  # To-do2: filter ranges
  
#   output$filters1 <- renderUI ({
#     conditionalPanel(
#       condition = "input.testdata.includes('1')",
#       sliderInput("pop_sum",label = "Total Population",
#                   value = c(0, 1e5), step = 5e3, min = 0, max = 6e5))
#   })
#   
#   output$filters2 <- renderUI ({
#     conditionalPanel(
#       condition = "input.testdata.includes('2')",
#       sliderInput("hhs_sum", label = "Total Households",
#                   value = c(0, 100), step = 50, min = 0, max = 11e4))
#   })
#   
#   output$filters3 <- renderUI ({
#     conditionalPanel(
#       condition = "input.testdata.includes('3')",
#       sliderInput("maize_sum", label = "Maize production (total)",
#                   value = c(0, 2e3), step = 5e3, min = 0, max = 2e5))
#   })
#   
#   output$filters4 <- renderUI ({
#     conditionalPanel(
#       condition = "input.testdata.includes('4')",
#       sliderInput("seasons", label = "Months of Growing Season",
#                   value = c(0, 12), step = 1, min = 0, max = 12))
#   })
  
  #-----------------------------------------------------------------------------
  ### Draw boxes:
  
  ## Get input for each filter and use input to combine rasters and return a
  # single 1/NA raster for painting:
#   getMerged <- reactive ({
#     # Take filters and put in a list:
#     pop.filt <- input$pop_sum; popt.filt <- input$hhs_sum
#     land.filt <- input$maize_sum; seasons.filt <- input$seasons
#     filtl <- list(pop.filt, popt.filt, land.filt, seasons.filt)
#     #print(filtl[[1]]); print(class(filtl[[1]]));print(length(filtl[[1]])); print("Just printed first filter")
#     #a <- length(filtl); b <- paste("Filters: ", a); print(b)
#     
#     # Get selected datasets:
#     dt <- loadDat1()
#     
#     # Pick only filters whose data was loaded
#     ld <- input$testdata
#     filtl.1 <- list() # Create a new list called filtl.1
#     
#     # Now only pick out filters from filtl that match selected data
#     # NOTE: this requires that the filters appear in a list
#     for (i in 1:length(ld)) {
#       filtl.1[[length(filtl.1) + 1]] <- filtl[[as.numeric(ld[i])]]
#     }
#     
#     mgd <- getStack(dt, filtl.1)
#     #print("class of stack: "); print(class(mgd))
#     return(mgd)
#     
#   })
#   
#   ## Paint merged raster to show boxes
#   output$map <- renderLeaflet({
#     spMerged.1 <- getMerged()
#     spMerged <- spMerged.1[[1]]
#     
#     # Only passing in one color instead of a palette
#     pal1 <- colorNumeric("#006400",domain = values(spMerged),
#                          na.color =  "#00000000")
#     leaflet() %>% addTiles() %>%  addRasterImage(spMerged, opacity = 0.5,
#                                                  colors = pal1, project = T)
#   })
  
 

  
  
  
  
  
  #-----------------------------------------------------------------------------
  ### Dynamically altering filter ranges based on resolution - TO-DO
  
  #   observe ({
  #     n <- as.character(input$res)
  #     n.1 <- paste("km", 5, sep = "")
  #     lcol <- paste(n.1, "min", sep = "_")
  #     ucol <- paste(n.1, "max", sep = "")
  #     
  #    min = f.range[,lcol] 
  #     updateSliderInput("pop_sum", min = 400, max = 500)
  #     updateSliderInput("hhs_sum", min = 400, max = 500)
  #     updateSliderInput("maize_sum", min = 400, max = 500)
  #     updateSliderInput("seasons", min = 400, max = 500)
  #     
  #   })
  
  
  
  
  
  
  
  
  
  
  
  
  
}) # Server input/output ends here

