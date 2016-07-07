# Boxes 2.0: Server script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Apprentice: Bernard Kiprop (bernard.kiprop@oneacrefund.org)
# Description: Back-end server script for Shiny app
# Date last  modified: 04 Jul 2016


# *****************************************************************************
## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 

# *****************************************************************************

#### SET-UP:
rm(list = ls()); cat("\014")

libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet", 
          "dplyr", "spatial.tools", "ff", "shiny", "shinyBS", "maptools", "shinythemes",
          "devtools", "sp")
lapply(libs, require, character.only = TRUE)
rm(libs)

wd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny"
cd <- paste(wd, "data", sep = "/")
dd <- paste(cd, "resRasters2", sep = "/")
bpath <- paste(cd, "borders", sep = "/")

## Load shapefiles
# To-do: load border files reactively
ssa <- readOGR(dsn = bpath, layer = "all_ssa")
coreOAF <- readOGR(dsn = bpath, layer = "OAF_core")
east <- readOGR(dsn = bpath, layer = "EastAfricaBorder")
west <- readOGR(dsn = bpath, layer = "WestAfricaBorder")
south <- readOGR(dsn = bpath, layer = "SouthAfricaBorder")
allOAF <- readOGR(dsn = bpath, layer = "OAF_all")

# *****************************************************************************
#### FUNCTIONS:

## Function that creates a file path for laoding data
chooseRes <- function (n) {
  n <- as.character(n)
  pth <- paste(dd, paste(n, "km", sep = ""), sep = "/")
  return (pth)
}

## Function that matches the checkboxgroup input to number values from the checkbox input:
dtList <- c("mean_r.pop.tif", "sum_pop.tif", "mean_av.size.tif",
            "mean_gs.l.tif")

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
## Functions for combining rasters and painting boxes (WIP):

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
  #return (bools)
  bool.1 <- lapply(bools, prod)
  pr.2 <- paste(class(bool.1), "- class of new raster; makes sense?"); print(pr.2)
  return(bool.1)
}

# Function that stacks 0/1 raster layers and returns a  single layer for painting
# Pass filter arguments here
# getStack <- function (rastlist, filtlist) {
#   a <- lapply(rastlist, getBool)
#   rsMerged <- lapply(a,)
#   return(rsMerged)
# }


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
  ## Paint base map - TO-DO
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
  
  output$filters1 <- renderUI ({
    conditionalPanel(
      condition = "input.testdata.includes('1')",
      sliderInput("pop_dense",label = "Population Density",
                  value = c(0, 500), step = 100, min = 0, max = 5700))
  })
  
  output$filters2 <- renderUI ({
    conditionalPanel(
      condition = "input.testdata.includes('2')",
      sliderInput("pop_sum", label = "Total Population",
                  value = c(0, 2000), step = 500, min = 0, max = 1e6))
  })
  
  output$filters3 <- renderUI ({
    conditionalPanel(
      condition = "input.testdata.includes('3')",
      sliderInput("av_land", label = "Average Land Size/Farm",
                  value = c(0, 10), step = 0.5, min = 0, max = 10))
  })
  
  output$filters4 <- renderUI ({
    conditionalPanel(
      condition = "input.testdata.includes('4')",
      sliderInput("seasons", label = "Months of Growing Season",
                  value = c(0, 12), step = 1, min = 0, max = 12))
  })
  
  #-----------------------------------------------------------------------------
  ### Draw boxes:
  
  ## Get input for each filter and use input to combine rasters and return a
  # single 1/NA raster for painting:
  getMerged <- reactive ({
    # Take filters and put in a list:
    pop.filt <- input$pop_dense; popt.filt <- input$pop_sum
    land.filt <- input$av_land; seasons.filt <- input$seasons
    filtl <- list(pop.filt, popt.filt, land.filt, seasons.filt)
    print(filtl[[1]]); print(class(filtl[[1]]));print(length(filtl[[1]])); print("Just printed first filter")
    a <- length(filtl); b <- paste("Filters: ", a); print(b)
    
    # Get selected datasets:
    dt <- loadDat1()
    
    # Pick only filters
    ld <- input$testdata
    filtl.1 <- list() # Create a new list called filtl.1
    # Now only pick out filters from filtl that match selected data
    for (i in 1:length(ld)) {
      filtl.1[[length(filtl.1) + 1]] <- filtl[[as.numeric(ld[i])]]
    }
    print(length(filtl.1)); print("Those are the new filters, number of")
    
    mgd <- getStack(dt, filtl.1)
    print("class of stack: "); print(class(mgd))
    return(mgd)
    
  })
  
  ## Paint merged raster to show boxes
  output$map <- renderLeaflet({
    spMerged.1 <- getMerged()
    spMerged <- spMerged.1[[1]] 
    # If we provide a palette it tries to use multiple colors?
    pal1 <- colorNumeric("#0C2C84",domain = values(spMerged),
                         na.color =  "#00000000")
    leaflet() %>% addTiles() %>%  addRasterImage(spMerged, opacity = 0.5,
                                                 colors = pal1, project = T)
  })
  
}) # Server input/output ends here

