# Boxes 2.0: Server script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Apprentice: Bernard Kiprop (bernard.kiprop@oneacrefund.org)
# Description: Back-end server script for Shiny app
# Date last  modified: 14 Jun 2016


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
ssa <- readOGR(dsn = bpath, layer = "all_ssa")
coreOAF <- readOGR(dsn = bpath, layer = "OAF_core")

# *****************************************************************************
#### FUNCTIONS:
# Function that creates a file path for laoding data
chooseRes <- function (n) {
  n <- as.character(n)
  pth <- paste(dd, paste(n, "km", sep = ""), sep = "/")
  return (pth)
}

# Function that matches the checkboxgroup input to names in the checkbox input:
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

# Function that loads the data and puts in a list (for easy access
# and manipulation)
# Takes in a default border but possible to specify border when calling
loadDat <- function (x, toLoad, bd = coreOAF) {
  #pth <- chooseRes(x)
  #toLoad <- list.files(pth, "*.tif")
  for (i in 1:length(toLoad)) {
    print("selected: "); print(length(toLoad))
    r <- raster(paste(x, toLoad[i], sep = "/"))
    r <- crop(r, bd)
    r <- mask(r, bd)
    a <- list()
    a[[length(a) + 1]] <- assign(toLoad[i],r)
    #print("created: "); print(length(a))
    return (a)
    print("done")
    
  }
}

# Function that crops data to chosen region, and returns a new list
crpDat <- function (a) {
  
}


#### ***************************************************************************


#### SERVER:
shinyServer(function(input, output, session) {
  
  ## Get selected border:
  bdr <- reactive({input$geo})
  
  ## Load selected data:
  isolate({ loadDat1 <- reactive ({
    dt <- input$testdata
    dt <- getNames(dt)
    pth <- chooseRes(input$res)
    a <- loadDat(pth, dt)
    return(a)
    #w <- paste(pth, dt[length(dt)], sep = "/")
    #return (w)
  }) })
  
  #output$txt <- renderText(loadDat1())
  
  isolate({
    output$map <- renderLeaflet({
      dt.1 <- loadDat1()
      print(length(dt.1)); print("avail for painting")
      #plot(dt[[1]])
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Drawing Map -- say yaay when you see it", value = 0)     
      
      r <- dt.1[[length(dt.1)]]
      pal1 <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                          na.color = "transparent")
      
      leaflet() %>% addTiles() %>% #colors = "Spectral"
        addRasterImage(r, colors = "Spectral", opacity = 0.7) #%>%
      # addLegend(pal = pal1, position = "bottomright", values = values(r),
       #          title = names(r))
      print("Done")
    })
  })
  
  
  
  ## Paint base map
  # output$map <- renderLeaflet({ leaflet() %>% addProviderTiles("MapQuestOpen.Aerial") %>% 
  #  setView(22.66, 7.961, 3) })
  
  
  ## Create filters for selected datasets
  
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
      sliderInput("pop_sum", label = "Average Land Size/Farm",
                  value = c(0, 10), step = 0.5, min = 0, max = 10))
  })
  
  output$filters4 <- renderUI ({
    conditionalPanel(
      condition = "input.testdata.includes('4')",
      sliderInput("pop_sum", label = "Months of Growing Season",
                  value = c(0, 12), step = 1, min = 0, max = 12))
  })
  
  
}) # Server input/output ends here


