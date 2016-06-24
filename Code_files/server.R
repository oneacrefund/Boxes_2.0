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


# *****************************************************************************
#### FUNCTIONS:
# Function that creates a file path for laoding data
chooseRes <- function (n) {
  n <- as.character(n)
  pth <- paste(dd, paste(n, "km", sep = ""), sep = "/")
  return (pth)
}

# Function that matches the checkboxgroup input to names in the checkbox input:
dtList <- c("mean_pop.dense.tif", "sum_pop.tif", "mean_av.size.tif",
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
loadDat <- function (x, toLoad) {
  #pth <- chooseRes(x)
  #toLoad <- list.files(pth, "*.tif")
  for (i in 1:length(toLoad)) {
    r <- raster(paste(x, toLoad[i], sep = "/"))
    a <- list()
    a[[length(a) + 1]] <- assign(toLoad[i],r)
    return (a)
    #return(r)
  }
}

# Function that crops data to chosen region, and returns a new list
crpDat <- function (a) {
  
}

# *****************************************************************************
#### SERVER:
shinyServer(function(input, output, session) {
  
  ## Get selected resolution
  #x <- reactive({input$res})
  
  ## Get selected border:
  bdr <- reactive({input$geo})
  
  ## Get selected datasets:
  #output$txt <- renderText(getNames(input$testdata))
  #output$txt2 <- renderText(chooseRes(input$res))
  
  # Load selected data:
  loadDat1 <- reactive ({
    dt <- input$testdata
    dt <- getNames(dt)
    pth <- chooseRes(input$res)
    a <- loadDat(pth, dt)
    return(a[[length(a)]])
    #w <- paste(pth, dt[length(dt)], sep = "/")
    #return (w)
  })
  
  #output$txt <- renderText(loadDat1())
  
  isolate({
   output$map <- renderLeaflet({
     loadDat1()
     progress <- shiny::Progress$new()   
     on.exit(progress$close())
     progress$set(message = "Drawing Map -- say yaay when you see it", value = 0)
     })
  })
  #loadDat(x, y())
  #loadDat(as.character(input$res), getNames(input$testdata))
  #    a <- reactive({getNames(input$testdata)})
  #   
  #     for(i in 1:length(a)) {
  #      output$txt <- renderText(a[[i]])
  #    }
  
  ## Paint base map
  # output$map <- renderLeaflet({ leaflet() %>% addProviderTiles("MapQuestOpen.Aerial") %>% 
  #  setView(22.66, 7.961, 3) })
  
  ## Create filters for selected datasets
  
  #   test.filt <- reactive({
  #     if(is.null(input$test)) {
  #       return()
  #     }
  #     else {
  #       #if(input$test == "Population (density)") {
  #         sliderInput("pop_dense", label = "Population Density",
  #                     value = c(0, 500), step = 100, min = 0, max = 800)
  #      # }
  #       if(input$test == "Population (total)"){
  #         sliderInput("pop_sum", label = "Total Population",
  #                     value = c(0, 2000), step = 500, min = 0, max = 1e6)
  #       }
  #     }
  #   })
  
  ## Display filters using renderUI, based on checked boxes
  #   output$filters <-  renderUI ({
  #     # Start with test filters; ranges are not true
  #     if(is.null(input$test)) {
  #       return()
  #     }
  #     
  #     if (input$testdata == "Population (density)") {
  #       sliderInput("pop_dense",label = "Population Density",
  #                   value = c(0, 500), step = 100, min = 0, max = 5700)
  #     }
  #     
  #     if (input$testdata == "Population (total)") {
  #       sliderInput("pop_sum", label = "Total Population",
  #                   value = c(0, 2000), step = 500, min = 0, max = 1e6)
  #     }
  #     
  #     if (input$testdata == "Est. avg. farm size") {
  #       sliderInput("av_farm", label = "Average Farm Size",
  #                   value = c(0, 100), step = 50, min = 0, max = 5e4)
  #     }
  #     
  #   })
  output$filters1 <- renderUI ({
    conditionalPanel(
      #condition = "input.testdata == 'Population (density)'",
      condition = "input.testdata == 1",
      sliderInput("pop_dense",label = "Population Density",
                  value = c(0, 500), step = 100, min = 0, max = 5700))
  })
  
  output$filters2 <- renderUI ({
    conditionalPanel(
      #condition = "input.testdata == 'Population (total)'",
      condition = "input.testdata == 2",
      sliderInput("pop_sum", label = "Total Population",
                  value = c(0, 2000), step = 500, min = 0, max = 1e6))
  })
  
  
  #   
  
  
  
  #     switch(input$testdata,
  #            "Population (density)" = 
  #              sliderInput("pop_dense",label = "Population Density",
  #                          value = c(0, 500), step = 100, min = 0, max = 5700),
  #            "Population (total)" = 
  #              sliderInput("pop_sum", label = "Total Population",
  #                          value = c(0, 2000), step = 500, min = 0, max = 1e6),
  #            "Est. avg. farm size" = 
  #              sliderInput("av_farm", label = "Average Farm Size",
  #                          value = c(0, 100), step = 50, min = 0, max = 5e4)
  #            
  #     )
  
  
  
}) # Server input/output ends here


