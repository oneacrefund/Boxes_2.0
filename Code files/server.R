# Boxes 2.0: Server script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: Back-end server script for Shiny app
# Date created: 29 Mar 2016
# Date modified: 4 Apr 2016

## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 

#### Set up ####
## directories ## 
wd <- "~/drive/Boxes_2.0/Shiny"
cd <- paste("~/drive/Boxes_2.0/Code files", sep = "/")
dd <- paste(wd, "data", sep = "/")

## libraries ##
libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet",
          "dplyr", "spatial.tools", "shiny", "shinythemes", "shinyBS")
lapply(libs, require, character.only = TRUE)
rm(libs)
cat("\014")

## source data set-up (loads latest data) ##
# source(paste(cd, "data_prep.r", sep = "/"))

#### data blocks ####
# core.ind <- brick(list(rain.m, rain.v, av.size, pop, pop.dense))
# crop.data <- brick(crop.share) # TODO
# geo.data <- brick(list(slp, gs.l, lc))

#### functions ####

#### server logic ####

shinyServer(function(input, output) {
  
  # output$core <- renderUI({
  #   checkboxInput("core", "Core Program Indicators")
  # })   

  output$map <- renderLeaflet({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Drawing Map -- this may take a while", value = 0)

    data <- switch(input$base,
                   "Default" = NULL,
                   "Population" = pop,
                   "Land Size" = av.size,
                   "Urban Areas" = rpu.des,
                   "Landcover" = lc,
                   "water" = wm)

     color <- switch(input$base,
                     "Default" = NULL,
                     "Population" = colorBin("YlOrRd",
                                             bins = c(0, 100, 150, 200, 300, 
                                                      400, 500, 600, Inf),
                                             na.color = "transparent"),
                     "Land Size" = colorBin("Greens", bins = c(0, 2, 5, 
                                                               10, Inf)),
                     "Urban Areas" = c("darkgreen", "yellow", "red"),
                     "Landcover" = "YlGn",
                     "water" = c("transparent", "darkblue"))
     if(input$base == "Default"){
       leaflet() %>% addProviderTiles("MapQuestOpen.Aerial") %>% 
         setView(22.66, 7.961, 3)
     } else{
      leaflet() %>% addRasterImage(data, color, project = FALSE, maxBytes = Inf)
     }
  })
        
     # observe({
     #     test <- "1"
     #     if(input$core == 1){
     #         output$core_data <- renderUI({
     #             checkboxGroupInput("core_data", label = "", 
     #                                choices = "test", 
     #                                selected = "test")
     #                                })
     #     } else return(NULL)
     #         })
     #     
})
