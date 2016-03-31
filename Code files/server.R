# Boxes 2.0: Server script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: Back-end server script for Shiny app
# Date created: 29 Mar 2016
# Date modified: 30 Mar 2016

## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 

#### Set up ####
## directories ## 
wd <- "~/drive/Boxes_2.0/Shiny"
cd <- paste(wd, "Code files", sep = "/")
dd <- paste(wd, "data", sep = "/")

## libraries ##
libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet",
          "dplyr", "spatial.tools", "shiny", "shinythemes", "shinyBS")
lapply(libs, require, character.only = TRUE)
rm(libs)
cat("\014")

## source data set-up (loads latest data) ##
source("data_prep.r")

#### functions ####

#### server logic ####

shinyServer(
    function(input, output) {
        output$map <- renderLeaflet({
           
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            progress$set(message = "Drawing Map", value = 0)
            
            data <- switch(input$base,
                           "Population" = pop,
                           "Land Size" = av.size,
                           "Urban Areas" = rpu.des,
                           "Landcover" = lc, 
                           "water" = wm)
            
            color <- switch(input$base,
                            "Population" = colorNumeric("YlOrRd", domain = c(0, 500)),
                            "Land Size" = colorNumeric("Greens", domain = c(0, 500)),
                            "Urban Areas" = c("darkgreen", "yellow", "red"),
                            "Landcover" = "YlGn",
                            "water" = c("transparent", "darkblue"))

            leaflet() %>%
                addRasterImage(data, color,
                               project = FALSE, maxBytes = Inf)
            
        })
    }
)

