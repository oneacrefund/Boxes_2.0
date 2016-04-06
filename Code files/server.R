# Boxes 2.0: Server script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: Back-end server script for Shiny app
# Date created: 29 Mar 2016
# Date modified: 6 Apr 2016


#### server logic description (execution TODOs) ####

# ****** "set-up" portion *******
# 0) basic setup of directories, raw data, etc. # DONE
# 1) create base layer with no highlighted areas # DONE
# 2) take data request inputs from UI # TODO
# 3) return data filters based on data request # TODO
# 4) take resolution request input from UI # TODO
# 5) pull matrix of values from reqested resolution # matrices DONE, pull # TODO
# 6) take data filter values from UI # TODO

# ****** "Boxes" portion *******
# 7) create vector of logical values for each cell based on filter values # TODO
# 8) create spatial points df of values in every layer for each cell 
# 9) add a "T/F" column to the spatial points df based on filter criteria
# 10a) create and display raster of logical value as "boxes" # TODO  ***OR***
# 10b) display as circle markers # TODO (recommended)
# 11) create popups based on spatial points dataframe tied to circle markers ****OR***
# 12) create popups based on underlying rasters (maybe how-to here: http://esri.github.io/esri-leaflet/examples/customizing-popups.html)

# ****** district portion  *******
# 13) take geo request and return relevant shapefiles to leaflet # TODO
# 14) extract values from values matrix for each shapefile # TODO
# 15) return spatial polygons dataframe with extracted values for each shapefile # TODO
# 16) add column to data frame with "T/F" values based on filter criteria # TODO
# 17) return shapefiles to leaflet colored by "T/F" values
# 18) create popups based on the sp dataframe for each shape # TODO

# *****************************************************************************
## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 
# *****************************************************************************

#### Set up ####
## directories ## 
wd <- "~/drive/Boxes_2.0/Shiny"
cd <- paste("~/drive/Boxes_2.0/Code files", sep = "/")

## source data prep file, which loads latest data ##
source(paste(cd, "data_prep.r", sep = "/"))
dd <- paste(wd, "data", sep = "/")


#### create data blocks for later use (may delete) ####
core.ind <-list("rain.m" = rain.m, "rain.v" = rain.v, "av.size" = av.size, 
                "pop" = pop, "pop.dense" = pop.dense)
crop.data <- list("crop.share" = ca.stack) # TODO
geo.data <- list("elev" = elev, "slp" = slp, "gs.l" = gs.l, "lc" = lc)

#### functions ####

#### server logic ####

shinyServer(function(input, output) {
    
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
                   "Population" = pop,
                   "Land Size" = av.size,
                   "Urban Areas" = rpu.des,
                   "Landcover" = lc,
                   "water" = wm)

    # create a variable for color schemes that fits base map (uses RColorBrewer)
     color <- switch(input$base,
                     "Default" = NULL,
                     "Population" = colorBin("YlOrRd",
                                             bins = c(-1, 100, 150, 200, 300, 
                                                      400, 500, 600, Inf),
                                             na.color = "transparent"),
                     "Land Size" = colorBin("Greens", bins = c(-1, 2, 5, 
                                                               10, Inf)),
                     "Urban Areas" = c("darkgreen", "yellow", "red"),
                     "Landcover" = "YlGn",
                     "water" = c("transparent", "darkblue"))
     
     # Create default map layer that opens instantaneously without data
     if(input$base == "Default"){
       leaflet() %>% addProviderTiles("MapQuestOpen.Aerial") %>% 
         setView(22.66, 7.961, 3)
    
    # render raster data sets for base map, send to UI
     } else{
      leaflet() %>% addRasterImage(data, color, project = FALSE, maxBytes = Inf)
     }
  })
        

})
