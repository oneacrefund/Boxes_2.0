
# Server (Colin):
#### create data blocks for later use (may delete) ####
#core.ind <-list("rain.m" = rain.m, "rain.v" = rain.v, "av.size" = av.size, 
#               "pop" = pop, "pop.dense" = pop.dense)
#crop.data <- list("crop.share" = ca.stack) # TODO
#geo.data <- list("elev" = elev, "slp" = slp, "gs.l" = gs.l, "lc" = lc)


#### UI set-up description ####
# 1) resolution selector (slider, 5km steps) **DONE**
# 2) dataset selector (checkboxes, grouped by topics from business case)
# 3) dataset filters (returned from a server "renderUI", only for data selected)
# 4) geographic area selection (checkbox)
# 5) district vs. regional vs. national data resolution selection (slider?)
# 4) button to download data (action button)
# 5) **potentially** button to "refresh" map if render times too long (button)
# 6) map (leafetOutput)




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

###**************************************************************************###

# Server (Bernard):
# 1. trying to make custom borders
#  b <- list()
# for (i in 1:length(cts)) {
#    if( borderList[[i]]$NAME_0 == cts[i]){
#       b[[length(b) + 1]] <- (borderList[[i]])
#      return(b)
# }
#return(b)
#b <- subset(borderList, borderList[[i]]$NAME_0 == cts[i])
#}
#return(b)
#for (i in 1:length(b)) {
#    a <- spRbind(b[[i]])
#}

#    return (a)


# Trying to load ff data again:
#load(paste(ffpth, "dat_mat.Rdata", sep = "/"))
#open.ff(dat.mat, readonly = T, assert = T)


