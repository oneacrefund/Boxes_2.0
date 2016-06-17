# # Boxes 2.0: Server script
# # Author: Colin Custer (colin.custer@oneacrefund.org)
# # Apprentice: Bernard Kiprop (bernard.kiprop@oneacrefund.org)
# # Description: Back-end server script for Shiny app
# # Date last  modified: 14 Jun 2016
# 
# 
# # *****************************************************************************
# ## Note, while under construction this lives in "Code Files" subdirectory. 
# ## it should be moved to a Shiny directory when finished. 
# 
# # *****************************************************************************
# 
# #### Set up ####
# ## A). libraries & directories ##
# # Using a local 'copy' of the Google Drive for now. Change these back when commiting
# rm(list = ls())
# cat("\014")
# 
# start <- Sys.time()
# libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet", 
#           "dplyr", "spatial.tools", "ff", "shiny", "shinyBS", "maptools", "shinythemes",
#           "devtools", "sp")
# lapply(libs, require, character.only = TRUE)
# rm(libs)
# 
# # ~ not working for rgdal, so using the full path name
# wd <- "C:/Users/OAFUser/Documents/Boxes_2.0/Shiny"
# cd <- paste(wd, "data", sep = "/")
# dd <- paste(cd, "resRasters2", sep = "/")
# bpath <- paste(cd, "borders", sep = "/")
# 
# ## source data prep file, which loads latest data ##
# # source(paste(cd, "data_prep.r", sep = "/"))
# 
# # Loading border files (regional borders were pre-made):
# east <- readOGR(dsn = bpath, layer = "EastAfricaBorder")
# west <- readOGR(dsn = bpath, layer = "WestAfricaBorder")
# south <- readOGR(dsn = bpath, layer = "SouthAfricaBorder")
# allOAF <- readOGR(dsn = bpath, layer = "OAF_all")
# coreOAF <- readOGR(dsn = bpath, layer = "OAF_core")
# ssa <- readOGR(dsn = bpath, layer = "all_ssa")
# 
# 
# 
# ## B). Loading the resRasters files (instead of sourcing from the data prep script)
# # Using the 5km resolution rasters as the default
# 
# # Will change to loading from the ff/Rdata files once I figure out how to
# #sdd <- paste(dd, "5km", sep = "/")
# #load(paste(dd,"cleanData.Rdata", sep = "/"))
# #load(paste(sdd, "dat_mat.Rdata", sep = "/"))
# #t <- attr(dat.mat, "physical")
# #attr(t, "filename") <- paste(sdd, "dat_mat.ffdata", sep = "/")
# #attr(dat.mat, "physical") <- t
# #open(dat.mat)
# 
# # Function that creates a filepath for a chosen res
# chooseRes <- function (km) {
#     pth <- paste(dd, km, sep = "/")
#     return (pth)
# }
# 
# # Loading default res rasters (5km):
# 
# pth <- chooseRes("5km")
# toLoad <- list.files(pth, "*.tif")
# for (i in 1:length(toLoad)) {
#     r <- raster(paste(pth, toLoad[i], sep = "/"))
#     #     #r <- crop(r, west, snap = "out")
#     #     #newProj <- projection(ssa)
#     #     #r <- projectRaster(r, crs = newProj)
#     #     #r <- crop(r, ssa, snap = "out")
#     assign(toLoad[i],r)
# }
# 
# 
# #*************************************************************************************
# 
# 
# #### functions #### 
# 
# # Function for switching out the raster files based on the resolution slider input:
# changeResData <- function(newRes) {
#     pth <- chooseRes(newRes)
#     for (i in 1:length(toLoad)) {
#         r <- raster(paste(pth, toLoad[i], sep = "/"))
#         assign(toLoad[i],r)
#     }
# }
# 
# # Function for cropping tif files to selected geography:
# # First function not quite working
# #chooseRegion <- function (reg) {
# #     for(i in 1:length(toLoad)){
# #          x <- get(ls(pattern = ".tif")[i])
# #          assign(x, toLoad[i])
# #          x <- crop(x, reg, snap = "out")
# #     }
# # }
# 
# chooseRegion <- function (reg) {
#     for (i in 1:length(toLoad)) {
#         r <- raster(paste(pth, toLoad[i], sep = "/"))
#         r <- crop(r, west, snap = "out")
#         assign(toLoad[i],r)
#         r
#     }
# }
# 
# #*************************************************************************************
# 
# 
# #### server logic ####
# 
# shinyServer(function(input, output, session) {
#     
#     # create baselayer map using leaflet and selected base map data set
#     # Note that the base layer map takes in a shapefile as an argument
#     paintBaseMap <- function (pgn) {
#         
#         output$map <- renderLeaflet({
#             
#             # Create a Progress object to indicate map is being loaded
#             progress <- shiny::Progress$new()
#             # Make sure it closes when we exit the reactive, even if there's an error
#             on.exit(progress$close())
#             progress$set(message = "Drawing Map -- this may take a while", value = 0)
#             
#             
#             # Create a variable for the dataset that switches based on changed input
#             data <- switch(input$base,
#                            "Default" = NULL,
#                            "Population" = mean_pop.dense.tif,
#                            "Land Size" = mean_av.size.tif,
#                            "Urban Population" = mean_u.pop.tif)
#             
#             # create a variable for color schemes that fits base map (uses RColorBrewer)
#             color <- switch(input$base,
#                             "Default" = NULL,
#                             "Population" = colorBin("YlOrRd",
#                                                     bins = c(-1, 100, 150, 200, 300, 
#                                                              400, 500, 600, Inf),
#                                                     na.color = "transparent"),
#                             "Land Size" = colorBin("Greens", bins = c(-1, 2, 5, 
#                                                                       10, Inf)),
#                             "Urban Population" = c("darkgreen", "yellow", "red"))
#             
#             # Create default map layer that opens instantaneously without data
#             
#             if(input$base == "Default"){
#                 leaflet() %>% addProviderTiles("MapQuestOpen.Aerial") %>% 
#                     setView(22.66, 7.961, 3)
#                 
#                 # render raster data sets for base map, send to UI
#             } else{
#                 leaflet(pgn) %>% addPolygons() %>% 
#                     addRasterImage(data, color, project = FALSE, maxBytes = Inf)
#                 
#             }
#             
#         })
#     }
#     
#     
#     # Listening to the slider input, and refreshing map for changes in resolution:
#     reschange <- function () {
#         observeEvent (input$res, {
#         newRes <- paste(as.character(input$res), "km", sep = "")
#         changeResData(newRes)
#         paintBaseMap(ssa)
#     })
#     }
#     
#     # Listening to geographical change:
#     geochange <- function () {
#         observeEvent(input$geo,{
#             newgeo <- switch (input$geo,
#                               "East Africa" = east,
#                               "Central Africa" = central,
#                               "South Africa" = south,
#                               "West Africa" = west,
#                               "OAF - All" = allOAF,
#                               "OAF - Core" = coreOAF,
#                               "SSA" = ssa)
#             chooseRegion(newgeo)
#             paintBaseMap(newgeo)
#         }
#         
#         )
#     }
#     reschange()
#     geochange()
# })
# duration <- Sys.time() - start; print(duration)

shinyServer(function(input, output, session) {})
