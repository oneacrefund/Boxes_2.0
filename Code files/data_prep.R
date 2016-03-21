# Boxes 2.0: Data prep script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: A script to prep data for upload and storage on the Shiny server
# Date created: 21 Mar 2016
# Date modified: 21 Mar 2016

#### Set up ####

## directories ##
wd <- "~/drive/Boxes_2.0/Prep"
dd <- paste(wd, "raw", sep = "/")
od <- paste(wd, "output", sep = "/")

## libraries ##
libs <- c("rgdal", "rgeos", "raster", "tiff", "ggplot2", "dplyr")
lapply(libs, require, character.only = TRUE)
rm(libs)
cat("\014")

### Load data for prepping ###

## Core Program Indicators ## 
# # monthly rainfall data for 1901 - 2014 (rain_ts, ~50km res) (TO DO, see http://harvestchoice.org/tools/long-term-climate-trends-and-variations-sub-saharan-africa)
# rain_ts_file <- paste(dd, "rainfall/time_series/CRU_pre_ts.dat", sep = "/")
# rain_ts <- read.delim(rain_ts_file) 

# WorldPop population estimates for 2010 (pop, 1km res)
pop <- raster(paste(dd, "AfriPOP_2010/WorldPop-Africa_updated/africa2010ppp.tif", 
                    sep = "/")) 

# land area raster (land_area, for pop density and farm size, 1km res)
land_area <- raster(paste(dd, "GRUMP/GL_AREAKM.tif", sep = "/"))

# landcover data (lc, for farm size 300m res)
lc <- raster(paste(dd, "landCover/ESA_GlobCover.tif", sep ="/"))

# urban extent data (GRUMP, 1km res)
GRUMP.raw <- readGDAL(paste(dd,"GRUMP/afurextents.bil", sep = "/"))
GRUMP <- raster(GRUMP.raw); rm(GRUMP.raw)

## crop data ##
# crop mix data (physical area cultivated for each crop, ca.stack, 10km res)
ca.stack <- list.files(paste(dd, "cropData", sep = "/"), pattern = "tiff", 
                       full.names = TRUE) %>% stack()

# yield gaps (TO DO)

# hybrid seed adoption (TO DO) 

## fertilizer data ##
# fertilizer consumption data (f.n.con, f.p.con, 10km res)
n.con <- raster(paste(dd, "fertConsumption/N.con.tif", sep = "/"))
p.con <- raster(paste(dd, "fertConsumption/P.con.tif", sep = "/"))
k.con <- raster(paste(dd, "fertConsumption/K.con.tif", sep = "/"))

# fertilizer application rates NOTE: this is crop-specific and will not be used

## soil fertility ##
# soil organic carbon ppm (soil.c, 10km res, 0-5cm and 5-15cm, SSA only)
soil.c.5 <- raster(paste(dd, "soil_carbon_ssa/soc_d5--SSA.tif", sep = "/"))
soil.c.15 <- raster(paste(dd, "soil_carbon_ssa/soc_d15--SSA.tif", sep = "/"))

# nitrogen content (g/kg) (soil.n,  250m res, 0-15cm and 15-30cm)
soil.n.15 # TO DO
soil.n.30 # TO DO

## Geography ##
# elevation data (elev, 900m resolution)
elev <- raster(paste(dd, "elevation/af_dem_30s.bil", sep = "/"))

# slope (slp, 10km res)
slp <- raster(paste(dd, "slope/slp.tif", sep = "/"))

# length of growing season (month) (gs.l, 10km res)
gs.l <- raster(paste(dd, "glp/lgp.tif", sep = "/"))

#### data processing: core program indicators #### 

## population density calculation ##
# TODO

## rainfall monthly mean ##
# TODO

## rainfall volatility ##
# TODO

## est. avg. farm size
# TODO
