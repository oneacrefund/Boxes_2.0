# Boxes 2.0: Data prep script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: A script to prep data for upload and storage on the Shiny server
# Date created: 21 Mar 2016
# Date modified: 6 Apr 2016

rm(list=ls())

start <- Sys.time()

#### Set up ####

# Note, if calculations not made, this code takes ~5.5 hrs to run. Make sure 
# directories are correctly referencing your computers directory versions

## directories ## 
wd <- "~/drive/Boxes_2.0/Prep"
dd <- paste(wd, "raw", sep = "/")
od <- paste(wd, "output", sep = "/")
sdd <- "~/drive/Boxes_2.0/Shiny/data"
## libraries ##
libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", "leaflet", 
          "dplyr", "spatial.tools", "ff", "shiny", "shinyBS")
lapply(libs, require, character.only = TRUE)
rm(libs)
cat("\014")


# create an "extent" raster for cropping of larget global data sets
extent <-
    raster(paste(dd, "AfriPOP_2010/WorldPop-Africa_updated/africa2010ppp.tif",
                 sep = "/"))

### Load data for prepping ###

# Note: "Pop" is the canonical dataset for spatial questions, all rasters sync
# to pop. 

## Core Program Indicators ## 

# in each instance, code checks to see if pre-processing is complete. If not, 
# processing of data is done as necessary, then output written to Shiny data dir

# WorldPop population estimates for 2010 (pop, 1km res)
# Processing: reproject for leaflet output (makes pop cannonical for Shiny app)
if(!file.exists(paste(sdd, "leaflet_pop.tif", sep = "/"))){
    pop <- raster(
        paste(dd, "AfriPOP_2010/WorldPop-Africa_updated/africa2010ppp.tif", 
                    sep = "/")) %>% projectRasterForLeaflet()
    writeRaster(pop, paste(sdd, "leaflet_pop.tif", sep = "/"))
} else{
    pop <- raster(paste(sdd, "leaflet_pop.tif", sep = "/")) 
}

# # monthly rainfall data for 1901 - 2014 (rain_ts, ~50km res) 

# import rain .csv files, convert to raster files
if(!file.exists(paste(sdd, "r_vol/rvol_apr.tif", sep = "/"))){
    rainpath <- paste(dd, "rainfall/time_series", sep = "/")
    rainlist <- list.files(rainpath, pattern = "*.csv", full.names = TRUE)
    # check if Google Drive has sync errors (i.e. duplicated files)
    # If so, abort load and throw error message
    if(!length(rainlist) == 1368) {
        stop("Rainfall files appear to have duplicates or missing files.
            Please check the input folder and try again.") 
    }
    # read all .csv files in as tables
    r.files <- lapply(rainlist, read.table, sep = ",", 
                      header = FALSE, col.names = c("y", 
                                                    seq(-18.2, by = 0.5, 
                                                        len = 141)), skip = 44)

    # create a function to convert CRU rainfall data to raster format
    r.convert <- function(csv_file, r.crs= "+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 
                            +y_0=0 +a=6378137 +b=6378137 +units=m+no_defs") {
        long.csv <- gather(csv_file, x, pre, X.18.2:X51.8) %>% select(x, y, pre)
        long.csv$x <- seq(-18.2, by = 0.5, len = 141) %>% rep(144) %>% sort()
        long.csv$y <- seq(-33.8, by = 0.5, len = 144)
        r.raster <- rasterFromXYZ(long.csv, crs = r.crs) 
        r.raster[r.raster == -10000] <- NA
        return(r.raster)
    }
    
    # apply convert function to rainfall .csv files, then create a stack
    r.stack <- lapply(r.files, r.convert) %>% stack()
}

# household data is simply population data divided by ** 6 **
if(!file.exists(paste(sdd, "hhs_Africa.tif", sep = "/"))) {
hhs <- pop / 6
writeRaster(hhs, paste(sdd, "hhs_Africa.tif", sep = "/"), overwrite = TRUE)
} else {
    hhs <- raster(paste(sdd, "hhs_Africa.tif", sep = "/"))
}

# land area raster (land_area, for pop density and farm size, 1km res)
if(!file.exists(paste(sdd, "land_area_leaflet.tif", sep = "/"))){
    land_area.raw <- raster(paste(dd, "GRUMP/GL_AREAKM_Africa.tif", sep = "/"))
    land_area <- land_area.raw %>% spatial_sync_raster(pop, method = "bilinear")
    writeRaster(land_area, paste(sdd, "land_area_leaflet.tif", sep = "/"))
} else
    land_area <- raster(paste(sdd, "land_area_leaflet.tif", sep = "/"))

# landcover data (lc, for farm size 300m res)
if(!file.exists(paste(sdd, "lc.tif", sep = "/"))) {
    lc.raw <- raster(paste(dd, "landCover/ESA_GlobCover.tif", sep ="/"))
    lc <- spatial_sync_raster(lc.raw, pop, 
                              filename = paste(sdd, "lc.tif", sep = "/"), 
                              overwrite = TRUE)
} else {
    lc <- raster(paste(sdd, "lc.tif", sep = "/"))
}

# urban extent data (GRUMP, 1km res) 
# read rpu.des in if exists, otherwise GRUMP is read to create rpu.des below
if(!file.exists(paste(sdd, "rpu_designator.tif", sep = "/"))){
    GRUMP.raw <- readGDAL(paste(dd,"GRUMP/afurextents.bil", sep = "/"))
    GRUMP <- raster(GRUMP.raw); rm(GRUMP.raw)
}

## crop data ##
# crop mix data (physical area cultivated for each crop, ca.stack, 10km res)
if(!file.exists(paste(sdd, "crop_data/prod_banana.tif", sep = "/"))) {
  # read all raw crop production data files into a stack
    ca.stack.raw <- list.files(paste(dd, "cropData", sep = "/"),
                               pattern = "tiff$", full.names = TRUE) %>% 
        stack()
  
  # crop stack to extent of Africa, then sync to population raster
    ca.stack.cropped <- crop(ca.stack.raw, extent)
    ca.stack <- spatial_sync_raster(ca.stack.cropped, pop, method = "ngb")
    writeRaster(ca.stack, paste(sdd, "crop_data/prod", sep = "/"), 
                format = "GTiff", bylayer = TRUE, suffix = "names")

} else {
    ca.stack <- list.files(paste(sdd, "crop_data", sep = "/"), pattern = "tif$",
                           full.names = TRUE) %>% stack()
}

# yield gaps (TODO)

# hybrid seed adoption (TODO) 

## fertilizer data (follows similar procedure to crop data)##
# fertilizer consumption data (f.n.con, f.p.con, 10km res)
if(!file.exists(paste(sdd, "n.con.tif", sep = "/"))){
    n.con <- raster(paste(dd, "fertConsumption/N.con.tif", sep = "/")) %>% 
        crop(extent) %>% 
    spatial_sync_raster(pop, method = "bilinear")
    writeRaster(n.con, paste(sdd, "n.con.tif", sep = "/"))
} else {
    n.con <- raster(paste(sdd, "n.con.tif", sep = "/"))
}
if(!file.exists(paste(sdd, "p.con.tif", sep = "/"))) {
p.con <- raster(paste(dd, "fertConsumption/P.con.tif", sep = "/")) %>% 
    crop(extent) %>% 
    spatial_sync_raster(pop, method = "bilinear")
    writeRaster(p.con, paste(sdd, "p.con.tif", sep = "/"))
} else{
    p.con <- raster(paste(sdd, "p.con.tif", sep = "/"))
}

if(!file.exists(paste(sdd, "k.con.tif", sep = "/"))){
    k.con <- raster(paste(dd, "fertConsumption/K.con.tif", sep = "/")) %>% 
        crop(extent) %>% 
    spatial_sync_raster(pop, method = "bilinear")
    writeRaster(k.con, paste(sdd, "k.con.tif", sep = "/"))
} else {
    k.con <- raster(paste(sdd, "k.con.tif", sep = "/"))
}

## soil fertility ##
# soil organic carbon ppm (soil.c, 10km res, 0-5cm and 5-15cm, SSA only)
if(!file.exists(paste(sdd, "soil.c.5.tif", sep = "/"))){
    soil.c.5 <- raster(paste(dd, "soil_carbon_ssa/soc_d5--SSA.tif", sep = "/"))
    soil.c.5 <- spatial_sync_raster(soil.c.5, pop, method = "ngb") 
    writeRaster(soil.c.5, paste(sdd, "soil.c.5.tif", sep = "/"))
} else {
    soil.c.5 <- raster(paste(sdd, "soil.c.5.tif", sep = "/"))
}
        
if(!file.exists(paste(sdd, "soil.c.5.tif", sep = "/"))){
    soil.c.15<- raster(paste(dd, "soil_carbon_ssa/soc_d15--SSA.tif", sep = "/"))
    soil.c.15 <- spatial_sync_raster(soil.c.15, pop, method = "ngb")
    writeRaster(soil.c.15, paste(sdd, "soil.c.15.tif", sep = "/"))
} else {
    soil.c.15 <- raster(paste(sdd, "soil.c.15.tif", sep = "/"))
}

# nitrogen content (g/kg) (soil.n,  250m res, 0-15cm and 15-30cm)
# soil.n.15 # TODO
# soil.n.30 # TODO

## Geography ##
# elevation data (elev, 900m resolution)
if(!file.exists(paste(sdd, "elev.tif", sep = "/"))){
    elev <- raster(paste(dd, "elevation/af_dem_30s.bil", sep = "/")) %>% 
        spatial_sync_raster(pop, method = "bilinear")
    writeRaster(elev, paste(sdd, "elev.tif", sep = "/"))
    } else {
    elev <- raster(paste(sdd, "elev.tif", sep = "/"))
}

# slope (slp, 10km res)
if(!file.exists(paste(sdd, "slp.tif", sep = "/"))){
    slp <- raster(paste(dd, "slope/slp.tif", sep = "/")) %>% 
        crop(extent) %>% 
        spatial_sync_raster(pop, method = "bilinear")
          writeRaster(slp, paste(sdd, "slp.tif", sep = "/"))
} else {
    slp <- raster(paste(sdd, "slp.tif", sep = "/"))
}
    

# length of growing season (month) (gs.l, 10km res)
if(!file.exists(paste(sdd, "growingseasons.tif", sep = "/"))){
    gs.l <- raster(paste(dd, "/glp/lgp.tif", sep = "/")) %>% 
    spatial_sync_raster(pop, method = "ngb")
    writeRaster(gs.l, paste(sdd, "growingseasons.tif", sep = "/"))
} else {
    gs.l <- raster(paste(sdd, "growingseasons.tif", sep = "/"))
}

# ***************************************************************************

#### data processing: core program indicators #### 
## rural, peri-urban, and urban population calculations ##

if(!file.exists(paste(sdd, "rpu_designator.tif", sep = "/"))) {
    GRUMP <- spatial_sync_raster(GRUMP, pop, method = "ngb") # sync GRUMP to pop
    vals <- unique(values(GRUMP)) # pull out unique values
    # create a new raster (GRUMP.u) that identifies ONLY urban areas
    recl <- matrix(c(vals, NA, NA, 2), ncol = 2)
    GRUMP.u <- reclassify(GRUMP, rcl = recl)
    # identify peri-urban boundaries as cells in GRUMP.u that touch NA cells
    pu.des <- boundaries(GRUMP.u, directions = 4)
    
    # remove peri-urban boundaries from urban classification
    u.des <- GRUMP.u-pu.des
    # you now have a raster (u.des) with values: rural = NA, peri = 1, urban = 2
    #add one to u.des so that r = NA, p = 2, u = 3
    u.des <- u.des + 1
  # Now, create RPU by overlaying p and u (2 and 3) onto original GRUMP
    rpu.des <- cover(u.des, GRUMP) # rural areas (NAs) remain 1s, p=2 and u=3
   # write to disk
    writeRaster(rpu.des, paste(sdd, "rpu_designator.tif", sep = "/"))
} else{
    rpu.des <- raster(paste(sdd, "rpu_designator.tif", sep = "/"))
}

### define rural pop, peri pop, and urban pop as separate population layers

## separate designator layer so that each classification has it's own layer  
# rural
recl <- matrix(c(1, 2, 3, 1, 0, 0), ncol = 2)
r.des <- reclassify(rpu.des, rcl = reclassify)

# peri
recl <- matrix(c(1, 2, 3, 0, 1, 0), ncol = 2)
p.des <- reclassify(rpu.des, rcl = reclassify)

# urban
recl <- matrix(c(1, 2, 3, 0, 0, 1), ncol = 2)
u.des <- reclassify(rpu.des, rcl = reclassify)

# multiply pop by each layer to get each population type, then stack
r.pop <- r.des * pop
p.pop <- p.des * pop
u.pop <- u.des * pop
popstack <- stack(r.pop, p.pop, u.pop)

# write popstack to disk
writeRaster(popstack, paste(dd, "resRasters", paste(1, "km/sum", sep = ""), sep = "/"),
            format = "GTiff", bylayer = TRUE, suffix = "names")

## rainfall monthly mean ##
## take stack of rainfall data, split into months, calculate mean, save to lists
## to change the years for which we're calculating mean, change "yr" start & len
if(!file.exists(paste(sdd, "r_vol/rvol_apr.tif", sep = "/"))) {
    # yr is a sequence identifying each January throughout the stack
    yr <- seq.int(1, by = 12, len = floor(nlayers(r.stack)/12))
    months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", 
                "oct", "nov", "dec")
    
    # create a loop identifying each month in turn, and create a stack per month
    for (i in 0:11) {
        if (i == 0) {
            r.months <- list() # create an empty list to hold each month's stack
            rm.months <- list()# create an empty list to hold each month's mean
        }
        mon <- stack(r.stack@layers[yr + i])
        r.months <- c(r.months, mon) # add each months' stack to list
        rm.months <- c(rm.months, mean(mon)) # add month's mean to list of means
    }
    rm(i)
    
    names(r.months) <- months; names(rm.months) <- months
    rain.m <- stack(rm.months)
    
    ## create func to calc volatility of rainfall, 
    ## write raster object to "output/rain_vol"
    r.vol <- function(r, rm) {
        vol <- sum(r < rm*0.7) / sum(!is.na(r)) # counts # of months rain was
        return(vol)                             #  <70% of mean, / by # months
    } 
    
    # run r.vol function over the each month's stack, stack results
    rain.v <- mapply(r.vol, r.months, rain.m) %>% stack()
    
    writeRaster(rain.v, paste(sdd, "r_vol/", sep = "/"), format = "GTiff",
                bylayer = TRUE, suffix = "names", overwrite = TRUE)
    
    writeRaster(rain.m, paste(sdd, "r_m/", sep = "/"), format = "GTiff", 
                bylayer = TRUE, suffix = "names", overwrite = TRUE)
} else {
    rainpath <- paste(sdd, "r_vol", sep = "/")
    rain.vol.list <- list.files(rainpath, pattern = "*.tif$", full.names = TRUE)
    r.vol.list <- lapply(rain.vol.list, raster)
    rain.v <- stack(r.vol.list)
    
    rainpath <- paste(sdd, "r_m", sep = "/")
    r.m.list <- list.files(rainpath, pattern = "*.tif$", full.names = TRUE)
    r.m.list <- lapply(r.m.list, raster)
    rain.m <- stack(r.m.list); rm(rainpath, r.vol.list, r.m.list, rain.vol.list)
}

## population density
if(!file.exists(paste(sdd, "pop.dense.tif", sep = "/"))) {
    pop.dense <- pop/land_area 
    writeRaster(pop.dens, filename = paste(sdd, "pop.dense.tif", sep = "/"))
} else {
    pop.dense <- raster(paste(sdd, "pop.dense.tif", sep = "/"))
}

## est. avg. farm size
# replace classifiers for cultivated land with values representing % of pixel
# dedicated to cultivated land, make all other values "0" 
# ******this takes a SUPER long time *******
if(!file.exists(paste(sdd, "crop_c.tif", sep = "/"))) {
repl <- data.frame(key = 
            c(11, 14, 20, 30, 40, 50, 60, 70, 90, 100, 110, 120, 130, 
              140, 150, 160, 170, 180, 190, 200, 210, 220),
            repl = c(1L, 1L, 0.8, 0.5, rep(0, 18)))
    crop.c <- subs(lc, repl, filename = paste(sdd, "crop_c.tif", sep = "/"))
} else {
    crop.c <- raster(paste(sdd, "crop_c.tif", sep = "/"))
}

# aggreageate 300m land use data up to the level of population data (~1km)
if(!file.exists(paste(sdd, "crop_pct.tif", sep = "/"))) {
       crop.pct <- aggregate(crop.c, fact = 3, fun = "mean", na.rm = TRUE, 
                             filename = paste(sdd, "crop_pct.tif", sep = "/")) 
} else {
    crop.pct <- raster(paste(sdd, "crop_pct.tif", sep = "/"))
}

# calculate average farm isze
if(!file.exists(paste(sdd, "av_farm_size.tif", sep = "/"))) {
    crop.pct <- spatial_sync_raster(crop.pct, pop)
    av.size <- (crop.pct * land_area) / hhs # sqkms per hh
    kms.to.acres <- 0.004046857 # number of sq kms in an acre
    av.size <- av.size / kms.to.acres # convert units to acres per household
    writeRaster(av.size, paste(sdd, "av_farm_size.tif", sep = "/"))
} else {
    av.size <- raster(paste(sdd, "av_farm_size.tif", sep = "/"))
}

#### prep data for leaflet and save to Shiny app data directory ####
# this may not need to be run

# sync "% of land that is cropland" data to "pop" raster
if(!file.exists(paste(sdd, "crop_pct.tif", sep = "/"))) {
    crop.pct <- spatial_sync_raster(crop.pct, pop, method = "bilinear")
    writeRaster(crop.pct, paste(sdd, "crop_pct.tif", sep = "/"))
}

# sync rainfall mean data to "pop" raster
if(!file.exists(paste(sdd, "r_m/rm_apr.tif", sep = "/"))) {
    rain.m <- spatial_sync_raster(rain.m, pop, method = "bilinear")
        writeRaster(rain.m, paste(sdd, "r_m/rm", sep = "/"),
                    format = "GTiff", bylayer = TRUE,
                    suffix = "names", overwrite = TRUE)
}

# sync rainfall volatility data to "pop" raster
if(!file.exists(paste(sdd, "r_vol/rvol_apr.tif", sep = "/"))) {
    rain.v <- spatial_sync_raster(rain.v, pop, method = "ngb")
        writeRaster(rain.v, paste(sdd, "r_vol/", sep = "/"),
                    format = "GTiff", bylayer = TRUE,
                    suffix = "names", overwrite = TRUE)
}

# sync land use data to "pop" raster
if(!file.exists(paste(sdd, "lc.tif", sep = "/"))) {
    lc <- spatial_sync_raster(lc, pop, method = "ngb")
    writeRaster(lc, paste(sdd, "lc.tif", sep = "/"))
}

# save data as an RData file
save.image(file = paste(sdd, "cleanData.RData", sep = "/"))

# output time of calculation
duration <- Sys.time() - start; duration

