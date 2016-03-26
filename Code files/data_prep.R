# Boxes 2.0: Data prep script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: A script to prep data for upload and storage on the Shiny server
# Date created: 21 Mar 2016
# Date modified: 26 Mar 2016

#### Set up ####

## directories ##
wd <- "~/drive/Boxes_2.0/Prep"
dd <- paste(wd, "raw", sep = "/")
od <- paste(wd, "output", sep = "/")

## libraries ##
libs <- c("rgdal", "tidyr", "rgeos", "raster", "tiff", "ggplot2", 
          "dplyr", "spatial.tools")
lapply(libs, require, character.only = TRUE)
rm(libs)
cat("\014")

### Load data for prepping ###

## Core Program Indicators ## 
# # monthly rainfall data for 1901 - 2014 (rain_ts, ~50km res) 

# import rain .csv files, convert to raster files, and write to disk
rainpath <- paste(dd, "rainfall/time_series", sep = "/")
rainlist <- list.files(rainpath, pattern = "*.csv", full.names = TRUE)
r.files <- lapply(rainlist, read.table, sep = ",", header = FALSE, col.names = 
                      c("y", seq(-18.2, by = 0.5, len = 141)), skip = 44)

r.convert <- function(csv_file, r.crs = "+proj=longlat +datum=WGS84 +no_defs 
                      +ellps=WGS84 +towgs84=0,0,0") {
    long.csv <- gather(csv_file, x, pre, X.18.2:X51.8) %>% select(x, y, pre)
    long.csv$x <- seq(-18.2, by = 0.5, len = 141) %>% rep(144) %>% sort()
    long.csv$y <- seq(-33.8, by = 0.5, len = 144)
    r.raster <- rasterFromXYZ(long.csv, crs = r.crs) 
    r.raster[r.raster == -10000] <- NA
    return(r.raster)
}

r.stack <- lapply(r.files, r.convert) %>% stack()

# WorldPop population estimates for 2010 (pop, 1km res)
pop <- raster(paste(dd, "AfriPOP_2010/WorldPop-Africa_updated/africa2010ppp.tif", 
                    sep = "/"))
# pop.tot <- cellStats(pop, sum, na.rm = TRUE) # 1,021,363,776

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
## rural, peri-urban, and urban population calculations ##
GRUMP <- spatial_sync_raster(GRUMP, pop, method = "ngb")
vals <- unique(values(GRUMP))
recl <- matrix(c(vals, NA, NA, 2), ncol = 2)
GRUMP.u <- reclassify(GRUMP, rcl = recl)
pu.des <- boundaries(GRUMP.u, directions = 4)
u.des <- GRUMP.u-pu.des
#add one to u.des s.t. r = 1, pu = 2, u = 3
u.des <- u.des + 1
rpu.des <- cover(u.des, GRUMP) # rural areas (NAs) remain 1s, p = 2 and u = 3
writeRaster(rpu.des, paste(od, "rpu_designator.tif", sep = "/"))



## rainfall monthly mean ##
## take stack of rainfall data, split into months, calculate mean, save to lists
## to change the years for which we're calculating mean, change "yr" start & len
yr <- seq.int(1, by = 12, len = nlayers(r.stack)/12)
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", 
            "oct", "nov", "dec")

for (i in 0:11) {
    if (i == 0) {
        r.months <- list()
        rm.months <- list()
    }
    mon <- stack(r.stack@layers[yr + i])
    r.months <- c(r.months, mon)
    rm.months <- c(rm.months, mean(mon))
}
rm(i)

names(r.months) <- months; names(rm.months) <- months

## calc volatility of rainfall, write raster object to "output/rain_vol"
r.vol <- function(r, rm) {
    vol <- sum(r < rm*0.7) / sum(!is.na(r))
    return(vol)
}

vol.stack <- mapply(r.vol, r.months, rm.months) %>% stack()

writeRaster(vol.stack, paste(od, "rain_vol/rvol", sep = "/"), format = "GTiff",
            bylayer = TRUE, suffix = "names", overwrite = TRUE)

writeRaster(stack(rm.months), paste(od, "rain_m/rm", sep = "/"), format = "GTiff", 
            bylayer = TRUE, suffix = "names", overwrite = TRUE)

## est. avg. farm size
# TODO
