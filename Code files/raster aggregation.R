# raster aggregation and value vector 
# created by: colin custer (colin.custer@oneacrefund.org)
# created for: Boxes 2.0 (https://github.com/ccc5vd/Boxes_2.0)
# date created: 5 Apr 2016
# date modified: 6 Apr 2016
# description: aggregate all data rasters to specificed resolutions
#              then return a matrix of values for each resolution. In this
#              matrix, each row is a cell, each column a layer. Then for each 
#              cell, you will be able to return a numeric vector of values for 
#              each layer, which can be evaluated based on filter criteria

rm(list = ls()) # clear workspace

# set up
cd <- paste("~/drive/Boxes_2.0/Code files", sep = "/")
source(paste(cd, "data_prep.r", sep = "/")) # source data prep file, which loads
swd <- "~/drive/Boxes_2.0/Shiny"            # raw data files for aggregation
dd <- paste(swd, "data", sep = "/")


# create a list of rasters to be aggregated using "mean" function
mean.rasters.raw <- list(av.size, rain.m, rain.v, pop.dense, slp, gs.l, elev, 
                     soil.c.5, soil.c.15)

# set names for easy reference later (if necessary)
names(mean.rasters.raw) <- c("av.size", "rain.m", "rain.v", "pop.dense", "slp", 
                         "gs.l", "elev", "soil.c.5", "soil.c.15")
stack(mean.rasters.raw) # test: throws error if raster extents are innacurate

# create a list of rasters to be aggregated using "sum" function
sum.rasters.raw <- list(ca.stack, hhs, k.con, n.con, p.con, pop, land_area)

# set names for easy reference later (if necessary)
names(sum.rasters.raw) <- c("ca.stack", "hhs", "k.con", "n.con", "p.con", "pop", 
                        "land_area")
stack(sum.rasters.raw) # test: throws error if raster extents are innacurate

# TODO: create data using rpu.des for urban vs. peri pop; figure out lc agg
other.rasters.raw <- list("rpu.des" = rpu.des, "lc" = lc)
stack(other.rasters.raw) # test: throws error if raster extents are innacurate

# create a sequence for loop below that specificies resolutions to be created
res.reqd <- c(seq(5, 80, by = 5)) # NB: sub-directories must be made 4 each res

#### aggregate data rasters to each of the res's specified above, write to disk ####

for(i in res.reqd) {
# aggreate all rasters that use "mean" to aggregate, then write to disk
    mean.agrasters <- lapply(mean.rasters.raw, aggregate, fact = i, 
                             fun = mean, na.rm = TRUE)
    mean.agrasters <- stack(mean.agrasters)
    writeRaster(mean.agrasters, 
              paste(dd, "resRasters", paste(i, "km/mean", sep = ""), sep = "/"),
              format = "GTiff", bylayer = TRUE, suffix = "names")
    
# aggreate all rasters that use "sum" to aggregate, then write to disk
    sum.agrasters <- lapply(sum.rasters.raw, aggregate, fact = i, 
                            fun = sum, na.rm = TRUE)
    sum.agrasters <- stack(sum.agrasters)
    writeRaster(sum.agrasters,
               paste(dd, "resRasters", paste(i, "km/sum", sep = ""), sep = "/"),
               format = "GTiff", bylayer = TRUE, suffix = "names")
    
    # stack all rasters together for translation into matrix
    all.agrasters <- stack(sum.agrasters, mean.agrasters)
    
#### create a data matrix to hold all values for every layer. ####
#### Rows = cells, Columns = layers                                                      
    
    # create variables for matrix dimensions
    nc <- ncell(all.agrasters) # number of rows in matrix
    nl <- nlayers(all.agrasters) # number of cells in matrix
    
    # create an empty matrix where the number of cells == nlayers x ncells/layer
    # write to disk to dedicate space and avoid working in memory. "ff" package
    # allows for writing of large matrixes (5km res is 2.11 GB for some reason)
    dat.mat <- ff(vmode="double", dim=c(nc,nl),
                file=paste(paste(dd, "resRasters", paste(i, "km", sep = ""), 
                                     sep = "/"), "dat_mat.ffdata", sep = "/"))
   
    # fill the empty matrix layer by layer, column by column
    # each row is a cell, each layer is a column. Now by querying a row, you get 
    # the value for each layer for the specified row. Then write to disk
    for(j in 1:nl){
        dat.mat[,j] <- all.agrasters[[j]][]
    }
    save(dat.mat,file=paste(paste(dd, "resRasters", paste(i, "km", sep = ""), 
                                  sep = "/"), "dat_mat.RData", sep = "/"))
    
    # Create ID rasters for each resolution that will allow us to identify which
    # matrix cells to query for T/F tests
    ID_raster <- raster(all.agrasters[[1]])
    ID_ratster[] <- 1:nc # raster where each cell value == cell index
    
}

    
