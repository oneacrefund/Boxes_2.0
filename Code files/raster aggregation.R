# raster aggregation and value vector 
# created by: colin custer (colin.custer@oneacrefund.org)
# created for: Boxes 2.0 (https://github.com/ccc5vd/Boxes_2.0)
# date created: 5 Apr 2016
# date modified: 5 Apr 2016
# description: aggregate all data rasters to specificed resolutions
#              then return a matrix of values for each resolution. In this
#              matrix, each row is a cell, each column a layer. Then for each 
#              cell, you will be able to return a numeric vector of values for 
#              each layer, which can be evaluated based on filter criteria

rm(list = ls())

# set up
load(paste(dd, "cleanData.RData", sep = "/"))
wd <- "~/drive/Boxes_2.0/Shiny"
cd <- paste("~/drive/Boxes_2.0/Code files", sep = "/")
dd <- paste(wd, "data", sep = "/")

mean.rasters.raw <- list(av.size, rain.m, rain.v, pop.dense, slp, gs.l, elev, 
                     soil.c.5, soil.c.15)
names(mean.rasters.raw) <- c("av.size", "rain.m", "rain.v", "pop.dense", "slp", 
                         "gs.l", "elev", "soil.c.5", "soil.c.15")

sum.rasters.raw <- list(ca.stack, hhs, k.con, n.con, p.con, pop, land_area)
names(sum.rasters.raw) <- c("ca.stack", "hhs", "k.con", "n.con", "p.con", "pop", 
                        "land_area")

other.rasters.raw <- list("rpu.des" = rpu.des, "lc" = lc) # TODO

res.reqd <- c(seq(5, 80, by = 5))

for(i in res.reqd) {
    mean.agrasters <- lapply(mean.rasters.raw, aggregate, fact = i, 
                             fun = mean, na.rm = TRUE)
    mean.agrasters <- stack(mean.agrasters)
    writeRaster(mean.agrasters, 
                paste(dd, "resRasters", paste(i, "km/mean", sep = ""), sep = "/"),
                format = "GTiff", bylayer = TRUE, suffix = "names")
    
    mean.mat <- ff(vmode="double",dim=c(ncell(mean.agrasters),nlayers(mean.agrasters)),
                    filename=paste(paste(dd, "resRasters", paste(i, "km", sep = ""), 
                                         sep = "/"), "mean_mat.ffdata", sep = "/"))
    
    for(j in 1:nlayers(mean.agrasters)){
        mean.mat[,j] <- mean.agrasters[[j]][]
    }
    save(mean.mat,file=paste(paste(dd, "resRasters", paste(i, "km", sep = ""), 
                                   sep = "/"), "mean_mat.ffdata", sep = "/"))
    
    sum.agrasters <- lapply(sum.rasters.raw, aggregate, fact = i, 
                            fun = sum, na.rm = TRUE)
    sum.agrasters <- stack(sum.agrasters)
    writeRaster(sum.agrasters,
                paste(dd, "resRasters", paste(i, "km/sum", sep = ""), sep = "/"),
                format = "GTiff", byLayer = TRUE, suffix = "names")
    sum.mat <- ff(vmode="double",dim=c(ncell(sum.agrasters),nlayers(sum.agrasters)),
                   filename=paste(paste(dd, "resRasters", paste(i, "km", sep = ""), 
                                        sep = "/"), "sum_mat.ffdata", sep = "/"))
    
    for(j in 1:nlayers(sum.agrasters)){
        sum.mat[,j] <- sum.agrasters[[j]][]
    }
    save(sum.mat,file=paste(paste(dd, "resRasters", paste(i, "km", sep = ""), 
                                  sep = "/"), "sum_mat.ffdata", sep = "/"))

    }
