#### BASIC INFO ####
# boxes server script
# last edited: 24 jul 2016 (jy)

#### SET-UP #### 
## clear environment and console
rm(list = ls()); cat("\014")

## specify data directory 
dd <- "data"; bpath <- paste (dd, "borders", sep = "/")

## load data needed for initial mapping, etc. 
# min/max specifications for each filter/resolution combo
f.range <- read.csv(paste(dd, "filterRanges_iqr_bk.csv", sep = "/"), header = T)

# border shapefile for KE, TZ, RW, BI (needed for crpDat)
coreOAF <- readOGR(dsn = bpath, layer = "OAF_core")

#### FUNCTIONS ####

## generate list of data file or filter names from user input
# options for settings: "load.data", "make.filters", or "call.filters"
getNames <- function(inputs, setting) {
    if(setting %in% c("load.data", "make.filters", "call.filters") == F) {
        stop("Whoopsies - please adjust the setting to either 'load.data', 
          'make.filters', or 'call.filters'.")
    } else {
        toLoad <- inputs %>% base::unique()
        if("mean_rm" %in% toLoad) {
            if(setting == "make.filters") { addRain <- NA } 
            else if(setting == "call.filters") { 
                addRain <- c("rain_mth", "rain_mm", "rain_vol")
            } else if(setting == "load.data") {
                # rain will be handled separately within server since we want
                # to only load data by month, instead of all at once
                addRain <- NA
            }
            toLoad <- toLoad[-which(toLoad == "mean_rm")]
            toLoad <- c(toLoad, addRain)
        }
        if("Fertilizer consumption" %in% toLoad) {
            if(setting == "make.filters") { addFert <- NA } 
            else if(setting == "call.filters") { 
                addFert <- c("fert_n", "fert_p", "fert_k")
            } else if(setting == "load.data") {
                addFert <- c("sum_p.con", "sum_n.con", "sum_k.con")
            }
            toLoad <- toLoad[-which(toLoad == "Fertilizer consumption")]
            toLoad <- c(toLoad, addFert)
        }
        if("mean_soil.c." %in% toLoad) {
            if(setting == "make.filters") { addSoilC <- NA } 
            else if(setting == "call.filters") { 
                addSoilC <- c("soil_c_5", "soil_c_15")
            } else if(setting == "load.data") {
                addSoilC <- paste0("mean_soil.c.", c(5, 15))
            }
            toLoad <- toLoad[-which(toLoad == "mean_soil.c.")]
            toLoad <- c(toLoad, addSoilC)
        }
        # TO DO: add conditional adjustments for N soil fertility
        
        # remove any NAs
        toLoad <- toLoad[!is.na(toLoad)]
        return(toLoad)
    }
}

## Load the data and puts in a list (for easy access and use)
loadDat <- function(path, toLoad, bdr) {
    # set up progress bar
    withProgress(message = "Yay!", value = 0, {
        # initialize a blank list prior to for loop
        a <- list()
        # for each file name specified... 
        for (i in 1:length(toLoad)) {
            # load the data from the given path
            r <- raster(paste(path, paste0(toLoad[i], ".tif"), sep = "/"))
            # crop the data according to a selected extent
            r <- crpDat(r, bdr)
            # plop the resultant cropped data into the list
            a[[length(a) + 1]] <- assign(toLoad[i],r)
            # increment progress bar
            incProgress(1/length(toLoad), 
                detail = paste(i, "sets loaded out of", length(toLoad)))
        }
    })
    return (a)
}

## Function that crops data to chosen region, and returns a new list
# Takes in a default border but possible to specify border when calling
crpDat <- function(a, bdr = coreOAF) {
    r <- crop(a, bdr)
    r <- mask(r, bdr)
    return(r)
}

## Function to generate filter ranges for the manually generated filters:
getRange <- function (nm, res) {
    # Get the min
    gmin <- f.range$min.act[which(f.range$res == res &
            f.range$var == paste0(nm, ".tif"))] %>% round(digits = 0)
    # Get the max
    gmax <- f.range$max.act[which(f.range$res == res &
            f.range$var == paste0(nm, ".tif"))] %>% round(digits = 0)
    # Return the min, max and upper selected value
    rt <- c(gmin, gmax)
    return (rt)
}

## Function that takes in a raster and filters and returns an equivalent size
# raster with 1s and NAs
getBool <- function(rast, rastfil) {
    rast$filt <- (rast >= rastfil[[1]] & rast <= rastfil[[2]])
    rast$filt[rast$filt == T] <- 1
    rast$filt[rast$filt == F] <- NA
    return(rast$filt)
}

## Function that combines 1/NA rasters for selected datasets to one for painting
getStack <- function(rastlist, rastfil) {
    bools <- list()
    for (i in 1:length(rastlist)){
        x <- getBool(rastlist[[i]], rastfil[[i]])
        bools[[length(bools) + 1]] <- x
    }
    bool.1 <- stack(bools)
    bool.1 <- calc(bool.1, fun = prod)
    return(bool.1)
}

#### DEFINE CHOICES FOR DATA SELECTION INPUTS ####
# note that bundles are not included here; they are coded instead within the
# selectInput generation
core.data.choices <- c(
    "Rainfall (mean monthly, mm; and volatility)" = "mean_rm", 
    "Population (total)" = "sum_pop",
    "Population (density)" = "mean_pop.dense",
    "Est. avg. farm size" = "mean_av.size",
    "Land use indicators*"
) 
crop.data.choices <- c(
    "Crop mix (% of cultivated area)*",
    "Yield gaps*",
    "Months of growing season" = "mean_gs.l",
    "Hybrid seed adoption*",
    "Fertilizer consumption",
    "Fertilizer application rates*"
)

other.data.choices <- c( 
    "Soil fertility (nitrogen g/kg)*" = "mean_soil.n.",
    "Soil fertility (carbon ppm)" = "mean_soil.c.", 
    "Elevation" = "mean_elev", 
    "Slope" = "mean_slp",
    "Land cover*"
)

#### SERVER ####
shinyServer(function(input, output, session) {
    
    # Generate "all data" widgets in server to allow dynamic bundling
    getAllData <- reactive({ 
        dList <- vector("list", 3)
        dList[[1]] <- checkboxGroupInput("core.data",
            label = em("Core program indicators"),
            choices = core.data.choices,
            if("New Country Expansion" %in% input$team.data) {
                selected = core.data.choices[c(
                    grep("mean_rm", x = core.data.choices, value = F),
                    grep("mean_pop.dense", x = core.data.choices, value = F),
                    grep("mean_av.size", x = core.data.choices, value = F))]
            } else if("Core program indicators" %in% input$topical.data) {
                selected = core.data.choices
            }
        )
        dList[[2]] <- checkboxGroupInput("crop.data",
            label = em("Crop data"),
            choices = crop.data.choices,
            if("Frontiers" %in% input$team.data) {
                # TO DO: adjust grep for crop mix, hybrid seed adoption, and 
                # yield gaps when data included
                selected = crop.data.choices[c(
                    grep("Fertilizer consumption", x = crop.data.choices, 
                        value = F),
                    grep("Crop mix", x = crop.data.choices, value = F),
                    grep("Hybrid seed", x = crop.data.choices, value = F),
                    grep("Yield gaps", x = crop.data.choices, value = F))]
            } else if("New Country Expansion" %in% input$team.data) { 
                # TO DO: adjust grep for crop mix when data included
                selected = crop.data.choices[c(
                    grep("Crop mix", x = crop.data.choices, value = F))]
            } else if("Crop data" %in% input$topical.data) {
                selected = crop.data.choices
            }
        )
        dList[[3]] <- checkboxGroupInput("other.data",
            label = em("Other data"),
            choices = other.data.choices,
            if("Soil fertility data" %in% input$topical.data &
                    "Topography data" %in% input$topical.data) {
                selected = other.data.choices
            } else if("Topography data" %in% input$topical.data & 
                    !("Soil fertility data" %in% input$topical.data)) {
                selected = other.data.choices[c(
                    grep("slp", x = other.data.choices, value = F),
                    grep("elev", x = other.data.choices, value = F),
                    # TO DO: adjust grep for land cover when data is available
                    grep("Land cover*", x = other.data.choices, value = F))]
            } else if("Soil fertility data" %in% input$topical.data &
                    !("Topography data" %in% input$topical.data)) {
                selected = other.data.choices[grep(
                    pattern = "soil", x = other.data.choices, value = F)]
            }
        )
        return(dList)
    }) 
    
    ## Render 'all-data' widgets
    output$select_all_data <- renderUI({ getAllData() })
    
    ## define a reactive expression of all data selections
    allData <- reactive({ 
        c(input$core.data, input$crop.data, input$other.data)
    })
    
    ## auto-generate *most* filters based on the datasets selected
    getAutoFilts <- reactive({
        # store resolution for future reference
        res <- input$res
        
        # get a list of data to auto-generate sliders for
        # note that getNames already takes care of removing rainfall, soil,
        # and fertilizer consumption (the special cases that need separate
        # filter generation)
        fNames <- getNames(inputs = allData(), setting = "make.filters")
        
        # only continue if fNames is not empty
        if(length(fNames) > 0) {
            # initialize list to input ranges
            fList <- vector("list", length(fNames))
            
            for (i in 1:length(fNames)) {
                # Get the min and max values for filter
                fRange <- getRange(fNames[i], res)
                
                # Create filter and add to list
                fList[[i]] <- list(sliderInput(fNames[i], label = fNames[i],
                    value = fRange, min = fRange[1], max = fRange[2]))
            }
        } else {
            fList <- NULL
        }
        return(fList) 
    })
    
    ## render auto-generated filters
    output$filters_auto <-  renderUI({ getAutoFilts() })
    
    ## generate rainfall filters
    # first, need to create widget for mth input, which will provide
    #       mth section needed to build other rain inputs
    getRainFilts1 <- reactive({ 
        # only do this if rainfall data is selected
        if("mean_rm" %in% input$core.data) {
            # store resolution for future reference
            res <- input$res
            # create filter to select month
            fList <- selectInput("rain_mth",
                label = "Rainfall (select month)", multiple = F, 
                choices = mths, selected = "jan"
            )
            return(fList)
        } else {
            NULL
        }
    })
    getRainFilts2 <- reactive({
        # only do this once rainfall month has been selected
        if(!is.null(input$rain_mth)) {
            # initialize list to hold rainfall filters
            fList <- vector("list", 2)
            # get resolution
            res <- input$res
            # create filter for mm of rain, based on mth selected
            nmM <- paste0("mean_rm_", input$rain_mth)
            mmRange <- getRange(nmM, res)
            fList[[1]] <- sliderInput("rain_mm", 
                label = "Rainfall (mm/selected month)",
                value = mmRange, 
                min = mmRange[1], max = mmRange[2])
            # create filter for volatility of rain, based on mth selected
            nmVol <- paste0("mean_rvol_", input$rain_mth)
            volRange <- getRange(nmVol, res)
            fList[[2]] <- sliderInput("rain_vol", 
                label = "Rainfall (volatility)",
                value = volRange, 
                min = volRange[1], max = volRange[2])
            return(fList)
        } else {
            NULL
        }
    })
    
    # render rainfall filters
    output$filters_rain1 <- renderUI({ getRainFilts1() })
    output$filters_rain2 <- renderUI({ getRainFilts2() })
    
    ## generate fertilizer consumption filters if datasets selected
    getFertConsFilts <- reactive({
        # get resolution
        res <- input$res
        # initialize list to hold filters
        fList <- vector("list", 2)
        # create filters
        if("Fertilizer consumption" %in% input$crop.data) {
            # n
            rgN <- getRange("sum_n.con", res)
            fList[[1]] <- sliderInput(inputId = "fert_n", 
                label = "Fertilizer consumption (nitrogen, kg)",
                min = rgN[1], max = rgN[2], value = rgN)
            # p
            rgP <- getRange("sum_p.con", res)
            fList[[2]] <- sliderInput(inputId = "fert_p", 
                label = "Fertilizer consumption (phosphorus, kg)",
                min = rgP[1], max = rgP[2], value = rgP)
            # k
            rgK <- getRange("sum_k.con", res)
            fList[[3]] <- sliderInput(inputId = "fert_k", 
                label = "Fertilizer consumption (potassium, kg)",
                min = rgK[1], max = rgK[2], value = rgK)
            # Return the filters:
            return(fList)
        } else {
            NULL
        }
    })
    
    ## render fertilizer consumption filters
    output$filters_fert_comp <- renderUI({ getFertConsFilts() })
    
    ## Generate soil fertility (carbon) filters if datasets selected    
    getSoilCFilts <- reactive({
        # Save resolution:
        res <- input$res
        # initialize list to hold filters
        fList <- vector("list", 2)
        # Now get the filters
        if("mean_soil.c." %in% input$other.data) {
            # Get the 5cm Carbon soil fertility filter 
            rg5 <- getRange("mean_soil.c.5", res)
            fList[[1]] <- sliderInput(inputId = "soil_c_5", 
                label = "Soil fertility (ppm carbon at 5cm)",
                min = rg5[1], max = rg5[2], value = rg5)
            
            # Get the 15 cm Carbon soil fertility filter
            rg15 <- getRange("mean_soil.c.15", res)
            fList[[2]] <- sliderInput(inputId = "soil_c_15", 
                label = "Soil fertility (ppm carbon at 15cm)",
                min = rg15[1], max = rg15[2], value = rg15)
            
            # Return the filters:
            return(fList)
        } else {
            NULL
        }
    })
    
    ## render soil fertility (carbon) filters
    output$filters_soil_c <- renderUI({ getSoilCFilts() })
    
    ## TO DO: generate N soil fertility filters if datasets selected  
    ## TO DO: generate population type filters if datasets selected
    
    ## load selected geographic area for border/cropping
    loadedBdr <- reactive({
        bdrLayer <- input$geo
        bdr <- readOGR(dsn = bpath, layer = bdrLayer)
        return(bdr)
    })

    ## define function to load selected data *except* rainfall data:
    loadedDat <- reactive({
        withProgress(message = "Loading data...", value = 0,
            detail = ifelse("mean_rm" %in% allData(), 
                "skipping rainfall for now", ""), {
            # specify resolution sub-directory and borders based on user input
            pth <- paste(dd, input$res, sep = "/")
            bdr <- loadedBdr()
            incProgress(0.2, detail = "setting your boundaries")
            
            # create vector of unique data set names to load
            toLoad <- getNames(inputs = allData(), setting = "load.data")
            incProgress(0.2, detail = "grabbing your files")
            
            # load data sets that are cropped to chosen border
            loadedDat <- loadDat(pth, toLoad, bdr)
            incProgress(0.6, detail = "woot - all data loaded!")
            return(loadedDat)
        })
    }) 
    
    ## after submit button is hit, load data *except* rainfall data
    # first initiate an empty reactive object
    getDat <- reactiveValues()
    # make loading data dependent on the submit.data action button
    observeEvent(input$submit.data, { 
        # create alert if user tries moving onto Step 2 before selecting data
        loadUs <- allData()
        if(is.null(loadUs)) {
            createAlert(session, anchorId = "dataAlert", alertId = "whoops",
                title = "Whoops!",
                content = "You need to select the data you want to visualize",
                append = F)
        } else if(length(loadUs) == 1 & ("mean_rm" %in% loadUs)) {
            closeAlert(session, "whoops")  
        } else {
            # otherwise, load selected data and store in reactive object
            # show an alert to calm people about the loading time
            closeAlert(session, "whoops")
            createAlert(session, anchorId = "loadingAlert", 
                alertId = "wondering",
                title = "Wondering why nothing seems to be happening?",
                content = "Fear not! We are loading your data sets; you can 
                check on the progress in the top-right corner. \n 
                Thanks for your patience!",
                append = F)
            getDat$loadedDat <- loadedDat()
            closeAlert(session, "wondering")
        }
    })
    
    ## after submit button is hit, collapse data panel 
    observeEvent(input$submit.data, {
        if(!is.null(allData())) {
            updateCollapse(session, id = "collapse.steps",
                close = panel1, open = panel2)
        }
    })
    
    ## define function to load selected month's rainfall data if necessary
    loadedRain <- reactive({
        if(!is.null(input$rain_mth)) {
            withProgress(message = "Loading more data...", value = 0,
                detail = "now let's get rainfall", {
                    # specify resolution sub-directory and border based on user input
                    pth <- paste(dd, input$res, sep = "/")
                    bdr <- loadedBdr()
                    incProgress(0.2, detail = "setting your boundaries")
                    
                    # construct file names
                    toLoad <- c(
                        paste0("mean_rm_",input$rain_mth),
                        paste0("mean_rvol_", input$rain_mth)
                    )
                    incProgress(0.2, 
                        detail = paste("grabbing", input$rain_mth, "files"))
                    
                    # load data sets that are cropped to chosen border
                    loadedDat <- loadDat(pth, toLoad, bdr)
                    incProgress(0.6, detail = "woot - all data loaded!")
                    return(loadedDat)
                })
        }
    }) 
    observeEvent(input$refresh.map, { 
        # show an alert to calm people about loading rain time
        if(!is.null(input$rain_mth)) {
            createAlert(session, anchorId = "loadingRainAlert", 
                alertId = "wonderingRain",
                title = "Wondering why nothing seems to be happening?",
                content = "We are loading your rainfall data now that you've
                chosen a month. Don't fear - should only take a couple seconds!",
                append = F)
            getDat$loadedRain <- loadedRain()
            closeAlert(session, "wonderingRain")
        }
    })
    
    ## Get input for each filter and use input to combine rasters and return a
    # single 1/NA raster for painting:
    getMerged <- reactive ({
        
        # Get selected datasets
        allDat <- allData()
        
        # Get list of ids of server-generated filters
        fId <- getNames(inputs = allDat, setting = "call.filters")
        # remove rain_mth if rainfall is selected
        if("rain_mth" %in% fId) { fId <- fId[-which(fId == "rain_mth")] }
        
        # do this only if data has been selected and loaded
        if(length(fId) != 0) {
            # Get the inputs of all filters
            fList <- vector("list", length(fId))
            for(i in 1:length(fId)) {
                id <- fId[i]
                fList[[i]] <- input[[id]]
            }
        }
        
        # Get selected datasets
        # if user only selected rainfall
        if(length(allDat) == 1 & ("mean_rm" %in% allDat)) {
            dt <- getDat$loadedRain
        } else {
            # if user selected another dataset
            dt <- getDat$loadedDat
            # if user selected another dataset and rainfall
            if("mean_rm" %in% allDat) {
                dt <- c(dt, getDat$loadedRain)
            }    
        }
        
        # Create stack
        str(dt)
        mgd <- getStack(dt, fList)
        return(mgd)
    })
    
    ## Event observer for changes to filters ranges and refreshing map:
    # when filters change, it's only the code under getMerged that runs
    gtMgd <- eventReactive(input$refresh.map, { getMerged() })
    
    ## Paint merged raster to show boxes
    observeEvent(input$refresh.map, {
        output$map <- renderLeaflet({
            # grab the merged raster layer to plot
            spMerged <- gtMgd()[[1]]
            
            makeMap <- leaflet() %>% addTiles() 
            
            # assuming not everything has been filtered out, add raster layer
            if(spMerged@data@min != Inf) {
                
                # Only passing in one color instead of a palette
                pal1 <- colorNumeric("#006400", domain = values(spMerged),
                    na.color =  "#00000000")
                
                makeMap <- makeMap %>%
                    addRasterImage(spMerged, opacity = 0.5, colors = pal1, 
                        project = T)
            }
            return(makeMap)
        })
    })
    
    ## WIP: Get downloadable data for the selected datasets
    #   downDat <- reactive ({
    #     # First get the loaded datasets:
    #     dt <- getDat()
    #    
    #     ## TO-DO: Add function for loading a border and using it to extract the data
    #     ## TO-DO: add progress bar
    #     
    #     # Then stack the rasters:
    #     dt <- stack(dt)
    #     
    #     # Now extract data based on the selected geo:
    #     ext <- extract(dt, coreOAF)
    #     #write.csv(ext, "Downlaoded_data.csv", header = T)
    #     print("________________________");print(nrow(ext[[1]]))
    #     
    #     # Return the data:
    #     return(ext)
    #     
    #   })
    #   
    #   # Event handler for donloading the data:
    #   output$download.data <- downloadHandler(
    #     filename = function () {
    #       paste0("OAF_Geoboxes_data_", Sys.Date(), ".csv")
    #     },
    #     content = function (file = downDat()){
    #       write.csv(file)
    #     }
    #   )
    
}) # Server input/output ends here

# rsconnect::deployApp("~/drive/Boxes_2.0/Code files", appName = "Boxes", account = "oneacrefund")
