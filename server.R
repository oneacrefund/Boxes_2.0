#### BASIC INFO ####
# boxes server script
# last edited: 14 jul 2016 (bk)

#### SET-UP #### 
## clear environment and console
rm(list = ls()); cat("\014")

## specifiy data directory 

dd <- "data"; bpath <- paste (dd, "borders", sep = "/")

## load data needed for initial mapping, etc. 
# min/max specifications for each filter/resolution combo
f.range <- read.csv(paste(dd, "filterRanges_iqr.csv", sep = "/"), header = T)

# border shapefile for KE, TZ, RW, BI (needed for crpDat)
coreOAF <- readOGR(dsn = bpath, layer = "OAF_core")

#### FUNCTIONS ####

## generate list of data file or filter names from user input
# options for settings: "load.data", "make.filters", or "call.filters"
getNames <- function(inputs, setting) {
  # TO DO: add data validation for "setting" parameter
  toLoad <- inputs %>% base::unique()
  print("bbbbbbb"); print(toLoad)
  if("mean_rm" %in% toLoad) {
    if(setting == "make.filters") { addRain <- NA } 
    else if(setting == "call.filters") { 
      addRain <- c("rain_mth", "rain_mm", "rain_vol")
    } else if(setting == "load.data") {
      addRain <- c(paste0("mean_rm_", mths), paste0("mean_rvol_", mths))
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
  toLoad <- toLoad[!is.na(toLoad)]; print("+++++++++++++++++++"); print(toLoad)
  return(toLoad)
}

## Load the data and puts in a list (for easy access and use)
loadDat <- function(path, toLoad, bdr) {
  # set up progress bar
  withProgress(message = "Loading data...", value = 0, {
    # initialize a blank list prior to for loop
    a <- list()
    print("Starting data loading..."); print(length(a))
    # for each file name specified... 
    for (i in 1:length(toLoad)) {
      # load the data from the given path
      r <- raster(paste(path, paste0(toLoad[i], ".tif"), sep = "/"))
      # crop the data according to a selected extent
      r <- crpDat(r, bdr)
      #print("Here's raster"); print(r)
      # plop the resultant cropped data into the list
      a[[length(a) + 1]] <- assign(toLoad[i],r)
      print("So far loaded: "); print(length(a))
      # increment progress bar
      incProgress(1/i, 
        detail = paste(i, "sets loaded out of", length(toLoad)))
    }
  })
  print("Done! Loaded: "); print(length(a))
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
  gmin <-  f.range$min.threshold[which(f.range$res == res &
      f.range$var == paste0(nm, ".tif"))]
  # Get the max
  gmax <- f.range$max.threshold[which(f.range$res == res &
      f.range$var == paste0(nm, ".tif"))]
  # Round the values
  if (gmin < 0) {gmin = 0}
  gmax <- round(gmax, digits = 0)
  #Get an upper selected value (arbitrarily set to 25% of max)
  upper <- gmax * 0.25; upper <- round(upper, digits = 0)
  # Return the min, max and upper selected value
  rt <- c(gmin, gmax, upper)
  return (rt)
}

## Function that takes in a raster and filters and returns an equivalent size
# raster with 1s and NAs
getBool <- function(rast, rastfil) {
  print("rrrrrrrrr"); print(rastfil[[1]]); print(rastfil[[2]])
  rast$filt <- (rast >= rastfil[[1]] & rast <= rastfil[[2]])
  rast$filt[rast$filt == T] <- 1
  rast$filt[rast$filt == F] <- NA
  return(rast$filt)
}

## Function that combines 1/NA rasters for selected datasets to one for painting
getStack <- function(rastlist, rastfil) {
  pr <- paste(length(rastlist), "rasters to stack"); print(pr)
  pr.1 <- paste(length(rastfil), "filters to use"); print (pr.1)
  bools <- list()
  for (i in 1:length(rastlist)){
    x <- getBool(rastlist[[i]], rastfil[[i]])
    bools[[length(bools) + 1]] <- x
  }
  pr.3 <- paste(length(bools), "is the # of rasters to stack"); print(pr.3)
  bool.1 <- stack(bools)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("stacked "); print(length(bool.1))#; print(str(bool.1))
  bool.1 <- calc(bool.1, fun = prod)
  pr.2 <- paste(length(bool.1), "- length of stack list"); print(pr.2)
  return(bool.1)
}

## Defining choices for data selection inputs up front
core.data.choices <- c(
  "Rainfall (mean monthly, mm; and volatility)" = "mean_rm", 
  "Population (total)" = "sum_pop",
  "Population (density)" = "mean_pop.dense",
  "Est. avg. farm size" = "mean_av.size"
  #"Land use indicators*"
) 
crop.data.choices <- c(
  #"Crop mix (% of cultivated area)*",
  #"Yield gaps*",
  "Months of growing season" = "mean_gs.l",
  #"Hybrid seed adoption*",
  "Fertilizer consumption (Nitrogen)" = "sum_n.con",
  "Fertilizer consumption (Phosphorous)" = "sum_p.con",
  "Fertilizer consumption (Potassium)" = "sum_k.con"
  #"Fertilizer application rates*" 
)

other.data.choices <- c( 
  #(note for BK: consumption === fertilizer usage; fertility === NPK soil content)
 # "Soil fertility (nitrogen g/kg)*" = "mean_soil.n.", 
  "Soil fertility (carbon ppm)" = "mean_soil.c.", 
  "Elevation" = "mean_elev", 
  "Slope" = "mean_slp"
  # "Land cover*"  = "mean_lc" # bk: we may need to change this id when the data is actually generated
)

nce.data.choices <- c(
  "Rainfall (mean monthly, mm; and volatility)" = "mean_rm",
  "Population (density)" = "mean_pop.dense",
  "Est. avg. farm size" = "mean_av.size"
  #"Crop mix (% of cultivated area)*"
)

frontiers.data.choices <- c(
  "Fertilizer consumption (Nitrogen)" = "sum_n.con",
  "Fertilizer consumption (Phosphorous)" = "sum_p.con",
  "Fertilizer consumption (Potassium)" = "sum_k.con"
  #"Crop mix (% of cultivated area)*",
  #"Yield gaps*",
  #"Hybrid seed adoption*"
)

#### SERVER ####
shinyServer(function(input, output, session) {
  
  # Generate "all data" widgets in server to allow dynamic bundling# 
  getAllData <- reactive({ 
    dList <- vector("list", 3)
    dList[[1]] <- checkboxGroupInput("core.data",
      label = em("Core program indicators"),
      choices = core.data.choices,
      if("Core program indicators" %in% input$topical.data) {
        selected = core.data.choices
      }
    )
    dList[[2]] <- checkboxGroupInput("crop.data",
      label = em("Crop data"),
      choices = crop.data.choices,
      if("Crop data" %in% input$topical.data) {
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
          grep(pattern = "slp", x = other.data.choices, value = F),
          grep(pattern = "elev", x = other.data.choices, value = F),
          grep(pattern = "lc", x = other.data.choices, value = F))]
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
    
    # First remove rainfall and C consumption datasets since these have
    # their filters generated elsewhere:
    allDt <- allData()
    rm.1 <- c("mean_rm","mean_soil.c.")
    
    for(i in 1:length(rm.1)) {
      g.s <- rm.1[i]
      if(g.s %in% allDt) {
        x <- match(g.s, allDt)
        allDt <- allDt[-x]
      }
    }
    
    # Then get a list of data to auto-generate sliders for
    fNames <- getNames(inputs = allDt, setting = "make.filters")
    
    # only continue if fNames is not empty
    if(length(fNames) > 0) {
      # initialize list to input ranges
      fList <- vector("list", length(fNames))
      
      for (i in 1:length(fNames)) {
        # Get the min and max values for filter
        minRange <- round(f.range$min.threshold[which(f.range$res == res & 
            f.range$var == paste0(fNames[i], ".tif"))], digits = 0)
        maxRange <- round(f.range$max.threshold[which(f.range$res == res & 
            f.range$var == paste0(fNames[i], ".tif"))], digits = 0)
        # Create filter and add to list (currently steps are not specified)
        fList[[i]] <- list(sliderInput(fNames[i], label = fNames[i],
          value = c(minRange, maxRange), min = minRange, max = maxRange))
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
  ## TO DO: generate population type filters if datasets selected
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
      res <- input$res
      # create filter for mm of rain, based on mth selected
      rain_mm_min <- f.range$min.threshold[which(f.range$res == res & 
          f.range$var == paste0("mean_rm_", input$rain_mth, ".tif"))]
      rain_mm_min <- round(rain_mm_min, digits = 0)
      rain_mm_max <- f.range$max.threshold[which(f.range$res == res & 
          f.range$var == paste0("mean_rm_", input$rain_mth, ".tif"))]
      rain_mm_max <- round(rain_mm_max, digits = 0)
      fList[[1]] <- sliderInput("rain_mm", 
        label = "Rainfall (mm/selected month)",
        value = c(rain_mm_min, rain_mm_max), 
        min = rain_mm_min, max = rain_mm_max)
      # create filter for volatility of rain, based on mth selected
      rain_vol_min <- f.range$min.threshold[which(f.range$res == res & 
          f.range$var == paste0("mean_rvol_", input$rain_mth, ".tif"))]
      rain_vol_min <- round(rain_vol_min, digits = 0)
      rain_vol_max <- f.range$max.threshold[which(f.range$res == res & 
          f.range$var == paste0("mean_rvol_", input$rain_mth, ".tif"))]
      rain_vol_max <- round(rain_vol_max, digits = 0)
      fList[[2]] <- sliderInput("rain_vol", 
        label = "Rainfall (volatility)",
        value = c(rain_vol_min, rain_vol_max), 
        min = rain_vol_min, max = rain_vol_max)
      return(fList)
    } else {
      NULL
    }
  })
  
  # render rainfall filters
  output$filters_rain1 <- renderUI({ getRainFilts1() })
  output$filters_rain2 <- renderUI({ getRainFilts2() })
  
  ## Generate C soil fertility filters if datasets selected    
  getSoilCFilts <- reactive({
    ## Save resolution:
    res <- input$res
    
    ## Now get the filters
    if("mean_soil.c." %in% input$other.data) {
      # Get the 5cm Carbon soil fertility filter 
      rg1 <- getRange("mean_soil.c.5", res)
      x <- sliderInput(inputId = "soil_c_5", label = "Soil Carbon fertility - 5cm",
        min = rg1[1], max = rg1[2], value = c(0, rg1[3], step = NULL))
      
      # Get the 15 cm Carbon soil fertility filter
      rg2 <- getRange("mean_soil.c.15", res)
      y <- sliderInput(inputId = "soil_c_15", label = "Soil Carbon fertility - 15cm",
        min = rg2[1], max = rg2[2], value = c(0, rg2[3], step = NULL))
      
      # Return the filters:
      filtL <- list(x,y)
      return(filtL)
    }
    else(
      return (NULL)
    )
    
  })
  
  ## render soil fertility (carbon) filters
  output$filters_soil_c <- renderUI({ getSoilCFilts() })
  
  ## TO DO: generate N soil fertility filters if datasets selected  
  
  ## Get rainfall data names (called by loadedDat):
  getRainNames <- reactive ({
    # Initialize an empty vector
    rnDat <- vector()
    # Make sure rain data month selection is not null:
    if(!is.null(input$rain_mth)) {
      mm <- paste0("mean_rm_",input$rain_mth)
      vol <- paste0("mean_rvol_", input$rain_mth)
      rnDat[length(rnDat) + 1] <- mm
      rnDat[length(rnDat) + 1] <- vol
      print("Check rain data load"); print(rnDat)
      return(rnDat)  
    }
    # If null, return the auto-selected month (Jan)
    else {
      x <- c("mean_rm_jan", "mean_rvol_jan")
      return (x)
    }
  })
  
  ## Load selected data:+
  loadedDat <- reactive ({
    # specify resolution sub-directory based on user input
    pth <- paste(dd, input$res, sep = "/")
    
    # Initialize vectors for rainfall & Carbon consumption data
    # Idea is to only load rainfall data for selected month, and both of
    # the Carbon consumption datasets (5cm and 15cm)
    raind <- vector(); carbd <- vector()
    alld <- allData()
    
    # Check if rainfall data is selected and if so remove from this list and load
    # reactively depending on selected month:
    r.m <- "mean_rm" #Rainfall ID
    if(r.m %in% alld) {
      x <- match(r.m, alld) # Get location of rainfall ID
      alld <- alld[-x] # Then remove it
      raind <- getRainNames() # And then load the rainfall data reactively
    }
    
    # Also check for Carbon consumption data and load both 5cm and 15 cm:
    f.t <- "mean_soil.c." # Carbon consumption I
    
    if (f.t %in% alld) {
      x <- match(f.t, alld)  # Get location of rainfall ID in selected datasets
      alld <- alld[-x] # Then remove it
      carbd <- c("mean_soil.c.15", "mean_soil.c.5") # And then add both 5cm and 15cm IDs
    }
    
    # If rain data was selected, add to list:
    if(!is.null(raind))
      alld <- c(alld, raind, carbd)
    # create vector of unique data set names to load
    toLoad <- getNames(inputs = alld, setting = "load.data")
    #toLoad2 <- getNames(inputs = rm.1, setting = "load.data")
    #toLoad3 <- getNames(inputs = rvol.1, setting = "load.data")
    #toLoad4 <- list(toLoad, toLoad2, toLoad3)
    
    # load selected border
    bdrLayer <- input$geo
    bdr <- readOGR(dsn = bpath, layer = bdrLayer)
    
    # load data sets that are cropped to chosen border
    loadedDat <- loadDat(pth, toLoad, bdr)
    return(loadedDat)
  }) 
  
  ## load data and collapse data panel after actionButton is hit
  getDat <- eventReactive(input$submit.data, {
    dt <- loadedDat()
    return(dt)
  })
  
  observeEvent(input$submit.data, {
    updateCollapse(session, id = "collapse.steps",
      close = "Step 1: choose what you want to visualize",
      open = "Step 2: filter your map")
  })
  
  ## Get input for each filter and use input to combine rasters and return a
  # single 1/NA raster for painting:
  getMerged <- reactive ({
    
    # Get selected datasets
    allDat <- allData()
    # Remove rainfall and Carbon consumption filter IDs if selected, since these have
    # their filters generated elsewhere:
    
    rm.1 <- c("mean_rm", "mean_soil.c.") #What to look for
    otherFils <- vector() # Initialize an empty vector to add the extra filter IDs
    
    if(rm.1[1] %in% allDat) { # If rainfall data was selected
      x <- match(rm.1[1], allDat) 
      allDat <- allDat[-x] # Index and remove from allDat
      otherFils <- c("rain_mm", "rain_vol") # And add rainfall filter Ids to empty vector
    }
    
    if(rm.1[2] %in% allDat) { # Do a similar things for Carbon consumption
      x <- match(rm.1[2], allDat)
      allDat <- allDat[-x]
      carb <- c("soil_c_5", "soil_c_15")
      otherFils <- c(otherFils, carb)
    }
    
    # Test: 
    print("Test of additional filters: "); print(otherFils)
    
    # Get list of ids of server-generated filters
    fId <- getNames(inputs = allDat, setting = "call.filters")
    print("Here are the ids of the server-generated filters"); print(fId)
    
    # Now add IDs for rainfall filters and Carbon consumption filters if selected
    if(!is.null(otherFils)) {
      fId <- c(fId, otherFils)  
    }
    # do this only if data has been selected and loaded
    # TO DO: figure out why there's initially a subscript out of bounds error; doesn't seem to be breaking code so ignoring for now
    
    print(is.null(fId));print(length(fId))
    if(length(fId) != 0) {
      # Get the inputs of all filters
      fList <- vector("list", length(fId))
      for(i in 1:length(fId)) {
        id <- fId[i]
        fList[[i]] <- input[[id]]
        #print("Test loop: "); print(str(fList[[i]]))
      }
    }
    
    # Get selected datasets
    #dt <- loadedDat()
    dt <- getDat()
    print("is the data still loaded?"); print(length(dt))
    
    # Create stack
    mgd <- getStack(dt, fList)
    return(mgd)
  })
  
  ## Event observer for changes to filters ranges and refreshing map:
   # BK - note: when filters change it's only the code under getMerged that runs
gtMgd <- eventReactive(input$refresh.map, {
     getMerged()
   })
  
  ## Paint merged raster to show boxes
    # WIP (BK): trying to isolate wherever getMerged is called
  observeEvent(input$refresh.map, {output$map <- renderLeaflet({
    spMerged.1 <- gtMgd()
    spMerged <- spMerged.1[[1]]
    
    # Only passing in one color instead of a palette
    pal1 <- colorNumeric("#006400",domain = values(spMerged),
      na.color =  "#00000000")
    leaflet() %>% addTiles() %>%
      addRasterImage(spMerged, opacity = 0.5, colors = pal1, project = T)
  })
  }) # End of observeEvent here
  
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