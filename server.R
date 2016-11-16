#### BASIC INFO ####
# boxes server script
# last edited:  16 nov 2016 (bk)

#### SET-UP #### 
## clear environment and console
rm(list = ls()); cat("\014")

## specify data directory 
dd <- "data"; bpath <- paste (dd, "borders1", sep = "/")
bpath1 <- paste(dd, "AdminBoundaries1", sep = "/")
bpath3 <- paste(dd, "AdminBoundaries3", sep = "/")

## load data needed for initial mapping, etc. 
# min/max/label specifications for each filter/resolution combo
f.range <- read.csv(paste(dd, "filterRanges_iqr_bk.csv", sep = "/"), header = T,
 stringsAsFactors = F)

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
  #Yield gap data will also be handled separately;
  #load by selected crops instead of all at once (do the same for crop mix)
  if("_yieldgap" %in% toLoad){
    addYield <- NA
   toLoad <- toLoad[-which(toLoad == "_yieldgap")]
   toLoad <- c(toLoad, addYield)
  }
  if("Fertilizer consumption (kg/acre)" %in% toLoad) {
   if(setting == "make.filters") { addFert <- NA } 
   else if(setting == "call.filters") { 
    addFert <- c("fert_n", "fert_p", "fert_k")
   } else if(setting == "load.data") {
    addFert <- c("mean_p.con", "mean_n.con", "mean_k.con")
   }
   toLoad <- toLoad[-which(toLoad == "Fertilizer consumption (kg/acre)")]
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

## Load the data and save to a list (for easy access and use)
loadDat <- function(path, toLoad, bdr) {
 # set up progress bar
 withProgress(message = "Yay!", value = 0, {
  # initialize a blank list prior to for loop
  a <- list()
  # for each file name specified... 
  for (i in 1:length(toLoad)) {
   # load the data from the given path
   r <- raster(paste(path, paste0(toLoad[i], ".tif"), sep = "/")) #
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
#TO-DO?: only crop data when the rendering boxes so that we don't have to reload
#       data when only the resolution changes
crpDat <- function(a, bdr) {
 r <- crop(a, bdr, snap = "out")
 r <- mask(r, bdr)
 return(r)
}

## Function to generate filter ranges for the filters:
# Also gets the assigned names for the filters:
getRange <- function (nm, res) {
 # Get the min
 gmin <- f.range$min.act[which(f.range$res == res &
   f.range$var == paste0(nm, ".tif"))] %>% round(digits = 0)
 # Get the max
 gmax <- f.range$max1.act[which(f.range$res == res &
   f.range$var == paste0(nm, ".tif"))] %>% round(digits = 0)
 # Get the filter name
 fname <- f.range$filt.name[which(f.range$res == res &
   f.range$var == paste0(nm, ".tif"))] %>% as.character()
 # Return the min, max and filter name
 rt <- c(gmin, gmax, fname) #PS: the return vector is a character vector
 return (rt)
}

## load selected region for cropping:
getRegion <- function (reg) {
 # First we find the corresponding layer name:
 pos <- match(reg, ctName)
 reg <- ctISO[pos]
 # Then we load the border shapefile
 bdr <- readOGR(dsn = bpath, layer = reg)
 return(bdr)
}

## Load selected country border for cropping, if level of detail is 'Regional':
getRegion1 <- function (reg) {
 # First we find the corresponding ISO code:
 pos <- match(reg, ctName)
 reg1 <- ctISO[pos]
 # Then create a string matching the border name
 reg1 <- paste0(reg1, "_adm1.rds")
 # And finally load and return the border
 bdr <- readRDS(file = paste(bpath1, reg1, sep = "/"))
 return(bdr)
}

## Load selected country border for cropping, if level of detail is 'District':
getRegion2 <- function (reg) {
 # First we find the corresponding ISO code:
 pos <- match(reg, ctName)
 reg1 <- ctISO[pos]
 # Then create a string matching the border name
 reg1 <- paste0(reg1, "_adm2.rds")
 # And finally load and return the border
 bdr <- readRDS(file = paste(bpath2, reg1, sep = "/"))
 return(bdr)
}

## Load selected country border for cropping, if level of detail is 'District':
getRegion3 <- function (reg) {
 # First we find the corresponding ISO code:
 pos <- match(reg, ctName)
 reg1 <- ctISO[pos]
 # Then create a string matching the border name
 reg1 <- paste0(reg1, "_adm3.rds")
 # And finally load and return the border
 bdr <- readRDS(file = paste(bpath3, reg1, sep = "/"))
 return(bdr)
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

## Function that creates html elements for pop-ups for admin-level mapping:
#Takes in extracted data (df with IDs, names, and names of each dataset as in fRange)
getPops <- function (dt, namesDt) {
 n <- ncol(dt) - 1 #This eliminates the last col which was added for leaflet mapping
 #Placeholder NULLs for objects to assigned inside if-else statements
 rgns <- NULL; nms2 <- NULL
 
 #Select region names and dataset names separately
 # For ward-level data:
 if("NAME_3" %in% namesDt) {
  rgns <- dt[,2:5]
  dt <- dt[,6:n]
  nms2 <- c("Country", "State/Region", "District", "Ward/Sub-district")
 }
 #Select for region/district borders only if its not a level-3 shapefile
 else if(!("NAME_3" %in% namesDt)){
  if (("NAME_2" %in% namesDt)) {
   rgns <- dt[,2:4]
   dt <- dt[,5:n]
   nms2 <- c("Country", "State/Region", "District")
  }
  else {
   rgns <- dt[,2:3] 
   dt <- dt[,4:n]
   nms2 <- c("Country", "State/Region")
  }
 }
 
 ##Round data to nearest whole numbers:
 dt <- format(dt, digits = 0, big.mark = ",")
 print(head(dt))
 ##Get names for selected datasets:
 #Names are same at all res, so using 5km res for all cases
 nms <- names(dt)
 nms1 <- vector(length = length(nms)); print(nms)
 for(i in 1:length(nms)) {
  x <- nms[i]
  nms1[i] <- f.range$filt.name[which(f.range$var == paste0(x, ".tif") & 
    f.range$res == "5km")] %>% as.character()
  print(nms1[i])
 }
 
 ##Combine region and dataset names and apply to dt:
 nms <- c(nms2, nms1)
 dt <- cbind(rgns, dt)
 names(dt) <- nms
 
 ##Store names globally that we'll use to rename dataset columns before download
 
 #Add HTML and make pop-up pretty:
 v <- apply(dt, 1, function(y) {
  sprintf("<table width='100%%'>%s</table>",
   paste0("<tr><td style='text-align:left'><b>", names(y),
    ":</b></td><td style='text-align:right'>", y, 
    collapse = "</td></tr>"))
 })
 names(v) <- NULL
 return(v)
}

## Function that extracts data for districts mapping for selected detail level:
##Extracting and filtering data for admin boundaries mapping and download:
# Returns a df of extracted data:
getAdminDat <- function(bdr, dt, fList){
 # Get loaded data and extract values summarized as means for each polygon feature
 # sp2 is a df with an ID col, and a col for each dataset (raster in stack)
 # Rows in sp2 correspond to the polygon features (admin regions)
 sp2 <- extract(dt, bdr, method = "simple", fun = mean, df = T, na.rm = T)
 
 # Now we filter; retain value if within filter ranges; else assign an NA
 # First we remove the ID column and store it for later use:
 sp2Id <- sp2$ID; sp2 <- sp2[,-1] %>% as.data.frame()
 
 #Then filter
 for(i in 1:ncol(sp2)) {
  colN <- sp2[,i] %>% as.vector()
  for(j in 1:length(colN)) {
   #There are NaNs in our data, and they break the loop, so we make them NAs first
   if(!is.finite(colN[j])){
    colN[j] <- NA
   }
   #Then we filter then non NaN values by retaining vals within filter criteria
   else if(colN[j] >= fList[[i]][1] && colN[j] <= fList[[i]][2]){
    colN[j] <- colN[j]
   }
   #And set values that falling outside filter criteria to NAs
   else {
    colN[j] <- NA
   }
  }
  # Then we replace the old column in sp2 with the new column
  sp2[,i] <- colN
 }
 
 # Comparing columns of sp2:
 # If a row has at least 1 NA then new data object gets an NA
 # Else it gets a 1
 newDt <- NULL #new data column
 for(i in 1:nrow(sp2)) {
  rowN <- sp2[i,] %>% as.numeric() #subset each row
  if(NA %in% rowN) { #If the row has an NA
   newDt[length(newDt) + 1] <- NA #make new datapoint an NA
  }
  else {
   newDt[length(newDt) + 1] <- 1 #else make new datapoint a 1
  }
 }
 
 #Add the new data column, newDt, and the ID column back to sp2 and return sp2
 sp2$DATA <- newDt
 sp2$ID <- sp2Id
 return(sp2)
 
}

#### DEFINE CHOICES FOR DATA SELECTION INPUTS ####
# note that bundles are not included here; they are coded instead within the
# selectInput generation
core.data.choices <- c(
 "Mean annual rainfall (mm)" = "mean_ann_rainf",
 "Monthly rainfall (mean and volatility)" = "mean_rm",
 "Population (total)" = "sum_pop",
 "Rural population (density, pp/sq.km)" = "mean_r.pop.dense",
 "Est. avg. farm size (acre/hh)" = "mean_av.size"
 #"Land use indicators*"
) 
crop.data.choices <- c(
 "Crop mix (% of cultivated area)*",
 "Yield gaps (ton/ha)" = "_yieldgap",
 "Months of growing season" = "mean_gs.l",
 "Hybrid seed adoption (%)*",
 "Fertilizer consumption (kg/acre)", 
 "Fertilizer application rates*" # TO DO: add units
)

other.data.choices <- c( 
 "Soil fertility (nitrogen g/kg)" = "mean_soil.n", 
 "Soil fertility (carbon ppm)" = "mean_soil.c.", 
 "Elevation (m)" = "mean_elev",
 "Slope (degree)" = "mean_slp" 
 #"Land cover*" # TO DO: add units
)

#### SERVER ####
shinyServer(function(input, output, session) {
 
 # Generate "all data" widgets in server to allow dynamic bundling
 getAllData <- reactive({ 
  dList <- vector("list", 3)
  dList[[1]] <- tags$div(title = "Core program data - metadata: \n
   - Rainfall: averages calculated from World Bank-provided 1990 - 2014 historical data at 50km x 50km resolution; Rain volatility is tentatively calculated as the # of months when rain fall below 70% of the mean in the given month \n
   - Population (total and density): 2010 data from WorldPOP project; Aggregated up from 1km x 1km resolution and divided by area to get density \n
   - Est avg farm size: data provided by NCE \n
   - Land use indicators: TBD",
   checkboxGroupInput("core.data",
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
    else {
     #selected = c("mean_ann_rainf", "mean_r.pop.dense") # TO-DO: remove this
    }
   ))
  dList[[2]] <- tags$div(title = "Crop data - metadata: \n 
   - Yield gaps (tons/hectare): estimates from the Global Landscape Initiative, earthstat.org; Represents average difference between observed yields and a yield potential based on what other farmers growing that crop in areas of similar climate have achieved; Aggregated from 10km x 10km resolution data \n 
   - Length of growing seasons (months): from FAO; 2010 data based on climatological observations aggregated from 10km x 10k resolution \n
   - Fertilizer application rate and total consumption: estimates from the Global Landscape Initiative, earthstat.org; Aggregated from 10km x 10km resolution data \n 
   ",
   checkboxGroupInput("crop.data",
    label = em("Crop data"),
    choices = crop.data.choices,
    if("Frontiers" %in% input$team.data) {
     # TO DO: adjust grep for crop mix, hybrid seed adoption, and 
     # yield gaps when data included
     selected = crop.data.choices[c(
      grep("Fertilizer consumption (kg/acre)", x = crop.data.choices, 
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
   ))
  dList[[3]] <- tags$div(title = "Other data - metadata: \n
   - Soil fertility (nitrogen): aggregated from 10km x 10km historical data (1950-2012) data from International Soil Reference and Information Center \n
   - Soil fertility (carbon): aggregated from 250m x 250m historical data (1960-2010) data from International Soil Reference and Information Center \n
   - Elevation: aggregated from 900m x 900m data from World Wildlife Fund, 2010 \n
   - Slope: aggregated from 10km x 10km data from FAO, 2010 \n
   - Land cover: aggregated from 10km x 10km data from Natural Earth Data, 2010",
   checkboxGroupInput("other.data",
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
   ))
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
    mn <- as.numeric(fRange[1])
    mx <- as.numeric(fRange[2])
    vl <- c(mn, mx)
    fList[[i]] <- list(sliderInput(fNames[i], label = fRange[3],
     value = vl, min = mn, max = mx, step = 0.01))
   }
  } else {
   fList <- NULL
  }
  return(fList) 
 })
 
 output$filters_auto <-  renderUI({ getAutoFilts() })
 
 ## Generate yield gap filters:
 #Step 1: generate checkboxgroupinput for selecting datasets of choice:
 getYieldOpts <- reactive({
  opts <- NULL
  msg <- NULL
  if("_yieldgap" %in% input$crop.data){
   msg <- em("Before we set your map type, I noticed that you selected yield
   gaps as one of your datasets. Please choose the crops whose yield gaps
    you wish to see from the options below:")
   opts <- checkboxGroupInput("yield_group", label = "", choices = cropsYield,
    selected = "maize", width = "100%")
  }
  rt <- list(msg, opts)
  return(rt)
 })
 
 output$yield_choices <- renderUI({ getYieldOpts() })
 #Step 2: generate filters for the selected crops:
 getYieldFilts <- reactive({
  fLst <- NULL
  if("_yieldgap" %in% input$crop.data) {
   #Get res and selected crops
   res <- input$res
   yieldDat <- input$yield_group
   #Create names matching names of raster files on files:
   yieldDat <- paste0(yieldDat, "_yieldgap")
   print(yieldDat)
   #Get labels and filter ranges then create a filter for each selected crop:
   if(length(yieldDat > 0)) {
    fLst <- vector("list", length(yieldDat))
    for(i in 1:length(yieldDat)) {
     fRange <- getRange(yieldDat[i], res)
     mn <- fRange[1] %>% as.numeric()
     mx <- fRange[2] %>% as.numeric()
     vl <- c(mn, mx)
     fLst[[i]] <- list(sliderInput(yieldDat[i], label = fRange[3],
      value = vl, min = mn, max = mx, step = 0.01))
    }
   }
  }
  return(fLst)
 })
 
 output$filters_yieldgap <- renderUI({ getYieldFilts() })
 
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
  # only do this once rainfall data has been selected; gives a console
  # error initially but doesn't affect UI and functioning after that
  if("mean_rm" %in% input$core.data) { 
   # initialize list to hold rainfall filters
   fList <- vector("list", 2)
   # get resolution
   res <- input$res
   # create filter for mm of rain, based on mth selected
   nmM <- paste0("mean_rm_", input$rain_mth)
   mmRange <- getRange(nmM, res)
   labl <- mmRange[3]
   mmRange <- c(as.numeric(mmRange[1]), as.numeric(mmRange[2]))
   fList[[1]] <- sliderInput("rain_mm", label = labl, 
    value = mmRange, min = mmRange[1], max = mmRange[2], step = 50)
   # create filter for volatility of rain, based on mth selected
   nmVol <- paste0("mean_rvol_", input$rain_mth)
   volRange <- getRange(nmVol, res)
   labl2 <- volRange[3]
   volRange <- c(as.numeric(volRange[1]), as.numeric(volRange[2]))
   fList[[2]] <- sliderInput("rain_vol", label = labl2,
    value = volRange, min = volRange[1], max = volRange[2], step = 0.1)
   return(fList)
  } else {
   NULL
  }
 })
 
 # render rainfall filters
 output$filters_rain1 <- renderUI({ getRainFilts1() })
 
 observeEvent(input$submit.data, {
  output$filters_rain2 <- renderUI({ getRainFilts2() }) 
 })
 
 
 ## generate fertilizer consumption filters if datasets selected
 getFertConsFilts <- reactive({
  # get resolution
  res <- input$res
  # initialize list to hold filters
  fList <- vector("list", 2)
  # create filters
  if("Fertilizer consumption (kg/acre)" %in% input$crop.data) {
   # n
   rgN <- getRange("mean_n.con", res)
   rgN <- c(as.numeric(rgN[1]), as.numeric(rgN[2]))
   fList[[1]] <- sliderInput(inputId = "fert_n", 
    label = "Fertilizer consumption (Nitrogen, kg/acre)",
    min = rgN[1], max = rgN[2], value = rgN)
   # p
   rgP <- getRange("mean_p.con", res)
   rgP <- c(as.numeric(rgP[1]), as.numeric(rgP[2]))
   fList[[2]] <- sliderInput(inputId = "fert_p", 
    label = "Fertilizer consumption (Phosphorus, kg/acre)",
    min = rgP[1], max = rgP[2], value = rgP)
   # k
   rgK <- getRange("mean_k.con", res)
   rgK <- c(as.numeric(rgK[1]), as.numeric(rgK[2]))
   fList[[3]] <- sliderInput(inputId = "fert_k", 
    label = "Fertilizer consumption (Potassium, kg/acre)",
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
   rg5 <- rg5[1:2] %>% as.numeric()
   fList[[1]] <- sliderInput(inputId = "soil_c_5", 
    label = "Soil fertility (ppm carbon at 5cm)",
    min = rg5[1], max = rg5[2], value = rg5)
   
   # Get the 15 cm Carbon soil fertility filter
   rg15 <- getRange("mean_soil.c.15", res)
   rg15 <- rg15[1:2] %>% as.numeric()
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
 
 ## TO DO: generate population type filters if datasets selected
 
 ## Generate and render selectizeInput for countries/regions:
 getBordPanel <- reactive ({
  lb <- h5(strong("Country/region selection"))
  sl <- selectizeInput("bdr",label = lb, choices = ctName,
   selected = "Uganda", multiple = F)
  return(sl)
 })
 
 output$geoui <- renderUI({getBordPanel()})
 
 ## define function to load selected data *except* rainfall and yield gap data:
 loadedDat <- reactive({
  det <- ""
  if("mean_rm" %in% allData()) {
   det <- "Skipping rainfall for now"}
  else if("_yieldgap" %in% allData()) {
   det <- "Skipping yield gap for now"}
  else if("mean_rm" %in% allData() & "_yieldgap" %in% allData()) {
   det <- "Skipping rainfall and yield gap data for now"}
  
  withProgress(message = "Loading data...", value = 0,
   detail = det, {
    # specify resolution sub-directory and borders based on user input
    pth <- paste(dd, input$res, sep = "/")
    bdr <- loadedBdr()
    incProgress(0.2, detail = "setting your boundaries")
    # create vector of unique data set names to load
    toLoad <- getNames(inputs = allData(), setting = "load.data")
    
    toLoad <- toLoad[!is.na(toLoad)]
    
    incProgress(0.2, detail = "grabbing your files")
    
    # load data sets that are cropped to chosen border
    loadedDat <- loadDat(pth, toLoad, bdr)
    # loadedDat <- loadDat(pth, toLoad)
    incProgress(0.6, detail = "woot - all data loaded!")
    return(loadedDat)
   })
 }) 
 
 ## Alert for high-res datasets; warns user on loading time
 observeEvent(input$res, {
  x <- input$res; wn <- c("5km", "10km", "15km")
  if (x %in% wn) {
   createAlert(session, anchorId = "highResAlert", alertId = "highRes",
    title = "Hey there.",
    content = "I noticed that you selected a high resolution. \n
    That's great because your maps will be more detailed but as a quick heads
    up, datasets at these resolutions are larger and will take a little longer 
    to load.", append = F)
  }
 })
 
 ## closing high-res alert warning when low-res selection changes:
 observeEvent(input$res, {
  x <- input$res
  watch <- paste0(seq(20, 80, by = 5), "km")
  if (x %in% watch) {
   closeAlert(session, "highRes")
  }
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
   
   createAlert(session, anchorId = "loadingAlert", 
    alertId = "wondering",
    title = "Wondering why nothing seems to be happening?",
    content = "Fear not! We are loading your data sets; you can 
    check on the progress in the top-right corner. \n 
    Thanks for your patience!",
    append = F)
   closeAlert(session, "whoops")
   closeAlert(session, "highRes")
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
 # need to make sure that if rainfall is selected then unselected in a
 # session, the app knows to not keep reloading the last-selected rf set
 loadedRain <- reactive({
  if(("mean_rm" %in% allData()) & !is.null(input$rain_mth)) {
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
     print(loadedDat)
     return(loadedDat)
    })
  }
 }) 
 
 observeEvent(input$submit.data, { 
  # show an alert to calm people about loading rain time
  if(("mean_rm" %in% allData()) & !is.null(input$rain_mth)) {
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
 
 ## Load
 
 ## Get input for each filter and use input to combine rasters and return a
 # single 1/NA raster for painting:
 getMerged <- reactive ({
  
  # Get selected datasets from checkbox inputs
  allDat <- allData()
  
  # Get list of ids of server-generated filters
  fId <- getNames(inputs = allDat, setting = "call.filters")
  # remove rain_mth if rainfall is selected
  if("rain_mth" %in% fId) { fId <- fId[-which(fId == "rain_mth")] }
  print(fId)
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
  dt1 <- unlist(dt) %>% stack()
  print(names(dt1))
  # Create stack
  mgd <- getStack(dt, fList)
  return(mgd)
 })
 
 ## WIP (BK): observeEvent for filter inputs
 #Listens to data submit button, and ignores inputs for filters whose data
 #was not loaded
 
 
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
    getDat$downMap <- makeMap
   }
   return(makeMap)
  })
 })
 
 ## Load the selected geography - country/region:
 loadedBdr <- reactive({
  # Get the input border name, and level of detail from districts mapping portion:
  selB <- input$bdr
  det <- input$detail
  mapOpt <- input$boxes_admin
  # Create a null object (to which we'll assign a border then return)
  tr <- NULL
  # Check if a region is selected and load separately (readOGR) then return
  if(selB %in% regions) {
   tr <- getRegion(selB)
   return(tr)}
  # Else if it's a country use readRDS and return border
  else {
   # If the map option is admin-based map:
   if(mapOpt == "Admin-based Map") {
    # If detail is 'Regional' load level-1 shapefile(s)
    if(det == "Regional" ) {
     tr <- getRegion1(selB)}
    else if (det == "District"){
     #Else load level-2 shapefile(s)
     tr <- getRegion2(selB) }
    else if(det == "Ward") {
     #Otherwise load level-3 shapefile(s) for a drill-down
     tr <- getRegion3(selB)}
   }
   #Otherwise for boxes map just load the level 1 shapefile since this doesn't
   #affect mapping for boxes
   else {
    tr <- getRegion1(selB)}
  }
  return(tr)
 })
 
 ### Creating UI outputs:
 
 ##uiOutput for choosing a variable for a heatmap:
 selChlVar <- reactive ({
  x <- NULL; y <- NULL
  opt <- input$map_type
  if(opt == "Heat Map") {
   x <- em("Select one variable whose heat map you wish to see")
   y <- showOpts() }
  rtn <- list (x, y)
  return(rtn)
 })
 
 ##renderUi for heatmap var selection:
 output$heatMap_opts <- renderUI({ selChlVar() })
 
 ##Show level-of-detail options only if districts mapping is selected:
 detOpts <- reactive ({
  #Get map type 1 options (boxes or districts)
  mapOpt <- input$boxes_admin
  rtn <- NULL
  if(mapOpt == "Admin-based Map") {
   msg <- em("For an admin-based map, please select the level of detail you'd like
    to see; the Regional option is less detailed than the Districts option")
   sel <- selectInput("detail", label = h5(strong("Summary level of detail:")), 
    choices = list("Regional","District"), selected = "Regional")
   rtn <- list(msg, sel)
  }
  return(rtn)
 })
 
 ##renderUI for level-of-detail options:
 output$detail <- renderUI({ detOpts() })
 
 ##Generate UI actionButtons based on selected map types:
 chooseButton <- reactive({
  #Get map type 1 (boxes or districts)
  mapOpt <- input$boxes_admin
  #Get map type 2 (green map or heat map)
  mapType <- input$map_type
  rtn <- NULL
  if(mapOpt == "Boxes Map") {
   if(mapType == "Binary Green Map") {
    rtn <- actionButton("refresh.map", label = "Generate boxes binary green map!",
     width = "100%") }
   else {
    rtn <- actionButton("chrolo.show", label = "Generate boxes heat map!",
     width = "100%") }
  }
  else if(mapOpt == "Admin-based Map"){
   if(mapType == "Binary Green Map") {
    rtn <- actionButton("show.admin", label = "Generate admin-based binary green map!",
     width = "100%") }
   else {
    rtn <- actionButton("admin.chloro",label = "Show admin-based heat map!",
     width = "100%") }
  }
  return(rtn)
 })
 
 ##renderUI for actionButton:
 output$set_button <- renderUI({ chooseButton() })
 
 ##Choosing between boxes and chrolopleth for drill-down option
 drillDownGeoType <- reactive ({
  bt <- NULL
  mapType <- input$map_type
  if(mapType == "Binary Green Map") {
   bt <- actionButton("show.ddwn", label = "Display selected wards as green map",
    width = "100%")
  }
  else if(mapType == "Heat Map") {
   bt <- actionButton("show.ddwn2", label = "Display selected wards as heat map",
    width = "100%")
  }
  return(bt)
 })
 
 ##Create a selectIze input + actionButton that will be rendered once
 # data extraction has been done for districts mapping, for a drill-down option
 #Create a reactive that will return a list -- selectIze input & actionButton
 getDrilldownGeo <- reactive ({
  opts <- getDat$ddOpts #Get vector of regions that are available for drill-down
  sl <- selectizeInput("ddwn", label = "Select up to 3 regions for a drill-down:",
   options = list(maxItems = 3, choices = opts), multiple = T, choices = opts)
  bt <- drillDownGeoType()
  rt <- list(sl, bt)
  return(rt)
 })
 
 
 #  ## WIP: Estimating the market size for selected drilldown regions:
 #Problem: selection of one dataset loses raster name (find fix for this first)
 #  getMarketSize <- reactive({
 #   #Get extracted data and check if pop data is present
 #   pop <- "sum_pop.tif"
 #   popDat <- NULL
 #   bdr <- loadedBdr() %>% unlist(); print(class(bdr))
 #   fList <- getDat$fList
 #   dt <- getDat$sp2
 #   nms <- getDat$loadedDat %>% unlist() %>% stack %>% names()
 #  #Check if pop data is loaded (total pop)
 #    if("sum_pop" %in% nms) {
 #    popDat <- subset(dt, select = sum_pop)
 #    print(str(popDat))
 #   }
 #   
 #   #If not, load pop data and extract to current shapefile
 #   else {
 #    rs <- loadDat(path = paste(dd, input$res, sep = "/"), toLoad = "sum_pop", bdr)
 #    rs <- rs[[1]]
 #    print(rs); print(class(rs))
 #   # popDat <- extract(rs, loadedBdr(), df = T, na.rm = T, method = "simple")
 #    popDat <- getAdminDat(bdr, rs, fList)
 #    print(head(popDat))
 #   }
 #   
 #   # Take 1/NA row from sp2 and multiply by popDat
 #   dt <- subset(dt, select = DATA) %>% as.numeric()
 #   popDat <- as.numeric(popDat)
 #   print(head(dt)); print(str(dt))
 #   
 #   dt <- popDat * dt
 #   print("we here now: "); print(head(dt)); print(tail(dt))
 #   
 #   dt <- sum(dt, na.rm = T)
 #   return(dt)
 # })
 
 #  ## UI output section for estimated addressable market (# of households)
 #  output$market_size <- renderText({getMarketSize()})
 
 ## UI output section for benchmark figures (data table)
 output$benchmarks <- DT::renderDataTable(
  DT::datatable({
   #Get names of loaded datasets:
   dt <- stack(unlist(getDat$loadedDat))
   nms <- names(dt)
   nms <- paste0(nms, ".tif")
   #Subset f.range (filterRanges_iqr_bk.csv)
   #First we stick only to the 5km rows (we don't need to replicate this for all res)
   data <- f.range[which(f.range$res == "5km"),]
   #select only rows corresponding to loaded data
   data <- data[data$var %in% nms,]
   #Then select only rows that have a threshold
   data <- data[which(data$Has.benchmark == "Y"),]
   #Now select only columns we want
   data <- subset(data,
    select = c(filt.name, Kenya.average, Zambia.average, Ethiopia.average))
   #And re-name columns
   names(data) <- c("Dataset", "Average-Kenya", "Average-Zambia", "Average-Ethiopia")
   #Then output our dataframe
   data
  })
 )
 
 ## reactive that generates a border file for leaflet based on level of detail
 # Also renders drilldown option if available
 getAdminBdr <- reactive({
  ## Filter extracted values to filter ranges:
  # First get the filters (code from getMerged):
  # Get selected datasets from checkbox inputs
  allDat <- allData()
  # Get list of ids of server-generated filters
  fId <- getNames(inputs = allDat, setting = "call.filters")
  print(fId); print("and"); print(allDat)
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
  print("output for the fIds"); print(fList); print("done"); print(length(fList)); print("done2")
  getDat$fList <- fList
  bdr <- loadedBdr()
  dt <- stack(unlist(getDat$loadedDat))
  print("and the data: "); print(length(dt)); print(dt); print("data, done")
  # Get data from getAdminDat function, and loaded shapefile:
  sp2 <- getAdminDat(bdr, dt, fList)
  
  # RenderUI for drill-down, if this options is available i.e if we have a level 3 shapefile
  nm <- input$bdr #Get border name
  pos <- match(nm, ctName) #Locate country in country list file
  ddwn <- countries$Drill.down[pos] #Check within country list file for shapefile (if available)
  det <- input$detail #Get level of detail selected (region/district)
  if(ddwn == "Yes") {
   #Extract names of 'districts' (level-2) for user to select district of interest
   #for drill-down
   if(det == "District") {
    nms <- bdr@data$NAME_2
    getDat$ddOpts <- nms #Store for later use 
   }
   else if(det == "Regional") {
    nms <- bdr@data$NAME_1
    getDat$ddOpts <- nms #Store for later use
   }
   # Render getDrilldownGeo reactive
   output$drillDown <- renderUI({getDrilldownGeo()})
  }
  
  # Combine sp2 data with bdr data (shapefiles)
  # If level-2 detail was selected, include district names
  if("NAME_2" %in% names(bdr@data)) {
   bdr@data <- left_join(bdr@data[,c("OBJECTID", "NAME_0","NAME_1", "NAME_2")],
    sp2, by = c("OBJECTID" = "ID"))
  }
  # Else select only up to the state level
  else {
   bdr@data <- left_join(bdr@data[,c("OBJECTID", "NAME_0","NAME_1")], sp2,
    by = c("OBJECTID" = "ID"))
  }
  
  #Store bdr@data in getDat for download, and also return shapefile for leaflet
  getDat$sp2 <- bdr@data
  return(bdr)
 })
 
 # reactive element for a ward drill-down:
 wardDdn <- reactive({
  # Getting border directly from function so as not to override existing level-2 border
  selB <- input$bdr
  ddBdr <- getRegion3(selB)
  
  # Get filter inputs for data extraction and filtering
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
  det <- input$detail #Get level of detail selected (region/district)
  ddOnly <- input$ddwn #Wards or disticts selected for a drill-down
  # subset based on NAME_2 for level_2 shapefile
  if(det == "District") {
   ddBdr <- ddBdr[ddBdr@data$NAME_2 %in% ddOnly, ] #Subset shapefile to selected wards
  }
  # Otherwise subset based on NAME_1 for level_1 shapefile
  else {
   ddBdr <- ddBdr[ddBdr@data$NAME_1 %in% ddOnly, ] #Subset shapefile to selected wards
  }
  dt <- stack(unlist(getDat$loadedDat)) # Get loaded datasets
  ddData <- getAdminDat(ddBdr, dt, fList) #Extract data for these datasets
  
  # Before merging extracted data to shapefile we first change IDs on shapefile
  #to match IDs of extracted data
  for(i in 1:nrow(ddData)) {
   ddBdr@data$OBJECTID[i] <- i %>% as.numeric()
  }
  # Add extracted data to shapefile:
  ddData <- left_join(ddBdr@data[, c("OBJECTID","NAME_0","NAME_1", "NAME_2", "NAME_3")],
   ddData, by = c("OBJECTID" = "ID"))
  ddBdr@data <- ddData
  return(ddBdr)
 })
 
 # Add an eventReactive and observeEvent to stop distrcits map from auto-refreshing:
 getDistrR <- eventReactive(input$show.admin, {getAdminBdr()})
 
 ## renderLeaflet to show admin boundaries(districts)
 observeEvent(input$show.admin,{
  output$map <- renderLeaflet({
   withProgress(message = "Creating map:", detail = "Getting data", 
    value = 0, {
     #Get loaded extracted and filtered data
     #also get selected shapefile and add our data to it
     bdr <- getDistrR()
     print(head(bdr@data)); print(names(bdr@data))
     ## Create color palette
     pal2 <- colorNumeric("#006400", domain = bdr@data$DATA,
      na.color = "#00000000")
     incProgress(0.3, detail = "Getting level of detail (region/district)")
     det <- input$detail #Get level of detail selected (region/district)
     wt <- 1
     #Change border weight based on level of detail selected
     if(det == "District") {
      wt <- 1.5 #Make border thicker for districts level map
     }
     
     ## Now create map and return map
     incProgress(0.5, detail = "Now making your map; almost there!")
     makeMap <- leaflet(bdr) %>% addTiles() %>% 
      addPolygons(stroke = T,fill = T, fillColor = pal2(bdr@data$DATA), fillOpacity = 0.5,
       smoothFactor = F, color = "white", weight = wt, dashArray = 3, 
       opacity = 0.5, popup = getPops(bdr@data, names(bdr@data)))
     
     #Save map for download:
     #getDat$downMap <- saveWidget(widget = makeMap, file = "map.png", selfcontained = F)
     getDat$downMap <- makeMap
     
     incProgress(0.2, detail = "BOOM") 
     return(makeMap) #Then display map
    })#End of withProgress
   
  })
 })
 
 
 ## Chrolopleths based on only one of the selected datasets:
 # Generating a selectizeInput for the loaded datasets:
 showOpts <- reactive ({
  #First get the loaded datasets:
  dt <- getDat$loadedDat
  if(!is.null(getDat$loadedRain)) {
   dt <- c(dt, getDat$loadedRain)
  }
  #Then we find their names to display for the user:
  nms <- vector()
  for(i in 1:length(dt)) {
   x <- paste0(names(dt[[i]]), ".tif") #Get the dataset's name
   pos <- match(x, f.range$var) #Find its position in filters_iqr (f.range)
   y <- f.range$filt.name[pos] #Use pos to find the dataset's full name
   nms[length(nms) + 1] <- y # Add it to the vector of names
   
  }
  
  #Now we create a selectizeInput with nms as options
  rt <- selectizeInput("chloro.opts", label = "", choices = nms, multiple = F,
   selected = nms[1])
  
  return(rt)
 })
 
 # Display the selectizeInput from showOpts above
 output$chloro_opts <- renderUI({showOpts()})
 
 #Reactive that isolates the input of the chrolopleth options
 getShowOpts <- reactive ({
  m <- input$chloro.opts
  return(m)
 }) 
 
 # Show chrolopleth map in boxes format:
 #eventReactive that stops boxes chloropleth from autorefershing when
 # input option changes and before actionButton is clicked
 retrieveOptsBoxes <- eventReactive(input$chrolo.show, {getShowOpts()})
 #ObserveEvent for boxes chloropleth
 observeEvent(input$chrolo.show, {
  
  output$map <- renderLeaflet( {
   #Get input from selectizeInput, and reverse above process to get raster file
   m <- retrieveOptsBoxes()
   #m <- input$chloro.opts # Get input
   # use input name to get raster name and fetch it among loaded data
   pos <- match(m, f.range$filt.name)
   mp1 <- f.range$var[pos]
   mp1 <- gsub(pattern = ".tif", replacement = "", mp1)
   dt <- c(getDat$loadedDat, getDat$loadedRain)
   mp <- NULL; pos1 <- NULL
   for(i in 1:length(dt)) {
    if(names(dt[[i]]) == mp1){
     mp <- dt[[i]]
     pos1 <- i #Save raster location; we'll use this to locate corresponding filter
     break()
    }
   }
   
   #Get filter input for selected dataset, and filter the data before mapping:
   # Get filter inputs for data extraction and filtering
   allDat <- allData()
   # Get list of ids of server-generated filters
   fId <- getNames(inputs = allDat, setting = "call.filters")
   # remove rain_mth if rainfall is selected
   if("rain_mth" %in% fId) { fId <- fId[-which(fId == "rain_mth")] }
   # do this only if data has been selected and loaded
   fList <- NULL
   if(length(fId) != 0) {
    # Get the input of filter corresponding to selected dataset
    id <- fId[pos1]
    fList[[length(fList) + 1]] <- input[[id]]
   }
   #pass filter and raster to getBool for filtering
   # rastfilt <- (mp >= fList[[1]][1] & mp <= fList[[1]][2])
   print(ncell(mp)); print(summary(mp))
   print(fList[[1]][1]); print(fList[[1]][2])
   mp[mp <= fList[[1]][1] & mp >= fList[[1]][2]] <- NA
   #    print(ncell(rastfilt)); print(summary(rastfilt))
   #    print("---------")
   #    print(ncell(mp)); print(summary(mp))
   # mp <- ifelse((mp >= fList[[1]][1] & mp <= fList[[1]][2]), mp, NA)
   #    rastfilt[rastfilt == T] <- mp
   #    rastfilt[rastfilt == F] <- NA
   #    #mp <- getBool(mp, fList)
   # mp <- rastfilt
   print("+++++++++")
   print(ncell(mp)); print(summary(mp))
   
   #Create a color palette for use in painting the map:
   #Start with getting the filter ranges to eliminate outliers
   rg <- getRange(mp1, input$res)
   rg <- c(rg[1], rg[2]) %>% as.numeric()
   #And now make a palette
   pal3 <- colorBin(palette = "YlGnBu", domain = rg, bins = 7,
    pretty = F, na.color = "#00000000")
   #Then create a map
   makeMap <- leaflet() %>% addTiles() %>% 
    addRasterImage(mp, opacity = 0.7, colors = pal3, project = T) %>% 
    addLegend(position = "bottomright", pal = pal3, values = rg,
     na.label = "Missing", title = "Map Key:")
   #And return the map
   getDat$downMap <- makeMap
   return(makeMap)
   
  })
 })
 
 ## Display chrolopleth map for districts mapping:
 #eventReactives to prevent autorefreshing:
 getDistrR2 <- eventReactive(input$admin.chloro, {getAdminBdr()})
 retrieveOptsDistr <- eventReactive(input$admin.chloro, {getShowOpts()})
 #Rendering chloropleth leaflet for districts:
 observeEvent(input$admin.chloro,
  output$map <- renderLeaflet({
   withProgress(message = "Creating map:", detail = "Getting data", 
    value = 0, {
     #Reverse process for displaying chrolopleth options to get dataset names
     m <- retrieveOptsDistr()  # Get input
     # use input name to get raster name:
     pos <- match(m, f.range$filt.name)
     mp1 <- f.range$var[pos]
     mp1 <- gsub(pattern = ".tif", replacement = "", mp1)
     #Get the border shapefile earlier created:
     bdr <- getDistrR2()
     print(head(bdr@data))
     #Then we locate the selected column from chrolopleth options:
     dt <- subset(bdr@data, select = mp1) %>% unlist() %>% as.numeric()
     print("Before"); print(head(dt)); print(str(dt)); print(summary(dt)); print(class(dt))
     
     #Create a color palette for the chrolopleth:
     dt1 <- format(dt, digits = 0) %>% as.numeric()
     print("after"); print(head(dt1)); print(table(dt1))
     pal4 <- colorBin(palette = "YlGnBu", domain = dt1, bins = 7,
      pretty = F, na.color = "#00000000")
     incProgress(0.2, message = "Making map",detail = "Getting level of detail")
     
     #Change border weight based on level of detail selected
     det <- input$detail #Get level of detail selected (region/district)
     wt <- 1
     if(det == "District") {
      wt <- 1.5 #Make border thicker for districts level map
     }
     #Now we create a leaflet map:
     incProgress(0.4, message = "Making map", detail = "Now creating map. Hold on!")
     makeMap <- leaflet(bdr) %>% addTiles() %>% 
      addPolygons(stroke = T,fill = T, fillColor = pal4(dt), fillOpacity = 0.5,
       smoothFactor = F, color = "white", weight = wt, dashArray = 3, 
       opacity = 0.5, popup = getPops(bdr@data, names(bdr@data))) %>% 
      addLegend(position = "bottomright", pal = pal4, values = dt1,
       na.label = "Missing", title = "Map Key:")
     getDat$downMap <- makeMap
     incProgress(0.4, message = "Yay!", detail = "BOOM!")
     return(makeMap)
    }) # End of progress
  })#End of renderLeaflet
 )#End of observeEvent
 
 
 # Event reactive elements for both boxes and chrolopleth maps for drill-down
 # Both call wardDdn() but handle the output differently - boxes vs heat map
 wardDdn1 <- eventReactive(input$show.ddwn, {wardDdn()})
 
 # Rendering leaflet map for ward drill-down: 'boxes' map
 observeEvent(input$show.ddwn, {
  output$map <- renderLeaflet({
   withProgress( message = "Loading map", detail = "Getting your data", value = 0, {
    #Get loaded extracted and filtered data
    #also get selected shapefile and add our data to it
    bdr <- wardDdn1()
    ## Create color palette
    pal2 <- colorNumeric("#006400", domain = bdr@data$DATA,
     na.color = "#00000000")
    ## Now create map and return map
    incProgress(0.5, detail = "Now creating map")
    makeMap <- leaflet(bdr) %>% addTiles() %>% 
     addPolygons(stroke = T,fill = T, fillColor = pal2(bdr@data$DATA), fillOpacity = 0.5,
      smoothFactor = F, color = "white", weight = 2, dashArray = 3, 
      opacity = 0.5, popup = getPops(bdr@data, names(bdr@data)))
    
    #Save map for download:
    #getDat$downMap <- saveWidget(widget = makeMap, file = "map.png", selfcontained = F)
    getDat$downMap <- makeMap
    incProgress(0.5, detail = "BOOM") 
    return(makeMap) #Then display map
   })#End of withProgress
  })#End of renderLeaflet
 }
 )
 
 ## Chrolopleth leaflet option for map drill-down:
 # eventReactive to stop map from autorefreshing:
 # retrieveOpts are separated despite originating from same reactive (getShowOpts)
 # since the actionButtons are different
 retrieveOptsWard <- eventReactive(input$show.ddwn2, {getShowOpts()})
 wardDdn2 <- eventReactive(input$show.ddwn2, {wardDdn()})
 # observeEvent for ward chloropleth
 observeEvent(input$show.ddwn2, {
  output$map <- renderLeaflet({
   withProgress(message = "Creating map:", detail = "Getting data", 
    value = 0, {
     #Reverse process for displaying chrolopleth options to get dataset names
     m <- retrieveOptsWard() # Get input
     # use input name to get raster name:
     pos <- match(m, f.range$filt.name)
     mp1 <- f.range$var[pos]
     mp1 <- gsub(pattern = ".tif", replacement = "", mp1)
     
     #Get the border shapefile earlier created:
     bdr <- wardDdn2()
     incProgress(0.3, message = "Now making your map; hold on!")
     #Then we locate the selected column from chrolopleth options:
     dt <- subset(bdr@data, select = mp1) %>% unlist() %>% as.numeric()
     #dt <- format(dt, digits = 0, big.mark = ",")
     #Create a color palette for the chrolopleth:
     pal4 <- colorBin(palette = "YlGnBu", domain = dt, bins = 7,
      pretty = F, na.color = "#00000000")
     incProgress(0.3, message = "Almost there!")
     #Now we create a leaflet map:
     makeMap <- leaflet(bdr) %>% addTiles()
     makeMap <- makeMap %>% addPolygons(stroke = T,fill = T, fillColor = pal4(dt), fillOpacity = 0.5,
      smoothFactor = F, color = "white", weight = 2, dashArray = 3, 
      opacity = 0.5, popup = getPops(bdr@data, names(bdr@data)))
     incProgress(0.2, message = "A few more secs")
     makeMap <- makeMap %>% addLegend(position = "bottomright", pal = pal4, values = dt,
      na.label = "Missing", title = paste("Map Key:", m))
     getDat$downMap <- makeMap
     incProgress(0.2, message = "BOOM!")
     return(makeMap)
    }) #End of progress   
  })#End of renderLeaflet
  
 })
 
 ## Reactive that gets data for download
 downDat <- reactive ({
  dt <- getDat$sp2
  return(dt)
 })
 
 ## Event handler for downloading the data:
 output$data.down <- downloadHandler(
  filename = function () {
   paste0("OAF_Geoboxes_data_", Sys.Date(), ".csv")
  },
  content = function (file){
   write.csv(downDat(), file)
  }
 )
 
 ##Map download option:
 #Note: just like data, ends up in the user's DOWLOADS folder
 #Reactive that gets the map from the reactiveValues variable
 downMap <- reactive ({
  mp <- getDat$downMap
  return(mp)
 })
 #Download handler that downloads the map
 #Uses the webhooks package and the phantomJS webkit
 #BK is not 100% sure how this works---magic!
 #Partly borrowed from:
 #  http://stackoverflow.com/questions/35384258/save-leaflet-map-in-shiny
 output$map.down <- downloadHandler(
  filename = function () {
   paste0("OAF_Geoboxes_map_", Sys.Date(), ".png")
  },
  content = function (file){
   owd <- setwd(tempdir())
   on.exit(setwd(owd))
   saveWidget(downMap(), "temp.html", selfcontained = F)
   webshot("temp.html", file = file, cliprect = "viewport")
   # resize("75%") %>% shrink()
  }
 )
 
}) # Server input/output ends here

# rsconnect::deployApp("~/drive/Boxes_2.0/Code files", appName = "Boxes", account = "oneacrefund")
