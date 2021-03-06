# Boxes 2.0: UI script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Apprentice: Bernard Kiprop (bernard.kiprop@oneacrefund.org)
# Description: UI tool for geospatial Shiny app
# Date created: 29 Mar 2016
# Date modified: 20 Jun 2016


# *****************************************************************************
## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 
# *****************************************************************************

library(leaflet)
library(shinythemes)
library(shinyBS)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("OAF Geodata Repository"),
  fluidRow(
    
    #Left panel of collapsible selectors for resolution, region and datasets
    column(width = 4,
           h3("Select parameters here:"),
           
           # Test data for creating filters
           bsCollapse(
             id = "test", #open = "Test Datasets",
             bsCollapsePanel(
               "Test Datasets",
               checkboxGroupInput(
                 "testdata", label = "Test Datasets",
                 choices = c(
                   "Population (total)" = 1, #sum_pop.tif
                   "Number of households" = 2, #mean_hhs.tif
                   "Total maize production" = 3, #sum_prod_maize.tif
                   "Months of growing season" = 4 #mean_gs.l.tif
                 ))
             )),
           
           # Resoluton selector:
           bsCollapse(
             #open = "Select data resolution:",
             id = "res.sel",
             bsCollapsePanel(
               "Select data resolution:",
               sliderInput("res", label = "Resolution (sqkm)", value = 5, 
                           min = 0, step = 5, max = 10, round = 2, post = "km")
               # TODO: make rounding to nearest 5 work
             )),
           
           # Region/country selector:
           bsCollapse(
             id = "geo.sel",
             bsCollapsePanel(
               "Select region:",
               selectInput(
                 "geo",label = "Select a region",
                 choices = list(
                   "East Africa",
                   "Central Africa",
                   "South Africa",
                   "West Africa", "OAF - All", "OAF - Core", "SSA"),
                 selected = "OAF - Core"
               ))),
           
           #Data selector:
           bsCollapse(
             id = "sel.dat",
             bsCollapsePanel(
               "Select data:",
               fluidRow(
                 column(
                   width = 12, h4("All data"),
                   bsCollapse(
                     id = "all.dat", multiple = T,
                     bsCollapsePanel(
                       "Core program indicators",
                       checkboxGroupInput(
                         "coredata",
                         label = "Core program indicators",
                         choices = list(
                           "Rainfall (mean monthly, mm)",
                           "Rainfall volatility",
                           "Population (total)",
                           "Population (density)",
                           "Est. avg. farm size",
                           "Land use indicators"
                         ))),
                     
                     bsCollapsePanel(
                       "Crop data",
                       checkboxGroupInput(
                         "cropdata", label = "Crop data",
                         choices = list(
                           "Crop mix (% of cultivated area)",
                           "Hybrid seed adoption",
                           "Yield gaps",
                           "Months of growing season"
                         ))),
                     
                     bsCollapsePanel(
                       "Other data",
                       checkboxGroupInput(
                         "otherdata", label = "Other",
                         choices = list(
                           "Fertilizer consumption",
                           "Fertilizer application rates",
                           "Soil fertility (Nitrogen g/kg)",
                           "Soil fertility (carbon ppm)",
                           "Geographic data bundle (elevation, slope, land cover)"
                         )))
                   ),
                   column(
                     width = 12, h4("Data bundles"),
                     bsCollapse(
                       id = "", multiple = T,
                       bsCollapsePanel(
                         "Topical bundles",
                         checkboxGroupInput(
                           "topicaldata",label = "Topical bundles",
                           choices = list(
                             "Core program indicators",
                             "Crop data",
                             "Fertilizer data",
                             "Soil fertility",
                             "All “other”"
                           ))),
                       
                       bsCollapsePanel(
                         "Team bundles",
                         fluidRow(
                           column(
                             width = 6, h5("NCE"),
                             checkboxGroupInput(
                               "ncedata", label = "NCE bundles",
                               choices = list(
                                 "Rainfall(mean)",
                                 "Rainfall(volatility)",
                                 "Population (density)",
                                 "Est. avg. farm size",
                                 "Crop mix"
                               ))
                           ),
                           column(
                             width = 6, h5("Frontiers"),
                             checkboxGroupInput(
                               "frontiersdata", label = "Frontiers bundles",
                               choices = list(
                                 "Crop mix",
                                 "Hybrid seed adoption",
                                 "Yield gaps",
                                 "Fertilizer consumption"
                               ))
                           )
                           
                         ))
                     ))
                 ))
               
             )),# Data selector portion ends here
           
           # Action Button: 'submits' data selection, collapses data selectors
           #& shows filters
           submitButton(text = "Submit"),
           
           # Well panel for filters
           wellPanel(
             uiOutput("filters1"),
             uiOutput("filters2"),
             uiOutput("filters3"),
             uiOutput("filters4")
           )
    ),

    
    # Right panel - map
    # To-do: include a description tab
    column(width = 8,
           h4("The map displays here"),
#            textOutput("txt"),
#            textOutput("txt2"),
           mainPanel(leafletOutput("map"))
           #h4("Can you see the map?")
    )
  )
))