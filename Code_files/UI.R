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
                   "Population (density)" = 1, #mean_pop.dense.tif
                   "Population (total)" = 2, #sum_pop.tif
                   "Est. avg. farm size" = 3, #mean_av.size.tif
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
                           min = 0, step = 5, max = 80, round = 2, post = "km")
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
                 selected = "SSA"
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
           #actionButton("mapit", "Submit"),
           
           # Well panel that will display the filters:
          wellPanel(
           #fluidRow(
           #conditionalPanel(
             #condition = "input.testdata == 'Population (density)'",
             uiOutput("filters1")
             #sliderInput("pop_dense",label = "Population Density",
              #           value = c(0, 500), step = 100, min = 0, max = 5700)
             #),
             ,
            # conditionalPanel(
              # condition = "input.testdata == 'Population (total)'",
               uiOutput("filters2")
#                sliderInput("pop_sum", label = "Total Population",
#                            value = c(0, 2000), step = 500, min = 0, max = 1e6)
             )
             
          # )
           ),
           
           
           # Right panel - map
           # To-do: include an instructions page?
           column(width = 8,
                  h4("The map displays here"),
                  textOutput("txt"),
                  textOutput("txt2"),
                  mainPanel(leafletOutput("map")),
                  h4("Can you see the map?")
           )
    )
  ))
  
  