# Boxes 2.0: UI script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: UI tool for geospatial Shiny app
# Date created: 29 Mar 2016
# Date modified: 14 Jun 2016


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
        column(width = 5,
               h2("Select paramters here:"),
               
               # Resoluton selector:
               bsCollapse(
                   open = "Select data resolution:", id = "res.sel",
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
                       selectInput("geo",
                                   label = "Select a region",
                                   choices = list("East Africa",
                                                  "Central Africa",
                                                  "South Africa",
                                                  "West Africa", "OAF - All", "OAF - Core", "SSA"),
                                   selected = "SSA"
                       ))),
               
               #Data selector:
               bsCollapse(
                   id = "sel.dat",
                   #bsCollapsePanel(
                       #"Select data:",
                       fluidRow(
                           column(width = 6,
                                  h4("All data"),
                                  bsCollapse(
                                      id = "all.dat", multiple = T,
                                      bsCollapsePanel(
                                          "Core program indicators",
                                          checkboxGroupInput("coredata",
                                                             label = "Core program indicators",
                                                             choices = list("Rainfall (mean monthly, mm)",
                                                                            "Rainfall volatility",
                                                                            "Population (total)",
                                                                            "Population (density)",
                                                                            "Est. avg. farm size",
                                                                            "Land use indicators"
                                                             ))),
                                      
                                      bsCollapsePanel(
                                          "Crop data",
                                          checkboxGroupInput("cropdata",
                                                             label = "Crop data",
                                                             choices = list("Crop mix (% of cultivated area)",
                                                                            "Hybrid seed adoption",
                                                                            "Yield gaps",
                                                                            "Months of growing season"
                                                             ))),
                                      
                                      bsCollapsePanel(
                                          "Other data",
                                          checkboxGroupInput("otherdata",
                                                             label = "Other",
                                                             choices = list("Fertilizer consumption",
                                                                            "Fertilizer application rates",
                                                                            "Soil fertility (Nitrogen g/kg)",
                                                                            "Soil fertility (carbon ppm)",
                                                                            "Geographic data bundle 
                                                                            (elevation, slope, land cover)"
                                                             )))
                                  ),
                                  column(width = 6,
                                         h4("Data bundles"),
                                         bsCollapse(
                                             id = "", multiple = T,
                                             bsCollapsePanel(
                                                 "Topical bundles",
                                                 checkboxGroupInput("topicaldata",
                                                                    label = "Topical bundles",
                                                                    choices = list("Core program indicators",
                                                                                   "Crop data",
                                                                                   "Fertilizer data",
                                                                                   "Soil fertility",
                                                                                   "All “other”"
                                                                    ))),
                                             
                                             bsCollapsePanel(
                                                 "Team bundles",
                                                 fluidRow(
                                                     column(width = 6,
                                                            h5("NCE"),
                                                            checkboxGroupInput("ncedata",
                                                                               label = "NCE bundles",
                                                                               choices = list("Rainfall(mean)",
                                                                                              "Rainfall(volatility)",
                                                                                              "Population (density)",
                                                                                              "Est. avg. farm size",
                                                                                              "Crop mix"
                                                                               ))
                                                     ),
                                                     column(width = 6,
                                                            h5("Frontiers"),
                                                            checkboxGroupInput("frontiersdata",
                                                                               label = "Frontiers bundles",
                                                                               choices = list("Crop mix",
                                                                                              "Hybrid seed adoption",
                                                                                              "Yield gaps",
                                                                                              "Fertilizer consumption"
                                                                               ))
                                                     )
                                                 ))
                                         ))
                           ))
                   
               )),
        
        # Right panel - map
        # To-do: include an instructions page?
        column(width = 7,
               h4("The map displays here"),
               mainPanel(leafletOutput("map"))
        )
    )
))

