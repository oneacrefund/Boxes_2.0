# Boxes 2.0: UI script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: UI tool for geospatial Shiny app
# Date created: 29 Mar 2016
# Date modified: 6 Apr 2016

## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 

shinyUI(fluidPage(
    titlePanel("Filters"),
    
    sidebarLayout(
      sidebarPanel(
          sliderInput("res", label = "Resolution (sqkm)", value = 10, 
                       min = 0, step = 5, max = 500, round = 2, post = "km"), # TODO: make rounding to nearest 5 work
          
        selectInput("base",
          label = h5("Choose a base layer to display"),
              choices = c("Default", "Population", "Land Size", "Urban Areas",
                          "Landcover", "water"),
              selected = "Default") 
        # uiOutput("core") 
        

           #
        #    #  uiOutput("core_data"),
           # 
           #  
           # checkboxGroupInput("core", label = h3("Core Program Indicators"),
           #                    choices = list("Rainfall (mean)" = rain.m,
           #                                   "Rainfall (volatility)" = rain.v,
           #                                   "Population (total)" = pop,
           #                                   "Population (density)" = pop.dens,
           #                                   "Est. Avg. Land Size" = av.size,
           #                                   "Land Use Indicators" = lc),
           #                    selected = "Population (total)"

        ),
        
      # mainPanel(NULL) # for UI testing, avoids rendering map
      mainPanel(leafletOutput("map"))
    )    
))