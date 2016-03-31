# Boxes 2.0: UI script
# Author: Colin Custer (colin.custer@oneacrefund.org)
# Description: UI tool for geospatial Shiny app
# Date created: 29 Mar 2016
# Date modified: 30 Mar 2016

## Note, while under construction this lives in "Code Files" subdirectory. 
## it should be moved to a Shiny directory when finished. 

shinyUI(fluidPage(
    titlePanel("Filters"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Set filter values"),
            
            selectInput("base", 
                        label = "Choose a base layer to display",
                        choices = c("Population", "Land Size", "Urban Areas", 
                                    "Landcover", "water"), 
                        selected = "Land Size")
            
            # sliderInput("range", 
            #             label = "Range of interest:",
            #             min = 0, max = 100, value = c(0, 100))
        ),
        
        mainPanel(leafletOutput("map"))
    )
))