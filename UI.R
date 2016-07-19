#### BASIC INFO ####
# boxes server script
# last edited: 15 jul 2016 (bk)

#### formatting to do list for version 2 #### 
# hover text
# make server-generated filter labels more user-friendly
# rainfall volatility sliders --> turn decimal into %
# make team bundle radio buttons prettier
# add OAF logo to title panel
# find a way to make title centered and "Boxes" bold
# fiddle with header font sizes

#### missing data to add for version 2 ####
# crop mix
# population types (i.e. rural, urban, peri-urban); 
# looks like mean_p.pop, mean_r.pop, and mean_u.pop are for peri-urban, 
# rural, and urban (but currently are only in the 5km resolution)
# land use indicators
# land cover
# yield gaps
# hybrid seed adoption
# fertilizer application rates (probably not reliable data)
# nitrogen soil fertility

#### other to dos for version 2 ####
# consider nixing growing seasons and total pop from filters (included for now for simplicity, but is probably tough for users to actually change meaningfully)
# clean up code (i.e. remove bks after reading them and TO DOs after doing them)
# Explore the usefulness of Archivist (http://www.r-bloggers.com/shiny-archivist-reproducible-interactive-exploration/)
# selectizeInput() for country/regional borders

#### start ui logic ####
shinyUI(fluidPage(theme = shinytheme("flatly"), 
  titlePanel("Boxes: One Acre Fund's geospatial repository"), br(),
  fluidRow(
    # Ultimately we want two collapsable panels that span the page width:
    # 1. panel for selecting data (like sketch)
    # 2. panel for filtering based on selected data
    column(width = 12,
      bsCollapse(id = "collapse.steps", multiple = T,
        open = "Step 1: choose what you want to visualize",
        # 1. panel for selecting data
        bsCollapsePanel("Step 1: choose what you want to visualize",
          fluidRow(
            column(width = 6,
              h4(strong("SELECT DATA"), align = "center"),
              fluidRow(
                # input data by variable
                column(width = 6, 
                  h5(strong("All data"), align = "center"),
                  uiOutput("select_all_data"),
                  em("* = missing data currently", 
                    style = "color:blue")
                ),
                # input data by bundle
                column(width = 6, 
                  h5(strong("Data bundles"), align = "center"),
                  checkboxGroupInput(
                    "topical.data", label = em("By topic"),
                    choices = list(
                      "Core program indicators",
                      "Crop data",
                      "Soil fertility data",
                      "Topography data" 
                    )
                  ),
                  checkboxGroupInput(
                    "team.data", label = em("By team"),
                    choices = list(
                      "New Country Expansion (rainfall, population density, farm size, crop mix)",
                      "Frontiers (crop mix, hybrid seed adoption, yield gaps, fertilizer consumption)"
                      # bk: to help with debugging,
                    )
                  ),
                  em("Note: data bundle loading is not yet functional", 
                    style = "color:blue")
                )
              )
            ),
            column(width = 6,
              h4(strong("SELECT GEOGRAPHY"), align = "center"), 
              fluidRow(
                # input resolution
                column(width = 4,
                  selectInput("res", 
                    label = h5(strong("Box size")), 
                    choices = paste0(c(5, 10, 20, 30, 40, seq(50, 80, 5)), "km"), 
                    selected = "5km")
                ),
                # input region for cropping
                column(width = 4,
                  selectInput("geo",
                    label = h5(strong("Country selection")),
                    choices = list("East Africa" = "EastAfricaBorder", 
                      "Central Africa*", # TO DO: prep Central Africa borders
                      "South Africa" = "SouthAfricaBorder", 
                      "West Africa" = "WestAfricaBorder", 
                      "OAF - All" = "OAF_all", 
                      "OAF - Core" = "OAF_core", 
                      "SSA" = "all_ssa"),
                    selected = "OAF_core")
                  
                ),
                # input summary level of detail
                column(width = 4,
                  selectInput("detail", 
                    label = h5(strong("Summary level of detail")), 
                    choices = list("National", "Regional",
                      "Provincial", "District", 
                      "Site / Sub-district"), 
                    selected = "National")
                )
              ),
              
              fluidRow(
                column(width = 6,
                  actionButton("submit.data",
                    label = "Done selecting data? Generate your map!")
                ),
                column(width = 6,
                  actionButton("download.data",
                    label = "Download Data")
                ), br(),
                # TO DO: make this helpText pop up only after submit.data is clicked 
                column(width = 12,
                  br(),
                  helpText(em(strong("Wait - why isn't anything happening?"), 
                    br(), "After you hit the button(s) above, it takes ~5s to load each dataset.",
                    br(), " - Some (like rainfall) require multiple datasets.",
                    br(), " - You can see how many data sets have been loaded thus far in the top-right corner.",
                    br(), " - Thanks for your patience!"))
                )
              )
            )
          )
        ),
        # 2. panel for filtering based on selected data
        bsCollapsePanel("Step 2: filter your map",
          sidebarLayout(position = "left", fluid = T,
            sidebarPanel(
              # show server-generated filters
              uiOutput("filters_auto"),
              uiOutput("filters_rain1"),
              uiOutput("filters_rain2"),
              uiOutput("filters_pop_type"),
              uiOutput("filters_fert"),
              uiOutput("filters_soil_c"),
              uiOutput("filters_soil_n")
            ),
            mainPanel(
              leafletOutput("map")    
            )
          )
        )
      )
    )
  )
))
