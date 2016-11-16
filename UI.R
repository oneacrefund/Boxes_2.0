#### BASIC INFO ####
# boxes server script
# last edited: 01 nov 2016 (bk)

#### formatting to do list for version 2 #### 
# hover text
# make server-generated filter labels more user-friendly
# fertilizer consumption: convert from kg to ton?
# rainfall volatility sliders --> turn decimal into %
# make team bundle radio buttons prettier
# add OAF logo to title panel
# find a way to make title centered and "Boxes" bold

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
# Explore the usefulness of Archivist (http://www.r-bloggers.com/shiny-archivist-reproducible-interactive-exploration/)
# selectizeInput() for country/regional borders

#### start ui logic ####
shinyUI(fluidPage(#theme = "bootstrap.css", 
 theme = shinytheme("spacelab"),
 #shinythemes::themeSelector(),
 
 # Title:
 titlePanel(div(h2(img(src = "green_leaf1.png"),tags$b("Boxes:"),
  "One Acre Fund's geospatial repository", align = "center")),
  windowTitle = "Boxes"),
 
 fluidRow(
  # Ultimately we want two collapsable panels that span the page width:
  # 1. panel for selecting data (like sketch)
  # 2. panel for filtering based on selected data
  column(width = 12,
   bsCollapse(id = "collapse.steps", multiple = T,
    open = panel1,
    # 1. panel for selecting data
    bsCollapsePanel(panel1,
     fluidRow(
      column(width = 6,
       h4(strong("SELECT DATA"), align = "center"), tags$hr(),
       fluidRow(
        # input data by variable
        column(width = 6, 
         h5(strong("All data"), align = "center"), tags$hr(),
         uiOutput("select_all_data")
        ),
        # input data by bundle
        column(width = 6, 
         h5(strong("Data bundles"), align = "center"), tags$hr(),
         checkboxGroupInput(
          "topical.data", label = em("By topic*"),
          choices = list(
           "Core program indicators",
           "Crop data",
           "Soil fertility data",
           "Topography data" 
          )
         ),
         checkboxGroupInput(
          "team.data", label = em("By team*"),
          choices = list(
           "New Country Expansion",
           "Frontiers"
          )
         )
        )
       )
      ),
      column(width = 6,
       h4(strong("SELECT GEOGRAPHY"), align = "center"), tags$hr(), 
       fluidRow(
        # input resolution
        column(width = 6,
         selectInput("res", 
          label = h5(strong("Data resolution")), 
          choices = paste0(c(5, 10, 20, 30, 40, 
           seq(50, 80, 5)), "km"), 
          selected = "10km")
        ),
        # input region for cropping
        column(width = 6,
         uiOutput("geoui")
        )
       ),
       
       #Setting data selection for rain, yieldgap and cropmix
       fluidRow(
        column( width = 12,
        uiOutput("filters_rain1")
       )
        ),
       fluidRow(
        column(width = 12,
         style = "background-color:#f2f6f9",
         uiOutput("yield_choices")
         )
       ), br(), br(),
       
       #Setting map type:
       
       fluidRow(
        h4(strong("SELECT GEOGRAPHY"), align = "center"), tags$hr(),
        column(width = 6
         
        ),
        column(width = 6
         
        )
       ),
       
       fluidRow(
        column(width = 12,
         actionButton("submit.data",
          label = "Done selecting data? Click Me!")
        ),
        # TO DO: make this helpText pop up only after submit.data is clicked 
        
        column(width = 12,
         br(),
         em(strong("* means this feature currently not fully functional:",  
          style = "color:firebrick")), 
         br(),
         em("thus, please do not try to run the app with any of these features yet",
          style = "color:firebrick"),
         br(),
         bsAlert("dataAlert"),
         br(),
         bsAlert("highResAlert"), 
         br(),
         bsAlert("loadingAlert")
         
        )
       )
      )
     )
    ),
    
    # 2. panel for filtering based on selected data
    bsCollapsePanel(panel2,
     sidebarLayout(position = "left", fluid = T, 
      
      sidebarPanel(
       style = "background-color:#F8F8FF",
       em("HINT: the panels below are collapsible; simply click on the
        headers to close or expand them!"), tags$hr(),
       bsCollapse(id = "sidebar", multiple = T, open = "panel1",
        ##WIP: bsCollapsePanel for all map type selections and filter setting
        # Excludes map drill-down for now for purposes of speed/code re-usability
        bsCollapsePanel(
         title = h4(strong("SET YOUR FILTERS AND MAP TYPE:"), align = "center", 
          style = "color:#006400"),value = "panel1",
          
         fluidRow(
          style = "background-color:#f2f6f9",
         #Section 1: selecting between boxes and districts map:         
         em("1. First, set your map type by choosing between a boxes map,
          and an admin-boundary-based map:"),
         selectInput("boxes_admin", label = "",
          choices = c("Boxes Map", "Admin-based Map"), selected = "Boxes Map",
          multiple = F, width = "100%"),
          # Section 1 b: UI output for choosing level of detail for admin mapping
          uiOutput("detail")
          ),#End of fluidRow1
         tags$hr(),
          
          fluidRow(
           style = "background-color:#f2f6f9",
           #Section 2: selecting between heat map and green binary map:
          em("2. Now choose between displaying a filtered binary green map for
           all your selected datasets, and a filtered heat map for only one
           of your datasets:"),
          selectInput("map_type", label = "",
           choices = c("Binary Green Map", "Heat Map"), selected = "Binary Green Map",
           multiple = F, width = "100%"),
          #Section 2 a: UI output for choosing a variable for heat map:
          uiOutput("heatMap_opts")
          ), #End of fluidRow 2
          tags$hr(), 
         
          fluidRow(
           style = "background-color:#f2f6f9",
           #Section 3: all data filters:
          em("3. Awesome! As a last step, please set the filters for your
           selected datasets below, and then hit the button just below to
           generate your map to the right side!"),
          # show server-generated filters
          uiOutput("filters_auto"),
          
          uiOutput("filters_rain2"),
          uiOutput("filters_fert_comp"),
          # uiOutput("filters_pop_type"),
          uiOutput("filters_soil_c"),
           uiOutput("filters_yieldgap")
         ),#End of fluidRow3
         tags$hr(),
         
         fluidRow(
          style = "background-color:#f2f6f9",
          #Section 4: uiOutput for the relevant actionButton:
          uiOutput("set_button")
         )#End of fluidrow4
        ), #Ene of bsCollapsePanel1
       
        #bsCollapsePanel 2: displaying map drill-down options:
        #Consider generating the entire bsCollapsePanel using renderUI in server
        bsCollapsePanel(
         title = h4(strong("ADMIN MAPPING: WARD DRILL-DOWNS"), align = "center", 
          style = "color:#006400"),value = "panel2",
         wellPanel(
          em("Below is a list of wards for drill-down mapping"), br(),
          em("** Please note that this is not always available, depending on
           the selected country/region"), br(),
          uiOutput("drillDown")
          
         )# End of wellPanel
        ) # End of bsCollapsePanel 2
       ) #End of bsCollapse
      ),#sidebarPanel ends here
      
      
      # main panel for map, # of HHs, benchmarks and rainfall data loading alert
      mainPanel(
       #Rainfall data loading alert
       bsAlert("loadingRainAlert"),
       #Map output
       fluidRow(
        leafletOutput("map", height = 600) 
       ), tags$hr(),
       #Market size (# of HHs) -- WIP
       #        fluidRow(
       #         column(width = 3,
       #          style = "background-color:#D3D3D3",
       #          h4(strong("Estimated Market Size (# of HHs):"), style = "color:#006400")
       #         ),
       #         column(width = 4,
       #          uiOutput("market_size")
       #         )
       #        ), tags$hr(),
       #Benchmark data table
       fluidRow(
        style = "background-color:#D3D3D3",
        h3(strong("Benchmark figures for selected datasets -- if available:"),
         style = "color:#006400")
       ),
       fluidRow(
        style = "background-color:#D3D3D3",
        DT::dataTableOutput("benchmarks")
       ),
       tags$hr()
      )
     )
    )
   )
  )
 )
))
