#### BASIC INFO ####
# boxes server script
# last edited: 18 oct 2016 (bk)

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
       bsCollapse( multiple = T, open = "panel1",
        bsCollapsePanel(
         title = h4(strong("SET FILTERS FOR YOUR DATA:"), align = "center", 
          style = "color:#006400"),value = "panel1",
         wellPanel(
          em("HINT:filters will affect all other aspects of your mapping
           and data downlaods"), tags$hr(),
          style = "background-color:#D3D3D3",
          # show server-generated filters
          uiOutput("filters_auto"),
          uiOutput("filters_rain1"),
          uiOutput("filters_rain2"),
          uiOutput("filters_fert_comp"),
          # uiOutput("filters_pop_type"),
          uiOutput("filters_soil_c"),
          # uiOutput("filters_soil_n")
          tags$hr(),
          em("Select the type of map you wis
           h to visualize:"),
          radioButtons("mapType",label = "", choices = c("Boxes", "Heat map")
          )
         )#end of wellPanel 1
        ), #end of bsCollapsePanel
        
        bsCollapsePanel(
         title = h4(strong("DISPLAY BOXES:"), align = "center",
          style = "color:#006400"), value = "panel2",
        #wellPanel 2: displaying boxes
        wellPanel(
         style = "background-color:#D3D3D3",
         
         fluidRow(
         #actionButton("refresh.map", 
          #label = "Generate boxes map!")
          uiOutput("boxes_opts")
         )
        ) #End of wellpanel 2
       ),#End of bsCollapsePanel
        
        #wellPanel 3: displaying districts mapping
        bsCollapsePanel(
         title = h4(strong("DISPLAY DISTRICTS:"), align = "center",
          style = "color:#006400"), value = "panel3",
        wellPanel(
         style = "background-color:#D3D3D3",
         fluidRow(
          em("Select level of detail, then click the button below to show map"),br(),
          selectInput("detail", 
           label = h5(strong("Summary level of detail:")), 
           choices = list("Regional","District"), 
           selected = "Regional")
         ),
         fluidRow(
          #actionButton("show.admin", label = "Generate filter map")
          uiOutput("districts_opts")
          
         ), br(), tags$hr(),
         fluidRow( #placeholder for summary of detail:
          #to generate after region/district is selected
          uiOutput("drillDown")
         ),
         # Data/map downlaod options
         # Consider moving these onto the map itself (see Shiny modules)
         tags$hr(),
         fluidRow(
          em("Use the button options below to download your map or data:"),br(),br(),
          column(width = 6,
           downloadButton("data.down", label = "Download Data")
          ),
          column(width = 6,
           downloadButton("map.down", label = "Download Map")
          )
         )
         
        )#end of wellPanel 3
         ) #End of bsCollapsePanel
      ) #End of bsCollapse
      ),#sidebarPanel ends here
      # main panel for map and rainfall data loading alert
      mainPanel(
       bsAlert("loadingRainAlert"),
       leafletOutput("map", height = 600)
      )
     )
    )
   )
  )
 )
))
