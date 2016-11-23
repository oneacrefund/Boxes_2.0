#### BASIC INFO ####
# boxes server script
# last edited: 23 nov 2016 (bk)


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
       wellPanel(style = "background-color:#f2f6f9",
        h4(strong("A: SELECT DATA"), align = "center",
         style = "color:#006400"), tags$hr(),
        helpText("Hint: Hover over the sub-sections under 'All Data' to get medata",
         style = "color:firebrick"),
        fluidRow(
         # input data by variable
         column(width = 6, 
          wellPanel(style = "background-color:#E8E8EE",
           h5(strong("All data"), align = "center"), tags$hr(),
           uiOutput("select_all_data")
          )
         ),
         # input data by bundle
         column(width = 6, 
          wellPanel(style = "background-color:#E8E8EE",
           h5(strong("Data bundles"), align = "center"), tags$hr(),
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
             "New Country Expansion",
             "Frontiers"
            )
           )
          )#End of data by bundle wellPanel
         )#End of data by bundle column
        )
       )#End of wellPanel inside whole data column
      ), #End of data column
      
      #Column for selecting geography and map type
      column(width = 6,
       wellPanel(style = "background-color:#f2f6f9",
        fluidRow(
         h4(strong("B: SELECT GEOGRAPHY"), align = "center",
          style = "color:#006400"), tags$hr(), 
         # input resolution
         column(width = 6,
          wellPanel(style = "background-color:#E8E8EE",
           selectInput("res", 
            label = h5(strong("Data resolution")), 
            choices = paste0(c(5, 10, 20, 30, 40, 
             seq(50, 80, 5)), "km"), 
            selected = "10km")
          )
         ),
         # input region for cropping
         column(width = 6,
          wellPanel(style = "background-color:#E8E8EE",
           uiOutput("geoui")
          )
         )
        ),# End of geography fluidRow
        #Setting data selection for rain, yieldgap and cropmix
        fluidRow(
         column( width = 12,
          style = "background-color:#f2f6f9",
          uiOutput("filters_rain1")
         )
        ),
        fluidRow(
         column(width = 12,
          style = "background-color:#f2f6f9",
          uiOutput("yield_choices")
         )
        )
       ),# End of geography/yieldgap/rain data wellPanel
       
       #Setting map type:
       wellPanel(style = "background-color:#f2f6f9",
        fluidRow(
         h4(strong("C: SET MAP TYPE:"), align = "center",
          style = "color:#006400"), tags$hr(),
         column(width = 6,
          wellPanel(style = "background-color:#E8E8EE",
           #Section 1: selecting between boxes and districts map:         
           em("1. First, set your map type by choosing between a boxes map,
          and an admin-boundary-based map:"),
           selectInput("boxes_admin", label = "",
            choices = c("Boxes Map", "Admin-based Map"), selected = "Boxes Map",
            multiple = F, width = "100%"),
           # Section 1 b: UI output for choosing level of detail for admin mapping
           uiOutput("detail")
          )
         ),
         column(width = 6,
          fluidRow(
           wellPanel(style = "background-color:#E8E8EE",
            #Section 2: selecting between heat map and green binary map:
            em("2. Now choose between displaying a filtered binary green map for
           all your selected datasets, and a filtered heat map for only one
           of your datasets:"),
            selectInput("map_type", label = "",
             choices = c("Binary Green Map", "Heat Map"), selected = "Binary Green Map",
             multiple = F, width = "100%")
           )
          )
         )
        )
       ),tags$hr(),
       
       
       fluidRow(
        column(width = 12,
         actionButton("submit.data",
          label = "Done? Click Me!", width = "50%", style = "align:center")
        ),
        
        column(width = 12,
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
          #Section 2 a: UI output for choosing a variable for heat map:
          uiOutput("heatMap_opts"),
          #Section 3: all data filters:
          em("Awesome! As a last step, please set the filters for your
           selected datasets below, and then hit the button just below to
           generate your map to the right side!"), br(), br(),
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
         ), tags$hr(),#End of fluidrow4
         fluidRow (
          style = "background-color:#f2f6f9",
         uiOutput("data_download")
         )
        ) #End of bsCollapsePanel1
       ) #End of bsCollapse
      ),#sidebarPanel ends here
      
      # main panel for map, # of HHs, benchmarks and rainfall data loading alert
      mainPanel(
       #Benchmark data table
       wellPanel(
       fluidRow(
        style = "background-color:#D3D3D3",
        h4(strong("Benchmark figures for selected datasets -- if available:"),
         style = "color:#006400", align = "center")
       ),
       fluidRow(
        style = "background-color:#D3D3D3",
        DT::dataTableOutput("benchmarks")
       )
       ),
       tags$hr(),
       #Rainfall data loading alert
       bsAlert("loadingRainAlert"),
       bsAlert("makingMapAlert"),
       #Separator for benchmark table and map:
       fluidRow(
        style = "background-color:#D3D3D3",
        h4(strong("Your map:"),
         style = "color:#006400", align = "center")
       ),
       #Map output
       fluidRow(
        leafletOutput("map", height = 600) 
       ), tags$hr()
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
       
      )# End of mainPanel
      
     )
    )# End of panel 2
   )# End of main bsCollapse
  )
 ),# End of big fluidRow
 
 ## UI output for map drilldown: creates a third 'panel' / row where a drilldown
 #option is available
 fluidRow(
  column(width = 12,
   uiOutput("ddwnUI")
  )#End of ddwn column
 )
))
