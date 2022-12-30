total_analytes <- df %>% filter(str_detect(analyte, 'Total')) %>% arrange(n_cl) %>% pull(analyte) %>% unique()

ui <- navbarPage(
  title = 'East St. Louis PCB Sampling Dashboard', 
  windowTitle = 'ESL PCB Dashboard', 
  # theme = bslib::bs_theme(
  #   version = 5,
  #   primary = "#002FA7",
  #   `form-check-input-checked-bg-color` = "#002FA7"
  # ),
  id = 'nav',
  
  ## INTERACTIVE MAP ---------------------------------------------------------
  tabPanel(
    'Interactive Map', icon = icon('map'), 
    
    useShinyjs(),
    div( tags$head(includeCSS('www/styles.css')), 
         tags$head(includeScript('www/script.js')) ),
    
    div(
      class = 'outer',

      # Map output
      # mapdeckOutput('map', width = '100%', height = '100%')
      leafletOutput('map', width = '100%', height = '100%'), 
      
      # Credit
      absolutePanel( 
       id = 'cite', style = 'background-color: white;',
       em("Created by", tags$a("Ankur Singhal", href = 'https://github.com/aksinghal86', target = '_blank'), 
          "at Environmental Health & Engineering, Inc. "),
       br(), 
       span("ATTORNEY-CLIENT PRIVILEGED", style = 'color: red; ')
      )
    ),
    
    #### Map Navigation Panel ####
    absolutePanel(
      id = 'controls', draggable = T, 
      left = 'auto', right = 20, bottom = 'auto', width = 'auto', height = 'auto', 
      
      h3('Map Tools', icon('map')), 
      radioButtons('mapStyle', 
                   label = tags$b('Layer'), 
                   choices = c('Map', 'Terrain', 'Satellite'), 
                   selected = 'Map', 
                   inline = T),
      selectizeInput('mapAnalyteGroup', label = NULL, choices = c('Select analyte' = '', total_analytes)),
      checkboxInput('logtransform', label = 'Log-transform', value = T),
      actionButton('clearLayers', 'Remove heatmap', icon('eraser'))
    ),
    
    #### Layer panel ####
    # # icon('layer-group', 'fa-2x layer-button', id = 'layerButton'), 
    # absolutePanel(
    #   id = 'layerPanel', draggable = F, fixed = T,
    #   
    #   radioButtons('mapVizType', 
    #                label = tags$b("Visualization style"), 
    #                choices = c('Parcel', 'Heatmap', 'IDW'), selected = 'Parcel', 
    #                inline = T)
    # ), 
    # 
    
    #### Information panel ####
    conditionalPanel(
      'input.mapAnalyteGroup != ""',
      absolutePanel(
        id = 'infoPanel', draggable = F, fixed = T,
        # plotOutput('densityPlot', height = '50%'), 
        uiOutput('infoText')
      )
    )
  ), 
  
  
  
  ## SUMMARY CHARTS ----------------------------------------------------------
  # tabPanel(
  #   'Summary Charts', icon = icon('chart-bar'),
  #   tags$p("This tools allows for comparison in emissions for a single or multiple facility over the years. ", 
  #          "Please select the facilities in the dropdown menu below. "), 
  #   tags$br(),
  #   fluidRow(
  #     column(
  #       3, offset = 1, 
  #       selectizeInput('site_name', NULL, 
  #                      choices = c('Select facilities' = '', sort(unique(emissions_for_plot$site_name))), 
  #                      multiple = T, width = '100%'), 
  #     ), 
  #     column(
  #       6,
  #       ggiraphOutput('emissions_plot')
  #     )
  #   )
  # ), 
  
  
  ## DATA TABLE ----------------------------------------------------------
  tabPanel(
    'Data explorer', icon = icon('table'),
    id = 'data-explorer', 
    style = 'width: 65%; margin-left: auto; margin-right: auto; ',
    div(style = 'display: inline-block;', 
         selectizeInput(
           'tableAnalyteGroup', label = NULL,
           choices = c('Select analyte group' = '', df %>% arrange(n_cl) %>% pull(analyte_pr) %>% unique())
           )
        ),

     div(style = 'font-size: 50px; 
           display: inline-block; 
           vertical-align: top; 
           color: #31708f; 
           padding: 0px; 
           margin-top: -22px;',  
         HTML('&#8592;')
         ), 
     div(style = 'display: inline-block; 
            font-size: 16px; 
            vertical-align: middle; 
            color: #31708f; 
            margin-bottom: 22px',  
          'Select PCB congener group to populate table!'),
    
    DT::dataTableOutput('table')
  ),
  
  ## ABOUT ---------------------------------------------------------------------
  # tabPanel(
  #   'About', icon = icon('info'),
  #   tags$p("Created by", tags$a("Ankur Singhal", href = 'https://github.com/aksinghal86', target = '_blank'), 
  #          "at Environmental Health & Engineering, Inc. ",
  #          "Attorney-client privileged")
  #          
  # )
)