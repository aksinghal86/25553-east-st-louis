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
         tags$head( 
           tags$script(" 
             // dynamically update Information panel heading
             Shiny.addCustomMessageHandler('info-heading', function (heading) { 
                $( '#js_heading ').html(heading);
             }); 
           ")
         ) 
    ),
    
    div(
      class = 'outer',

      # Map output
      leafletOutput('map', width = '100%', height = '100%'), 
      
      # Credit
      absolutePanel( 
       id = 'cite', style = 'background-color: white;',
       em("Created by", tags$a("Ankur Singhal", href = 'https://www.ankursinghal.me', target = '_blank'), 
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
      selectizeInput('mapAnalyteGroup', label = NULL, choices = c('Select congener group' = '', total_analytes)),
      checkboxInput('logtransform', label = 'Log-transform', value = T),
      actionButton('clearLayers', 'Remove heatmap', icon('eraser'))
    ),
    
    
    #### Information panel ####
    absolutePanel(
      id = 'infoPanel', draggable = F, fixed = T,
      
      # This heading is updated dynamically based on user selected analyte.
      h4(id = 'js_heading', style = 'text-align: center;'),

      p(style = 'font-size: 12px; ',
         'Individual parcel results are shown on the map upon selection of the PCB congener group in the dropdown menu to the right.', 
         'Hovering over a parcel will provide the parcel number, total concentration, and the rank.', 
         'In addition, a heatmap will be superimposed with interpolated values in the region of interest.',
         'Possible alternate sources (not comprehensive) are shown as orange markers.',
         br(), br(),
         'Refer to the', em("Data Explorer"), 'tab in the navigation panel for the raw data.'
      ),
      plotOutput('detectionPlot', height = '12vw'), 
      plotOutput('densityPlot', height = '10vw'),
      div(tableOutput('mapTable'), style = 'font-size: 12px;')
    )
  ), 

    
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
    
    DT::dataTableOutput('table', width = '100%')
  )
)