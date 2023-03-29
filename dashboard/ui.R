total_analytes <- totals %>% arrange(n_cl) %>% pull(analyte) %>% unique()
csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename), 
    class = 'btn btn-default', 
    style = 'float: right;'
  )
}


ui <- navbarPage(
  title = 'East St. Louis PCB Sampling Dashboard', 
  windowTitle = 'E. STL PCB Dashboard', 
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
      
      #### Credit ####
      absolutePanel(
       id = 'cite',
       em("Created by", tags$a("Ankur Singhal", href = 'https://www.ankursinghal.me', target = '_blank'), 
          "at Environmental Health & Engineering, Inc. "),
       br(), 
       tags$b('DRAFT'), 
       br(),
       span(tags$b("ATTORNEY-CLIENT PRIVILEGED"), style = 'color: red; ')
      )
    ),
    
    #### Map Navigation Panel ####
    absolutePanel(
      id = 'controls', draggable = F, fixed = T, 
      left = 'auto', right = 20, bottom = 'auto', width = 'auto', height = 'auto', 
      
      h3('Map Tools', icon('map')), 
      radioButtons('mapStyle', 
                   label = tags$b('Layer'), 
                   choices = c('Map', 'Terrain', 'Satellite'), 
                   selected = 'Map', 
                   inline = T),
      selectizeInput('mapAnalyteGroup', label = NULL, 
                     choices = c('Select homolog group' = '', total_analytes), 
                     selected = 'Total PCBs'),
      sliderInput('heatmapTransparency', tags$b('Heatmap transparency'), min = 0, max = 1, step = 0.01, ticks = F, value = 0.8),
      # shinyWidgets::sliderTextInput('heatmapTransparency', tags$b('Heatmap transparency'), choices = seq(0, 1, 0.01), grid = F, selected = 0.8),
      shinyWidgets::switchInput('logtransform', 'Log-transform', value = T, size = 'normal', labelWidth = '150px'),
      shinyWidgets::switchInput('windrose', 'Windrose', value = F, size = 'normal', labelWidth = '150px'),
      
      # shinyWidgets::switchInput('showHeatmap', 'Show heatmap', value = T,  size = 'small', labelWidth = '150px')
    ),
    
    #### Slider panel ####
    conditionalPanel(
      'input.mapAnalyteGroup != ""',
      absolutePanel(
        id = 'sliderPanel', draggable = F, fixed = T, 
        sliderInput('threshold', tags$b('Concentration threshold (ppb)'), 
                    min = 0, max = 23000, round = T, sep = ',', value = 0, step = 100,
                    width = '25vw'), 
        actionButton('setThreshold', 'Set slider to 1,000 ppb', icon = icon('wrench'),  
                     style = 'font-size: 12px; ')
      ), 
      
    ), 
    
    #### Information panel ####
    absolutePanel(
      id = 'infoPanel', draggable = F, fixed = T,
      
      # This heading is updated dynamically based on user selected analyte.
      h4(id = 'js_heading', style = 'text-align: center;'),

      p(style = 'font-size: 12px; ',
         'Individual parcel results are shown on the map upon selection of the PCB congener group in the dropdown menu to the right.', 
         'Hovering over a parcel provides the parcel number, total concentration, and the rank.', 
         'A heatmap is also superimposed with interpolated values in the region of interest.',
         'Former Monsanto plant boundary (Gonzalez et al. 2010) is shown as red outline. ',
         'Slider at the bottom center allows filtering of parcels above the concentration threshold. ',
         br(), br(),
         'Refer to the', em("Data Explorer"), 'tab in the navigation panel for the raw data.'
      ),
      plotOutput('detectionPlot', height = '180px'), 
      plotOutput('densityPlot', height = '125px'),
      div(tableOutput('mapTable'), style = 'font-size: 12px;')
    )
  ), 

    
  ## DATA TABLE ----------------------------------------------------------
  tabPanel(
    'Data explorer', icon = icon('table'),
    id = 'data-explorer', 
     # style = 'width: 95%; margin-left: auto; margin-right: auto; ',
    actionButton('expandTable', 'Show all rows'),
    csvDownloadButton('table', filename = 'estl-pcb-data.csv'),
  
    # DT::dataTableOutput('table', width = '100%')
    div(reactableOutput('table'), style = 'margin-top: 15px;')
  ), 
  
  
  
  tabPanel( 
    'Supporting Info', icon = icon('circle-info'), 
    shiny::includeMarkdown('www/Supporting Info.md'))
)