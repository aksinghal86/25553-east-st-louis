server <- function(input, output, session) { 
  
  
  output$densityPlot <- renderPlot({
    req(input$mapAnalyteGroup)
    
    plotdata <- df %>% filter(analyte == input$mapAnalyteGroup)
    
    p <- ggplot(plotdata, aes(x = est_conc)) +
      geom_histogram(aes(y = ..density..), alpha = 0.5, fill = '#bba567', color = '#f5f5f5') +
      geom_density(color = '#bf7e65', size = 1) +
      theme_classic() +
      theme(plot.title.position = 'plot',
            plot.background = element_rect(fill = '#f5f5f5'),
            panel.background = element_rect(fill = '#f5f5f5'),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            text = element_text(size = 12))
    if (input$logtransform)
      p +
      scale_x_log10() +
      labs(x = 'Concentration, log scale (ppb)', y = 'Density')
    else
      p +
      labs(x = 'Concentration (ppb)', y = 'Density')
    
  }) %>% 
    bindCache(input$mapAnalyteGroup)
  
  output$mapTable <- renderTable({
    req(input$mapAnalyteGroup)
    
    tabledata <- df %>% filter(analyte == input$mapAnalyteGroup)
    tribble(
      ~ Metric, ~ Value,
      # "Avg % detected", paste0(round(mean(tabledata$detected), 2)*100, '%'),
      "Arithmetic mean", paste0(round(mean(tabledata$est_conc)), ' ppb'),
      "Geometric mean", paste0(round(exp(mean(log(tabledata$est_conc)))), ' ppb'),
      "Maximum", paste0(round(max(tabledata$est_conc)), ' ppb'),
    )
  }, width = '100%', colnames = T) %>% 
    bindCache(input$mapAnalyteGroup, input$logtransform)
  
  
  output$infoText <- renderUI({
    req(input$mapAnalyteGroup)
    
    infodata <- df %>% filter(analyte == input$mapAnalyteGroup)
    tagList(
      tags$h4(input$mapAnalyteGroup, style = 'text-align: center; '),
      
      tags$br(), 
      tags$p(style = 'font-size: 12px; ',
             'Individual parcel results for', tolower(input$mapAnalyteGroup), 'are shown on the map.', 
             'Hovering over a parcel will provide the parcel number, total concentration, and the rank.', 
             'In addition, a heatmap is superimposed with interpolated values in the region of interest.',
             'Possible alternate sources (not comprehensive) are shown as orange markers.',
             br(), br(),
             'Pertinent summary statistics are shown below along with a distribution of the data.', 
             'Refer to the', em("Data Explorer"), 'tab in the navigation panel for the raw data.'
      ),
      
      plotOutput('densityPlot', height = '10vw'),
      
      tags$br(), 
      tags$p(style = 'font-size: 12px; ',
             'Approximately', paste0(round(mean(infodata$detected), 2)*100, '%'), 
             'of the individual congeners had a reported value above the detection limit.',
      ),
      
      div(tableOutput('mapTable'), style = 'font-size: 12px'),
    )
    
  }) %>% 
    bindCache(input$mapAnalyteGroup)
  
  ## MAP ---------------------------------------------------------------------
  output$map <- renderLeaflet({
    
    alt_icons <- awesomeIcons(
      icon = 'circle-exclamation', 
      iconColor = '#fff',
      library = 'fa', 
      markerColor = 'orange'
    )
    
    leaflet( options = leafletOptions(zoomControl = F) ) %>%
      setView(lng = -90.17, lat = 38.605, zoom = 14) %>%
      addAwesomeMarkers(
        data = alt_sources,
        lng = ~lon,
        lat = ~lat, 
        label = ~name,
        icon = alt_icons,
        labelOptions = labelOptions(noHide = T)
        
      )
  })

  
  # output$map <- renderMapdeck({
  #   token <- read_lines('assets/mapdeck-token.txt')
  #   mapdeck(style = mapdeck_style('dark'), token = token) %>%
  #     mapdeck_view(location = c(-90.17, 38.605), zoom = 14, pitch = 10)
  # })

  
  ## Map styles and layers
  observeEvent(input$mapStyle, {
    
    if(input$mapStyle == 'Map') {
      leafletProxy('map') %>%
        clearTiles() %>% 
        # addProviderTiles(providers$CartoDB.DarkMatter)
        addProviderTiles(providers$CartoDB.Positron)
      # mapdeck_update(map_id = 'map') %>%
      #   update_style(style = mapdeck_style('dark'))
    } else if (input$mapStyle == "Terrain") {
      leafletProxy('map') %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldTopoMap)
      # mapdeck_update(map_id = 'map') %>%
      #   update_style(style = mapdeck_style('outdoors'))
    } else if (input$mapStyle == 'Satellite') {
      leafletProxy('map') %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addProviderTiles(providers$CartoDB.PositronOnlyLabels)
      # mapdeck_update(map_id = 'map') %>%
      #   update_style(style = mapdeck_style('satellite-streets'))
    }
  })
  
  
  # Format data
  parceldata <- reactive({
    sfdf %>%
      filter(analyte == input$mapAnalyteGroup) %>%
      arrange(desc(est_conc)) %>%
      rownames_to_column(var = 'rank') %>%
      mutate(results = case_when(input$logtransform ~ log(est_conc), TRUE ~ est_conc),
             tooltip = paste0('Parcel: ', parcelnumb, '<br>',
                              input$mapAnalyteGroup, ': ', round(est_conc), ' ppb', '<br>',
                              'Rank: ', rank))
  }) %>% 
    bindCache(input$mapAnalyteGroup, input$logtransform)
  
  observe({ 
    req(input$mapAnalyteGroup) 
    
    heatmap_rasters <- terra::rast(paste0('data/rasters/', input$mapAnalyteGroup, '.tif'))
    heatmaps <- sf::st_as_sf(terra::as.polygons(heatmap_rasters, na.rm = T)) 
    heatmaps$fillby <- if (input$logtransform) heatmaps$log.result.pred else exp(heatmaps$log.result.pred)
    
    pal <- colorNumeric('YlOrRd', heatmaps$fillby)
    pal2 <- colorNumeric('YlOrRd', parceldata()$results)
    # xmark <- makeIcon(
    #   iconUrl = 'https://cdn-icons-png.flaticon.com/128/2976/2976286.png', 
    #   iconWidth = 5, iconHeight = 5
    # )
    
    
    
    leafletProxy('map') %>% 
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(
        data = heatmaps, 
        weight = 0.2, 
        fillColor = ~ pal(fillby), 
        fillOpacity = 0.8, 
        smoothFactor = 0.8
      )  %>% 
      addPolygons(
        data = parceldata(), 
        color = 'black', 
        weight = 0.8,
        opacity = 0.5,
        fillColor = ~ pal2(results), 
        fillOpacity = 0.6, 
        label = ~ lapply(tooltip, HTML), 
        labelOptions = labelOptions(
          textsize = '12px', className = 'leafletLabel',
          style = list(
            "background-color" = "#f5f5f5", 
            'border-color' = '#f5f5f5'
          ))
      ) %>% 
      addLegend(
        data = parceldata(),
        position = 'bottomright', 
        pal = pal2, 
        bins = 5, 
        values = ~ results, 
        labFormat = labelFormat(transform = function(x) if (input$logtransform) round(exp(x)) else x), 
        title = HTML('Concentration<br>(ppb)')
      )  
      # addMarkers(
      #   data = parceldata, 
      #   lng = ~lon, 
      #   lat = ~lat, 
      #   icon = xmark, 
      #   options = markerOptions(opacity = 0.5)
      # )
      # 
    
  })
  
  observeEvent(input$clearLayers, {
    leafletProxy('map') %>% 
      clearShapes() %>% 
      clearControls()
      
  })
  # observe({
  #   req(input$mapAnalyteGroup)
  #   # req(input$mapVizType)
  # 
    # # Format data
    # mapdata <- sfdf %>%
    #   filter(analyte == input$mapAnalyteGroup) %>%
    #   arrange(desc(est_conc)) %>%
    #   rownames_to_column(var = 'rank') %>%
    #   mutate(results = case_when(input$logtransform ~ log(est_conc), TRUE ~ est_conc),
    #          stroke_colour = '#FFFFFF',
    #          tooltip = paste0('Parcel: ', parcelnumb, '<br>',
    #                           input$mapAnalyteGroup, ': ', round(est_conc), ' ppb', '<br>',
    #                           'Rank: ', rank))
  # 
  #   mapdeck_update(map_id = 'map') %>%
  #     clear_polygon('polygon') %>%
  #     clear_heatmap('heatmap')
  # 
  #   if (input$mapVizType == 'Parcel') {
  #     mapdeck_update(map_id = 'map') %>%
  #       add_polygon(
  #         mapdata,
  #         stroke_width = 0.2,
  #         fill_colour = 'results',
  #         fill_opacity = 0.85,
  #         tooltip = 'tooltip',
  #         auto_highlight = T,
  #         update_view = F,
  #         palette = 'diverge_hcl',
  #         layer_id = 'polygon'
  #       )
  #   } else if (input$mapVizType == "Heatmap") {
  #     mapdeck_update(map_id = 'map') %>%
  #       add_heatmap(
  #         mapdata,
  #         lat = 'lat',
  #         lon = 'lon',
  #         weight = 'results',
  #         update_view = F,
  #         layer_id = 'heatmap'
  #       )
  #   } else if (input$mapVizType == 'IDW') {
      # r <- terra::rast(paste0('data/rasters/', input$mapAnalyteGroup, '.tif'))
      # rp <- sf::st_as_sf(terra::as.polygons(r))
  # 
  #     mapdeck_update(map_id = 'map') %>%
  #       add_polygon(
  #         data = rp,
  #         fill_colour = 'log.result.pred',
  #         fill_opacity = 0.75,
  #         palette = 'viridis',
  #         layer = 'krig') %>%
  #       add_polygon(
  #         data = mapdata,
  #         fill_opacity = 0,
  #         stroke_colour = 'stroke_colour', 
  #         stroke_width = 0.8,
  #         tooltip = 'tooltip'
  #       )
  # 
  #   }
  # })
  # 
  # 
  
  
  
  ## DATA TABLE ----------------------------------------------------------------
  
  output$table <- DT::renderDataTable({ 
    tabledata <- df %>% 
      filter(analyte_pr == input$tableAnalyteGroup, 
             !str_detect(analyte, 'Total'), est_conc > 0) %>% 
      select(parcelnumb, analyte, lab, detected, est_conc) %>% 
      mutate(detected = as.logical(detected))
    
   
    
    # reactable(
    #   tabledata, 
    #   striped = T, 
    #   compact = T, 
    #   searchable = F, 
    #   filterable = T, 
    #   pagination = T, 
    #   defaultSorted = c('analyte'), 
    #   defaultPageSize = 25, 
    #   columns = list(
    #     analyte = colDef(
    #       name = 'Analyte', 
    #       minWidth = 250, 
    #       class = 'analyte-col'
    #     ), 
    #     lab = colDef(
    #       name = 'Lab'
    #     ), 
    #     parcelnumb = colDef(
    #       name = 'Parcel'
    #     ), 
    #     detected = colDef(
    #       name = 'Detected', 
    #       class = 'detected-col'
    #     ), 
    #     est_conc = colDef(
    #       name = 'Concentration<br>(ppb)',
    #       class = 'conc-col', 
    #       html = T
    #     )
    #   ), 
    #   borderless = T, 
    #   class = 'explorer-table'
    # )      
    
    DT::datatable(
      tabledata,
      colnames = c('Parcel', 'Lab', 'Analyte', 'Detected', 'Concentration (ppb)'),
      class = 'compact',
      filter = 'top',
      options = list(
        autoWidth = T, 
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 3, 4)), 
          list(width = '80px', targets = c(3, 4, 5)),
          list(width = '150px', targets = 1)),
        pageLength = 20,
        searching = F))

  }) 
  
}