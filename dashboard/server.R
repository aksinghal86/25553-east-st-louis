server <- function(input, output, session) {
  
  map_selected_data <- eventReactive(input$mapAnalyteGroup, { 
    totals %>% 
      filter(analyte == input$mapAnalyteGroup)
  }) 
  
  map_selected_data_sf <- eventReactive(c(input$mapAnalyteGroup, input$logtransform), { 
    sfdf %>% 
      filter(analyte == input$mapAnalyteGroup) %>% 
      arrange(desc(est_conc)) %>%
      rownames_to_column(var = 'rank') %>%
      mutate(results = case_when(input$logtransform ~ log(est_conc), TRUE ~ est_conc),
             tooltip = paste0('Parcel: ', parcelnumb, '<br>',
                              input$mapAnalyteGroup, ': ', round(est_conc), ' ppb', '<br>',
                              'Rank: ', rank))
  }) 
  
  
  # Heatmaps 
  heatmap <- eventReactive(c(input$mapAnalyteGroup, input$logtransform), { 
    # Get the heatmap image for the selected analyte
    heatmap_raster <- terra::rast(paste0('data/rasters/', input$mapAnalyteGroup, '.tif'))
    heatmap <- sf::st_as_sf(terra::as.polygons(heatmap_raster, na.rm = T))
    heatmap$fillby <- if (input$logtransform) heatmap$log.result.pred else exp(heatmap$log.result.pred)
    heatmap
  })
  
  
  ## INFO PANEL ----------------------------------------------------------------
  
  ####  UI components ----------------------------------------------------------
  
  # # Slider input enable / disable
  # observe({ 
  #   toggleState('threshold', condition = input$mapAnalyteGroup != '')
  # })
  
  observeEvent(input$mapAnalyteGroup, { 
    updateSliderInput(session, 'threshold', max = signif(max(map_selected_data()$est_conc), 2), value = 0)  
  })
  
  ## Dynamically update information panel heading based on user input
  observe({
    heading <- if (input$mapAnalyteGroup == '') { 
      'Information Panel'
    } else { 
      input$mapAnalyteGroup
    }
    
    session$sendCustomMessage('info-heading', heading)
  })
  
  #### Detection plot ----------------------------------------------------------
  output$detectionPlot <- renderPlot({
    req(input$mapAnalyteGroup) 
    
    plotdata <- totals %>% 
      group_by(analyte, analyte_pr, n_cl) %>% 
      summarize(detected = mean(detected)*100) %>% 
      mutate(highlight = case_when(analyte == input$mapAnalyteGroup ~ '#bf7e65'))
    
    ggplot(plotdata, aes(x = reorder(analyte_pr, n_cl), y = detected)) + 
      geom_point(aes(color = highlight), alpha = 0.85, size = 2) +
      geom_segment(aes(xend = analyte_pr, y = 0, yend = detected, color = highlight), alpha = 0.5, size = 0.85) + 
      labs(y = '% detected', x = NULL, title = 'Avg % detected by congener group', 
           subtitle = 'Selected congener group in orange') + 
      coord_flip() + 
      theme_classic() + 
      theme(plot.title.position = 'plot',
            plot.background = element_rect(fill = '#f5f5f5'),
            panel.background = element_rect(fill = '#f5f5f5'),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            text = element_text(size = 12),
            legend.position = 'none')
  }) %>% 
    bindCache(input$mapAnalyteGroup)
  
  
  #### Density plot ------------------------------------------------------------
  output$densityPlot <- renderPlot({
    req(input$mapAnalyteGroup)
    
    plotdata <- map_selected_data()
    
    p <- ggplot(plotdata, aes(x = est_conc)) +
      geom_histogram(aes(y = ..density..), alpha = 0.5, fill = '#bba567', color = '#f5f5f5') +
      geom_density(color = '#bf7e65', size = 1) +
      theme_classic() +
      theme(plot.title.position = 'plot',
            plot.background = element_rect(fill = '#f5f5f5'),
            panel.background = element_rect(fill = '#f5f5f5'),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            text = element_text(size = 12),
            legend.position = 'none')
    if (input$logtransform)
      p +
      scale_x_log10() +
      labs(x = 'Concentration, log scale (ppb)', y = 'Density', title = 'Distribution of data on log scale')
    else
      p +
      labs(x = 'Concentration (ppb)', y = 'Density', title = 'Distibution of data')
    
  }) %>% 
    bindCache(input$mapAnalyteGroup, input$logtransform)
  
  
  
  #### Table for Info panel ---------------------------------------------------
  output$mapTable <- renderTable({
    req(input$mapAnalyteGroup)
    
    tabledata <- map_selected_data()
    tribble(
      ~ Metric, ~ Value,
      "Avg % detected", paste0(round(mean(tabledata$detected), 2)*100, '%'),
      "Arithmetic mean", paste0(round(mean(tabledata$est_conc)), ' ppb'),
      "Geometric mean", paste0(round(exp(mean(log(tabledata$est_conc)))), ' ppb'),
      "Maximum", paste0(round(max(tabledata$est_conc)), ' ppb'),
    )
  }, width = '100%', colnames = T, spacing = 'xs') %>% 
    bindCache(input$mapAnalyteGroup)
  
  ## MAP ---------------------------------------------------------------------
  
  #### Default map ----------------------------------------------------------
  output$map <- renderLeaflet({
    
    # alt_icons <- awesomeIcons(
    #   icon = 'circle-exclamation', 
    #   iconColor = '#fff',
    #   library = 'fa', 
    #   markerColor = 'orange'
    # )
    
    leaflet( options = leafletOptions(zoomControl = F) ) %>%
      setView(lng = -90.17, lat = 38.605, zoom = 14) %>% 
      # addAwesomeMarkers(
      #   data = alt_sources,
      #   lng = ~lon,
      #   lat = ~lat, 
      #   label = ~name,
      #   icon = alt_icons,
      #   labelOptions = labelOptions(noHide = T)
      # ) %>% 
      addPolygons(
        group = 'monsanto', 
        data = monsanto, 
        color = 'red', 
        weight = 2, 
        fill = 'red', 
        fillOpacity = 0.2
      ) %>% 
      addMarkers(
        layerId = 'monsanto', 
        data = st_centroid(monsanto), 
        label = 'Monsanto', 
        labelOptions = labelOptions(noHide = T)
      )
  })

  #### Map styles and layers ---------------------------------------------------
  observeEvent(input$mapStyle, {
    
    if(input$mapStyle == 'Map') {
      leafletProxy('map') %>%
        clearTiles() %>% 
        # addProviderTiles(providers$CartoDB.DarkMatter)
        addProviderTiles(providers$CartoDB.Positron)
    } else if (input$mapStyle == "Terrain") {
      leafletProxy('map') %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldTopoMap)
    } else if (input$mapStyle == 'Satellite') {
      leafletProxy('map') %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addProviderTiles(providers$CartoDB.PositronOnlyLabels)
    }
  })
  
  
  
  #### Data selection, format, plus layered map --------------------------------
  
  # Make the heatmaps and plot on map
  observe({
    req(input$mapAnalyteGroup)

    parceldata_sf <- map_selected_data_sf() %>% 
      mutate(fill_opacity = case_when(est_conc > input$threshold ~ 0.6, TRUE ~ 0), 
             weight = case_when(est_conc > input$threshold ~ 0.8, TRUE ~ 0.05), 
             opacity = case_when(est_conc > input$threshold ~ 0.5, TRUE ~ 0))
    
  
    pal <- colorNumeric('YlOrRd', heatmap()$fillby)
    pal2 <- colorNumeric('YlOrRd', parceldata_sf$results)
    
    ## In case we want to superimpose an x mark on top of parcels (see below)
    # xmark <- makeIcon(
    #   iconUrl = 'https://cdn-icons-png.flaticon.com/128/2976/2976286.png',
    #   iconWidth = 5, iconHeight = 5
    # )
    
    leafletProxy('map') %>%
      clearGroup(c('heatmap', 'parcelConc')) %>%
      # clearShapes() %>%
      clearControls() %>%
      addPolygons(
        group = 'heatmap',
        data = heatmap(),
        weight = 0.2,
        fillColor = ~ pal(fillby),
        fillOpacity = 0.8,
        smoothFactor = 0.8
      ) %>% 
      addPolygons(
        group = 'parcelConc',
        data = parceldata_sf,
        color = 'black',
        weight = ~weight,
        opacity = 0.5,
        fillColor = ~ pal2(results),
        fillOpacity = ~fill_opacity,
        label = ~ lapply(tooltip, HTML),
        labelOptions = labelOptions(
          textsize = '12px', className = 'leafletLabel',
          style = list(
            "background-color" = "#f5f5f5",
            'border-color' = '#f5f5f5'
          ))
      ) %>%
      addLegend(
        data = parceldata_sf,
        position = 'bottomright',
        pal = pal2,
        bins = 5,
        values = ~ results,
        labFormat = labelFormat(transform = function(x) if (input$logtransform) round(exp(x)) else x),
        title = HTML('Concentration<br>(ppb)')
      )
    ## In case we want to superimpose to x mark on top of parcels (see above)
    # addMarkers(
    #   data = parceldata,
    #   lng = ~lon,
    #   lat = ~lat,
    #   icon = xmark,
    #   options = markerOptions(opacity = 0.5)
    # )

  })


  ## MAP TOOLS PANEL  -----------------------------------------------------------

  #### UI components -----------------------------------------------------------

  # This removes all layers and resets the dropdown menu
  observeEvent(input$clearLayers, {

    leafletProxy('map') %>%
      clearGroup(c('heatmap')) 

    
  })


  
  
  ## DATA TABLE ----------------------------------------------------------------
  output$table <- DT::renderDataTable({ 
    tabledata <- df %>% 
      filter(analyte_pr == input$tableAnalyteGroup, 
             !str_detect(analyte, 'Total'), 
             est_conc > 0) %>% 
      select(parcelnumb, analyte, lab, detected, est_conc) %>% 
      mutate(detected = as.logical(detected))
    
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