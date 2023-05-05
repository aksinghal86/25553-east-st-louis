server <- function(input, output, session) {
  
  map_selected_data <- reactive({ 
    totals %>% 
      filter(analyte == input$mapAnalyteGroup)
  }) %>% 
    bindEvent(input$mapAnalyteGroup)
  
  map_selected_data_sf <- reactive({ 
    sfdf %>% 
      filter(analyte == input$mapAnalyteGroup) %>% 
      arrange(desc(est_conc)) %>%
      rownames_to_column(var = 'rank') %>%
      mutate(results = case_when(input$logtransform ~ log(est_conc), TRUE ~ est_conc),
             tooltip = paste0('Parcel: ', parcelnumb, '<br>',
                              'Parcel area: ', scales::comma(area_sqft), ' sq ft', '<br>', 
                              '<div style = "text-align: center;">-----------------------</div>', 
                              input$mapAnalyteGroup, ': ', scales::comma(round(est_conc)), ' ppb', '<br>',
                              'Rank: ', rank, '<br>', 
                              'Sample ID(s): ', sample_ids))
  }) %>% 
    bindEvent(input$mapAnalyteGroup, input$logtransform)
  
  
  # Heatmaps 
  heatmap <- reactive({ 
    # Get the heatmap image for the selected analyte
    heatmap_raster <- terra::rast(paste0('data/rasters/', input$mapAnalyteGroup, '.tif'))
    heatmap <- sf::st_as_sf(terra::as.polygons(heatmap_raster, na.rm = T))
    heatmap$fillby <- if (input$logtransform) heatmap$log.result.pred else exp(heatmap$log.result.pred)
    heatmap
  }) %>% 
    bindEvent(input$mapAnalyteGroup, input$logtransform)
  
  ## INFO PANEL ----------------------------------------------------------------
  
  ####  UI components ----------------------------------------------------------
  
  # # Slider input enable / disable
  # observe({ 
  #   toggleState('threshold', condition = input$mapAnalyteGroup != '')
  # })
  
  observeEvent(input$mapAnalyteGroup, { 
    updateSliderInput(session, 'threshold', max = signif(max(map_selected_data()$est_conc), 2), value = 0)  
  })
  
  
  observeEvent(input$setThreshold, {
    updateSliderInput(session, 'threshold', value = 1000)
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
      # filter(!is.na(n_cl)) %>%
      group_by(analyte, analyte_pr, n_cl) %>% 
      summarize(detected = mean(detected)*100) %>% 
      mutate(highlight = case_when(analyte == input$mapAnalyteGroup ~ '#bf7e65'))

    ggplot(plotdata, aes(x = reorder(analyte_pr, n_cl), y = detected)) + 
      geom_point_interactive(aes(tooltip = detected, data_id = analyte_pr, color = highlight), #color = if( input$mapAnalyteGroup == 'Total PCBs') NULL else highlight), 
                 alpha = 0.85, size = 2) +
      geom_segment(aes(xend = analyte_pr, y = 0, yend = detected, 
                       color = highlight), #color = if( input$mapAnalyteGroup == 'Total PCBs') NULL else highlight), 
                   alpha = 0.85, size = 0.85) + 
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

  }) 
  
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
      annotation_logticks(sides = 'b') + 
      labs(x = 'Concentration, log scale (ppb)', y = 'Density', 
           title = 'Distribution of data on log scale', 
           subtitle = 'NDs = DL/2')
    else
      p +
      labs(x = 'Concentration (ppb)', y = 'Density', title = 'Distibution of data', 
           subtitle = 'NDs = DL/2')
    
  })
  
  
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
  }, width = '100%', colnames = T, spacing = 'xs') 
  
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
        weight = 3,
        fill = NA,
        fillOpacity = 0.10,
      ) %>%
      addPolygons(
        group = 'esl', 
        data = esl, 
        fillOpacity = 0, 
        weight = 2.5, 
        dashArray = "5,10", 
        label = 'East St. Louis City Boundary',
        labelOptions = labelOptions(textsize = '12px')
      ) %>% 
      addPolygons(
        group = 'monsanto-storage', 
        data = monsanto_storage,
        color = '#c12c2c', 
        weight = 2.5, 
        fill = '#c12c2c', 
        fillOpacity = 0.8,
        label = 'Former PCB manufacturing facility', 
        labelOptions = labelOptions(textsize = '12px')
      ) %>% 
      addPolygons(
        group = 'monsanto-incinerator', 
        data = monsanto_incin,
        color = '#c1522c', 
        weight = 2.5, 
        fill = '#c1522c', 
        fillOpacity = 0.8,
        label = 'Former PCB incinerator', 
        labelOptions = labelOptions(textsize = '12px')
      ) %>% 
      addMarkers(
        layerId = 'monsanto', 
        # lng = -90.16982, 
        # lat = 38.59643, 
        data = st_centroid(monsanto),
        label = 'Monsanto', 
        labelOptions = labelOptions(noHide = F, textsize = '12px')
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

    pal <- colorNumeric('YlOrRd', heatmap()$fillby)
    pal2 <- colorNumeric('YlOrRd', map_selected_data_sf()$results)
    
    leafletProxy('map') %>%
      clearGroup('heatmap') %>%
      clearControls() %>%
      addPolygons(
        group = 'heatmap',
        data = heatmap(),
        weight = 0.2,
        fillColor = ~ pal(fillby),
        fillOpacity = ~ input$heatmapTransparency,
        smoothFactor = 0.8
      ) %>%
      addLegend(
        data = map_selected_data_sf(),
        position = 'bottomright',
        pal = pal2,
        bins = 5,
        values = ~ results,
        labFormat = labelFormat(transform = function(x) if (input$logtransform) round(exp(x)) else x),
        title = HTML('Concentration<br>(ppb)')
      )
  })
  
  
  observe({
    req(input$mapAnalyteGroup)
    
    
    parceldata_sf <- map_selected_data_sf() |> 
        filter(est_conc > input$threshold) 
    
    if (nrow(parceldata_sf) == 0) return(NULL)
    # 
    # parceldata_sf <- map_selected_data_sf() %>%
    #   mutate(fill_opacity = case_when(est_conc > input$threshold ~ 0.6, TRUE ~ 0),
    #          weight = case_when(est_conc > input$threshold ~ 0.8, TRUE ~ 0.05),
    #          opacity = case_when(est_conc > input$threshold ~ 0.5, TRUE ~ 0))

    pal2 <- colorNumeric('YlOrRd', map_selected_data_sf()$results)
    
    ## In case we want to superimpose an x mark on top of parcels (see below)
    # xmark <- makeIcon(
    #   iconUrl = 'https://cdn-icons-png.flaticon.com/128/2976/2976286.png',
    #   iconWidth = 5, iconHeight = 5
    # )
    
    leafletProxy('map') %>%
      clearGroup('parcelConc') %>%
      # clearShapes() %>%
      addPolygons(
        group = 'parcelConc',
        data = parceldata_sf,
        color = 'black',
        weight = 0.8, 
        # weight = ~weight,
        opacity = 0.5,
        fillColor = ~ pal2(results),
        fillOpacity = ~ input$heatmapTransparency - 0.2, 
        # fillOpacity = ~fill_opacity,
        label = ~ lapply(tooltip, HTML),
        labelOptions = labelOptions(
          textsize = '12px', className = 'leafletLabel',
          style = list(
            "background-color" = "#f5f5f5",
            'border-color' = '#f5f5f5'
          ))
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

  # # This removes all layers and resets the dropdown menu
  # observeEvent(input$clearLayers, {
  # 
  #   leafletProxy('map') %>%
  #     clearGroup(c('heatmap')) 
  # 
  #   
  # })
  
  
  #### Windrose modal ----------------------------------------------------------
  observe({
    if(input$windrose) {
      showModal(shinyjqui::draggableModalDialog(
        tags$img(src = 'windrose.png',
                 alt = 'Windrose from Diagram 3 of US EPA (2011) archives', 
                 width = '80%'),
        tags$br(),
        tags$p(
          'Figure from Diagram 3 of US EPA (2011) archives. Original figure from report produced by RME.
          Winds are predominantly southerly winds in the region, while sometimes also blowing from the west.', 
          br(), br(), 
          'Windrose was constructed by RME with five-year surface meteorological data from the  
          St. Louis Lambert Field Airport (Station ID: 13994) and upper air meteorological data
          from the Salem, Illinois Airport station (Station ID: 3879).'
        ), 
        easyClose = F,
        footer = tagList(actionButton('close', 'Close'))
      ))
    }
    
  })
  
  observeEvent(input$close, {
    removeModal()
    shinyjs::reset('windrose')
  })
  

  ## DATA TABLE ----------------------------------------------------------------
  output$table <- renderReactable({
    tabledata <- df |> 
      # mutate(analyte_pr = factor(analyte_pr, levels = c('Mono', 'Di', 'Tri', 'Tetra', 'Penta', 'Hexa', 'Hepta', 'Octa', 'Nona', 'Deca', 'Total'))) |> 
      select(-geoid, -city, -county, -lat, -lon, -area_acre, -units, -sampling_d, -est_conc, -est_method, -sample_ids, -lab_ids) |> 
      filter(!str_detect(analyte, 'Total')) 
    
    reactable(
      tabledata, 
      
      # striped = T, 
      showSortable = T, 
      filterable = T, 
      bordered = T,
      highlight = T,
      compact = T, 
      pagination = T,
      showPageSizeOptions = T, 
      pageSizeOptions = c(10, 25, 50, 100), 
      defaultPageSize = 10, 
      
      # defaultSorted = c('analyte_pr', 'analyte'), 
      defaultColDef = colDef(
        headerClass = 'header', 
        # footerClass = 'footer', 
        align = 'left'
      ), 
      
      rowClass = JS('function (rowInfo) { 
        let detected = rowInfo.values["detected"]
        
        if (detected == 0) { 
          return "not-detected"
        } 
      }'),
      
      groupBy = c("parcelnumb", "analyte_pr"), 
      
      columns = list(
        analyte_pr = colDef(
          'Homolog'  
        ), 
        
        n_cl = colDef(
          show = F
        ), 
        
        parcelnumb = colDef(
          'Parcel', 
          minWidth = 125
        ), 
        
        area_sqft = colDef(
          'Parcel area (sq. ft.)',
          aggregate = 'mean', 
          filterable = F
        ), 
        
        lab = colDef(
          'Lab'
        ), 
        
        analyte = colDef(
          'Analyte'
        ), 
        
        conc = colDef(
          aggregate = 'sum',
          'Concentration (ppb)', 
          align = 'right', 
          format = colFormat(digits = 1)
        ), 
        
        detected = colDef(
          'Detected',
          filterable = F, 
          align = 'right', 
          aggregate = JS('function (values, rows) { 
            let totalAnalytes = 0
            let totalDetected = 0
            rows.forEach(function(row) {
              totalDetected += row["detected"]
              totalAnalytes += 1
            })
            let detectedPct = totalDetected/totalAnalytes * 100
            return detectedPct.toFixed(1) + "%"
          }'), 
          
          cell = JS('function (colInfo) { 
            if (colInfo.value == 1) { 
              return "\u2713"
            }
          }'), 
          style = JS('function(rowInfo) { 
            const value = rowInfo.values["detected"]
            // let color, background, fontWeight
            
            if (value == 1) { 
              return { color: "#111", background: "#63c1ab", fontWeight: "bold" }
            }
            
            
          }'),
          maxWidth = 100, 
        )
      )
      
    )
  })
  
  observeEvent(input$expandTable, { 
    updateReactable('table', expanded = T)  
  })
  # 
  # output$table <- DT::renderDataTable({ 
  #   tabledata <- df %>% 
  #     filter(analyte_pr == input$tableAnalyteGroup, 
  #            !str_detect(analyte, 'Total'), 
  #            est_conc > 0) %>% 
  #     select(parcelnumb, analyte, lab, detected, est_conc) %>% 
  #     mutate(detected = as.logical(detected))
  #   
  #   DT::datatable(
  #     tabledata,
  #     colnames = c('Parcel', 'Lab', 'Analyte', 'Detected', 'Concentration (ppb)'),
  #     class = 'compact',
  #     filter = 'top',
  #     options = list(
  #       autoWidth = T, 
  #       columnDefs = list(
  #         list(className = 'dt-center', targets = c(1, 3, 4)), 
  #         list(width = '80px', targets = c(3, 4, 5)),
  #         list(width = '150px', targets = 1)),
  #       pageLength = 20,
  #       searching = F))
  # }) 
  # 
  

}