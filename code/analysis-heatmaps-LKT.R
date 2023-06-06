# Analysis

# Libraries -------------------------------------------------------------------
library(tidyverse) 
library(sf) 
library(ggspatial)
library(flextable)
library(patchwork)
library(openxlsx)
library(terra)
library(gstat)

# Data -------------------------------------------------------------------------

coords <- read.xlsx('data/Sample_Coordinates.xlsx')
dat    <- read.xlsx('data/pcb-conc-from-studies.xlsx')

esl      <- sf::st_read('dashboard/data/esl.kml')
monsanto <- sf::st_read('dashboard/data/gis/monsanto.kml', quiet = T) %>% sf::st_zm()
monsanto_storage <- st_read('dashboard/data/gis/monsanto-storage.kml', quiet = T) %>% st_zm()
monsanto_incin   <- st_read('dashboard/data/gis/monsanto-incinerator.kml', quiet = T) %>% st_zm()


negative <- function(x){ 
  x <- as.numeric(paste("-", x, sep = "")) 
  return(x)
}

dat_coords <- dat %>%
  left_join(coords, by = c('Study', 'Matrix', 'Sample.ID'='Sample_ID')) %>%
  select(-contains("QC")) %>%
  rename(lon = Longitude,
         lat = Latitude,
         analyte = Analyte,
         est_conc = Concentration) %>%
  ## Note: some longitude coordinates are pos 90 instead of neg 90, fix:
  mutate(lon = case_when(lon > 0 ~ negative(lon), TRUE ~ lon))


sfdf <- sf::st_as_sf(dat_coords, coords=c('lon','lat'))
st_crs(sfdf) <- 4326

totals <- sfdf %>% filter(str_detect(analyte, "Total")) %>%
  mutate(log_conc = log(est_conc))

# write_csv(data.frame(totals), 'data/study_conc_geo.csv')


# Basic Maps --------------------------------------------------------------------------

## All studies, zoomed out
ggplot(totals) +
  annotation_map_tile(type = 'cartolight', zoom = 13) +
  # geom_sf(fill = NA, size = 0.25) + ## points, no color, small size
  geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', linewidth = .75) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  geom_sf(aes(color = Study, size=est_conc, fill = Study), ## colored/sized on log conc with outline
          shape = 21, colour = "black") +
  # labs(title = 'East St. Louis city boundary (gray outline), former Monsanto plant (or') +
  theme(legend.position = 'none', 
        plot.title.position = 'plot') + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(1, 'cm'), width = unit(1, 'cm'),
                         pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_orienteering) +
  scale_size_continuous(limits = c(0, 45))+ #, breaks=c(-2,-1, 0, 1, 2, 3, 4))+
  theme_void() +
  guides(fill = guide_legend("Study"), 
         color = guide_legend("Study"),
         size = guide_legend("PCB Concentration (ppm)")) +
  ggtitle("Total PCB Sample Concentrations",
          subtitle = "Gonzalez (2010), Hermanson (2016), & USEPA (1976)\nEast St. Louis city boundary (gray outline), former Monsanto plant (orange)")
ggsave('output/estl-all-studies-color.jpg', height = 6, width = 10, units = 'in')

#EPA bubble chart
ggplot(totals %>% filter(Study == "USEPA (1976)")) +
  annotation_map_tile(type = 'cartolight', zoom = 14) + 
  geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
  geom_sf(data = monsanto, fill = NA, color = 'steelblue4', linewidth = .75) +
  geom_sf(data = monsanto_incin, fill = 'steelblue4', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'steelblue4', color = NA) +
  geom_sf(aes(color = est_conc, 
              size = est_conc,
              fill = est_conc),
          shape = 21, colour = "black") +
  coord_sf(xlim = c(-90.192, -90.13), ylim = c(38.585, 38.63), crs = 4326) + 
#  scale_fill_viridis_b(option="inferno", direction = -1,
#                       limits = c(0, 25),
#                       breaks=c(0, 5, 10, 15, 20, 25))+
  scale_fill_gradientn(colours = c("#FAEBDDFF", '#F6AA82FF', '#F06043FF', "#CB1B4FFF",
                                   "#611F53FF", "#30173AFF"),
                       name =  "PCB Concentration (ppm)",
                       limits = c(0, 30),
                       breaks=c(0, 5, 10, 15, 20, 25))+
  #  scale_fill_distiller(palette = "YlGnBu", direction = 1,
  #                       limits = c(0,25),
  #                       breaks=c(0, 5, 10, 15, 20, 25))+
  scale_size_continuous(limits = c(0, 25), 
                        breaks=c(0, 5, 10, 15, 20, 25))+
  theme_void() +
  guides(fill = guide_legend("PCB Concentration (ppm)"), 
         color = guide_legend("PCB Concentration  (ppm)"),
         size = guide_legend("PCB Concentration (ppm)")) +
  ggtitle("USEPA (1976) Total PCB Soil Sample Concentrations",
          subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)")

ggsave('output/estl-bubble-USEPA.jpg', height = 6, width = 10, units = 'in')



#Hermanson bubble chart
ggplot(totals %>% filter(Study == "Hermanson (2016)")) +
  annotation_map_tile(type = 'cartolight', zoom = 14) + 
  geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
  geom_sf(data = monsanto, fill = NA, color = 'steelblue4', linewidth = .75) +
  geom_sf(data = monsanto_incin, fill = 'steelblue4', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'steelblue4', color = NA) +
  geom_sf(aes(color = est_conc, 
              size = est_conc,
              fill = est_conc),
          shape = 21, colour = "black") +
  coord_sf(xlim = c(-90.192, -90.13), ylim = c(38.585, 38.63), crs = 4326) + 
  # scale_fill_viridis_b(option="inferno", direction = -1,
  #                      limits = c(0, 3),
  #                      breaks=c(0, 0.5, 1, 1.5, 2.0, 2.5))+
  scale_fill_gradientn(colours = c("#FAEBDDFF", '#F6AA82FF', '#F06043FF', "#CB1B4FFF",
                                   "#611F53FF", "#30173AFF"),
                       name =  "PCB Concentration (ppm)",
                       limits = c(0, 3),
                       breaks=c(0, 0.5, 1, 1.5, 2.0, 2.5))+
  scale_size_continuous(limits = c(0, 3),
                        breaks=c(0, 0.5, 1, 1.5, 2.0, 2.5))+
  theme_void() +
  guides(fill = guide_legend("PCB Concentration (ppm)"), 
         color = guide_legend("PCB Concentration  (ppm)"),
         size = guide_legend("PCB Concentration (ppm)")) +
  ggtitle("Hermanson (2016) Total PCB Tree Bark Sample Concentrations",
          subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)")
ggsave('output/estl-bubble-Hermanson.jpg', height = 6, width = 10, units = 'in')

#Gonzalez (2010) bubble chart
ggplot(totals %>% filter(Study == "Gonzalez (2010)")) +
  annotation_map_tile(type = 'cartolight', zoom = 14) + 
  geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
  geom_sf(data = monsanto, fill = NA, color = 'steelblue4', linewidth = .75) +
  geom_sf(data = monsanto_incin, fill = 'steelblue4', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'steelblue4', color = NA) +
  geom_sf(aes(color = est_conc, 
              size = est_conc,
              fill = est_conc),
          shape = 21, colour = "black") +
  coord_sf(xlim = c(-90.192, -90.13), ylim = c(38.585, 38.63), crs = 4326) + 
  scale_fill_gradientn(colours = c("#FAEBDDFF", '#F6AA82FF', '#F06043FF', "#CB1B4FFF",
                                   "#611F53FF", "#30173AFF"),
                       name =  "PCB Concentration (ppm)",
                       limits = c(0, 50),
                       breaks=c(0, 5, 10, 20, 30, 40))+
  scale_size_continuous(limits = c(0, 50),
                        breaks=c(0, 5, 10, 20, 30, 40))+
  theme_void() +
  guides(fill = guide_legend("PCB Concentration (ppm)"), 
         color = guide_legend("PCB Concentration  (ppm)"),
         size = guide_legend("PCB Concentration (ppm)")) +
  ggtitle("Gonzalez (2010) Total PCB House Dust Sample Concentrations",
          subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)")
ggsave('output/estl-bubble-Gonzalez.jpg', height = 6, width = 10, units = 'in')



# colfunc_reds <- colorRampPalette(c('#f79540', '#fc4f00', '#b22222', '#b71375', '#8b1874', '#581845'))
colfunc_grays <- colorRampPalette(c('#d6d6d6', '#bdbdbd', '#949494', '#737373', '#4d4d4d', '#262626'))
# reds <- colfunc_reds(20)
reds <- viridis::viridis_pal(direction = -1, option = 'F')(25)
grays <- colfunc_grays(25)
scales::show_col(reds)
scales::show_col(grays)


bins_and_colors <- tibble(breaks = cut(totals$est_conc, breaks = c(0.034, 0.25, 0.50, 1, 5, 10, Inf))) |>
  distinct(breaks) |>
  #  add_row(breaks="(0.0,0.034]") |>
  arrange(breaks) |>
  mutate(labels = factor(c('<0.25', '0.25 - 0.5', '0.5 - 1', '1 - 5', '5 - 10', '>10')),
         cutoff_1 = factor(c(grays[10], grays[16], grays[22], reds[11], reds[16], reds[21])),
         cutoff_0.25 = factor(c(grays[10], reds[6], reds[8], reds[11], reds[16], reds[21])),
         cutoff_0.034 = factor(c(reds[2], reds[6], reds[8], reds[11], reds[16], reds[21])))


basemap <- function(cutoff = 0.034, study,
                    coord_xmin = -90.172, coord_xmax= -90.148,
                    coord_ymin = 38.595, coord_ymax= 38.612) {
  dfcol <- paste0('cutoff_', cutoff)
  bc <- bins_and_colors |> select(breaks, labels, colors = dfcol)
  
  mapdata <- totals |>
    filter(Study == study) |>
    mutate(breaks = cut(est_conc, breaks = c(0.034, 0.25, 0.50, 1, 5, 10, Inf))) |>
    left_join(bc, by = 'breaks') |>
    mutate(labels = factor(labels, levels = bc$labels),
           colors = factor(colors, levels = bc$colors))
  
  ggplot(mapdata) +
    annotation_map_tile(type = 'cartolight', zoom = 15) +
    geom_sf(aes(color=colors, size=colors),alpha = 0.85) +
    geom_sf(data = esl, fill = NA, color = 'gray') +
    geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
    geom_sf(data = monsanto_incin, fill = 'orange', color = NA) +
    labs(color  = "Guide name", size = "Guide name") +
    scale_colour_identity('Concentration\n(ppm)',
                          labels = bins_and_colors$labels,
                          guide = 'legend') +
    scale_size('Concentration\n(ppm)',
               labels = bins_and_colors$labels,
               guide = 'legend') +
    coord_sf(xlim = c(coord_xmin, coord_xmax), ylim = c(coord_ymin, coord_ymax), crs = 4326) +
    annotation_scale(location = "br", width_hint = 0.2) +
    cowplot::theme_map() +
    annotation_north_arrow(location = "br", which_north = "true",
                           pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_orienteering) 
}

basemap(cutoff = 0.034, "Gonzalez (2010)", -90.192,-90.136, 38.569, 38.62) + 
  labs(title = 'Gonzalez (2010) Total PCB Sample Concentrations') 
#subtitle = 'Samples with >1 ppm concentration shown as shades of red; otherwise gray')
ggsave('output/map-filled-Gonzalez-2010.png', height = 6, width = 8, units = 'in') 

basemap(cutoff = 0.034, "Hermanson (2016)", -90.192, -90.13, 38.585, 38.63) + 
  labs(title = 'Hermanson (2016) Total PCB Sample Concentrations') 
ggsave('output/map-filled-Gonzalez-2010.png', height = 6, width = 8, units = 'in') 

basemap(cutoff = 0.034, "USEPA (1976)", -90.19, -90.158, 38.585, 38.607) + 
  labs(title = 'USEPA (1976)Total PCB Sample Concentrations') 
ggsave('output/map-filled-Gonzalez-2010.png', height = 6, width = 8, units = 'in') 


# Heat maps ---------------------------------------------------------------------


#### Model with nested df and functions:


# Create raster - setting to the boundaries of all study points plus some extra room
r <- rast(nrows = 1000, ncols = 1000,   
          xmin = min(dat_coords$lon)-.01, xmax = max(dat_coords$lon)+.01, 
          ymin = min(dat_coords$lat)-.01, ymax = max(dat_coords$lat)+.01)
names(r) <- 'result' #'log.result'


# library(maptiles)
# bg <- get_tiles(ext(sf))
# terra::plotRGB(bg)
# terra::points(sf)

# MAPPED MODELS

# Create variogram for each Study group and fit the variogram
vmods <- dat_coords %>%
  filter(analyte == "Total PCBs") %>%
  select(Study, x = lon, y = lat, result = est_conc, ) %>%
  group_by(Study) %>% 
  nest() %>% 
  mutate(
    ## Create SpatVector
    sf = map(data, ~ vect(., geom = c('x', 'y'))), 
    ## Create variogram for each study group
    #    v = map(data, ~ variogram(log(result) ~ 1, ~ x + y, data = .)),  
    v = map(data, ~ variogram(result ~ 1, ~ x + y, data = .)), 
    ## Fit the variogram
    mu = map(
      v, ~ fit.variogram(., 
                         ## Optimization params -- may need to refine
                         vgm(psill=max(.x$gamma)*0.9,
                             model = 'Sph',
                             range = max(.x$dist)/2,
                             nugget = mean(.x$gamma)/4)
      )), 
    k = map2(
      data, mu, ~ gstat(id = 'result', formula = result ~ 1,
                        # id = 'log.result', formula = log(result) ~ 1,
                        locations = ~ x + y, data = .x, model = .y))
  )


## Interpolate using the model. 
# This will take some time based on raster dimensions, so beware the curse of dimensionality.
start <-  Sys.time()
vmods <- vmods %>% 
  mutate(kp = map( k, ~ terra::interpolate(r, .x) ))
end <- Sys.time()
print(end-start)


# Plot the models: v1 is blue/yellow color scheme
plot_heat_v1 <- function(study, type){
  
  totals.study <- totals %>% filter(Study == study)
  vmods2 <- vmods %>% filter(Study == study)
  
  ggplot() +
    ## Add base map
    annotation_map_tile(type = 'cartolight', zoom = 15) + 
    
    ## Add raster -- alpha so we can slight see base map
    layer_spatial(vmods2$kp[[1]]$result.pred, alpha=.75) +
    
    ## Colors -- calling "v1" plots the YlGnBu version, "v2" the virids version
    scale_fill_distiller(palette = "YlGnBu", direction = -1, name = "PCB Concentration (ppm)") +
    
    ## Add sample points
    geom_sf(data=totals.study, fill=NA) +
    
    ## Add E St Louis + Monsanto outlines:
    geom_sf_text(data=totals.study, label = c(totals.study$Sample.ID),
                 nudge_x = 1) +
    geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
    geom_sf(data = monsanto, fill = NA, color = 'orange', linewidth = .75) +
    geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
    geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
    
    ## Map size
    coord_sf(xlim = c(min(vmods2$data[[1]]$x)-.005, max(vmods2$data[[1]]$x)+.005),
             ylim = c(min(vmods2$data[[1]]$y)-.005, max(vmods2$data[[1]]$y)+.005), 
             crs = 4326) +
    
    ## Add Labels to points -- not working... fix at some point?
    # geom_sf_text(data=totals.study, label = c(totals.study$Sample.ID),
    #               nudge_x = 1) +
    
    ## output settings of map
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(title = paste0(study,' Total PCB ', type, ' Sample Concentrations'),
         subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (orange)") 
  
}

#v2 is yellow to purple color scheme
plot_heat_v2 <- function(study, type){
  
  totals.study <- totals %>% filter(Study == study)
  vmods2 <- vmods %>% filter(Study == study)
  
  ggplot() +
    ## Add base map
    annotation_map_tile(type = 'cartolight', zoom = 15) + 
    
    ## Add raster -- alpha so we can slight see base map
    layer_spatial(vmods2$kp[[1]]$result.pred, alpha=.75) +
    
    ## Colors -- calling "v1" plots the YlGnBu version, "v2" the virids version
    scale_fill_viridis_c(option = "plasma", direction = -1, name = "PCB Concentration (ppm)") +
    
    ## Add sample points
    geom_sf(data=totals.study, fill=NA) +
    
    ## Add E St Louis + Monsanto outlines:
    geom_sf_text(data=totals.study, label = c(totals.study$Sample.ID),
                 nudge_x = 1) +
    geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
    geom_sf(data = monsanto, fill = NA, color = 'steelblue', linewidth = .75) +
    geom_sf(data = monsanto_incin, fill = 'steelblue', color = NA) + 
    geom_sf(data = monsanto_storage, fill = 'steelblue', color = NA) +
    
    ## Map size
    coord_sf(xlim = c(min(vmods2$data[[1]]$x)-.005, max(vmods2$data[[1]]$x)+.005),
             ylim = c(min(vmods2$data[[1]]$y)-.005, max(vmods2$data[[1]]$y)+.005), 
             crs = 4326) +
    
    ## output settings of map
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(title = paste0(study,' Total PCB ', type, ' Sample Concentrations'),
         subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)",) 
  
}

#v2 is grey to red/purple color scheme (matches histogram)
plot_heat_v3 <- function(study, type){
  
  totals.study <- totals %>% filter(Study == study)
  vmods2 <- vmods %>% filter(Study == study)
  
  ggplot() +
    ## Add base map
    annotation_map_tile(type = 'cartolight', zoom = 15) + 
    
    ## Add raster -- alpha so we can slight see base map
    layer_spatial(vmods2$kp[[1]]$result.pred, alpha=.75) +
    
    ## Colors -- v3 colors 
    scale_fill_gradientn(colours = c("#FAEBDDFF", '#F6AA82FF', '#F06043FF', "#CB1B4FFF",
                                     "#611F53FF", "#30173AFF"),
                         name =  "PCB Concentration (ppm)") +
    
    ## Add sample points
    geom_sf(data=totals.study, fill=NA) +
    
    ## Add E St Louis + Monsanto outlines:
    geom_sf_text(data=totals.study, label = c(totals.study$Sample.ID),
                 nudge_x = 1) +
    geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
    geom_sf(data = monsanto, fill = NA, color = 'steelblue', linewidth = .75) +
    geom_sf(data = monsanto_incin, fill = 'steelblue', color = NA) + 
    geom_sf(data = monsanto_storage, fill = 'steelblue', color = NA) +
    
    ## Map size
    coord_sf(xlim = c(min(vmods2$data[[1]]$x)-.005, max(vmods2$data[[1]]$x)+.005),
             ylim = c(min(vmods2$data[[1]]$y)-.005, max(vmods2$data[[1]]$y)+.005), 
             crs = 4326) +
    
    ## output settings of map
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(title = paste0(study,' Total PCB ', type, ' Sample Concentrations'),
         subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)",) 
  
}

plot_heat_v1("USEPA (1976)","Soil")
ggsave('output/heatmap-EPA1976-v1.png',  height = 6, width = 8, units = 'in') 

plot_heat_v2("USEPA (1976)","Soil")
ggsave('output/heatmap-EPA1976-v2.png',  height = 6, width = 8, units = 'in') 

plot_heat_v3("USEPA (1976)","Soil")
ggsave('output/heatmap-EPA1976-v3.png',  height = 6, width = 8, units = 'in') 

plot_heat_v1("Hermanson (2016)", "Tree Bark")
ggsave('output/heatmap-Hermanson2016-v1.png', height = 6, width = 8, units = 'in') 

plot_heat_v2("Hermanson (2016)", "Tree Bark")
ggsave('output/heatmap-Hermanson2016-v2.png', height = 6, width = 8, units = 'in') 

plot_heat_v3("Hermanson (2016)", "Tree Bark")
ggsave('output/heatmap-Hermanson2016-v3.png', height = 6, width = 8, units = 'in') 

plot_heat_v1("Gonzalez (2010)", "House Dust") 
ggsave('output/heatmap-Gonzalez2010-v1.png', height = 6, width = 8, units = 'in') 

plot_heat_v2("Gonzalez (2010)", "House Dust") 
ggsave('output/heatmap-Gonzalez2010-v2.png', height = 6, width = 8, units = 'in') 

plot_heat_v3("Gonzalez (2010)", "House Dust") 
ggsave('output/heatmap-Gonzalez2010-v3.png', height = 6, width = 8, units = 'in') 




# ## Individually made heatmaps - ignore ---------------------------------------------

# Create raster
r <- rast(nrows = 1000, ncols = 1000, xmin = -90.195, xmax = -90.15, ymin = 38.58, ymax = 38.615)
# This raster is for the whole city but it is a far bigger area than that sampled.
# r <- rast(nrows = 100, ncols = 100, xmin = -90.18666, ymin = 38.57875, xmax = -90.04169, ymax = 38.64267)
names(r) <- 'log.result'

r


## SINGLE MODEL
# Create variogram for each analyte group and fit the variogram
vmods <- dat_coords %>%
  filter(analyte == "Total PCBs",
         Study == "USEPA (1976)") %>%
  select(Study, x = lon, y = lat, result = est_conc )

sf = vect(vmods, geom=c('x','y'))
v = variogram(vmods$result ~ 1, ~ x + y, data = vmods)
mu = fit.variogram(
  v, vgm(psill = max(v$gamma)*.5,
         model = "Sph",
         range = max(v$dist)/2,
         nuggt = mean(v$gamma)/4)
)
k = gstat(id = 'log.result', formula = result ~ 1, locations = ~ x + y, data = vmods,
          model = mu)

start <-  Sys.time()
kp = terra::interpolate(r, k)
end <- Sys.time()
print(end-start)


#plot(kp$log.result.pred)
#points(sf)

totals.epa <- totals %>% filter(str_detect(Study, "EPA"))

#EPA Zoomed in
ggplot() +
  annotation_map_tile(type = 'cartolight', zoom = 15) +
  layer_spatial(kp$log.result.pred, alpha=.85) +
  scale_fill_continuous(type = 'viridis', name = "Log(concentration)") +
  geom_sf(data=totals.epa, fill=NA) +
  geom_sf_text(data=totals.epa, label = c(totals.epa$Sample.ID),
               nudge_x = 1) +
  geom_sf(data = esl, fill = NA, color = 'gray') +
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) +
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  coord_sf(xlim = c(-90.195, -90.15), ylim = c(38.580, 38.615), crs = 4326) +
  geom_sf_text(data=totals.epa, label = c(totals.epa$Sample.ID),
               nudge_x = 1) +
  theme(axis.title = element_blank()) +
  labs(title = 'USEPA (1976) Log Transformed Total PCB Sample Concentrations')

# 
# ggsave('output/map-heat-EPA.png', height = 6, width = 8, units = 'in') 
# 
# 
# 
# ## SINGLE MODEL
# # Create variogram for each analyte group and fit the variogram
# # Create raster
# r <- rast(nrows = 1000, ncols = 1000,  
#           xmin = -90.198, xmax = -90.128, ymin = 38.575, ymax = 38.635)
# names(r) <- 'log.result'
# 
# vmods <- dat_coords %>%
#   filter(analyte == "Total PCBs",
#          Study == "Hermanson (2016)") %>%
#   select(Study, x = lon, y = lat, result = est_conc ) 
# 
# sf = vect(vmods, geom=c('x','y'))
# v = variogram(log(vmods$result) ~ 1, ~ x + y, data = vmods)
# mu = fit.variogram(
#   v, vgm(psill = max(v$gamma)*.5,
#          model = "Sph",
#          range = max(v$dist)/2,
#          nuggt = mean(v$gamma)/4)
# )
# k = gstat(id = 'log.result', formula = log(result) ~ 1, locations = ~ x + y, data = vmods, 
#           model = mu)
# 
# start <-  Sys.time()
# kp = terra::interpolate(r, k)
# end <- Sys.time()
# print(end-start)
# 
# 
# #plot(kp$log.result.pred)
# #points(sf)
# 
# totals.hm <- totals %>% filter(str_detect(Study, "Hermanson"))
# 
# #EPA Zoomed in
# ggplot() +
#   annotation_map_tile(type = 'cartolight', zoom = 15) + 
#   layer_spatial(kp$log.result.pred, alpha=.85) +
#   scale_fill_continuous(type = 'viridis', name = "Log(concentration)") +
#   geom_sf(data=totals.hm, fill=NA) +
#   geom_sf_text(data=totals.hm, label = c(totals.hm$Sample.ID),
#                nudge_x = 1) +
#   geom_sf(data = esl, fill = NA, color = 'gray') + 
#   geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
#   geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
#   geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
#   coord_sf(xlim = c(-90.192, -90.13), ylim = c(38.58, 38.63), crs = 4326) +
#   geom_sf_text(data=totals.hm, label = c(totals.hm$Sample.ID),
#                nudge_x = 1) +
#   theme(axis.title = element_blank()) +
#   labs(title = 'Hermanson (2016) Log Transformed Total PCB Sample Concentrations') 
# 
# 
# ggsave('output/map-heat-Hermanson.png', height = 6, width = 8, units = 'in') 
# 
# 
# ## SINGLE MODEL
# # Create variogram for each analyte group and fit the variogram
# # Create raster
# r <- rast(nrows = 1000, ncols = 1000,   
#           xmin = -90.192, xmax = -90.136, ymin = 38.569, ymax = 38.62)
# names(r) <- 'log.result'
# 
# vmods <- dat_coords %>%
#   filter(analyte == "Total PCBs",
#          Study == "Gonzalez (2010)",
#          ### NOTE: there was one point over 40ppm while all others are around 3 and below
#          ###       was causing singular model. Remove to fix model
#          est_conc < 30) %>%
#   select(Study, x = lon, y = lat, result = est_conc ) 
# 
# sf = vect(vmods, geom=c('x','y'))
# v = variogram(log(result) ~ 1, ~ x + y, data = vmods)
# mu = fit.variogram(v, 
#                    vgm(psill = max(v$gamma)*.9,
#                        model = "Sph",
#                        range = max(v$dist)/2,
#                        nuggt = mean(v$gamma)/4)
# )
# k = gstat(id = 'log.result', formula = log(result) ~ 1, locations = ~ x + y,
#           data = vmods, 
#           model = mu)
# 
# start <-  Sys.time()
# kp = terra::interpolate(r, k)
# end <- Sys.time()
# print(end-start)
# 
# 
# #plot(kp$log.result.pred)
# #points(sf)
# 
# totals.hm <- totals %>% filter(str_detect(Study, "Gonzalez"))
# 
# ggplot() +
#   annotation_map_tile(type = 'cartolight', zoom = 15) + 
#   layer_spatial(kp$log.result.pred, alpha=.85) +
#   scale_fill_continuous(type = 'viridis', name = "Log(concentration)") +
#   geom_sf(data=totals.hm, fill=NA) +
#   geom_sf_text(data=totals.hm, label = c(totals.hm$Sample.ID),
#                nudge_x = 1) +
#   geom_sf(data = esl, fill = NA, color = 'gray') + 
#   geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
#   geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
#   geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
#   coord_sf(xlim = c(-90.192,-90.136), ylim = c(38.569, 38.62), crs = 4326) +
#   geom_sf_text(data=totals.hm, label = c(totals.hm$Sample.ID),
#                nudge_x = 1) +
#   theme(axis.title = element_blank()) +
#   labs(title = 'Gonzalez (2010) Log Transformed Total PCB Sample Concentrations') 
# 
# 
# ggsave('output/map-heat-Gonzalez.png', height = 6, width = 8, units = 'in') 
# 



