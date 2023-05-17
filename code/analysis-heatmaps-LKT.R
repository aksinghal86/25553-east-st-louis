# Analysis

# Libraries -------------------------------------------------------------------
library(tidyverse) 
library(sf) 
library(ggspatial)
library(flextable)
library(patchwork)
library(openxlsx)

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

totals <- sfdf %>% filter(str_detect(analyte, "Total"))

# Maps --------------------------------------------------------------------------

## All studies, zoomed out
ggplot(totals) +
  annotation_map_tile(type = 'cartolight', zoom = 13) +
  geom_sf(fill = NA, size = 0.25) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  # labs(title = 'East St. Louis city boundary (gray outline), former Monsanto plant (or') +
  theme(legend.position = 'none', 
        plot.title.position = 'plot') + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(1, 'cm'), width = unit(1, 'cm'),
                         pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_orienteering)


#EPA Zoomed in
ggplot(totals %>% filter(Study == "USEPA (1976)")) +
  annotation_map_tile(type = 'cartolight', zoom = 14) + 
  geom_sf(aes(color = log(est_conc), size=log(est_conc))) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  coord_sf(xlim = c(-90.19, -90.158), ylim = c(38.585, 38.607), crs = 4326) + 
  theme(
    # axis.text = element_blank(), 
    #  axis.ticks = element_blank()
  )
ggsave('output/estl-zoomed-in-USEPA.png', height = 6, width = 10, units = 'in')

#Hermanson Zoomed in
ggplot(totals %>% filter(Study == "Hermanson (2016)")) +
  annotation_map_tile(type = 'cartolight', zoom = 14) + 
  geom_sf(aes(color = log(est_conc), size=log(est_conc))) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  coord_sf(xlim = c(-90.192, -90.13), ylim = c(38.585, 38.63), crs = 4326) + 
  theme(
    # axis.text = element_blank(), 
    #  axis.ticks = element_blank()
  )
ggsave('output/estl-zoomed-in-Hermanson.png', height = 6, width = 10, units = 'in')

#Gonzalez (2010) Zoomed in
ggplot(totals %>% filter(Study == "Gonzalez (2010)")) +
  annotation_map_tile(type = 'cartolight', zoom = 14) + 
  geom_sf(aes(color = log(est_conc), size=log(est_conc))) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  coord_sf(xlim = c(-90.192, -90.136), ylim = c(38.569, 38.62), crs = 4326) + 
  theme(
    # axis.text = element_blank(), 
    #  axis.ticks = element_blank()
  )
ggsave('output/estl-zoomed-in-Gonzalez.png', height = 6, width = 10, units = 'in')



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
    geom_sf(aes(color=colors),alpha = 0.85, size = 3) +
    geom_sf(data = esl, fill = NA, color = 'gray') +
    geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
    geom_sf(data = monsanto_incin, fill = 'orange', color = NA) +
    scale_colour_identity('Concentration\n(ppm)',
                        labels = bins_and_colors$labels,
                        guide = 'legend') +
    coord_sf(xlim = c(coord_xmin, coord_xmax), ylim = c(coord_ymin, coord_ymax), crs = 4326) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    cowplot::theme_map()
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
  #                        style = north_arrow_orienteering)
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
library(terra)
library(gstat)


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
v = variogram(log(vmods$result) ~ 1, ~ x + y, data = vmods)
mu = fit.variogram(
  v, vgm(psill = max(v$gamma)*.5,
         model = "Sph",
         range = max(v$dist)/2,
         nuggt = mean(v$gamma)/4)
)
k = gstat(id = 'log.result', formula = log(result) ~ 1, locations = ~ x + y, data = vmods, 
          model = mu)

start <-  Sys.time()
kp = terra::interpolate(r, k)
end <- Sys.time()
print(end-start)


plot(kp$log.result.pred)
points(sf)
#terra::text(sf, dat_coords$Sample.ID) ## labels aren't in correct order (1-4 should be in top right)

totals.epa <- totals %>% filter(str_detect(Study, "EPA"))

#EPA Zoomed in
ggplot() +
  annotation_map_tile(type = 'cartolight', zoom = 15) + 
  layer_spatial(kp$log.result.pred, alpha=.85) +
  scale_fill_continuous(type = 'viridis') +
  geom_sf(data=totals.epa) +
  geom_sf_text(data=totals.epa, label = c(totals.epa$Sample.ID),
               nudge_x = 1) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  coord_sf(xlim = c(-90.195, -90.15), ylim = c(38.585, 38.615), crs = 4326)
  

writeRaster(hmapr, paste0('data/rasters/', Study, '.png'), overwrite = T)



library(maptiles)
bg <- get_tiles(ext(sf))
terra::plotRGB(bg)
terra::points(sf)

# MAPPED MODELS

# Create variogram for each analyte group and fit the variogram
vmods <- dat_coords %>%
  filter(analyte == "Total PCBs") %>%
  select(Study, x = lon, y = lat, result = est_conc, ) %>%
  group_by(Study) %>% 
  nest() %>% 
  mutate(
    # Create SpatVector
    sf = map(data, ~ vect(., geom = c('x', 'y'))), 
    # Create variogram for each analyte group
    v = map(data, ~ variogram(log(result) ~ 1, ~ x + y, data = .)),  
    # Fit the variogram
    mu = map(
      v, 
      ~ fit.variogram(
        ., 
        ## Optimization params
        vgm(psill=max(.x$gamma)*0.5,
            model = 'Sph',
            range = max(.x$dist)/2,
            nugget = mean(.x$gamma)/4)
      )), 
    k = map2(
      data, mu, 
      ~ gstat(id = 'log.result', formula = log(result) ~ 1, locations = ~ x + y, data = .x, model = .y)
    )
  )


## Interpolate using the model. 
# This will take some time based on raster dimensions, so beware the curse of dimensionality.
# For a 100 x 100 raster grid, it takes about 5 seconds, but 
# for a 1,000 x 1,000 raster grid, it takes about 7 - 10 mins on my machine
start <-  Sys.time()
vmods <- vmods %>% 
  mutate(kp = map( k, ~ terra::interpolate(r, .x) ))
end <- Sys.time()
print(end-start)

# East St. Louis boundaries from TIGRIS shape files
esl <- tigris::places('Illinois', cb = T) %>%
  tigris::filter_place('East St. Louis') %>% 
  rmapshaper::ms_dissolve() %>% 
  sf::st_transform('EPSG:4326')

# vmods2 <- vmods %>% 
#   mutate(
#     kp_esl = map(kp, ~ terra::mask(.x, esl)), 
#     heatmaps = map(kp_esl, ~ as.polygons(.x))
#   )

save_heatmap_rasters <- function(Study, hmapr) {
  hmapr$Study <- Study
  writeRaster(hmapr, paste0('data/rasters/', Study, '.png'), overwrite = T)
  # writeRaster(hmapr, paste0('dashboard/data/rasters/', Study, '.tif'), overwrite = T)
}

walk2(vmods$Study, vmods2$kp_esl, ~ save_heatmap_rasters(.x, .y))

#install.packages("ggmap")
library(ggmap)

map <- get_map(location = c(lon = -90.15, lat= 38.6), source = "stamen")

