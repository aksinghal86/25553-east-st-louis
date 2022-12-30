library(tidyverse)
library(terra)
library(gstat)

sfdf <- terra::vect('data/gis/all-data-with-geos.shp')
df <- values(sfdf) %>% mutate_at(vars('lon', 'lat', 'est_conc'), as.numeric)

ggplot(df %>% filter(str_detect(analyte, 'Total')), aes(x = lon, y = lat)) + 
  geom_point(aes(color = log(est_conc)), size = 3) + 
  facet_wrap(~analyte) + 
  scale_color_gradient2(low = 'yellow', mid = 'orange', high = 'red') + 
  theme_void()

totals <- df %>% filter(str_detect(analyte, 'Total')) 
totals %>% group_by(analyte) %>% count

# Create raster
# Ideally, this can be done using marmap to get bathymetric grids but I'm unable to do this on my computer. 
# For some reason, there appears to be a conflict between rgdal, raster, and marmap packages
r <- rast(sfdf, ncol = 1000, nrow = 1000)

# This raster is for the whole city but it is a far bigger area than that sampled. 
# r <- rast(nrows = 100, ncols = 100, xmin = -90.18666, ymin = 38.57875, xmax = -90.04169, ymax = 38.64267)
names(r) <- 'log.result'

# Create variogram for each analyte group and fit the variogram
vmods <- totals %>%
  select(analyte, x = lon, y = lat, result = est_conc) %>%
  group_by(analyte) %>% 
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
        vgm(psill=max(.x$gamma)*0.9,
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
esl <- tigris::places('Illinois') %>%
  tigris::filter_place('East St. Louis') %>% 
  rmapshaper::ms_dissolve() %>% 
  sf::st_transform('EPSG:4326')
  
vmods2 <- vmods %>% 
  mutate(
    kp_esl = map(kp, ~ terra::mask(.x, esl)), 
    heatmaps = map(kp_esl, ~ as.polygons(.x))
  )

save_heatmap_rasters <- function(analyte, hmapr) {
  hmapr$analyte <- analyte
  writeRaster(hmapr, paste0('data/rasters/', analyte, '.tif'), overwrite = T)
  writeRaster(hmapr, paste0('dashboard/data/rasters/', analyte, '.tif'), overwrite = T)
}

walk2(vmods2$analyte, vmods2$kp_esl, ~ save_heatmap_rasters(.x, .y))

# 
# 
# xx$polygons[[1]]
# plot(xx$polygons[[1]])
# ggplot(sf::st_as_sf(xx$polygons[[1]])) + geom_sf(aes(fill = log.result.pred)) + scale_fill_viridis_b()
# 
# preds <- vmods %>% 
#   mutate(pred = map(kp, ~ terra::as.data.frame(., xy = T)))
# 
# plots <- preds %>% 
#   mutate(plot = map2(
#     data, pred, 
#     ~ ggplot() + 
#       geom_raster(data = .y, aes(x, y, fill = log.result.pred)) +
#       geom_sf(data = sf::st_as_sf(sfdf[sfdf$analyte == 'Total Mono PCBs', ]), color = 'white', fill = NA) + 
#       geom_point(data = .x, aes(x, y), shape = 4) +
#       scale_fill_viridis_c(direction = -1) +
#       theme_void() + 
#       theme(legend.position = 'none')
#     ))
# 
# plots$plot[[5]] 
# plots %>% select(analyte, pred, plot) %>% write_rds('data/heatmaps.rds')
# plots %>% select(analyte, pred, plot) %>% write_rds('dashboard/data/heatmaps.rds')
# # lapply(vmods$kp, function (x) plot(x$log.result.pred))
# 
# 
# heptas <- df %>% filter(analyte == 'Total Hepta PCBs') %>% select(x = lon, y = lat, result = est_conc)
# r <- rast(sfdf, ncol = 100, nrow = 100)
# 
# 
# v <- variogram(log(result) ~ 1, ~ x+y, data = heptas)
# mu <- fit.variogram(v, vgm(psill=max(v$gamma)*0.9, model = 'Sph', range=max(v$dist)/2, nugget = mean(v$gamma)/4))
# plot(v, mu)
# k <- gstat(id = "log.result", formula = log(result) ~ 1, locations = ~ x + y, data = heptas, model = mu)
# names(r) <- 'log.result'
# kp <- terra::interpolate(r, k, debug.level = 0)
# 
# plot(kp$log.result.pred)
# 
# 
# 
# idw <- gstat(formula = log(result) ~ 1, locations = ~ x + y, data = heptas, nmax = 7, set = list(idp = 2.0))
# z <- interpolate(r, idw, debug.level = 0, index = 1)
# # z <- mask(z, r)
# plot(z)
# 

xx <- totals %>% 
  select(parcelnumb, analyte, est_conc, x = lon, y = lat) %>% 
  pivot_wider(names_from = analyte, values_from = est_conc) %>% 
  janitor::clean_names()

gCoK <- gstat(NULL, 'monos', log(total_mono_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'dis', log(total_di_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'tris', log(total_tri_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'tetras', log(total_tetra_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'pentas', log(total_penta_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'hexas', log(total_hexa_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'heptas', log(total_hepta_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'octas', log(total_octa_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'nonas', log(total_nona_pc_bs) ~ 1, xx, locations = ~ x + y)
gCoK <- gstat(gCoK, 'decas', log(total_deca_pcb) ~ 1, xx, locations = ~ x + y)

coV <- variogram(gCoK)
plot(coV, type='b', main='Co-variogram')
coV.fit <- fit.lmc(coV, gCoK, vgm(model='Sph', range=1000))
coV.fit
plot(coV, coV.fit, main='Fitted Co-variogram')
coK <- interpolate(r, coV.fit)
plot(coK)


plotdata <- terra::as.data.frame(coK, xy = T) %>%
  select(x, y, contains('pred')) %>% 
  pivot_longer(contains('pred'), names_to = 'analyte', values_to = 'value')
ggplot(plotdata, aes(x, y, fill = value)) + 
  geom_raster() + 
  facet_wrap(~ analyte) + 
  scale_fill_distiller(palette = "YlGnBu", direction = -1) +
  theme_void()
