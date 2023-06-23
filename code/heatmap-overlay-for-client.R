
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

# Using the avg conc per parcel (cannot have two points at the same location in heatmap interpolation)
ehe_dat <- terra::vect('data/gis/data-by-parcel.shp')
ehe_dat <- values(ehe_dat) %>%
  mutate_at(vars('lon', 'lat', 'avg_conc'), as.numeric) %>%
  rename(parcel = parcelnumb) %>%
  mutate(conc = avg_conc/1000, units = 'ppm') %>%
  filter(analyte == "Total PCBs")


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

study_totals <- sfdf %>% filter(str_detect(analyte, "Total")) %>%
  mutate(log_conc = log(est_conc))


# Heatmap with EHE data ---------------------------------------------------

test <- ehe_dat %>%
  group_by(lat, lon) %>%
  mutate(n = n())

## SINGLE MODEL
# Create variogram for each analyte group and fit the variogram
# Create raster
r <- rast(#nrows = 100, ncols = 100, 
  nrows = 1000, ncols = 1000,
          xmin = -90.172, ymin = 38.595, xmax = -90.148, ymax = 38.616)
names(r) <- 'result'

vmods <- ehe_dat %>%
  data.frame() %>%
  select(x = lon, y = lat, result = conc )

sf = vect(vmods, geom=c('x','y'))
v = variogram(log(result) ~ 1, ~ x + y, data = vmods)
mu = fit.variogram(v,
                   vgm(psill = max(v$gamma)*.9,
                       model = "Sph",
                       range = max(v$dist)/2,
                       nuggt = mean(v$gamma)/4))
k = gstat(id = 'result', formula = log(result) ~ 1, locations = ~ x + y,
          data = vmods,
          model = mu)

start <-  Sys.time()
kp = terra::interpolate(r, k)
end <- Sys.time()
print(end-start)


plot(kp$result.pred)
#points(sf)

# Remove section of heatmap outside East St Louis
kp_esl = terra::mask(kp, esl) 

# Export heatmpa polygons to shapefile for use in tableau
test <- as.polygons(kp_esl$result.pred, dissolve=FALSE)
plot(test)
writeVector(test, 'output/heat-base-raster.shp',overwrite=TRUE)


# Remove Study Points Outside East St Louis
study_totals = st_intersection(study_totals, esl)

## PLOT
basemap <- ggplot() +
  # base map
  annotation_map_tile(type = 'cartolight', zoom = 15) +
  
  # heat map -- parcels
  layer_spatial(kp_esl$result.pred, alpha=.85) +
  
  # outlines of E St Louis, Monsanto
  geom_sf(data = esl, fill = NA, color = 'gray', linewidth = .75) + 
  geom_sf(data = monsanto, fill = NA, color = 'steelblue', linewidth = .75) +
  geom_sf(data = monsanto_incin, fill = 'steelblue', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'steelblue', color = NA) +
  
  # Add colors and size 
  scale_fill_gradientn(colours = c("#FAEBDDFF", '#F6AA82FF', '#F06043FF', "#CB1B4FFF",
                                   "#611F53FF", "#30173AFF"),
                       name =  "PCB Log Concentrations (ppm)",
                       na.value = 'transparent')+
  scale_size_continuous(name = "PCB Log Concentrations (ppm)") 

## Gonzalez
totals.gz <- study_totals %>% filter(str_detect(Study, "Gonzalez"))

basemap +
  # Add study specific data
  geom_sf(data = totals.gz,
          aes(color = totals.gz$log_conc, 
              size = totals.gz$log_conc,
              fill = totals.gz$log_conc),
          shape = 21, colour = "black") +
  ## output settings of map
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  
  # Set coordinate limits -- still showing some white from where it's masked....
  coord_sf(xlim = c(-90.175,-90.148), ylim = c(38.594, 38.616), crs = 4326) +
 # coord_sf(xlim = c(-90.171,-90.15), ylim = c(38.596, 38.615), crs = 4326) +
  
  # Add title
  labs(title = paste0('xx',' Total PCB ', 'type', ' Sample Concentrations'),
       subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)",) 


## Hermanson
totals.hm <- study_totals %>% filter(str_detect(Study, "Herm"))

basemap +
  # Add study specific data
  geom_sf(data = totals.hm,
          aes(color = totals.hm$log_conc, 
              size = totals.hm$log_conc,
              fill = totals.hm$log_conc),
          shape = 21, colour = "black") +
  ## output settings of map
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  
  # Set coordinate limits -- still showing some white from where it's masked....
  coord_sf(xlim = c(-90.175,-90.148), ylim = c(38.594, 38.616), crs = 4326) +
  # coord_sf(xlim = c(-90.171,-90.15), ylim = c(38.596, 38.615), crs = 4326) +
  
  # Add title
  labs(title = paste0('xx',' Total PCB ', 'type', ' Sample Concentrations'),
       subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)",) 



### EPA
totals.epa <- study_totals %>% filter(str_detect(Study, "EPA"))

basemap +
  # Add study specific data
  geom_sf(data = totals.epa,
          aes(color = totals.epa$log_conc, 
              size = totals.epa$log_conc,
              fill = totals.epa$log_conc),
          shape = 21, colour = "black") +
  ## output settings of map
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  
  # Set coordinate limits -- still showing some white from where it's masked....
  coord_sf(xlim = c(-90.175,-90.148), ylim = c(38.594, 38.616), crs = 4326) +
  # coord_sf(xlim = c(-90.171,-90.15), ylim = c(38.596, 38.615), crs = 4326) +
  
  # Add title
  labs(title = paste0('xx',' Total PCB ', 'type', ' Sample Concentrations'),
       subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)",) 

ggsave('output/EHE-Base-EPA1976.png',  height = 6, width = 8, units = 'in') 


### ALL
basemap +
  # Add study specific data
  geom_sf(data = study_totals, #color = 'blue',
          aes(color = study_totals$Study, 
              size = study_totals$log_conc,
              ),)+
         # shape = 21, colour = "black") +
  scale_color_discrete(name = 'Study',
                       type = c('yellow', 'darkgreen', 'darkblue')) +
  ## output settings of map
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  
  # Set coordinate limits -- still showing some white from where it's masked....
  coord_sf(xlim = c(-90.175,-90.148), ylim = c(38.594, 38.616), crs = 4326) +
  # coord_sf(xlim = c(-90.171,-90.15), ylim = c(38.596, 38.615), crs = 4326) +
  
  # Add title
  labs(title = paste0('Total PCB Log Sample Concentrations'),
       subtitle = "East St. Louis city boundary (gray outline), former Monsanto plant (blue)",) 

ggsave('output/EHE-Base-All-Studies.png',  height = 6, width = 8, units = 'in') 



