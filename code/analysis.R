### Mostly junk. Ignore

library(tidyverse)

sfdf <- terra::vect('data/gis/all-data-with-geos.shp')
df <- tibble(values(sfdf)) # %>% mutate(detected = as.logical(detected))

df %>% group_by(lab, casrn) %>% count %>% spread(lab, n) %>% data.frame

# Discrepancy between Alpha and Cape Fear results. Cape Fear results appear to be ~2x than Alpha.  
ggplot(df, aes(x = est_conc)) + stat_ecdf(aes(color = lab)) + scale_x_log10()
df %>% group_by(lab) %>% summarize(est_conc = mean(est_conc))

# This appears to partially be due to Cape Fear reporting total PCB results, which Alpha does not. 
# But there is still a discrepancy at lower concentrations 
ggplot(df %>% filter(str_detect(analyte, 'Total')), aes(x = est_conc)) + stat_ecdf(aes(color = lab)) + scale_x_log10()
ggplot(df %>% filter(str_detect(analyte, 'Total')), aes(x = est_conc)) + geom_density(aes(color = lab)) + scale_x_log10()

# Some of it is due to differences in detection limit perhaps
ggplot(df %>% filter(!str_detect(analyte, 'Total'), as.logical(detected)), aes(x = est_conc)) + stat_ecdf(aes(color = lab)) +
  facet_wrap(~n_cl) + 
  scale_x_log10()


geos <- sf::st_read('data/gis/il_st_clair.shp') %>% 
  select(geoid, parcelnumb, city, county) %>% 
  filter(city == 'east-st-louis') 

sfdf <- tigris::geo_join(geos, df, 'parcelnumb', 'parcel', how = 'inner')

pal <- colorFactor(topo.colors(2), sfdf$lab)

library(mapdeck)

mapdeck(token = "pk.eyJ1IjoiYWtzaW5naGFsODgiLCJhIjoiY2tuZHgyeWxyMWEycDJwbzB1dDBqMGR0NiJ9.XFjK_TTS-nKfFYkQY70wIQ") %>% 
  add_geojson(
    xx %>% filter(!str_detect(analyte, 'Total')) %>% group_by(parcelnumb) %>% summarize(est_conc = sum(est_conc)), 
    # radius = 'est_conc', 
    fill_colour = 'est_conc', 
    tooltip = 'est_conc', 
    palette = 'reds'
  )

library(tidyverse) 
library(ggvoronoi) 

sfdf <- terra::vect('data/gis/all-data-with-geos.shp')
df <- terra::values(sfdf)

df %>% filter(str_detect(analyte, 'Total')) %>% group_by(parcelnumb, analyte) %>% count %>% filter(n>1)
ggplot(df %>% filter(str_detect(analyte, 'Total')), aes(x = lon, y = lat)) +
  stat_voronoi(aes(fill = est_conc), color = 'lightgrey') +
  scale_fill_gradient2(low = 'skyblue4', mid = 'white', high = 'firebrick4') + 
  facet_wrap(~n_cl) +
  theme_void()
xx <- df %>% filter(analyte == 'Total Hepta PCBs') %>% select(x = lon, y = lat, est_conc) %>% distinct(x, y)
voronoi_polygon(data = xx, x = 'x', y = 'x')

