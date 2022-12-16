library(tidyverse)

df <- read_csv('data/all-data.csv') %>% 
  mutate(parcel = str_pad(parcel, 11, 'left', '0'))

df %>% group_by(lab, casrn) %>% count %>% spread(lab, n) %>% data.frame

# Discrepancy between Alpha and Cape Fear results. Cape Fear results appear to be ~2x than Alpha.  
ggplot(df, aes(x = est_conc)) + stat_ecdf(aes(color = lab)) + scale_x_log10()
df %>% group_by(lab) %>% summarize(est_conc = mean(est_conc))

# This appears to partially be due to Cape Fear reporting total PCB results, which Alpha does not. 
# But there is still a discrepancy at lower concentrations 
ggplot(df %>% filter(!str_detect(analyte, 'Total')), aes(x = est_conc)) + stat_ecdf(aes(color = lab)) + scale_x_log10()
ggplot(df %>% filter(!str_detect(analyte, 'Total')), aes(x = est_conc)) + geom_density(aes(color = lab)) + scale_x_log10()

# Some of it is due to differences in detection limit perhaps
ggplot(df %>% filter(!str_detect(analyte, 'Total'), detected), aes(x = est_conc)) + stat_ecdf(aes(color = lab)) + scale_x_log10()

df %>% group_by(parcel, analyte)  


geos <- sf::st_read('data/gis/il_st_clair.shp') %>% 
  select(geoid, parcelnumb, city, county) %>% 
  filter(city == 'east-st-louis') 

sfdf <- tigris::geo_join(geos, df, 'parcelnumb', 'parcel', how = 'inner')

pal <- colorFactor(topo.colors(2), sfdf$lab)

leaflet(data = sfdf %>% filter(casrn == '2051-60-7')) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(fill = ~est_conc, label = ~paste(lab, est_conc), stroke = F, color = ~pal(lab)) 
  
leaflet(data = sfdf %>% filter(casrn == '2051-60-7')) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(fill = ~est_conc, label = ~paste(lab, est_conc), stroke = F, fillColor = 'Red') 
  
