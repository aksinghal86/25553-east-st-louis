library(tidyverse) 

alpha <- read_csv('data/alpha.csv') 
cape <- read_csv('data/cape-fear.csv')

alpha <- alpha %>% 
  rename(lab_id = lab_sample_id, 
         sample_matrix = sample_type) %>% 
  mutate(lab = 'Alpha', 
         sampling_date = lubridate::mdy(sampling_date), 
         est_conc = conc)

cape <- cape %>% 
  rename(location = sample_id, 
         sampling_date = collection_date) %>% 
  mutate(lab = 'Cape Fear', 
         lab_id = as.character(lab_id), 
         detected = !qualifier %in% c('U', 'CU') | is.na(qualifier),
         est_conc = ifelse(detected, conc, dl)) %>% 
  mutate_at(c('dl', 'rl', 'conc', 'est_conc'), ~ .x/1e3) %>%
  mutate(units = 'ug/kg')

df <- alpha %>% 
  bind_rows(cape) %>% 
  mutate(parcel = str_pad(parcel, 11, 'left', '0'))

write_csv(df, 'data/all-data.csv')



prefixes <- cape %>% 
  filter(str_detect(analyte, 'Total')) %>% 
  distinct(analyte) %>% 
  mutate(analyte_prefix = str_squish(str_replace_all(analyte, 'Total|PCBs|PCB', ''))) %>% 
  slice(1:n()-1) %>% 
  mutate(n_cl = 1:10) %>% 
  mutate(abbr = c('Mo', 'Di', 'Tr', 'Te', 'Pe', 'Hx', 'Hp', 'Oc', 'No', 'De')) %>% 
  rename(analyte_group = analyte)

cape2 <- cape %>% 
  filter(!str_detect(analyte, 'Total')) %>% 
  mutate(abbr = str_replace(str_extract(analyte, '-[:alpha:]{2}'), '-', '')) %>% 
  left_join(prefixes, by = 'abbr') %>% 
  select(-abbr)
cape3 <- cape2 %>% 
  group_by(lab, parcel, location, lab_id, units, sampling_date, analyte_group, n_cl, analyte_prefix) %>% 
  summarize(est_conc = sum(est_conc), 
            detected = sum(detected/n())) %>%  
  mutate(analyte = analyte_group) %>% 
  bind_rows(cape2) %>% 
  arrange(parcel, n_cl, analyte)
cape_by_parcel <- cape3 %>% 
  group_by(lab, parcel, units, sampling_date, analyte, n_cl, analyte_prefix) %>% 
  summarize(est_conc = mean(est_conc), 
            detected = mean(detected)) %>% 
  arrange(parcel, n_cl, analyte)
# 
# cape2 <- cape %>% 
#   filter(!str_detect(analyte, 'Total')) %>% 
#   mutate(abbr = str_replace(str_extract(analyte, '-[:alpha:]{2}'), '-', '')) %>% 
#   left_join(prefixes, by = 'abbr') %>%
#   group_by(parcel, analyte_group) %>% 
#   summarize(est_conc = sum(est_conc), 
#             method = 'manual')
cape_totals <- cape %>% 
  filter(str_detect(analyte, 'Total')) %>% 
  select(parcel, analyte_group = analyte, est_conc) %>% 
  mutate(method = 'lab')

alpha2 <- alpha %>% 
  mutate(abbr = str_replace(str_extract(analyte, '-[:alpha:]{2}'), '-', ''), 
         abbr = ifelse(is.na(abbr), 'De', abbr)) %>%
  left_join(prefixes, by = 'abbr') %>% 
  select(-abbr) 
alpha3 <- alpha2 %>% 
  group_by(lab, parcel, location, lab_id, units, sampling_date, analyte_group, n_cl, analyte_prefix) %>% 
  summarize(est_conc = sum(est_conc), 
            detected = sum(detected)/n()) %>%  
  mutate(analyte = analyte_group) %>% 
  bind_rows(alpha2) %>% 
  arrange(parcel, n_cl, analyte)

alpha_by_parcel <- alpha3 %>% 
  group_by(lab, parcel, units, sampling_date, analyte, n_cl, analyte_prefix) %>% 
  summarize(est_conc = mean(est_conc), 
            detected = mean(detected)) %>% 
  arrange(parcel, n_cl, analyte)

df_by_parcel <- alpha_by_parcel %>% 
  bind_rows(cape_by_parcel) %>% 
  mutate(parcel = str_pad(parcel, 11, 'left', '0')) %>% 
  ungroup()

total_pcbs <- df_by_parcel %>% 
  filter(str_detect(analyte, 'Total')) %>% 
  group_by(lab, parcel, units, sampling_date) %>% 
  summarize(analyte = 'Total PCBs', 
            est_conc = sum(est_conc)) %>% 
  ungroup()

df_by_parcel <- df_by_parcel %>% bind_rows(total_pcbs) %>% arrange(parcel, n_cl)

write_csv(df_by_parcel, 'data/trimmed-data-by-parcel.csv')

## With shapes
library(terra)

geos <- sf::st_read('data/gis/il_st_clair.shp') %>% 
  select(geoid, parcelnumb, city, county, lat, lon) %>%
  filter(city == 'east-st-louis') %>% 
  group_by(parcelnumb) %>% 
  slice(1) # for some reason there are duplicates in the shape file

sfdf <- terra::merge(terra::vect(geos), df_by_parcel, all.x = F, by.x = 'parcelnumb', by.y = 'parcel', how = 'inner') 

df_by_parcel %>% distinct(parcel) %>% count


df_by_parcel %>% filter(!df_by_parcel$parcel %in% unique(sfdf$parcelnumb)) %>% distinct(parcel)
# two parcels 00000000ROW and RIGHT OF WAY are not actual parcels
# therefore, no geocodes associted with them. 
# apparently, these were collected on the boundary near Monsanto Way. 

writeVector(sfdf, 'data/gis/all-data-with-geos.shp', overwrite = T)
writeVector(sfdf, 'dashboard/data/gis/all-data-with-geos.shp', overwrite = T)
