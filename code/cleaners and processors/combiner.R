library(tidyverse)

alpha <- read_csv('data/updated-alpha.csv') 
cape <- read_csv('data/cape-fear.csv')

# Basic formatting and standardization
# ND = 0 for conc; and ND = DL/2 for 'est_conc'
# Largely, ND = 0 will be used for analysis but keeping ND = DL/2 in case that decision is reversed. 
alphax <- alpha %>% 
  ## remove where no parcel info
  filter(!is.na(parcel)) %>%
  rename(lab_id = lab_sample_id, 
         sample_matrix = sample_type) %>% 
  mutate(lab = 'Alpha', 
         sampling_date = lubridate::mdy(sampling_date), 
        # dl = case_when(!detected ~ conc),  ## LKT removed - dl is present in the updated data
         conc = ifelse(detected, conc, 0), # ND = 0
         est_conc = ifelse(detected, conc, dl/2), # ND = DL/2 
         est_method = 'Half DL', # For documentation
         old_analyte = analyte, 
         analyte = str_replace(str_replace(old_analyte, '^.+\\(', ''), '\\)$', ''))


# QC to check that all analytes with coeluters ALWAYS coelute. 
# pct should always be 1, 0 or NA. (either always elutes, never elutes, or NA)
# if pct == 1, conc === 0. conc should always be zero if the chemical has a comment that it coelutes.  
cape |> 
  filter(!str_detect(analyte, 'Total')) |>
  group_by(qq = str_detect(qualifier, 'C[0-9].+')) |> 
  summarize(pct = sum(qq)/n(), 
            conc = sum(conc, na.rm = T)) 

# 209 PCB congeners analyzed by Cape Fear per sample, which is the exact possible number of PCB congeners
# 11 possible totals
cape |> filter(!str_detect(analyte, 'Total')) |> group_by(sample_id) |> count()

# Of the 209 congeners, 43 coelute and always have a zero. This leave a total of 166 possible results per sample. 
cape |> 
  filter(!str_detect(analyte, 'Total')) |> 
  group_by(analyte, qq = str_detect(qualifier, 'C[0-9].+')) |> 
  count() |> 
  filter(qq) |> 
  ungroup() |> 
  count()


# Some analytes coelute in which case Cape Fear assigns it a concentration of zero
# and puts a comment in the for 'Cnum', were num is the congener that it coelutes with. 
# Objective is to standardize with Alpha, which puts all coeluters in one row 
# separated by a '/'.
cape_coeluters <-  cape |> 
  filter(!str_detect(analyte, 'Total')) |> 
  mutate(analyte = paste0('PCB-', str_replace(analyte, '\\-.+$', '')), 
         coeluters = case_when(str_detect(qualifier, 'C[0-9].+') ~ str_replace(qualifier, 'C', 'PCB-'),
                               TRUE ~ analyte)) |> 
  ungroup() |> 
  group_by(coeluters) |> 
  summarize(analyte = paste0(unique(analyte), collapse = '/'), 
            casrn = paste0(unique(casrn), collapse = '/'))


capex <- cape |>  
  filter(!str_detect(analyte, 'Total')) |>
  select(-casrn) |> 
  mutate(old_analyte = analyte, 
         analyte = paste0('PCB-', str_replace(old_analyte, '\\-.+$', ''))) |> 
  left_join(cape_coeluters, by = c('analyte' = 'coeluters')) |> 
  filter(!is.na(analyte.y)) |> 
  mutate(analyte = analyte.y) |> 
  select(-analyte.y) |> 
  # will eventually be dropped but keep for now for QC
  bind_rows(cape |> filter(str_detect(analyte, 'Total')))

capex |> group_by(qualifier) |> count()

capey <- capex |> 
  rename(location = sample_id, 
         sampling_date = collection_date) %>% 
  mutate(lab = 'Cape Fear', 
         lab_id = as.character(lab_id), 
         detected = !qualifier %in% c('U', 'CU') | is.na(qualifier),
         conc = ifelse(detected, conc, 0), # Equivalent to ND = 0
         est_conc = ifelse(detected, conc, dl/2), # ND = DL/2
         est_method = 'Half DL') %>% # For doc purposes
  mutate_at(c('dl', 'rl', 'conc', 'est_conc'), ~ .x/1e3) %>%
  mutate(units = 'ug/kg')

capey |> filter(!str_detect(analyte, 'Total')) |> group_by(location) |> count()

df <- alphax %>% 
  bind_rows(capey) %>% 
  mutate(parcel = str_pad(parcel, 11, 'left', '0'))

# Raw data as provided by lab. 
# Level 1 clean, i.e., contains total homolog estimates by Cape Fear and 
# not harmonized between labs, e.g., co-eluters are reported separately by Cape Fear. 
# 'est_conc' contains estimates by half of DL. 'conc' contains the raw number provided by lab. 
# NOTE: this database is just for QC purposes. DO NOT USE. Use the production data below. 
write_csv(df, 'data/all-data.csv')


# Assign congener groups and calculate my own totals rather than those 
# provided by Cape Fear. Also calculate Total PCBs. 
# Labs use 0 for NDs, which is not appropriate. 

# Congener group -- used to calculate totals
prefixes <- capey %>% 
  filter(str_detect(analyte, 'Total')) %>% 
  distinct(analyte) %>% 
  mutate(analyte_prefix = str_squish(str_replace_all(analyte, 'Total|PCBs|PCB', ''))) %>% 
  slice(1:n()-1) %>% 
  mutate(n_cl = 1:10) %>% 
  mutate(abbr = c('Mo', 'Di', 'Tr', 'Te', 'Pe', 'Hx', 'Hp', 'Oc', 'No', 'De')) %>% 
  rename(analyte_group = analyte)


### Process Cape Fear
cape2 <- capey %>% 
  filter(!str_detect(analyte, 'Total')) %>% # Remove totals provided by lab
  mutate(abbr = str_replace(str_extract(old_analyte, '-[:alpha:]{2}'), '-', '')) %>% 
  left_join(prefixes, by = 'abbr') %>% 
  select(-abbr)

## Calculate my own totals
# by homolog 
cape_cong_totals <- cape2 %>% 
  group_by(lab, parcel, location, lab_id, units, sampling_date, analyte_group, n_cl, analyte_prefix, est_method) %>% 
  summarize(conc = sum(conc), 
            est_conc = sum(est_conc), 
            detected = sum(detected/n())) 
# total pcbs
cape_total_pcbs <- cape2 %>% 
  group_by(lab, parcel, location, lab_id, units, sampling_date, est_method) %>% 
  summarize(analyte_group = 'Total PCBs', 
            analyte_prefix = 'Total', 
            conc = sum(conc), 
            est_conc = sum(est_conc), 
            detected = sum(detected/n()))

# Combine all the data together
cape_combined <- cape_cong_totals %>% 
  bind_rows(cape_total_pcbs) %>% 
  mutate(analyte = analyte_group) %>% 
  bind_rows(cape2) %>% 
  ungroup() %>% 
  arrange(parcel, n_cl, analyte)

# Duplicates were collected in some of the parcels. Take an average. 
cape_by_parcel <- cape_combined %>% 
  group_by(lab, parcel, units, sampling_date, analyte, n_cl, analyte_prefix, est_method) %>% 
  summarize(conc = mean(conc), # Average of duplicates
            est_conc = mean(est_conc),  # Average of duplicates
            detected = mean(detected), 
            dl = mean(dl),     # Average of duplicates
            rl = mean(rl),     # Average of duplicates,
            qualifier = paste0(unique(qualifier), collapse = ", "), # list of qualifiers associated with analyte result
            sample_ids = paste0(unique(location), collapse = ', '), 
            lab_ids = paste0(unique(lab_id), collapse = ', ')) %>% 
  arrange(parcel, n_cl, analyte)
cape_by_parcel

# 166 PCBs + 11 totals
cape_by_parcel |> group_by(sample_ids) |> count()

## Totals provided by lab 
# cape_lab_totals <- cape %>% 
#   filter(str_detect(analyte, 'Total')) %>% 
#   select(parcel, analyte_group = analyte, est_conc) %>% 
#   mutate(method = 'lab')


## Do the same for Alpha
alpha2 <- alphax %>% 
  mutate(abbr = str_replace(str_extract(old_analyte, '-[:alpha:]{2}'), '-', ''), 
         abbr = ifelse(is.na(abbr), 'De', abbr)) %>%
  left_join(prefixes, by = 'abbr') %>% 
  select(-abbr) 

# Calculate my own totals
alpha_cong_totals <- alpha2 %>% 
  group_by(lab, parcel, location, lab_id, units, sampling_date, analyte_group, n_cl, analyte_prefix, est_method) %>% 
  summarize(conc = sum(conc),
            est_conc = sum(est_conc), 
            detected = sum(detected/n())) 

alpha_total_pcbs <- alpha2 %>% 
  group_by(lab, parcel, location, lab_id, units, sampling_date, est_method) %>% 
  summarize(analyte_group = 'Total PCBs', 
            analyte_prefix = 'Total', 
            conc = sum(conc), 
            est_conc = sum(est_conc), 
            detected = sum(detected/n()))

# Combine all the data together
alpha_combined <- alpha_cong_totals %>% 
  bind_rows(alpha_total_pcbs) %>% 
  mutate(analyte = analyte_group) %>% 
  bind_rows(alpha2) %>% 
  ungroup() %>% 
  arrange(parcel, n_cl, analyte)

# Duplicates were collected in some of the parcels. Take an average. 
alpha_by_parcel <- alpha_combined %>% 
  group_by(lab, parcel, units, sampling_date, analyte, n_cl, analyte_prefix, est_method) %>% 
  summarize(conc = mean(conc), # Average of duplicates
            est_conc = mean(est_conc), # Average of duplicates
            detected = mean(detected),
            dl = mean(dl),     # Average of duplicates
            rl = mean(rl),     # Average of duplicates,
            qualifier = paste0(unique(qualifier), collapse = ", "), # list of qualifiers associated with analyte result
            sample_ids = paste0(unique(location), collapse = ', '), 
            lab_ids = paste0(unique(lab_id), collapse = ', ')) %>% 
  arrange(parcel, n_cl, analyte) 
alpha_by_parcel 

# Data by sample id and not averaged by parcel, i.e., contains duplicates. 
# This is effectively the raw data provided by the labs but cleaned up, 
# transformed into long format, and harmonized for consistency between the two labs
# This data set should be used for production!
# LKT -- keeping qualifier, rl, dl
data_for_production <- cape_combined |> 
  bind_rows(alpha_combined) |> 
  mutate(parcel = str_pad(parcel, 11, 'left', '0')) |> 
  arrange(parcel, analyte_prefix, analyte) |> 
  select(-analyte_group, -sample_matrix, -sheet, -old_analyte) |> 
  rename(homolog = analyte_prefix)

# This data set contains data averaged by parcel so it does not contain any duplicates
# This should be used for analysis by parcel, for creating heatmaps, etc. 
df_by_parcel <- alpha_by_parcel %>% 
  bind_rows(cape_by_parcel) %>% 
  mutate(parcel = str_pad(parcel, 11, 'left', '0')) %>% 
  ungroup()

write_csv(data_for_production, 'data/all-data-clean-for-production.csv')
write_csv(df_by_parcel, 'data/trimmed-data-by-parcel.csv')
rm(list=ls())


## Add GIS geometry to each parcel (purchased from ReGRID)
library(terra)

data_for_production <- read_csv('data/all-data-clean-for-production.csv')
df_by_parcel <- read_csv('data/trimmed-data-by-parcel.csv') 
geos <- sf::st_read('data/gis/il_st_clair.shp') %>% 
  select(geoid, parcelnumb, city, county, lat, lon, area_sqft = ll_gissqft, area_acre = ll_gisacre) %>%
  filter(city == 'east-st-louis') %>% 
  group_by(parcelnumb) %>% 
  slice(1) # for some reason there are duplicates in the shape file

sfdf <- terra::merge(terra::vect(geos), data_for_production, all.x = F, by.x = 'parcelnumb', by.y = 'parcel', how = 'inner')
sfdf_by_parcel <- terra::merge(terra::vect(geos), df_by_parcel, all.x = F, by.x = 'parcelnumb', by.y = 'parcel', how = 'inner') 

df_by_parcel %>% distinct(parcel) %>% count

df_by_parcel %>% 
  filter(!df_by_parcel$parcel %in% unique(sfdf$parcelnumb)) %>% 
  distinct(parcel)
# two parcels 00000000ROW and RIGHT OF WAY are not actual parcels
# therefore, no geocodes associated with them. 
# apparently, these were collected on the boundary near Monsanto Way. 

writeVector(sfdf, 'data/gis/all-data-for-production-with-geos.shp', overwrite = T)
writeVector(sfdf, 'dashboard/data/gis/all-data-for-production-with-geos.shp', overwrite = T)

writeVector(sfdf_by_parcel, 'data/gis/data-by-parcel.shp', overwrite = T)
writeVector(sfdf_by_parcel, 'dashboard/data/gis/data-by-parcel.shp', overwrite = T)

rm(list=ls())
