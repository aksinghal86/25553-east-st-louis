
## Moved these to the combiner.R code
# data.frame(terra::vect('data/gis/all-data-for-production-with-geos.shp')) |> 
#   select(-est_conc, -est_method, -sampling_d) |> 
#   rename(parcel = parcelnumb) |> 
#   mutate(conc = conc/1000, units = 'ppm') |> 
#   write_csv('data/for-cindi/data-by-sample.csv') 
# 
# 
# data.frame(terra::vect('data/gis/data-by-parcel.shp')) |> 
#   select(-est_conc, -est_method, -sampling_d) |> 
#   rename(parcel = parcelnumb) |> 
#   mutate(conc = conc/1000, units = 'ppm') |> 
#   write_csv('data/for-cindi/data-by-parcel.csv') 
# 



test <- sf::st_read('data/gis/data-by-parcel.shp') %>% 
  filter(analyte == "Total PCBs") %>%
  select(-est_method, -sampling_d) |> 
  rename(parcel = parcelnumb) |> 
  mutate(
    avg_conc = avg_conc/1000,
    avg_rl = avg_rl/1000,
    max_conc = max_conc/1000,
    max_rl = max_rl/1000,
    units = 'ppm',
  )
sf::st_write(test, 'data/gis/total-pcb-by-parcel.shp', append=FALSE)
