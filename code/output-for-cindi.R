
data.frame(terra::vect('data/gis/all-data-for-production-with-geos.shp')) |> 
  select(-est_conc, -est_method, -sampling_d) |> 
  rename(parcel = parcelnumb) |> 
  mutate(conc = conc/1000, units = 'ppm') |> 
  write_csv('data/for-cindi/data-by-sample.csv') 


data.frame(terra::vect('data/gis/data-by-parcel.shp')) |> 
  select(-est_conc, -est_method, -sampling_d) |> 
  rename(parcel = parcelnumb) |> 
  mutate(conc = conc/1000, units = 'ppm') |> 
  write_csv('data/for-cindi/data-by-parcel.csv') 
