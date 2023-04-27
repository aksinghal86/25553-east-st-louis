# Analysis

library(tidyverse) 
library(flextable)
library(sf) 
library(ggspatial)


df <- read_csv('data/all-data-clean-for-production.csv') |> 
  filter(!str_detect(parcel, 'ROW|RIGHT OF WAY')) |> 
  mutate(conc = conc/1000, 
         units = 'ppm')

homologs <- df |> 
  filter(!str_detect(analyte, 'Total')) |> 
  mutate(detected = as.logical(detected)) |>
  filter(detected) |> 
  group_by(lab, parcel, location, lab_id, n_cl, homolog, units) |> 
  summarize(conc = sum(conc)) |> 
  ungroup()
total_pcbs <- homologs |> 
  group_by(lab, parcel, location, lab_id, units) |> 
  summarize(conc = sum(conc)) |> 
  mutate(homolog = 'Total PCBs') |>  
  ungroup()

totals <- homologs |> bind_rows(total_pcbs) 


geos <- sf::st_read('data/gis/il_st_clair.shp') %>% 
  select(geoid, parcelnumb, city, county, lat, lon, area_sqft = ll_gissqft, area_acre = ll_gisacre) %>%
  filter(city == 'east-st-louis') %>% 
  group_by(parcelnumb) %>% 
  slice(1)

sf_total_pcbs <- terra::merge(terra::vect(geos), total_pcbs, all.x = F, by.x = 'parcelnumb', by.y = 'parcel', how = 'inner') 

sf_total_pcbs <- sf::st_as_sf(sf_total_pcbs)
esl <- sf::st_read('dashboard/data/esl.kml')
monsanto <- sf::st_read('dashboard/data/gis/monsanto.kml', quiet = T) %>% sf::st_zm()
monsanto_storage <- st_read('dashboard/data/gis/monsanto-storage.kml', quiet = T) %>% st_zm()
monsanto_incin <- st_read('dashboard/data/gis/monsanto-incinerator.kml', quiet = T) %>% st_zm()



df |> distinct(lab_id) |> count() # 354 samples
df |> distinct(parcel) |> count() # 273 parcels sampled

ggplot(sf_total_pcbs |> distinct(parcelnumb, .keep_all = T)) +
  annotation_map_tile(type = 'cartolight', zoom = 13) +
  geom_sf(fill = NA, size = 0.25) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  # labs(title = 'East St. Louis city boundary (gray outline), former Monsanto plant (or') +
  theme(legend.position = 'none', 
        plot.title.position = 'plot') + 
  # coord_sf(xlim = c(-90.172, -90.148), ylim = c(38.595, 38.612), crs = 4326) + 
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(1, 'cm'), width = unit(1, 'cm'),
                         pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_orienteering)
ggsave('output/estl-zoomed-out.png', height = 6, width = 10, units = 'in')


ggplot(sf_total_pcbs |> distinct(parcelnumb, .keep_all = T)) + 
  annotation_map_tile(type = 'cartolight', zoom = 14) + 
  geom_sf(fill = NA, size = 0.25) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  geom_sf(data = monsanto_storage, fill = 'orange', color = NA) +
  coord_sf(xlim = c(-90.172, -90.148), ylim = c(38.595, 38.612), crs = 4326) + 
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank()
  )
ggsave('output/estl-zoomed-in-parcels.png', height = 5, width = 5, units = 'in')


detects <- df |> 
  filter(!str_detect(analyte, 'Total')) |> 
  group_by(homolog, n_cl) |> 
  summarize(n = n(), 
            detects = sum(detected), 
            pct_detects = detects/n*100) |> 
  arrange(n_cl)

ggplot(detects, aes(x = reorder(homolog, n_cl), y = pct_detects)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = paste0(round(pct_detects), '%')), hjust = -0.20) + 
  labs(y = "% detected", x = NULL) + #title = "Percentage of detected values by PCB homolog in E. STL soil samples") +
  scale_y_continuous(limits = c(0, 105)) + 
  cowplot::theme_cowplot() + 
  theme(
    plot.title.position = 'plot'
  )
ggsave('output/detects-by-homolog.png', height = 6, width = 8, units = 'in') 


ggplot(totals, aes(x = conc)) + 
  stat_ecdf() + 
  facet_wrap(~ reorder(homolog, n_cl)) +
  scale_x_log10(labels = scales::label_log()) +
  labs(y = "Cumulative probability distribution", x = "Concentration, log scale (ppm)") +
       # title = "PCB concentration by homolog and for total PCBs in samples collected in E. STL") + 
  # annotation_logticks(sides = 'b') + 
  cowplot::theme_cowplot() + 
  theme(
    plot.title.position = 'plot'
  )
ggsave('output/ecdf.png', height = 6, width = 8, units = 'in') 


summ_stats <- totals |> 
  group_by(parcel, homolog, n_cl) |>
  summarize(conc = mean(conc)) |> 
  ungroup() |> 
  group_by(homolog, n_cl) |> 
  summarize(
    n = n(), 
    Min = min(conc), 
    Mean = mean(conc), 
    GM = exp(mean(log(conc))), 
    GSD = exp(sd(log(conc))), 
    Median = median(conc), 
    Max = max(conc)
  ) |> 
  arrange(n_cl) |>
  flextable() |> 
  set_caption(caption = 'Summary statistics by PCB homolog and for total PCBs in soil samples collected in E. STL') |> 
  colformat_double(j = c("Min", "Median"), digits = 5) |> 
  # colformat_double(j = c("Mean", "Max"), digits = 2) |> 
  add_header_row(colwidths = c(3, 6), values = c('', 'ppm'), top = F) 
summ_stats
flextable::save_as_docx(summ_stats, path = 'output/summary-statistics.docx')


totals |> 
  group_by(parcel, homolog, n_cl) |>
  summarize(conc = mean(conc)) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(homolog, n_cl), y = conc)) + 
  geom_boxplot(coef = Inf, outlier.shape = NA) + 
  stat_summary(geom = 'point', fun = mean, size = 3) + 
  scale_y_log10(labels = scales::label_log()) + 
  annotation_logticks(sides = 'l') + 
  labs(y = 'Concentration, log scale (ppm)', x = NULL) +  
       # title = 'PCB concentration by homolog and for total PCBs') + 
  geom_hline(yintercept = 1, color = 'firebrick4', linetype = 'dashed') + 
  annotate(x = 2, y = 1.5, geom = 'text', label = '1 ppm clean-up threshold', color = 'firebrick4') + 
  cowplot::theme_cowplot() + 
  theme(plot.title.position = 'plot')
ggsave('output/concentation-boxplot.png', height = 6, width = 8, units = 'in') 


total_pcbs <- total_pcbs |> 
  group_by(parcel) |> 
  summarize(conc = mean(conc))
total_pcbs |> 
  filter(conc >= 1) |> 
  count()

total_pcbs <- total_pcbs |> 
  mutate(breaks = cut(conc, breaks = c(0, 0.50, 0.75, 1, 5, 10, Inf)))
total_pcbs |> group_by(breaks) |> count()

sf_total_pcbs <- sf_total_pcbs |> 
  group_by(parcelnumb) |> 
  summarize(conc = mean(conc))
sf_total_pcbs <- sf_total_pcbs |> 
  mutate(breaks = cut(conc, breaks = c(0, 0.50, 0.75, 1, 5, 10, Inf)), 
         colors = case_when(
           conc < 0.50 ~ '#f2f3f4',
           conc < 0.75 ~ "#bdc3c7", 
           conc < 1 ~  "#626567", # '#ff5733',
           conc < 5 ~ '#c70039',
           conc < 10 ~ '#900c3f',
           TRUE ~ '#581845'))

ggplot(sf_total_pcbs, aes(fill = colors)) + 
  annotation_map_tile(type = 'cartolight', zoom = 15) +
  geom_sf(alpha = 0.75, size = 0.25) +
  geom_sf(data = esl, fill = NA, color = 'gray') + 
  geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  # scale_fill_gradient(low = 'orange', high = 'red') +
  scale_fill_identity('Concentration\n(ppm)', 
                      labels = c('<0.50', '0.50 - 0.75', '0.75 - 1', '1 - 5', '5 - 10', '>10'), 
                      breaks = c('#f2f3f4', '#bdc3c7', '#626567', '#c70039', '#900c3f', '#581845'), 
                      guide = 'legend') +
  # theme(legend.position = 'none') + 
  coord_sf(xlim = c(-90.172, -90.148), ylim = c(38.595, 38.612), crs = 4326) +
  annotation_scale(location = "bl", width_hint = 0.2) 
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
  #                        style = north_arrow_orienteering)
ggsave('output/map-filled-parcels-zoomed-out.png', height = 6, width = 8, units = 'in') 


ggplot(sf_total_pcbs, aes(fill = colors)) + 
  annotation_map_tile(type = 'cartolight', zoom = 18) +
  geom_sf(alpha = 0.75, size = 0.25) +
  # geom_sf(data = esl, fill = NA, color = 'gray') + 
  # geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  # geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  # scale_fill_gradient(low = 'orange', high = 'red') +
  scale_fill_identity('Concentration\n(ppm)', 
                      labels = c('<0.50', '0.50 - 0.75', '0.75 - 1', '1 - 5', '5 - 10', '>10'), 
                      breaks = c('#f2f3f4', '#bdc3c7', '#626567', '#c70039', '#900c3f', '#581845'), 
                      guide = 'legend') +
  # theme(legend.position = 'none') + 
  coord_sf(xlim = c(-90.171, -90.160), ylim = c(38.606, 38.612), crs = 4326) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = 'none'
  )
ggsave('output/map-filled-parcels-zoomed-in-left.png', height = 4, width = 5, units = 'in') 


ggplot(sf_total_pcbs, aes(fill = colors)) + 
  annotation_map_tile(type = 'cartolight', zoom = 18) +
  geom_sf(alpha = 0.75, size = 0.25) +
  # geom_sf(data = esl, fill = NA, color = 'gray') + 
  # geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
  # geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
  # scale_fill_gradient(low = 'orange', high = 'red') +
  scale_fill_identity('Concentration\n(ppm)', 
                      labels = c('<0.50', '0.50 - 0.75', '0.75 - 1', '1 - 5', '5 - 10', '>10'), 
                      breaks = c('#f2f3f4', '#bdc3c7', '#626567', '#c70039', '#900c3f', '#581845'), 
                      guide = 'legend') +
  # theme(legend.position = 'none') + 
  coord_sf(xlim = c(-90.164, -90.158), ylim = c(38.598, 38.605), crs = 4326) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = 'none'
  )
ggsave('output/map-filled-parcels-zoomed-in-middle.png', height = 4, width = 3, units = 'in') 
