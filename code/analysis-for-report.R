# Analysis

# Libraries
library(tidyverse) 
library(sf) 
library(ggspatial)
library(flextable)
library(patchwork)

sfdf <- terra::vect('data/gis/all-data-for-production-with-geos.shp') |> 
  sf::st_as_sf() |> 
  filter(!str_detect(parcelnumb, 'ROW|RIGHT OF WAY')) |> 
  mutate(conc = conc/1000, 
         units = 'ppm') |> 
  select(-est_conc, -est_method)
sfdf_by_parcel <- terra::vect('data/gis/data-by-parcel.shp') |> 
  sf::st_as_sf() |> 
  filter(!str_detect(parcelnumb, 'ROW|RIGHT OF WAY')) |> 
  mutate(conc = conc/1000, 
         units = 'ppm') |> 
  select(-est_conc, -est_method)
sf_total_pcbs <- sfdf_by_parcel |> filter(analyte == "Total PCBs")

df <- read_csv('data/all-data-clean-for-production.csv') |> 
  filter(!str_detect(parcel, 'ROW|RIGHT OF WAY')) |> 
  mutate(conc = conc/1000, 
         units = 'ppm') |> 
  select(-est_conc, -est_method)
df_by_parcel <- read_csv('data/trimmed-data-by-parcel.csv') |> 
  filter(!str_detect(parcel, 'ROW|RIGHT OF WAY')) |> 
  mutate(conc = conc/1000, 
         units = 'ppm') |> 
  select(-est_conc, -est_method)
totals <- df_by_parcel |> filter(str_detect(analyte, 'Total'))
total_pcbs <- df_by_parcel |> filter(analyte == 'Total PCBs')
homologs <- totals |> filter(!is.na(n_cl))

esl <- sf::st_read('dashboard/data/esl.kml')
monsanto <- sf::st_read('dashboard/data/gis/monsanto.kml', quiet = T) %>% sf::st_zm()
monsanto_storage <- st_read('dashboard/data/gis/monsanto-storage.kml', quiet = T) %>% st_zm()
monsanto_incin <- st_read('dashboard/data/gis/monsanto-incinerator.kml', quiet = T) %>% st_zm()

df |> distinct(lab_id) |> count() # 354 samples
df |> distinct(parcel) |> count() # 273 parcels sampled


#### Maps -----

# Full zoomed out map with E STL boundary + Monsanto plant
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

# Zoomed-in to show sampled parcels
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

total_pcbs |> 
  filter(conc >= 1) |> 
  count()

# colfunc_reds <- colorRampPalette(c('#f79540', '#fc4f00', '#b22222', '#b71375', '#8b1874', '#581845'))
colfunc_grays <- colorRampPalette(c('#d6d6d6', '#bdbdbd', '#949494', '#737373', '#4d4d4d', '#262626'))
# reds <- colfunc_reds(20)
reds <- viridis::viridis_pal(direction = -1, option = 'F')(25)
grays <- colfunc_grays(25)
scales::show_col(reds)
scales::show_col(grays)

bins_and_colors <- tibble(breaks = cut(total_pcbs$conc, breaks = c(0, 0.034, 0.25, 0.50, 1, 5, 10, Inf))) |>
  distinct(breaks) |>
  arrange(breaks) |>
  mutate(labels = factor(c('<0.034', '0.034 - 0.25', '0.25 - 0.5', '0.5 - 1', '1 - 5', '5 - 10', '>10')),
         cutoff_1 = factor(c(grays[2], grays[10], grays[16], grays[22], reds[11], reds[16], reds[21])),
         cutoff_0.25 = factor(c(grays[2], grays[10], reds[6], reds[8], reds[11], reds[16], reds[21])),
         cutoff_0.034 = factor(c(grays[2], reds[2], reds[6], reds[8], reds[11], reds[16], reds[21])))

basemap <- function(cutoff = 1) {
  dfcol <- paste0('cutoff_', cutoff)
  bc <- bins_and_colors |> select(breaks, labels, colors = dfcol)

  mapdata <- sf_total_pcbs |>
    mutate(breaks = cut(conc, breaks = c(0, 0.034, 0.25, 0.50, 1, 5, 10, Inf))) |>
    left_join(bc, by = 'breaks') |>
    mutate(labels = factor(labels, levels = bc$labels),
           colors = factor(colors, levels = bc$colors))

  ggplot(mapdata, aes(fill = colors)) +
    annotation_map_tile(type = 'cartolight', zoom = 15) +
    geom_sf(alpha = 0.85, size = 0.25) +
    geom_sf(data = esl, fill = NA, color = 'gray') +
    geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
    geom_sf(data = monsanto_incin, fill = 'orange', color = NA) +
    scale_fill_identity('Concentration\n(ppm)',
                        labels = bins_and_colors$labels,
                        guide = 'legend') +
    coord_sf(xlim = c(-90.172, -90.148), ylim = c(38.595, 38.612), crs = 4326) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    cowplot::theme_map()
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
  #                        style = north_arrow_orienteering)
}

basemap(cutoff = 1) + 
  labs(title = 'Parcels with Total PCB concentration >1 ppm', 
       subtitle = 'Parcels with >1 ppm concentration shown as shades of red; otherwise gray')
ggsave('output/map-filled-parcels-zoomed-out-1ppm.png', height = 6, width = 8, units = 'in') 

basemap(cutoff = 0.25) + 
  labs(title = 'Parcels with Total PCB concentration >0.25 ppm', 
       subtitle = 'Parcels with >0.25 ppm concentration shown as shades of red; otherwise gray')
ggsave('output/map-filled-parcels-zoomed-out-0.25ppm.png', height = 6, width = 8, units = 'in') 

basemap(cutoff = 0.034) +
  labs(title = 'Parcels with Total PCB concentration >0.034 ppm', 
       subtitle = 'Parcels with >0.034 ppm concentration shown as shades of red; otherwise gray')
ggsave('output/map-filled-parcels-zoomed-out-0.034ppm.png', height = 6, width = 8, units = 'in') 


# ggplot(sf_total_pcbs, aes(fill = colors)) + 
#   annotation_map_tile(type = 'cartolight', zoom = 15) +
#   geom_sf(alpha = 0.85, size = 0.25) +
#   geom_sf(data = esl, fill = NA, color = 'gray') + 
#   geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
#   geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
#   scale_fill_identity('Concentration\n(ppm)',
#                       labels = c('<0.034', '0.034 - 0.25', '0.25 - 0.5', '0.5 - 1', '1 - 5', '5 - 10', '>10'),
#                       guide = 'legend') +
#   coord_sf(xlim = c(-90.172, -90.148), ylim = c(38.595, 38.612), crs = 4326) +
#   annotation_scale(location = "bl", width_hint = 0.2) +
#   cowplot::theme_map()
# # annotation_north_arrow(location = "bl", which_north = "true",
# #                        pad_x = unit(0.50, "in"), pad_y = unit(0.25, "in"),
# #                        style = north_arrow_orienteering)
# ggsave('output/map-filled-parcels-zoomed-out.png', height = 6, width = 8, units = 'in') 

zoomed_left <- function(cutoff = 1) { 
  dfcol <- paste0('cutoff_', cutoff)
  bc <- bins_and_colors |> select(breaks, labels, colors = dfcol)
  
  mapdata <- sf_total_pcbs |>
    mutate(breaks = cut(conc, breaks = c(0, 0.034, 0.25, 0.50, 1, 5, 10, Inf))) |>
    left_join(bc, by = 'breaks') |>
    mutate(labels = factor(labels, levels = bc$labels),
           colors = factor(colors, levels = bc$colors))
  
  ggplot(mapdata, aes(fill = colors)) + 
    annotation_map_tile(type = 'cartolight', zoom = 18) +
    geom_sf(alpha = 0.85, size = 0.25) +
    # geom_sf(data = esl, fill = NA, color = 'gray') + 
    # geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
    # geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
    # scale_fill_gradient(low = 'orange', high = 'red') +
    scale_fill_identity() +
    coord_sf(xlim = c(-90.171, -90.160), ylim = c(38.606, 38.612), crs = 4326) +
    cowplot::theme_map() 
}

zoomed_left(cutoff = 1)
ggsave('output/map-filled-parcels-zoomed-in-left-1ppm.png', height = 4, width = 5, units = 'in')

zoomed_left(cutoff = 0.25)
ggsave('output/map-filled-parcels-zoomed-in-left-0.25ppm.png', height = 4, width = 5, units = 'in') 

zoomed_left(cutoff = 0.034)
ggsave('output/map-filled-parcels-zoomed-in-left-0.034ppm.png', height = 4, width = 5, units = 'in') 

# 
# ggplot(sf_total_pcbs, aes(fill = colors)) + 
#   annotation_map_tile(type = 'cartolight', zoom = 18) +
#   geom_sf(alpha = 0.85, size = 0.25) +
#   # geom_sf(data = esl, fill = NA, color = 'gray') + 
#   # geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
#   # geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
#   # scale_fill_gradient(low = 'orange', high = 'red') +
#   scale_fill_identity() +
#   coord_sf(xlim = c(-90.171, -90.160), ylim = c(38.606, 38.612), crs = 4326) +
#   cowplot::theme_map() 
# ggsave('output/map-filled-parcels-zoomed-in-left.png', height = 4, width = 5, units = 'in') 


zoomed_right <- function(cutoff = 1) { 
  dfcol <- paste0('cutoff_', cutoff)
  bc <- bins_and_colors |> select(breaks, labels, colors = dfcol)
  
  mapdata <- sf_total_pcbs |>
    mutate(breaks = cut(conc, breaks = c(0, 0.034, 0.25, 0.50, 1, 5, 10, Inf))) |>
    left_join(bc, by = 'breaks') |>
    mutate(labels = factor(labels, levels = bc$labels),
           colors = factor(colors, levels = bc$colors))
  
  ggplot(mapdata, aes(fill = colors)) + 
    annotation_map_tile(type = 'cartolight', zoom = 18) +
    geom_sf(alpha = 0.85, size = 0.25) +
    scale_fill_identity() +
    # geom_sf(data = esl, fill = NA, color = 'gray') + 
    # geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
    # geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
    # scale_fill_gradient(low = 'orange', high = 'red') +
    coord_sf(xlim = c(-90.164, -90.158), ylim = c(38.598, 38.605), crs = 4326) +
    cowplot::theme_map() 
}

zoomed_right(cutoff = 1) 
ggsave('output/map-filled-parcels-zoomed-in-middle-1ppm.png', height = 4, width = 3, units = 'in') 

zoomed_right(cutoff = 0.25) 
ggsave('output/map-filled-parcels-zoomed-in-middle-0.25ppm.png', height = 4, width = 3, units = 'in') 

zoomed_right(cutoff = 0.034) 
ggsave('output/map-filled-parcels-zoomed-in-middle-0.034ppm.png', height = 4, width = 3, units = 'in') 

# ggplot(sf_total_pcbs, aes(fill = colors)) + 
#   annotation_map_tile(type = 'cartolight', zoom = 18) +
#   geom_sf(alpha = 0.85, size = 0.25) +
#   scale_fill_identity() +
#   # geom_sf(data = esl, fill = NA, color = 'gray') + 
#   # geom_sf(data = monsanto, fill = NA, color = 'orange', size = 0.5) +
#   # geom_sf(data = monsanto_incin, fill = 'orange', color = NA) + 
#   # scale_fill_gradient(low = 'orange', high = 'red') +
#   coord_sf(xlim = c(-90.164, -90.158), ylim = c(38.598, 38.605), crs = 4326) +
#   cowplot::theme_map() 
# ggsave('output/map-filled-parcels-zoomed-in-middle.png', height = 4, width = 3, units = 'in') 



#### Plots ----

## Detections by homolog and for total PCBs
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


## Lognormal distribution plot by homolog and total PCBs
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


## Lognormal distribution for total PCBs only
p1 <- ggplot(totals |> filter(is.na(n_cl)), aes(x = conc)) + 
  stat_ecdf() + 
  scale_x_log10(labels = scales::label_log()) +
  annotation_logticks(sides = 'b') + 
  labs(y = "Cumulative probability distribution", x = "Concentration, log scale (ppm)") +
  # title = "PCB concentration by homolog and for total PCBs in samples collected in E. STL") + 
  # annotation_logticks(sides = 'b') + 
  cowplot::theme_cowplot() + 
  theme(
    plot.title.position = 'plot'
  )
p1
ggsave(plot = p1, 'output/ecdf-total-pcbs-only.png', height = 6, width = 8, units = 'in') 


## Boxplot by homolog and for total pcbs
totals |> 
  group_by(parcel, homolog, n_cl) |>
  summarize(conc = mean(conc)) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(homolog, n_cl), y = conc)) + 
  geom_boxplot(coef = Inf, outlier.shape = NA) + 
  geom_jitter(alpha = 0.20) + 
  stat_summary(geom = 'point', fun = mean, size = 4, shape = 18) + 
  scale_y_log10(labels = scales::label_log()) + 
  annotation_logticks(sides = 'l') + 
  labs(y = 'Concentration, log scale (ppm)', x = NULL) +  
  # title = 'PCB concentration by homolog and for total PCBs') + 
  geom_hline(yintercept = 1, color = '#922b21', linetype = 'dashed') + 
  geom_hline(yintercept = 0.25, color = '#a04000', linetype = 'dashed') + 
  geom_hline(yintercept = 0.034, color = '#b9770e', linetype = 'dashed') + 
  annotate(x = 1, y = 1.45, geom = 'text', label = '1 ppm clean-up threshold', color = '#922b21', hjust = 0) + 
  annotate(x = 1, y = 0.35, geom = 'text', label = 'Alternate 0.25 ppm clean-up threshold', color = '#a04000', hjust = 0) + 
  annotate(x = 1, y = 0.045, geom = 'text', label = 'Alternate 0.034 clean-up threshold', color = '#b9770e', hjust = 0) + 
  cowplot::theme_cowplot() + 
  theme(plot.title.position = 'plot')
ggsave('output/concentation-boxplot.png', height = 6, width = 8, units = 'in') 


## Boxplot for total PCBs only
p2 <- ggplot(total_pcbs, aes(x = "Total PCBs", y = conc)) + 
  geom_boxplot(coef = Inf, outlier.shape = NA) + 
  geom_jitter(alpha = 0.25) + 
  stat_summary(geom = 'point', fun = mean, size = 4, shape = 18) + 
  scale_y_log10(labels = scales::label_log()) + 
  annotation_logticks(sides = 'l') + 
  labs(y = 'Concentration, log scale (ppm)', x = NULL) +  
  # title = 'PCB concentration by homolog and for total PCBs') + 
  geom_hline(yintercept = 1, color = '#922b21', linetype = 'dashed') + 
  geom_hline(yintercept = 0.25, color = '#a04000', linetype = 'dashed') + 
  geom_hline(yintercept = 0.034, color = '#b9770e', linetype = 'dashed') + 
  annotate(x = 0.5, y = 1.3, geom = 'text', label = '1 ppm clean-up threshold', color = '#922b21', hjust = 0) + 
  annotate(x = 0.5, y = 0.3, geom = 'text', label = 'Alternate 0.25 ppm clean-up threshold', color = '#a04000', hjust = 0) + 
  annotate(x = 0.5, y = 0.04, geom = 'text', label = 'Alternate 0.034 ppm clean-up threshold', color = '#b9770e', hjust = 0) + 
  cowplot::theme_cowplot() + 
  theme(plot.title.position = 'plot')
p2
ggsave(plot = p2, 'output/concentation-boxplot-total-pcbs-only.png', height = 6, width = 8, units = 'in') 

## Lognormal distribution + boxplot for total PCBs only
# TODO: @Laura Perhaps these are better as individual plots. But please make sure that the boxplot 
# color scheme is consistent with what you use in the maps. 
p1 + p2

# Alternate layout. Not a big fan
layout <- c(
  area(t = 2, l = 1, b = 5, r = 4), 
  area(t = 1, l = 4, b = 2, r = 5) 
)
p2 + p1 + 
  plot_layout(design = layout)
ggsave('output/total-pcbs-only-log-cum-and-boxplot.png', height = 6, width = 8, units = 'in') 



# Bar plot to demonstrate the number of parcels per bin visually
total_pcbs |> 
  group_by(breaks) |> 
  count()
plotdata <- total_pcbs |>
  mutate(breaks = cut(conc, breaks = c(0, 0.034, 0.25, 0.50, 1, 5, 10, Inf))) |>
  group_by(breaks) |> 
  count() |> 
  left_join(bins_and_colors |> select(breaks, labels, colors = cutoff_0.034), by = 'breaks') 
ggplot(plotdata, aes(x = breaks, y = n, fill = colors)) + 
  geom_col() + 
  scale_fill_identity() + 
  geom_text(aes(label = n), vjust = -0.5) + 
  scale_x_discrete(labels = bins_and_colors$labels) + 
  labs(x = 'Total PCBs (ppm)', y = 'Number of parcels') + 
  cowplot::theme_cowplot()
ggsave('output/num-parcels-by-bin.png', height = 6, width = 8, units = 'in') 

#### Tables ----
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


# @Laura: ignore the stuff below. 
#### The dirty dozen ----- 
sfdf <- terra::merge(sf_total_pcbs |> select(parcelnumb), df, all.x = F, by.x = 'parcelnumb', by.y = 'parcel', how = 'inner')
dirty_dozen_pcbs <- c("PCB-77", "PCB-81", "PCB-105", "PCB-114", "PCB-118", "PCB-123", 
                 "PCB-126", "PCB-156", "PCB-157", "PCB-167", "PCB-169", "PCB-189")
dirty_dozen <- df |> filter(analyte %in% dirty_dozen_pcbs)

ggplot(dirty_dozen, aes(x = reorder(analyte, n_cl), y = conc)) + 
  geom_boxplot(coef = Inf, outlier.shape = NA) + 
  geom_jitter(alpha = 0.20) + 
  stat_summary(geom = 'point', fun = mean, size = 4, shape = 18) + 
  scale_y_log10(labels = scales::label_log()) + 
  annotation_logticks(sides = 'l') +
  cowplot::theme_cowplot()

sf_dirty_dozen <- sfdf |> filter(analyte %in% dirty_dozen_pcbs) |> 
  mutate(group = 'dirty dozen') 

sf_pcb209 <- sfdf |> filter(analyte == 'PCB-209') |> mutate(group = 'pcb-209') 

p1 <- sf_dirty_dozen |> 
  group_by(parcelnumb) |> 
  summarize(conc = sum(conc, na.rm = T)) |> 
  ggplot() +
  geom_sf(aes(fill = log(conc)), color = 'lightgray', size = 0.5) + 
  scale_fill_gradient(low = 'white', high = 'red') + 
  labs(title = 'Dirty dozen') +
  cowplot::theme_map()

p2 <- sf_pcb209 |> 
  group_by(parcelnumb) |> 
  summarize(conc = sum(conc, na.rm = T)) |> 
  ggplot() + 
  geom_sf(aes(fill = log(conc)), color = 'lightgray', size = 0.5) + 
  scale_fill_gradient(low = 'white', high = 'red') + 
  labs(title = 'PCB-209') + 
  cowplot::theme_map()

p1 + p2 +
  plot_layout(guides = 'collect')

df |> filter(analyte %in% c(dirty_dozen_pcbs, "PCB-209")) |> 
  mutate(group = ifelse(analyte == "PCB-209", "pcb209", 'dirtydozen')) |> 
  group_by(parcel, group) |> 
  summarize(conc = sum(conc, na.rm = T))  |> 
  pivot_wider(names_from = group, values_from = conc) |> 
  ggplot(aes(x = dirtydozen, y = pcb209)) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method = 'loess', se = F) 
  