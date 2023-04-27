### Mostly junk. Ignore

library(tidyverse)
library(terra) 

sfdf <- terra::vect('data/gis/all-data-with-geos.shp')
df <- tibble(values(sfdf)) 

not_totals <- df %>% filter(!str_detect(analyte, 'Total'))
totals <- df %>% filter(str_detect(analyte, 'Total'), analyte != 'Total PCBs')

totals %>% group_by(analyte) %>% count

# Discrepancy between Alpha and Cape Fear results. 
# Below 1 ppb, results from Cape Fear are higher than Alpha labs
ggplot(not_totals, aes(x = est_conc, color = lab)) + 
  stat_ecdf() + 
  scale_x_log10() +
  annotation_logticks(sides = 'b') +
  labs(x = 'Concentration (ppb)', y = 'Cumulative probability', 
       title = 'Cumulative distribution plot of concentrations by lab', 
       subtitle = '(NDs replaced with DL)') +
  theme_bw() +
  theme(
    legend.title = element_blank()
  )

ggplot(not_totals, aes(x = est_conc)) + 
  geom_histogram(aes(y = ..density.., fill = lab), color = 'white', alpha = 0.25) +
  geom_density(aes(color = lab), size = 1) + 
  scale_x_log10() +
  annotation_logticks(sides = 'b') +
  labs(x = 'Concentration (ppb)', y = 'Density', 
       title = 'Density plot of concentrations by lab', 
       subtitle = '(NDs replaced with DL)') +
  theme_bw() +
  theme(
    legend.title = element_blank()
  )

not_totals %>% 
  group_by(lab) %>% 
  summarize(
    n = n(), 
    conc = mean(est_conc)
  )

# The greatest difference is in concentrations below 1 ppb, where concentrations
# reported by Cape Fear are approximately 1.7-fold higher when geometric means 
# are compared
not_totals %>% 
  group_by(lab, intervals = cut(est_conc, breaks = c(-Inf,  0.01, 0.1, 1, 10, 100, 1000, Inf))) %>% 
  # summarize(median_conc = median(est_conc)) %>%
  summarize(gm_conc = exp(mean(log(est_conc)))) %>%
  spread(lab, gm_conc) %>% 
  mutate(diff = `Cape Fear`/Alpha) %>% 
  ggplot(aes(intervals, diff)) + 
  geom_col() +
  geom_text(aes(label = round(diff, 2)), vjust = -1) + 
  labs(title = 'Cape Fear:Alpha') +
  geom_hline(yintercept = 1) + 
  theme_bw() 

# The differences are consistent across congener groups though with some variability. 
# For example, they seem to diverge more as the # of chlorinated carbons goes up. 
ggplot(not_totals, aes(x = est_conc, color = lab)) + 
  stat_ecdf() + 
  facet_wrap(~n_cl) + 
  scale_x_log10() +
  annotation_logticks(sides = 'b') +
  labs(x = 'Concentration (ppb)', y = 'Cumulative probability', 
       title = 'Cumulative distribution plot of concentrations by lab', 
       subtitle = '(NDs replaced with DL)') +
  theme_bw() +
  theme(
    legend.title = element_blank()
  )

sfdf <- sf::st_as_sf(sfdf)
parcels <- sfdf %>% 
  filter(analyte == 'Total PCBs')
ggplot(parcels) + 
  geom_sf(aes(color = lab, fill = est_conc)) +
  scale_fill_gradient2() +
  theme_bw()
# the difference could potentially be due to parcels closest to former Monsanto plant
# were analyzed by Cape Fear. The corollary, i.e., results are higher closer to Monsanto
# because those samples were analyzed by Cape Fear does not hold weight for the following reasons: 
# 1. differences between parcels is much greater than the differences between labs
# 2. often, the concentrations from Alpha are orders of magnitude higher than Cape Fear. 
# 3. in the case of some PCBs, e.g., total PCBs, 9 of the 10  highest concentrations are from Alpha not from Cape Fear
# 4. Differences between labs is greatest at concentrations <1 ppb and, in this case, concentrations are in 1000 of ppbs
# so any difference between labs is largely negligible in comparison. 

df %>% 
  filter(analyte == 'Total PCBs') %>% 
  arrange(desc(est_conc)) %>% 
  select(parcelnumb, lab, est_conc)
