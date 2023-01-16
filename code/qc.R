library(tidyverse) 

df <- read_csv('data/all-data.csv')
head(df)

df %>% 
  group_by(lab, location, lab_id) %>% 
  count 

df %>% 
  filter(!str_detect(analyte, 'Total')) %>% 
  mutate(halfdl = ifelse(detected, est_conc, est_conc/2), 
         zerodl = ifelse(detected, est_conc, 0)) %>% 
  group_by(sheet, lab, location, lab_id) %>% 
  summarize(total_pcbs_dl = sum(est_conc), 
            total_pcbs_half_df = sum(halfdl), 
            total_pcbs_zero_dl = sum(zerodl),
            n = n(), 
            n_detected = sum(detected), 
            nds = n - n_detected) %>% 
  write_csv('data/qc-delete.csv')
