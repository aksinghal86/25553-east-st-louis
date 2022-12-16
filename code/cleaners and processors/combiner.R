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

write_csv(alpha %>% bind_rows(cape), 'data/all-data.csv')
