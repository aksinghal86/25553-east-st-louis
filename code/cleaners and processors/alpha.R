library(tidyverse)

create_sample_cols <- function(r) { 
  alpha %>% 
    filter(row == r) %>% 
    select(character, numeric, date) %>% 
    unite('value', character:date, na.rm = T) %>% 
    filter(value != '') %>% 
    janitor::row_to_names(row_number = 1) %>% 
    janitor::clean_names()
}

fname <- 'data/All Samples.xlsx'
sheets <- tidyxl::xlsx_sheet_names(fname)
alpha <- tidyxl::xlsx_cells(fname, sheets = sheets[str_detect(sheets, 'Alph')])
glimpse(alpha)

alpha_sample_cols <- map(4:8, create_sample_cols) %>% 
  bind_cols %>%
  filter(location != 'LOCATION') %>% 
  mutate(sheet = c(rep(sheets[3], 99), rep(sheets[4], 100), rep(sheets[5], 48)))


alpha_pcb <- alpha[between(alpha$row, 44, 214), ]
alpha_analytes <- alpha_pcb[alpha_pcb$col == 2, c('sheet', 'character')] %>% 
  mutate(casrn = alpha_pcb[alpha_pcb$col == 3, ]$character) %>% 
  rename(analyte = character)
alpha_data <- alpha_pcb[alpha_pcb$col >= 6 , c('sheet', 'character', 'numeric', 'local_format_id')] %>% 
  unite("value", character:numeric, na.rm = T)


clean_alpha <- alpha_analytes %>% 
  left_join(alpha_sample_cols, by = 'sheet') %>% 
  bind_cols(alpha_data %>% select(value, local_format_id)) %>%
  mutate(units = 'ug/kg', 
         conc = as.numeric(value)) %>% 
  filter(!is.na(conc)) 

formats <- tidyxl::xlsx_formats(fname)
nds_id <- which(formats$local$numFmt == "\"ND\"(General)")

clean_alpha <- clean_alpha %>% 
  mutate(detected = local_format_id != nds_id) %>% 
  select(-value, -local_format_id)
write_csv(clean_alpha, 'data/alpha.csv')
