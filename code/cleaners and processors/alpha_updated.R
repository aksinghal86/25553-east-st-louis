
library(tidyverse)

fname <- "P:/26214/Monitoring Results/all lab data kwb.xlsx"
sheets <- tidyxl::xlsx_sheet_names(fname)

create_sample_cols <- function(r) { 
  cf %>% 
    filter(row == r) %>% 
    select(character, numeric, date) %>% 
    unite('value', character:date, na.rm = T) %>% 
    filter(value != '') %>% 
    janitor::row_to_names(row_number = 1) %>% 
    janitor::clean_names()
}


col_wanted <- function(name) { 
  cf[cf$row == 8 & cf$character == name, 'col']
}

# New Alpha Excel
cf <- tidyxl::xlsx_cells(fname, sheet = sheets[str_detect(sheets, 'Samples ')]) %>%
  filter(!(sheet=="Samples 241-320" & col >= 213)) ## There are placeholders for more samples in the excel but no data

glimpse(cf)

cf_sample_cols <- map(c(2,3,4,6), create_sample_cols) %>%
  bind_cols %>%
  filter(sample_id != "SAMPLE ID: ") %>%
  mutate(sheet = c(rep(sheets[2], 80), rep(sheets[3], 80), 
                   rep(sheets[4], 80), rep(sheets[5],70)),
         parcel = NA) %>%
  select(location = sample_id, 
         parcel,
         sampling_date = collection_date,
         lab_sample_id = lab_id,
         sample_type = sample_matrix,
         sheet)


cf2 <- cf[between(cf$row, 8, 209), ] 

analytes <- cf2 <- cf[between(cf$row, 39, 209), ]  # starts at PCB 1, ends at PCB 209 (excludes TEQ, total solids)

analytes <- cf2[cf2$col == 1, c('sheet','character')] %>%
  mutate(casrn = cf2[cf2$col==1, ]$character) %>%
  rename(analyte = character) %>%
  unique

cfdf <- merge(cf_sample_cols, analytes, all = T)%>% 
  mutate(conc = cf2[cf2$col %in% col_wanted('Result')$col, ]$numeric, 
         qualifier = cf2[cf2$col %in% col_wanted('Flg')$col, ]$character, 
         rl = NA, 
         dl = cf2[cf2$col %in% col_wanted('RL')$col, ]$numeric)

## LKT TO DO -- get location, casrn, do a quick QC check

write_csv(cfdf, 'data/cape-fear.csv')
