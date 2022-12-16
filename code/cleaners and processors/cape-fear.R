library(tidyverse)

fname <- 'data/All Samples.xlsx'
tidyxl::xlsx_sheet_names(fname)

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

# Cape Fear data
cf <- tidyxl::xlsx_cells(fname, sheet = 'Cape Fear')
glimpse(cf)

cf_sample_cols <- map(2:7, create_sample_cols) %>% bind_cols

cf2 <- cf[between(cf$row, 10, 229), ]
analytes <- tibble(analyte = cf2[cf2$col == 1, ]$character, 
                   casrn = cf2[cf2$col == 2, ]$character)
cfdf <- merge(cf_sample_cols, analytes, all = T) %>% 
  mutate(conc = cf2[cf2$col %in% col_wanted('Conc')$col, ]$numeric, 
         qualifier = cf2[cf2$col %in% col_wanted('Q')$col, ]$character, 
         rl = cf2[cf2$col %in% col_wanted('RL')$col, ]$numeric, 
         dl = cf2[cf2$col %in% col_wanted('DL')$col, ]$numeric, 
         sheet = 'Cape Fear')

write_csv(cfdf, 'data/cape-fear.csv')
