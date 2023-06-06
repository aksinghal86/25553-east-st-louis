
library(tidyverse)

fname <- "P:/26214/Monitoring Results/all lab data kwb.xlsx"
sheets <- tidyxl::xlsx_sheet_names(fname)

create_sample_cols <- function(r) { 
  ## For a given row, combines any col that has a value (character, numeric, or date)
  ## Removes empty cells and cleans the column name
  al %>% 
    filter(row == r) %>% 
    select(character, numeric, date) %>% 
    unite('value', character:date, na.rm = T) %>% 
    filter(value != '') %>% 
    janitor::row_to_names(row_number = 1) %>% 
    janitor::clean_names()
}


col_wanted <- function(name) { 
  al[al$row == 8 & al$character == name, 'col']
}

# New Alpha Excel
al <- tidyxl::xlsx_cells(fname, sheet = sheets[str_detect(sheets, 'Samples ')]) %>%
  filter(!(sheet=="Samples 241-320" & col >= 213)) ## There are placeholders for more samples in the excel but no data starting at column 213

glimpse(al)

## Selects the header of the sheet (rows 2-6), makes df with the basic sample info (IDs, date, matrix) 
al_sample_cols <- map(c(2,3,4,6), create_sample_cols) %>%
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


al2 <- al[between(al$row, 39, 209), ] # starts at PCB 1, ends at PCB 209 (excludes TEQ, total solids)

analytes <- al2[al2$col == 1, c('character')] %>%
  mutate(casrn = NA) %>%
  rename(analyte = character) %>%
  unique


aldf <- merge(al_sample_cols, analytes, all = T) %>%
  mutate(analyte = factor(analyte, ordered = TRUE, levels = c(unique(analyte))),
         sheet = factor(sheet, ordered = TRUE, levels = c(unique(sheet)))) %>%
  arrange(sheet, analyte) %>%
  mutate(
    # Finds the result column (concentration) 
    conc = al2[al2$col %in% col_wanted('Result')$col, ]$numeric, 
    
    # Finds the result column where it interpreted as a character
    conc_ch = al2[al2$col %in% col_wanted('Result')$col, ]$character, 
    
    # Finds the flag column
    qualifier = al2[al2$col %in% col_wanted('Flg')$col, ]$character, 
    
    # There is no reporting limit in this dataset so set to NA
    rl = NA, 
    
    # Finds the detection limit column
    dl = al2[al2$col %in% col_wanted('RL')$col, ]$numeric,
    dl_ch =al2[al2$col %in% col_wanted('RL')$col, ]$character
  ) 



# The sample IDs did not have parcel location included in the data set so I'm going
# to merge the original alpha data output of sample ID + parcel with the new 
# data output. They also did not have the CASRN
dat_original <- read.csv("C:/Users/ltravis/EH&E Dropbox/Laura Travis/25553 e stl/data/alpha.csv")
dat_parcel <- dat_original %>%
  select(location, parcel, lab_sample_id) %>%
  distinct()

# merge data with parcel info
alpha_parcel <- aldf %>%
  select(-parcel) %>%
  left_join(dat_parcel)

# check if there is missing parcel info -- there is 
test <- alpha_parcel %>% filter(is.na(parcel)) %>% select(location, lab_sample_id) %>% distinct()
nrow(test)

# 310 sample IDs in the new data file
nrow(unique(alpha_parcel%>% select(location, lab_sample_id)))

# 245 sample IDs in the old data file --> 65 missing IDs --> matches the number without
# parcel info when the two are merged. Need to get from Ankur. 
nrow(unique(dat_parcel%>% select(location, lab_sample_id)))
310-245 == nrow(test)

# Pull the CAS numbers associated with each analyte
dat_cas <- dat_original %>%
  select(analyte, casrn) %>%
  distinct() %>%
  mutate(analyte = factor(analyte, ordered = TRUE, levels = c(unique(analyte))))

# Join in the CAS number for each analyte - right join so it keeps the PCB order correct
alpha_parcel <- alpha_parcel %>%
  select(-casrn) %>%
  left_join(dat_cas) %>%
  arrange(sheet, lab_sample_id, analyte)

# check if there is missing CAS info -- No!
alpha_parcel %>% filter(is.na(casrn)) %>% select(location, lab_sample_id, analyte) %>% distinct()


# Change the Qualifier column to NA if blank
alpha_parcel <- alpha_parcel %>%
  mutate(qualifier = case_when(
    qualifier == "" | qualifier == " " ~ NA_character_,
    TRUE ~ qualifier
  ))

# Add a units column
alpha_parcel <- alpha_parcel %>% 
  mutate(units = "ug/kg")

# Fix concentration column -- sometimes value interpreted as character
alpha_parcel <- alpha_parcel %>% 
  mutate(conc = case_when(
    is.na(conc) ~ as.numeric(conc_ch),
    TRUE ~ conc
  )) %>%
  # Fix DL column -- sometimes value interpreted as character
  mutate(dl = case_when(
    is.na(dl)    ~ as.numeric(dl_ch),
    dl_ch == "-" ~ NA_real_,
    TRUE         ~ dl
  ))

test <- alpha_parcel %>% filter(is.na(conc)) # check where there is an NA in conc
unique(test$conc_ch) # only for data where the results was listed as ND or "-"

test <- alpha_parcel %>% filter(is.na(dl)) # check where there is an NA in DL
unique(test$dl_ch) # only for data where the DL was listed as "-"

# Add a detection column
alpha_parcel <- alpha_parcel %>%
  mutate(detected = case_when(
    is.na(conc) ~ FALSE,
    TRUE ~ TRUE)
  ) 

## Saving the data with missing values in concentration or missing parcel info to separate dataset
alpha_missing <- alpha_parcel %>%
  filter(is.na(parcel) | str_detect(dl_ch, "-"))


# Subset to final columns, remove data with the missing info
alpha_final <- alpha_parcel %>%
  filter(!is.na(dl)) %>%
  select(sheet, analyte, casrn, location, parcel, sampling_date, lab_sample_id,
         sample_type, units, conc, detected, rl, dl, qualifier)

# check we didn't lose any rows
nrow(alpha_missing) + nrow(alpha_final)==nrow(aldf)


write_csv(alpha_final, 'data/updated-alpha.csv')
write_csv(alpha_missing, 'data/alpha-missing-data.csv')
