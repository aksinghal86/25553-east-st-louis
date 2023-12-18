
# Pulled the list of aroclor that had all "--" from here:
# "P:\26100\Data Extraction\Aroclor data\congener percentages_QC.xlsx"
# Filtered the second sheet to where the value was 0.01/sqrt(2)

library(tidyverse)
library(readxl)


pcb_nonaro<- c(
  "PCB-100", "PCB-104", "PCB-106", "PCB-108", "PCB-11", "PCB-111", "PCB-116", "PCB-120", "PCB-121", "PCB-107",
  "PCB-112", "PCB-127", "PCB-162", "PCB-14", "PCB-140", "PCB-142", "PCB-143", "PCB-145", "PCB-148", "PCB-150",
  "PCB-152", "PCB-155", "PCB-159", "PCB-160", "PCB-161", "PCB-165", "PCB-168", "PCB-169", "PCB-182", "PCB-184",
  "PCB-186", "PCB-188", "PCB-192", "PCB-204", "PCB-209", "PCB-21", "PCB-36", "PCB-38", "PCB-39", "PCB-68", "PCB-65",
  "PCB-62", "PCB-61", "PCB-58", "PCB-78", "PCB-79", "PCB-80", "PCB-90", "PCB-98"
)

dat <- read.csv("C:\\Users\\ltravis\\EH&E Dropbox\\Laura Travis\\GitProjects\\25553-east-st-louis\\data\\for-cindi\\data-by-sample.csv")

dat2 <- dat %>%
  separate(analyte, sep ="/", into=c("A1", "A2", "A3",
                                     "A4", 'A5', 'A6'),
           remove=FALSE) %>%
  mutate(nonaroclor_flag = case_when(
    A1 %in% pcb_nonaro ~ 1,
    A2 %in% pcb_nonaro ~ 1,
    A3 %in% pcb_nonaro ~ 1,
    A4 %in% pcb_nonaro ~ 1,
    A5 %in% pcb_nonaro ~ 1,
    A6 %in% pcb_nonaro ~ 1,
    TRUE ~ 0
  )) %>% 
  select(-c(A1:A6)) %>%
  mutate()

dat2_totals <- dat2 %>%
  filter(str_detect(analyte, "Total"))

dat2_reg <- dat2 %>%
  filter(!str_detect(analyte, "Total"))

dat2_reg_nonaro <- dat2_reg %>%
  filter(nonaroclor_flag == 1)

dat2_reg_aro <- dat2_reg %>%
  filter(nonaroclor_flag != 1)

dat2_reg_nonaro_tot <- dat2_reg_nonaro %>%
  ungroup() %>%
  group_by(parcel, geoid, owner, city, county, lat, lon, area_sqft,
           area_acre, address, zipcode, lab, location, lab_id, units, n_cl, homolog) %>%
  summarize(conc = sum(conc),
            analyte = paste0("Total non-aroclor ", homolog),
            nonaroclor_flag = 1) %>%
  distinct()

dat2_reg_nonaro_tot2 <- dat2_reg_nonaro_tot %>%
  ungroup() %>%
  group_by(parcel, geoid, owner, city, county, lat, lon, area_sqft,
           area_acre, address, zipcode, lab, location, lab_id, units) %>%
  summarize(homolog = "Total",
            conc = sum(conc),
            analyte = "Total non-aroclor PCBs",
            nonaroclor_flag = 1) %>%
  distinct()


dat2_reg_aro_tot <- dat2_reg_aro %>%
  ungroup() %>%
  group_by(parcel, geoid, owner, city, county, lat, lon, area_sqft,
           area_acre, address, zipcode, lab, location, lab_id, units, n_cl, homolog) %>%
  summarize(conc = sum(conc),
            analyte = paste0("Total aroclor ", homolog),
            nonaroclor_flag = 0) %>%
  distinct()

dat2_reg_aro_tot2 <- dat2_reg_aro_tot %>%
  ungroup() %>%
  group_by(parcel, geoid, owner, city, county, lat, lon, area_sqft,
           area_acre, address, zipcode, lab, location, lab_id, units) %>%
  summarize(homolog = "Total",
            conc = sum(conc),
            analyte = "Total aroclor PCBs",
            nonaroclor_flag = 0) %>%
  distinct()

# Combine all --
dat3 <- dat2 %>%
  bind_rows(dat2_reg_aro_tot) %>%
  bind_rows(dat2_reg_aro_tot2) %>%
  bind_rows(dat2_reg_nonaro_tot) %>%
  bind_rows(dat2_reg_nonaro_tot2) %>%
  arrange(parcel, homolog, analyte)


write.csv(dat3, "C:\\Users\\ltravis\\EH&E Dropbox\\Laura Travis\\GitProjects\\25553-east-st-louis\\data\\for-cindi\\data-by-sample_aroclor.csv")




