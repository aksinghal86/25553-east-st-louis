
# Compare categories set in expert report #

# Libraries -------------------------------------------------------
library(tidyverse)
library(readxl)
library(FSA)
library(flextable)

# Functions -----------------------------------------------------

ehe_table_format <-function(data, title, date_cols = NULL){
  
  flextable(data) %>%
    add_header_lines(values = c(title)) %>%
    
    #format first row of header
    flextable::bg(part = "header",i= 1, bg = "#D9D9D6") %>%
    flextable::color(part = "header",i = 1, color = "#005293") %>% 
    flextable::line_spacing(part = "header",i=1,space=1) %>% 
    flextable::valign(part = "header",i=1,valign = "center") %>% 
    flextable::align(part = "header",i=1,align = "left") %>% 
    flextable::padding(padding = 5, part = "header",i= 1) %>% 
    
    #format second row of header
    flextable::color(part = "header",i = 2,color = "#006B65") %>%
    flextable::bg(part = "header",i = 2, bg = "#F2F2F2") %>% 
    flextable::valign(part = "header",i=2,valign = "bottom") %>% 
    flextable::align(part = "header",i=2,align = "center") %>%
    flextable::padding(padding = 2, part ="header", i=2) %>%
    
    #bold header rows
    flextable::bold(part = "header", i =2) %>%
    
    ## footer depends on morning or afternoon, defined in source
    #add_footer_lines(values = footer) %>%
    flextable::add_footer_lines(values="") %>%
    
    #change to font to arial narrow, 10 pt
    flextable::fontsize(part = "body", size = 10) %>% 
    flextable::fontsize(part = "header", i = 1, size = 11) %>% 
    flextable::fontsize(part = "header", i = 2, size = 10) %>% 
    flextable::fontsize(part = "footer", size = 9) %>% 
    flextable::font(part = "all", fontname = "Arial Narrow") %>% 
    
    flextable::padding(padding=0.2, part = "body") %>%
    flextable::padding(padding=8, part = "footer") %>%
    
    flextable::align(align = "center", part = "body") %>%
    flextable::valign(valign = "top",  part = "body") %>%
    
    
    #all borders 
    flextable::border(border = officer::fp_border(color = "black", width=0.5), part = "all") %>%
    
    #format  columns
    flextable::set_formatter_type(fmt_double = "%.0f", fmt_integer = "%.0f",fmt_date = "%Y-%m-%d", fmt_datetime = "%Y-%m-%d %H:%M:%S") %>% 
    flextable::colformat_num(digits = 0) %>%
    autofit()
  
  
}



# Data ------------------------------------------------------------

## Data processed once, load from save below:
# 
# # parcel categories from Monsanto Expert Report
# parcel_cat <- read_excel('P:/26214/Monitoring Results/Expert Report Analysis/Parcel_Matching_Expert_Report.xlsx') %>%
#   # add leading 0 back in
#   mutate(parcelnumb = paste0("0", parcelnumb))
# 
# descr <- c('Disturbances Observed as both Ground Cover Alterations and Structure Construction or Demolition',
#            'Disturbances Observed as Ground Cover Alterations',
#            'Disturbances Observed as Structure Construction or Demolition',
#            'Parcel with No Observed Land Cover Change')
# Final_Cat <- c('Green', 'Orange', 'Purple', 'Blue')
# cats <- data.frame(Final_Cat, descr)
# 
# # Total PCB by parcel
# data <- read_csv('data/for-cindi/data-by-parcel.csv') 
# 
# data <- data %>%
#   mutate(parcelnumb = gsub('"', "", parcel),
#          parcelnumb = gsub("=", "", parcelnumb)) %>%
#   left_join(parcel_cat) %>%
#   left_join(cats) %>%
#   mutate(log_conc = log(avg_conc),
#          descr = ifelse(is.na(descr), "Unable to identify in report", descr)
#          ) %>%
#   rename(Category = Final_Cat,
#          Description = descr)
# 
# saveRDS(data, file = "P:/26214/Monitoring Results/Expert Report Analysis/data_with_categories.rds")

## read in data from above:
data <- readRDS("P:/26214/Monitoring Results/Expert Report Analysis/data_with_categories.rds")

writexl::write_xlsx(data, "P:/26214/Monitoring Results/Expert Report Analysis/data_with_categories.xlsx")

# Statistics table ---------------------------------------------------

plot(density(data$avg_conc))
plot(density(data$log_conc))

sumstats <- data %>%
  group_by(Category, Description) %>%  
  summarize(
    n = n(), 
    Min = min(avg_conc), 
    Mean = mean(avg_conc), 
    GM = exp(mean(log(avg_conc))), 
    GSD = exp(sd(log(avg_conc))), 
    SD = sd(avg_conc),
    Median = median(avg_conc), 
    Max = max(avg_conc)
  )%>%
  mutate(across(where(is.numeric), round, 3)) %>%
  ehe_table_format(., "Average Parcel Total PCB Concentrations Summary Statistics by Expert Report Category")
sumstats

log_sumstats <- data %>%
  group_by(Category, Description) %>%  
  summarize(
    n = n(), 
    Min = min(log_conc), 
    Mean = mean(log_conc), 
    GM = exp(mean(log_conc)), 
    GSD = exp(sd(log_conc)), 
    SD = sd(log_conc),
    Median = median(log_conc), 
    Max = max(log_conc)
  ) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  ehe_table_format(., "Log Transformed Average Parcel Total PCB Concentrations Summary Statistics by Expert Report Category")
log_sumstats

# Non-parametric tests --------------



df <- data %>%
  select(Category, avg_conc, log_conc, Description) 

summary(df)

ggplot(df) +
  aes(x = str_wrap(Description, 20), y = avg_conc, fill = Description) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "green", "orange", "magenta", "grey")) +
  theme(legend.position = "none") +
  xlab("Category from Expert Report") +
  ylab("Average Total PCB Concentration in Parcel")

ggplot(df) +
  aes(x = str_wrap(Description, 20), y = log_conc, fill = Description) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "green", "orange", "magenta", "grey")) +
  theme(legend.position = "none")+
  xlab("Category from Expert Report") +
  ylab("Log Transformed Average Total PCB Concentration in Parcel")

## Kruskal-Wallis nonparametric test 
## Excluding the uncategorized 3 parcels (unable to identify)
kruskal.test(log_conc ~ Category,
             data = df)
## NOTE - running with non-transformed data generates same output
# output: p=0.009, statistically significantly difference between categories 
# Kruskal-Wallis chi-squared = 11.468, df = 3, p-value = 0.009448

summary(aov(df$log_conc ~ df$Category))

## Post-hoc Dunn Test
dunnTest(log_conc ~ Category,
         data = df,
         method = "holm")

## Output Dunn Test P.Adj:
# Blue vs Purple category nearing significance at alpha=0.05 (p=0.0543)
# Orange vs Purple category significantly different at alpha=0.05 (p=0.007).
#        Orange cat has higher avg. PCB concentrations than Purple.
#        Orange - Purple  Z=3.2334936 P-unadj=0.001222861 P-adj=0.007337165
# All other category comparisons are not significant (p>0.05)


