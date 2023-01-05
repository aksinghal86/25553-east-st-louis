library(shiny)
library(shinyjs)
library(tidyverse)
# library(ggiraph) 
# library(geomtextpath)
library(sf)
# library(terra)
# library(mapdeck)
library(leaflet)
library(reactable)

# shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "esl-dashboard-cache")))
# shinyOptions(cache = cachem::cache_disk("./esl-dashboard-cache"))


# Data files
sfdf <- st_read('data/gis/all-data-with-geos.shp') %>% 
  mutate_at(c("lat", "lon"), as.numeric) 
df <- st_drop_geometry(sfdf) 
totals <- df %>% 
  filter(str_detect(analyte, 'Total'))

# Plant boundary from Gonzalez et al. (2010) paper. https://www.researchgate.net/publication/266902787
monsanto <- st_read('data/gis/monsanto.kml') %>% st_zm()
# cerro_copper <- st_read('data/gis/cerro-copper.kml') %>% st_zm()



# alt_sources <- tribble(
#   ~name, ~lat, ~lon, 
#   'Afton Chemical Corp', 38.60162059772649, -90.16832814460439,
#   'Flexsys America', 38.598763935003724, -90.1680904373274,
#   'Alton & Southern Railway', 38.59516968720018, -90.14601745949054, 
#   'St. Louis Downtown Airport', 38.57783798303547, -90.15730785689381
#   ) %>% 
#   mutate(color = 'orange')


# Functions
# switchInput <- function(inputId, label, value = FALSE, disabled = FALSE, width = NULL) {
#   value <- shiny::restoreInput(id = inputId, default = value)
#   inputTag <- htmltools::tags$input(id = inputId, type = "checkbox", role = "switch", class = "form-check-input")
#   if (!is.null(value) && value) {
#     inputTag$attribs$checked <- NA
#   }
#   if (!is.null(disabled) && disabled) {
#     inputTag$attribs$disabled <- NA
#   }
#   htmltools::tags$div(
#     class = "form-group shiny-input-container", style = htmltools::css(width = shiny::validateCssUnit(width)),
#     htmltools::tags$div(
#       class = "from-check form-switch",
#       inputTag,
#       htmltools::tags$label(label, class = "form-check-label")
#     ),
#   )
# }
# 
# updateSwitchInput <- shiny::updateCheckboxInput