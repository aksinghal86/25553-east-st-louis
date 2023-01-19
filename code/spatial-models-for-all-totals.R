library(tidyverse)
library(terra)
library(sf) 
library(gstat)


## Data -----------------------------------------------------------------------
sfdf <- terra::vect('data/gis/all-data-with-geos.shp')
df <- values(sfdf) %>% mutate_at(vars('lon', 'lat', 'est_conc'), as.numeric)

totals <- df %>%
  filter(str_detect(analyte, 'Total')) %>%
  select(analyte, n_cl, x = lon, y = lat, est_conc) %>%
  # log-transform since the values are approximately lognormally distributed
  mutate(log_conc = log(est_conc))

sftotals <- st_as_sf(vect(totals, c('x', 'y'), crs = crs(sfdf), keepgeom= T))

ggplot(totals, aes(x = est_conc)) +
  geom_density() +
  facet_wrap(~analyte) +
  scale_x_log10()

# Use mean of all values as the predicted value 
RMSE <- function( observed, predicted ) { 
  sqrt( mean( (predicted - observed)^2, na.rm = T) )
}

## NULL model -----------------------------------------
mods <- totals %>%
  group_by(analyte, n_cl) %>%
  nest() %>%
  mutate(
    rmse_null = map(data, ~ RMSE( mean(.$log_conc), .$log_conc))[[1]]
  )

ggplot(mods, aes(x = factor(n_cl), y = rmse_null)) +
  geom_col() +
  theme_bw() + 
  coord_flip()

## NN -----------------------------------------------------
# first, create a 250x250 grid raster around the parcel data collected in the field
r <- rast(
  nrows = 250, ncols = 250,
  xmin = -90.172, ymin = 38.595, xmax = -90.148, ymax = 38.616
  )

# Fit the model and interpolate for the raster grid based on the model
nn_gs <- function(d) {
gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d, 
      nmax = 25, set = list(idp = 0)
      )
}
 
mods <- mods %>%
  mutate(nn = map(data, ~ interpolate(r, nn_gs(.), debug.level = 0)))

set.seed(23456789)

nfolds <- 5
mods <- mods %>%
  mutate(kfolds = map(data, ~ sample(1:nfolds, nrow(.x), replace = T)), 
         rmse_nn = map(analyte, ~ list() ))

for (k in 1:nfolds) {
  # Create train and test data sets
  mods <- mods %>%
  mutate(test = map(data, ~ .x[kfolds[[1]] == k, ]),
         train = map(data, ~ .x[kfolds[[1]] != k,]))
  
  # # Train the model on training data set and predict on test set
  mods <- mods %>%
  mutate(gscv = map(train, ~ nn_gs(.)),
         p = map2(gscv, test, ~ predict(.x, .y, debug.level = 0)$var1.pred),
         rmse = map2(test, p, ~ RMSE(.x$log_conc, .y)),
         rmse_nn = map(rmse_nn, function(x) append(x, rmse))
         )
}

#### Model error -------------------------------------------------------
mods <- mods %>%
  mutate(rmse_nn = map(rmse_nn, ~ mean(unlist(.)))[[1]]) %>%
  select(-train, -test, -gscv, -p, -rmse)

ggplot(mods %>% pivot_longer(contains('rmse')), aes(x = factor(n_cl), y = value, fill = name)) +
  geom_col(position = 'dodge') +
  theme_bw() + 
  theme(legend.title = element_blank())

## IDW (basic) ---------------------------------------------------

idw_gs <- function(d, nmx = Inf, ...) {
  args <- list(...)
  if('idp' %in% names(args))
    gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d,
          nmax = nmx, set = list(idp = args$idp))
  else
    gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d,
          nmax = nmx)
}

mods <- mods %>%
  mutate(idw = map(data, ~ interpolate(r, idw_gs(.), debug.level = 0)), 
         rmse_idw = map(analyte, ~ list() ))

gdnt <- colorRampPalette(RColorBrewer::brewer.pal(8, 'YlOrRd'))
par(mfrow = c(4, 3))
mapply(function(a, mod) plot(mod, 1, main = a, col = gdnt(10)), mods$analyte, mods$idw)

for (k in 1:nfolds) {
  # Create train and test data sets
  mods <- mods %>%
    mutate(test = map(data, ~ .x[kfolds[[1]] == k, ]),
           train = map(data, ~ .x[kfolds[[1]] != k,]))
  
  # Train the model on training data set and predict on test set
  mods <- mods %>%
    mutate(gscv = map(train, ~ idw_gs(.)),
           p = map2(gscv, test, ~ predict(.x, .y, debug.level = 0)$var1.pred),
           rmse = map2(test, p, ~ RMSE(.x$log_conc, .y)),
           rmse_idw = map(rmse_idw, function(x) append(x, rmse))
           )
}

mods <- mods %>%
  mutate(rmse_idw = map(rmse_idw, ~ mean(unlist(.)))[[1]]) %>%
  select(-train, -test, -gscv, -p, -rmse)

ggplot(mods %>% pivot_longer(contains('rmse')),
      aes(x = factor(n_cl), y = value, fill = factor(name, levels = c('rmse_null', 'rmse_nn', 'rmse_idw')))) +
  geom_col(position = 'dodge') +
  theme_bw() + 
  theme(legend.title = element_blank())


#### IDW with optimized nmax and idp
# Optimized on "Total PCBs" (see spatial-models-and-analysis file)

mods <- mods %>%
  mutate(idw_o = map(data, ~ interpolate(r, idw_gs(., nmx = 10.6, idp = 1.29), debug.level = 0)))

par(mfrow = c(4, 3))
mapply(function(a, mod) plot(mod, 1, main = a, col = gdnt(10)), mods$analyte, mods$idw_o)

mods <- mods %>%
  mutate( rmse_idw_o = map(analyte, ~ list() ))

for (k in 1:nfolds) {
  # Create train and test data sets
  mods <- mods %>%
    mutate(test = map(data, ~ .x[kfolds[[1]] == k, ]),
           train = map(data, ~ .x[kfolds[[1]] != k,]))
  # Train the model on training data set and predict on test set
  mods <- mods %>%
    mutate(gscv = map(train, ~ idw_gs(., nmx = 10.6, idp = 1.29)),
           p = map2(gscv, test, ~ predict(.x, .y, debug.level = 0)$var1.pred),
           rmse = map2(test, p, ~ RMSE(.x$log_conc, .y)),
           rmse_idw_o = map(rmse_idw_o, function(x) append(x, rmse))
           )
}

mods <- mods %>%
  mutate(rmse_idw_o = map(rmse_idw_o, ~ mean(unlist(.)))[[1]]) %>%
  select(-train, -test, -gscv, -p, -rmse)

ggplot(mods %>% pivot_longer(contains('rmse')),
       aes(x = factor(n_cl), y = value, fill = factor(name, levels = c('rmse_null', 'rmse_nn', 'rmse_idw', 'rmse_idw_o')))) +
  geom_col(position = 'dodge') +
  theme_bw() + 
  theme(legend.title = element_blank())


## Ordinary Kriging ---------------------------------------------------------
ok_gs <- function(d, model = NULL) {
  gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d, model = model)
}

mods <- mods %>%
  mutate(v_ok = map(data, ~ variogram(ok_gs(.))),
         fves_ok = map(v_ok, ~ fit.variogram(.x, vgm(psill=max(.x$gamma)*0.9,
                                               model = 'Sph',
                                               range = max(.x$dist)/2,
                                               nugget = mean(.x$gamma)/4))),
         k_ok = map2(data, fves_ok, ~ok_gs(.x, .y)),
         kp_ok = map(k_ok, ~ interpolate(r, .)))

mods <- mods %>%
  mutate( rmse_ok = map(analyte, ~ list() ))

for (k in 1:nfolds) {
  # Create train and test data sets
    mods <- mods %>%
      mutate(test = map(data, ~ .x[kfolds[[1]] == k, ]),
             train = map(data, ~ .x[kfolds[[1]] != k,]))
    # Train the model on training data set and predict on test set
    mods <- mods %>%
      mutate(gscv = map2(train, fves_ok, ~ ok_gs(.x, .y)),
             p = map2(gscv, test, ~ predict(.x, .y, debug.level = 0)$var1.pred),
             rmse = map2(test, p, ~ RMSE(.x$log_conc, .y)),
             rmse_ok = map(rmse_ok, function(x) append(x, rmse))
             )
}
mods <- mods %>%
  mutate(rmse_ok = map(rmse_ok, ~ mean(unlist(.)))[[1]]) %>%
  select(-train, -test, -gscv, -p, -rmse)

ggplot(mods %>% pivot_longer(contains('rmse')),
       aes(x = factor(n_cl), y = value, fill = factor(name, levels = c('rmse_null', 'rmse_nn', 'rmse_idw', 'rmse_idw_o', 'rmse_ok')))) +
  geom_col(position = 'dodge') +
  theme_bw() +
  theme(legend.title = element_blank())


## Universal Kriging -------------------------------------------------------

uk_gs <- function (sfd) {
  gstat(formula = log_conc ~ x + y, data = sfd)
}

nested_sftotals <- sftotals %>%
  group_by(analyte) %>%
  nest() %>%
  mutate(
    v = map(data, ~ variogram(uk_gs(.), cloud = F)),
    fves = map(v, ~ fit.variogram(.x, vgm(psill = 2, 
                                          model = 'Sph',
                                          range = 0.4,
                                          nugget = 0.2)))
    )


nested_sftotals <- nested_sftotals %>%
  mutate(kfolds = map(data, ~ sample(1:nfolds, nrow(.x), replace = T)),
         rmse_uk = map(analyte, ~ list() ))

for (k in 1:nfolds) {
  # Create train and test data sets
  nested_sftotals <- nested_sftotals %>%
    mutate(test = map(data, ~ .x[kfolds[[1]] == k, ]),
           train = map(data, ~ .x[kfolds[[1]] != k,]))
    # Train the model on training data set and predict on test set
  nested_sftotals <- nested_sftotals %>%
    mutate(gscv = map(train, ~ uk_gs(.)),
           p = map2(gscv, test, ~ predict(.x, .y, debug.level = 0)$var1.pred),
           rmse = map2(test, p, ~ RMSE(.x$log_conc, .y)),
           rmse_uk = map(rmse_uk, function(x) append(x, rmse))
           )
}

nested_sftotals <- nested_sftotals %>%
  mutate(rmse_uk = map(rmse_uk, ~ mean(unlist(.)))[[1]]) %>%
  select(-train, -test, -gscv, -p, -rmse)
mods$rmse_uk <- nested_sftotals$rmse_uk

ggplot(mods %>% pivot_longer(contains('rmse')),
       aes(x = factor(n_cl), y = value,
           fill = factor(name, levels = c('rmse_null', 'rmse_nn', 'rmse_idw', 'rmse_idw_o', 'rmse_ok', 'rmse_uk')))) +
  geom_col(position = 'dodge') +
  theme_bw() +
  theme(legend.title = element_blank())
