library(tidyverse)
library(terra)
library(gstat)

## Data -----------------------------------------------------------------------
sfdf <- terra::vect('data/gis/all-data-with-geos.shp')
df <- values(sfdf) %>% mutate_at(vars('lon', 'lat', 'est_conc'), as.numeric)

# Majority of the contamination is to the west/southwest
# especially as the number of chlorinated carbons goes up
ggplot(df %>% filter(str_detect(analyte, 'Total')), aes(x = lon, y = lat)) + 
  geom_point(aes(color = log(est_conc)), size = 3) + 
  facet_wrap(~n_cl) + 
  scale_color_gradientn(colors = c("yellow", "orange", "blue", "dark blue")) + 
  theme_bw()

# Will use "Total PCBs" to fit the best interpolation model
total_pcbs <- df %>% 
  filter(analyte == 'Total PCBs') %>% 
  # log-transform since the values are approximately lognormally distributed
  mutate(log_conc = log(est_conc))

ggplot(total_pcbs, aes(x = est_conc)) + geom_density() + scale_x_log10()

## Models ---------------------------------------------------------------------

#### NULL model ----------------------------------------------------------------
# Use mean of all values as the predicted value 
RMSE <- function( observed, predicted ) { 
  sqrt( mean( (predicted - observed)^2, na.rm = T) )
}

null <- RMSE( mean(total_pcbs$log_conc), total_pcbs$log_conc )
null
## RMSE = 1.244281
# Aim is to beat this


#### NN --------------------------------------------

# Nearest neighbors interpolation
# Using 25 neighbors (~10% of the parcels) and setting 'inverse distance power' (idp) to zero so 
# that all neighbors are equally weighted

# create data set. `gstat` likes lon and lat to be in x and y. 
d <- total_pcbs %>% select(parcelnumb, x = lon, y = lat, est_conc, log_conc)
head(d)
# also create a spatvect data set for creating voronoi polygons
spatd <- vect(total_pcbs, c('lon', 'lat'), crs = crs(sfdf))
spatd


###### Model fit and interpolation -----------------------------------------

# Fit the model
gs <- gstat(
  formula = log_conc ~ 1, locations = ~x+y, data = d, 
  nmax = 25, set = list(idp = 0)
)

# interpolate for the raster grid based on model

# first, create a 250x250 grid raster around the parcel data collected in the field
r <- rast(
  nrows = 250, ncols = 250, 
  xmin = -90.172, ymin = 38.595, xmax = -90.148, ymax = 38.616
)

# Create a voronoi polygon from density of points to fill the raster grid 
# This is primarily for prediction and plotting purposes so that the entire grid
# is filled rather than just the points/parcels where data were collected.
v <- voronoi(spatd)
plot(v)
points(spatd)

# Fill polygons based on concentration
gdnt <- colorRampPalette(c('dark green', 'green', 'yellow', 'orange', 'red', 'dark red'))
plot(v, 'log_conc', col = gdnt(10))

# Create a raster from log_conc values from the vector (for plotting purposes)
vr <- rasterize(v, r, 'log_conc')
plot(vr, col = gdnt(10))

# Interpolate values in the grid where no values exist
nn <- interpolate(r, gs, debug.level = 0)

# mask the values (shouldn't do anything since there are no NAs but good practice)
nmsk <- mask(nn, vr)

# Predicts that concentrations are higher on the western boundary, with the 
# highest concentrations in the southwest region. 
plot(nmsk, 1, col = gdnt(10))

###### Cross validation ----------------------------------------
# five folds
nfolds <- 5
# 25 neighbors
neighbs <- 25

set.seed(321)
kfolds <- sample(1:nfolds, nrow(spatd), replace=TRUE)
rmse_nn <- rep(NA, nfolds)

for (k in 1:nfolds) {
  # Create train and test data sets
  test <- d[kfolds == k, ]
  train <- d[kfolds != k, ]
  
  # Train the model on training data set
  gscv <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = train, 
                nmax = neighbs, set = list(idp = 0))
  
  # Predict on test test 
  p <- predict(gscv, test, debug.level=0)$var1.pred
  
  # RMSE for the model
  rmse_nn[k] <- RMSE(test$log_conc, p)
}

###### Model error -------------------------------------------------------
rmse_nn
## [1] 0.9464922 0.9534012 0.9436552 0.9646932 0.9240465
mean(rmse_nn)
## [1] 0.9464577

1 - (mean(rmse_nn) / null)
## [1] 0.2393538
## 24% reduction compared to NULL model 


#### IDW  ---------------------------------------------------------

# Inverse-distance weighted interpolation
# Similar to NN except that points further away are given less weight. This 
# should be relevant since contamination appears to localized to the western 
# boundary 

###### Model fit and interpolation -----------------------------------------
# Basic idw

gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d) 
idw <- interpolate(r, gs, debug.level = 0) 
idwr <- mask(idw, vr)
plot(idwr, 1, col = gdnt(10))

rmse_idw <- rep(NA, nfolds) 
for (k in 1:5) {
  test <- d[kfolds == k, ]
  train <- d[kfolds != k, ]
  gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = train)
  p <- predict(gs, test, debug.level=0)
  rmse_idw[k] <- RMSE(test$log_conc, p$var1.pred)
}

rmse_idw
## [1] 0.6538752 0.8139294 0.8593740 0.8656410 0.8943150
mean(rmse_idw)
## [1] 0.8174269
1 - (mean(rmse_idw) / null)
## [1] 0.3430528
# Improving. 34% reduction in RMSE compared to NULL model. Significant improvement over NN, as well. 

### Optimization 

# IDW takes two critical parameters: number of neighbors to consider (nmax) and the 
# the distance decay, i.e., inverse distance power (idp).

# Can find the optimum nmax and idp using the optim function.
# The optim function takes a value from a function that we want to minimize. 
# Starting from initial parameters, optim will find the optimal values
# In this case, goal is to minimize RMSE for the number of neighbors.

RMSE_idw <- function(x, test, train) {
  nmx <- x[1]
  idp <- x[2]
  if (nmx < 1) return(Inf)
  if (idp < .001) return(Inf)
  m <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = train, 
             nmax = nmx, set = list(idp = idp)
             )
  p <- predict(m, newdata = test, debug.level=0)$var1.pred
  RMSE(test$log_conc, p)
}

set.seed(23456789)
i <- sample(nrow(spatd), 0.2 * nrow(spatd))
tst <- d[i,]
trn <- d[-i,]

n <- c(5, 10, 15, 25)
i <- c(0.5, 0.75, 1, 2.5, 5)

opts <- tribble(~no, ~io, ~nmax, ~idp, ~rmse)
for (n0 in n) { 
  for (i0 in i) { 
    opt <- optim(c(n0, i0), RMSE_idw, test = tst, train = trn)
    opts <- opts %>% add_row(no = n0, io = i0, nmax = opt$par[1], idp = opt$par[2], rmse = opt$value)
  }
}

plot1 <- ggplot(opts, aes(x = nmax, y = rmse)) + geom_point() + geom_line()
plot2 <- ggplot(opts, aes(x = idp, y = rmse)) + geom_point() + geom_line()
gridExtra::grid.arrange(plot1, plot2)

( params <- opts %>% filter(rmse == min(rmse)) %>% slice(1) )
gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d,
            nmax = params$nmax, set = list(idp = params$idp)) 
idw <- interpolate(r, gs, debug.level = 0) 
idwr <- mask(idw, vr)
plot(idwr, 1, col = gdnt(10))

###### Cross validation ----------------------------------------------

rmse_idw <- rep(NA, nfolds) 
for (k in 1:5) {
  test <- d[kfolds == k, ]
  train <- d[kfolds != k, ]
  gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = train, 
              nmax = params$nmax, set = list(idp = params$idp))
  p <- predict(gs, test, debug.level=0)
  rmse_idw[k] <- RMSE(test$log_conc, p$var1.pred)
}


###### Model error -------------------------------------------------------
rmse_idw
mean(rmse_idw)
1 - (mean(rmse_idw) / null)
# Performance is variable depending on the seed chosen but fluctuates around base IDW
# After some exploration, it appears that the best outcome, at least for total PCBs, 
# is for nmax = 10.6, and idp = 1.29


### With optimal parameters
gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d, nmax = 10.6, set = list(idp = 1.29)) 
idw <- interpolate(r, gs, debug.level = 0) 
idwr <- mask(idw, vr)
plot(idwr, 1, col = gdnt(10))

rmse_idw <- rep(NA, nfolds) 
for (k in 1:5) {
  test <- d[kfolds == k, ]
  train <- d[kfolds != k, ]
  gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = train, nmax = 10.6, set = list(idp = 1.29))
  p <- predict(gs, test, debug.level=0)
  rmse_idw[k] <- RMSE(test$log_conc, p$var1.pred)
}

rmse_idw
## [1] 0.6439121 0.8200495 0.8635036 0.8256221 0.8512593
mean(rmse_idw)
## [1] 0.8008693
1 - (mean(rmse_idw) / null)
## [1] 0.3563598
# Definitely better than base IDW but the optimal nmax and idp parameters will likely depend on 
# the congener group selected, which is compute and resource intensive over marginal performance gain. 


#### Kriging -----------------------------------------------------------------

# Kriging interpolation is similar to other methods in that it interpolates values
# based on nearby points; however, Kriging is more complex because it also 
# incorporates the spatial correlation between sampled points: the interpolation 
# is based on the spatial arrangement of the empirical observations, rather than on 
# a presumed model of spatial distribution. 
# (https://www.publichealth.columbia.edu/research/population-health-methods/kriging-interpolation)

###### Model fit and interpolation -----------------------------------------

# Create a model and fit an empirical variogram (also called semi-variogram)
# A variogram is a visual depiction of the covariance exhibited between each pair 
# of points in the sampled data. The gamma value (or semi-variance) is a measure of the 
# half mean-squared differed between each pair of values. 
gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d) 
v <- variogram(gs)
plot(v)

# Fit a model to variogram (Exponential or Spherical may be good fits based on 
# variogram plot
fves <- fit.variogram(v, vgm(psill=max(v$gamma)*0.9,
                             model = 'Sph',
                             range = max(v$dist)/2,
                             nugget = mean(v$gamma)/4))
plot(v, fves, main = 'Spherical') 
fvee <- fit.variogram(v, vgm(psill=max(v$gamma)*0.9,
                             model = 'Exp',
                             range = max(v$dist)/2,
                             nugget = mean(v$gamma)/4))
plot(v, fvee, main = 'Exponential')

# Both models look equally good -- spherical seems slightly better 

# Interpolation
k <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = d, model = fves)

# predicted values
kp <- interpolate(r, k, debug.level = 0)

ok <- mask(kp, idw[[1]])
names(ok) <- c('prediction', 'variance')

plot(ok, col = gdnt(10))

###### Cross validation ----------------------------------------------------

rmse_ok <- rep(NA, nfolds) 
for (k in 1:5) {
  test <- d[kfolds == k, ]
  train <- d[kfolds != k, ]
  gs <- gstat(formula = log_conc ~ 1, locations = ~ x + y, data = train, model = fves)
  p <- predict(gs, test, debug.level=0)
  rmse_ok[k] <- RMSE(test$log_conc, p$var1.pred)
}

###### Model error -------------------------------------------------------
rmse_ok
## [1] 0.6374186 0.8418739 0.8793533 0.8607594 0.8488733
mean(rmse_ok)
## [1] 0.8136557
1 - (mean(rmse_ok) / null)
## [1] 0.3460837
# About the same as base IDW


#### Thin plate spline model ----------------------------------------------

###### Model fit and interpolation -----------------------------------------
library(fields) 
m <- fields::Tps(d[, c("x", "y")], d$log_conc)
tps <- interpolate(r, m)
tps <- mask(tps, idw[[1]])

plot(tps, col = gdnt(10))

###### Cross validation ----------------------------------------------------
rmse_tps <- rep(NA, nfolds) 
for (k in 1:5) {
  test <- d[kfolds == k, ]
  train <- d[kfolds != k, ]
  m <- Tps(train[, c('x', 'y')], train$log_conc) 
  p <- predict(m, test[, c('x', 'y')]) 
  rmse_tps[k] <- RMSE(test$log_conc, p)
}

###### Model error -------------------------------------------------------
rmse_tps
## [1] 0.5998948 0.9023322 1.0284172 0.8888535 1.0081105
mean(rmse_tps)
## [1] 0.8855216
1 - (mean(rmse_tps) / null)
## [1] 0.2883266
# Worse than base IDW but better than NN


## Conclusion ----------------------------------

# One of IDW or Kriging approaches will suffice but in the case of IDW, 
# nmax and idp will likely have to be optimized for each PCB congener group. 
# This is resource and compute expensive but does not provide enough value
# given similar model performance as base IDW and Kriging. 
# Furthermore, IDW performance and optimal parameter identification is 
# dependent on the seed and the actual performance can vary quite a bit depending
# on the seed. 

# Therefore, either ordinary IDW and Kriging is appropriate but I prefer Kriging
# due to the spatial correlation aspect. 