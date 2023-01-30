
This page describes the data cleanup, data processing, and analytical techniques used for producing the plots, maps, and heatmaps in this dashboard. 

## Data collection

Environmental Health & Engineering, Inc. scientists collected soil samples in 273 different parcels in East St. Louis, IL. For a few parcels, duplicates were collected for ensuring data quality. Field samples were sent to two labs -- Alpha lab and Cape Fear lab -- for PCB analysis.

## Data processing 

The two labs reported the results in a slightly different manner with the key difference being that Cape Fear included results by total congener group, e.g., by total mono, di, tri, etc., and total PCBs per sample. Alpha labs only provided total for the entire PCB panel, not by congener groups. 

To ensure consistency between labs, the following steps were taken: 

1. For each sample analyzed by Cape Fear, totals per PCB congener group and total PCB estimates were removed. For Alpha labs, total PCB estimates were removed.
2. For analytes where the result was reported as a non-detect (ND), **the value was replaced with the half of detection limit (DL)** reported by the lab. 
3. If a duplicate sample was taken in a parcel, concentrations of each analyte were averaged for that parcel to arrive at one concentration per analyte per parcel sampled. 
4. Totals were calculated by PCB congener group (number of chlorinated carbons in the biphenyl group), i.e., mono, di, tri, tetra, penta, hexa, hepta, octa, nona, and deca; however, for deca PCB, there is only congener possibility. Total PCB concentration was also calculated in a similar manner. 

## Data analysis and visualization

All analyses and visualizations were conducted in the open-source software *R v4.2.1* (https://www.r-project.org/). 

An outline of the former Monsanto PCB manufacturing plant was obtained from Gonzalez et al. (2010), with specific building locations obtained from Diagram 2 of RME (2011). Wind rose was adapted from Diagram 3 of RME (2011). 

Total PCB congener concentrations were plotted for each parcel from parcel coordinates and boundaries obtained commercially from the REGRID data store (https://regrid.com/). Concentrations where no samples were collected were estimated with Ordinary Kriging interpolation using the *gstat* package, v2.1-0, in R (Gräler et al. 2016; Pebesma 2004; Pebesma and Gräler 2022). 

1. A 1,000 x 1,000 cell grid was created for the city of East St. Louis such that the boundary box was defined by the following coordinates: (-90.172, -90.148) and (38.595, 38.616). 
2. Ordinary Kriging model was fit for each congener group (Total Mono PCBs to Total Deca PCB) and Total PCBs. 
3. Using the Ordinary Kriging model fit to the data, predictions were made in the grid where no data were available to ultimately generate a data point for each cell available in the grid. 
4. The filled grid aka heatmap was then trimmed to the shape shown in the map (southwest boundary clipped to reflect the actual southwest boundary of East St. Louis) because this is the region of interest and where the parcels were sampled by the EH&E team. 

*Note:*  Ordinary Kriging was chosen as the model of choice because it had better predictive performance, i.e., lowest root mean square errors (RMSEs), than the other models evaluated, i.e., NULL model, nearest neighbors (NN) model, and the inverse-distance weighting (IDW) model. Ordinary Kriging presumably performed better because it interpolates missing values in the grid based not only on nearby points (like IDW) but also on spatial correlation between those points. Nevertheless, the outputs from IDW and Ordinary Kriging were comparable and could be used interchangeably without affecting the conclusions.  


## References

Benedikt Gräler, Edzer Pebesma and Gerard Heuvelink, 2016. Spatio-Temporal Interpolation using gstat. The R Journal 8(1), 204-218.

Gonzalez, J; Feng, L; Sutherland, Anders; Waller, C; Sok, Helen; Hesse, R; Rosenfeld, P. 2011. PCBs and dioxins/furans in attic dust collected near former PCB production and secondary copper facilities in Sauget, IL. Procedia Environmental Sciences 4: 113-125. 

Pebesma, E.J., 2004. Multivariable geostatistics in S: the gstat package. Computers & Geosciences, 30: 683-691.

Pebesma and Gräler. 2022. Spatial and Spatio-Temporal Geostatistical Modelling, Prediction and Simulation. October 19, 2022. https://cran.r-project.org/web/packages/gstat/gstat.pdf. 

Risk Management & Engineering, Ltd. (RME). 2011. Air modeling analysis of potential historical releases at Solutia, Inc. W.G. Krummrich Plant, Sauget, Illinois. Prepared for Booz Allen Hamilton, Inc. by David A. Weeks. EPA archive REPA4-2526-002. 