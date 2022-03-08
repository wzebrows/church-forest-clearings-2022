######################################################################
# Data Setup
######################################################################

library(rstudioapi)
library(dplyr)
library(plyr)
library(tidyr)
library(ape)
library(HSAR)
library(tibble)
library(spdep)
library(spatialreg)

#setup
rm(list = ls())

### Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Maindata
data_ess <- read.csv("ess_input_mc.csv")
data_sa <- read.csv("spatial_analysis_9-2-19.csv")
data_id <- read.csv("ess_id_churchs.csv")
data_latlong <- read.csv("forest_latlong.csv")
data_popden <- read.csv("pop_dense_church_all_2016.csv")
data_elev <- read.csv("strm elev points.csv")

### Calc Avg Nearest Neighbor Distance
library(spatstat)
#take coords of all forests
coords_all = subset(data_sa, select = c(long,lat) )
#calc mean nn distance, km
radius = nndist(coords_all) %>% mean() / 1000 /2
catchment = radius^2*4

### Merge Datasets 
maindata <- join_all(list(data_sa,data_id,data_latlong,data_popden,data_elev), by = 'id', type = 'left')
maindata <- maindata[!is.na(maindata$pop_density),]

### Re-scale vars
#hectares
maindata$e_area_h <- maindata$e_area / 10000
maindata$d_area_h <- maindata$d_area / 10000
#gap size: 1000 
maindata$gap_size_h10 <- maindata$gap_size / 1000
#pop dense: multiple by catchment area, then divide by 100 to interpret
maindata$pop_density_catch_100 <- maindata$pop_density * catchment /100
maindata$pop_density_catch_100_2 <- maindata$pop_density_catch_100^2
#rescale elev to km
maindata$elev_km <- maindata$elev / 1000

#use -1000 to 1000 NDVI. i already have these vars, but don't want to recode var inputs throughout script
maindata$e_ndvi_r = maindata$e_ndvi
maindata$d_ndvi_r = maindata$d_ndvi
maindata$d_ndvi_r_10 = maindata$d_ndvi / 100
#precip with gap is useless
maindata$d_prec_m = maindata$e_prec_m 

### Drop forests under 0.5 hectare
maindata <- maindata[maindata$d_area_h >=0.5,]

### Write CSV
write.csv(maindata, file = "compiled_spatial_data.csv")

#########
### Recode ESS vars
data_ess <- read.csv("ess_input_mc.csv")

## recode edu to be binary: some or none
data_ess$hh_education[data_ess$hh_education>0] <- 1

## recode Firewood minutes to hours
data_ess$tsfirewood <- data_ess$tsfirewood/60 

## Write CSV
write.csv(data_ess, file = "ess_input_mc2.csv")
