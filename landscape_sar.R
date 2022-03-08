######################################################################
# Landscape SAR
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
maindata <- read.csv("compiled_spatial_data.csv")

### Re-scale vars pt 2
#logging vars
maindata$e_area_h <- maindata$e_area_h %>% log()
maindata$d_area_h <- maindata$d_area_h %>% log()
maindata$e_earatio_km <- maindata$e_earatio_km %>% log()
maindata$d_earatio_km <- maindata$d_earatio_km %>% log()
maindata$pop_density_catch_100 <- maindata$pop_density_catch_100 %>% log()
maindata$gap_size_h10 <- maindata$gap_size_h10 %>% log()

####### Natural Full Sample SAR
set.seed(42)
#create coordinate matrix
coords <- as.matrix(cbind(maindata$X, maindata$Y))
#create nearest neighbor pairs at 100,000m
coord.nb <- dnearneigh(coords, 0, 100000, longlat=FALSE)
coord.list <- nb2listw(coord.nb, style="W")

#no gaps SAR
lm.0.1 <- lm(e_ndvi_r ~ e_area_h + e_earatio_km + elev_km + e_prec_m + pop_density_catch_100, data=maindata)
sem.lm.0.1 <- errorsarlm(lm.0.1, listw=coord.list)
sem.lm.0.1 %>% summary()

#gaps sar
lm.0.2 <- lm(d_ndvi_r ~ d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100, data=maindata)
sem.lm.0.2 <- errorsarlm(lm.0.2, listw=coord.list)
sem.lm.0.2 %>% summary()

#gaps 2 sar
lm.0.3 <- lm(d_ndvi_r ~ d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + gap_size_h10, data=maindata)
sem.lm.0.3 <- errorsarlm(lm.0.3, listw=coord.list)
sem.lm.0.3 %>% summary()

#forest area SAR
lm.0.4 <- lm(d_area_h ~ d_ndvi_r_10 + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + gap_size_h10, data=maindata)
sem.lm.0.4 <- errorsarlm(lm.0.4, listw=coord.list)
sem.lm.0.4 %>% summary()

#gap size SAR
lm.0.5 <- lm(gap_size_h10 ~ d_ndvi_r_10 + d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100, data=maindata)
sem.lm.0.5 <- errorsarlm(lm.0.5, listw=coord.list)
sem.lm.0.5 %>% summary()
