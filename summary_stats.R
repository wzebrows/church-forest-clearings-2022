######################################################################
# Summary Stats
######################################################################

library(vtable)

#setup
rm(list = ls())

### Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Maindata
maindata <- read.csv("compiled_spatial_data.csv")
data_ess <- read.csv("ess_input_mc2.csv")
#rescale var
maindata$pop_density_catch_100 = maindata$pop_density_catch_100 *100

### run summary tables
##ESS summaries
sumtable(data_ess, out='csv', file='table1.csv', fixed.digits=TRUE, digits=3,
         vars=c('dist_market',
                'tsfirewood',
                'hh_education',
                'iddir',
                'bicycle',
                'electric',
                'tv'),
         summ=c('mean(x)',
                'median(x)',
                'sd(x)',
                'min(x)',
                'max(x)'))

##landscape summaries
sumtable(maindata, out='csv', file='table2.csv', fixed.digits=TRUE, digits=3,
         vars=c('elev_km',
                'e_prec',
                'pop_density_catch_100',
                'e_ndvi_r',
                'e_area_h',
                'e_earatio_km',
                'd_ndvi_r',
                'd_area_h',
                'd_earatio_km',
                'gap_size_h10'),
         summ=c('mean(x)',
                'median(x)',
                'sd(x)',
                'min(x)',
                'max(x)'))

