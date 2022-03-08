# This was the second and most current implementation. mc_analysis is the first and is outdated

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
data_ess <- read.csv("ess_input_mc2.csv")
data_id <- read.csv("ess_id_churchs.csv")

### Re-scale vars
##logging vars
maindata$e_area_h <- maindata$e_area_h %>% log()
maindata$d_area_h <- maindata$d_area_h %>% log()
maindata$e_earatio_km <- maindata$e_earatio_km %>% log()
maindata$d_earatio_km <- maindata$d_earatio_km %>% log()
maindata$pop_density_catch_100 <- maindata$pop_density_catch_100 %>% log()
maindata$gap_size_h10 <- maindata$gap_size_h10 %>% log()
##rescale ESS vars
#Market distance: per 10 km
data_ess$dist_market <- data_ess$dist_market / 10
#change binaries to %
data_ess$iddir <- data_ess$iddir * 10
data_ess$bicycle <- data_ess$bicycle * 10
data_ess$tv <- data_ess$tv * 10
data_ess$electric <- data_ess$electric * 10

### Subset Datasets
# drop unneeded variables
data_ess = subset(data_ess, select = -c(X,lat_dd_mod,household_id2,lon_dd_mod) )
# subset by ess_id
ess_split <- split(data_ess, data_ess$ess_id)
new_names <- c("ess1","ess2","ess3","ess4","ess5","ess6","ess7","ess8","ess9","ess10","ess11","ess12","ess13","ess14","ess15","ess16","ess17","ess18","ess19","ess20","ess21","ess22","ess23","ess24","ess25","ess26","ess27","ess28","ess29","ess30","ess31","ess32","ess33","ess34","ess35","ess36")
for (i in 1:length(ess_split)) {
  assign(new_names[i], ess_split[[i]])
}
#subset church dataset to only those with an ess_id
maindata <- drop_na(maindata, ess_id)
#replace ess_id NAs with 0. Since the data does not have any other NAs, this method is fastest. This is required for the MC coding
maindata[is.na(maindata)] <- 0

# robustness prep: exclude churches that are only in ESS ea's with a single household: 6, 9, 10, 15, 23
maindata <- maindata[!((maindata$ess_id==6 | maindata$ess_id==9 | maindata$ess_id==10 | maindata$ess_id==15 | maindata$ess_id==23) & maindata$ess_id2==0),]

##################
#### Monte Carlo

##set up working dataframe
#working df
maindata_mc <- maindata

#add empty ess variables to the working dataframe
maindata_mc$dist_market = NA
maindata_mc$tsfirewood = NA
maindata_mc$hh_education = NA
maindata_mc$car_num = NA
maindata_mc$car = NA
maindata_mc$bicycle_num = NA
maindata_mc$bicycle = NA
maindata_mc$motorcycle_num = NA
maindata_mc$motorcycle = NA
maindata_mc$kerosene_num = NA
maindata_mc$kerosene = NA
maindata_mc$electric_num = NA
maindata_mc$electric = NA
maindata_mc$mobile_num = NA
maindata_mc$mobile = NA
maindata_mc$tv_num = NA
maindata_mc$tv = NA
maindata_mc$iddir = NA
maindata_mc$total_cons_ann = NA

#create a list of ess dfs
dfList <- list(ess1,ess2,ess3,ess4,ess5,ess6,ess7,ess8,ess9,ess10,ess11,ess12,ess13,ess14,ess15,ess16,ess17,ess18,ess19,ess20,ess21,ess22,ess23,ess24,ess25,ess26,ess27,ess28,ess29,ess30,ess31,ess32,ess33,ess34,ess35,ess36)

#DFs for betas of vars
coef <- data.frame(e_area_h=double(),e_earatio_km=double(),elev_km=double(),e_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double())
coef_g <- data.frame(d_area_h=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double())
coef_g2 <- data.frame(d_area_h=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double(),gap_size_h10=double())
coef_g3 <- data.frame(d_ndvi_r=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double(),gap_size_h10=double())
coef_g4 <- data.frame(d_ndvi_r=double(),d_area_h=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double())

p <- data.frame(e_area_h=double(),e_earatio_km=double(),elev_km=double(),e_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double())
p_g <- data.frame(d_area_h=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double())
p_g2 <- data.frame(d_area_h=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double(),gap_size_h10=double())
p_g3 <- data.frame(d_ndvi_r=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double(),gap_size_h10=double())
p_g4 <- data.frame(d_ndvi_r=double(),d_area_h=double(),d_earatio_km=double(),elev_km=double(),d_prec_m=double(),pop_density_catch_100=double(),pop_density_catch_100_2=double(),dist_market=double(),tsfirewood=double(),hh_education=double(),iddir=double(),bicycle=double(),electric=double(),tv=double(),total_cons_ann=double())

##Set up SARerr regressions
#create coordinate matrix
coords <- as.matrix(cbind(maindata_mc$X, maindata_mc$Y))
#create nearest neighbor pairs at 100,000m
coord.nb <- dnearneigh(coords, 0, 100000, longlat=FALSE)
coord.list <- nb2listw(coord.nb, style="W")

###Loop
set.seed(42)

for (j in 1:200){

  #bootstrap sample
  for (row in 1:nrow(maindata_mc)) {
    id1 <- maindata_mc[row, "ess_id"]
    id2 <- maindata_mc[row, "ess_id2"]
    id3 <- maindata_mc[row, "ess_id3"]
    
    for(i in 1:length(dfList))
    {
      if (id1 == max(dfList[[i]]$ess_id)){
        mc_agg <- dfList[[i]]
      }
    }
    
    for(i in 1:length(dfList))
    {
      if (id2 == max(dfList[[i]]$ess_id)){
        mc_agg <- rbind(mc_agg, dfList[[i]])
      }
    }
    
    for(i in 1:length(dfList))
    {
      if (id3 == max(dfList[[i]]$ess_id)){
        mc_agg <- rbind(mc_agg, dfList[[i]])
      }
    }
    
    bsmpl <- nrow(mc_agg)
    
    mc_agg <- sample_n(mc_agg, bsmpl, replace = TRUE, )
    
    maindata_mc$dist_market[row] = mean(mc_agg$dist_market)
    maindata_mc$tsfirewood[row] = mean(mc_agg$tsfirewood)
    maindata_mc$hh_education[row] = mean(mc_agg$hh_education)
    maindata_mc$car_num[row] = mean(mc_agg$car_num)
    maindata_mc$car[row] = mean(mc_agg$car)
    maindata_mc$bicycle_num[row] = mean(mc_agg$bicycle_num)
    maindata_mc$bicycle[row] = mean(mc_agg$bicycle)
    maindata_mc$motorcycle_num[row] = mean(mc_agg$motorcycle_num)
    maindata_mc$motorcycle[row] = mean(mc_agg$motorcycle)
    maindata_mc$kerosene_num[row] = mean(mc_agg$kerosene_num)
    maindata_mc$kerosene[row] = mean(mc_agg$kerosene)
    maindata_mc$electric_num[row] = mean(mc_agg$electric_num)
    maindata_mc$electric[row] = mean(mc_agg$electric)
    maindata_mc$mobile_num[row] = mean(mc_agg$mobile_num)
    maindata_mc$mobile[row] = mean(mc_agg$mobile)
    maindata_mc$tv_num[row] = mean(mc_agg$tv_num)
    maindata_mc$tv[row] = mean(mc_agg$tv)
    maindata_mc$iddir[row] = mean(mc_agg$iddir)
    maindata_mc$total_cons_ann[row] = mean(mc_agg$total_cons_ann)
    
  }

  ##Set formula for SARerr regressions: needs to be inside loop because it doesn't like NAs that exist prior to loop
  #no gaps lm formula
  lm.1 <- lm(e_ndvi_r ~ e_area_h + e_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + dist_market + 
               tsfirewood + hh_education + iddir + bicycle + electric + tv , data=maindata_mc)
  
  #gaps lm formula
  lm.2 <- lm(d_ndvi_r ~ d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + dist_market + 
               tsfirewood + hh_education + iddir + bicycle + electric + tv, data=maindata_mc)
  
  #gaps 2 lm formula
  lm.3 <- lm(d_ndvi_r ~ d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + dist_market + 
               tsfirewood + hh_education + iddir + bicycle + electric + tv + gap_size_h10, data=maindata_mc)
  #forest size
  lm.4 <- lm(d_area_h ~ d_ndvi_r_10 + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + dist_market + 
               tsfirewood + hh_education + iddir + bicycle + electric + tv + gap_size_h10, data=maindata_mc)
  
  #clearing size
  lm.5 <- lm(gap_size_h10 ~ d_ndvi_r_10 + d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + dist_market + 
               tsfirewood + hh_education + iddir + bicycle + electric + tv, data=maindata_mc)
  
  ###Gen No-Gaps Stats
    ##Run SARerr
    sem.lm.1 <- errorsarlm(lm.1, listw=coord.list)
    
    ##record var betas
    coef_add <-  as.data.frame(t(sem.lm.1[["coefficients"]]))
    coef <- rbind(coef, coef_add)

    p_add <- as.data.frame(t(summary(sem.lm.1)$Coef[,4]))
    p <-  rbind(p, p_add)
    
  ###Gen Gaps Stats
    ##Run SARerr
    sem.lm.2 <- errorsarlm(lm.2, listw=coord.list)
    
    ##record var betas
    coef_add <-  as.data.frame(t(sem.lm.2[["coefficients"]]))
    coef_g <- rbind(coef_g, coef_add)
    
    p_add <- as.data.frame(t(summary(sem.lm.2)$Coef[,4]))
    p_g <-  rbind(p_g, p_add)
    
  ###Gen Gaps 2 Stats
    ##Run SARerr
    sem.lm.3 <- errorsarlm(lm.3, listw=coord.list)
    
    ##record var betas
    coef_add <-  as.data.frame(t(sem.lm.3[["coefficients"]]))
    coef_g2 <- rbind(coef_g2, coef_add)
    
    p_add <- as.data.frame(t(summary(sem.lm.3)$Coef[,4]))
    p_g2 <-  rbind(p_g2, p_add)
    
  ###Gen Forest size Stats
    ##Run SARerr
    sem.lm.4 <- errorsarlm(lm.4, listw=coord.list)
    
    ##record var betas
    coef_add <-  as.data.frame(t(sem.lm.4[["coefficients"]]))
    coef_g3 <- rbind(coef_g3, coef_add)
    
    p_add <- as.data.frame(t(summary(sem.lm.4)$Coef[,4]))
    p_g3 <-  rbind(p_g3, p_add)
    
  ###Gen Gap size Stats
    ##Run SARerr
    sem.lm.5 <- errorsarlm(lm.5, listw=coord.list)
    
    ##record var betas
    coef_add <-  as.data.frame(t(sem.lm.5[["coefficients"]]))
    coef_g4 <- rbind(coef_g4, coef_add)  
    
    p_add <- as.data.frame(t(summary(sem.lm.5)$Coef[,4]))
    p_g4 <-  rbind(p_g4, p_add)
    
}

###Create Table Matrix
table <- data.frame(vc1=double(33),vc2=double(33),vc3=double(33),vc4=double(33),vc5=double(33),vc6=double(33),vc7=double(33),vc8=double(33),vc9=double(33),vc10=double(33))
colnames(table) <- c("NDVI: No Gaps", "P-Value1", "NDVI: Gaps", "P-Value2","NDVI: Gaps 2","P-Value3","Forest Size","P-Value4","Gap Size","P-Value5")
rownames(table) <- c("NDVI","se15","Area (h)","se1","Edge:Area Ratio","se2","Elevation (km)","se3","Precipitation (m)","se4","Population Catchment","se5",
                     "Population Catchment^2","se6","Distance to Market (km)","se7","Firewood Collection Time","se8",
                     "Household Head Education","se9","Iddir Membership","se10","Own Bicycle","se11","Own Electric Stove","se12",
                     "Own Television","se13","Gap Size (h/10)","se14","Intercept","se16","SSR")
table[table==0] <- NA

##Record No-Gaps Stats
  ##R-Squared and Lambda
  table$'NDVI: No Gaps'[33] <- sem.lm.1[["SSE"]]
  
  ##Variable Vars
  #gen p-value binary. Tests are single-tailed, based on whether the mean for the var is + or -
  coef$e_ar_p <- ifelse(coef$e_area_h < 0, 1, 0)
  coef$e_ea_p <- ifelse(coef$e_earatio_km > 0, 1, 0)
  coef$e_el_p <- ifelse(coef$elev_km < 0, 1, 0)
  coef$e_pr_p <- ifelse(coef$e_prec_m < 0, 1, 0)
  coef$pop_p <- ifelse(coef$pop_density_catch_100 > 0, 1, 0)
  #coef$pop_p2 <- ifelse(coef$pop_density_catch_100_2 < 0, 1, 0)
  coef$dm_p <- ifelse(coef$dist_market > 0, 1, 0)
  coef$tsf_p <- ifelse(coef$tsfirewood > 0, 1, 0)
  #coef$tca_p <- ifelse(coef$total_cons_ann > 0, 1, 0)
  coef$he_p <- ifelse(coef$hh_education < 0, 1, 0)
  coef$id_p <- ifelse(coef$iddir < 0, 1, 0)
  coef$bi_p <- ifelse(coef$bicycle < 0, 1, 0)
  coef$el_p <- ifelse(coef$electric > 0, 1, 0)
  coef$tv_p <- ifelse(coef$tv > 0, 1, 0)
  coef$int_p <- ifelse(coef$'(Intercept)' < 0, 1, 0)
  
  #P-Values, using form: (r+1)/(n+1), where r is the number of observations which reject the null
  table$`P-Value1`[3] <- 2*(sum(coef$e_ar_p)+1)/(200+1)
  table$`P-Value1`[5] <- 2*(sum(coef$e_ea_p)+1)/(200+1)
  table$`P-Value1`[7] <- 2*(sum(coef$e_el_p)+1)/(200+1)
  table$`P-Value1`[9] <- 2*(sum(coef$e_pr_p)+1)/(200+1)
  table$`P-Value1`[11] <- 2*(sum(coef$pop_p)+1)/(200+1)
  #table$`P-Value1`[13] <- 2*(sum(coef$pop_p2)+1)/(200+1)
  table$`P-Value1`[15] <- 2*(sum(coef$dm_p)+1)/(200+1)
  table$`P-Value1`[17] <- 2*(sum(coef$tsf_p)+1)/(200+1)
  table$`P-Value1`[19] <- 2*(sum(coef$he_p)+1)/(200+1)
  table$`P-Value1`[21] <- 2*(sum(coef$id_p)+1)/(200+1)
  table$`P-Value1`[23] <- 2*(sum(coef$bi_p)+1)/(200+1)
  table$`P-Value1`[25] <- 2*(sum(coef$el_p)+1)/(200+1)
  table$`P-Value1`[27] <- 2*(sum(coef$tv_p)+1)/(200+1)
  table$`P-Value1`[31] <- 2*(sum(coef$int_p)+1)/(200+1)
  
  #calculate mean effects
  table$'NDVI: No Gaps'[3] <- mean(coef$e_area_h)
  table$'NDVI: No Gaps'[5] <- mean(coef$e_earatio_km)
  table$'NDVI: No Gaps'[7] <- mean(coef$elev_km)
  table$'NDVI: No Gaps'[9] <- mean(coef$e_prec_m)
  table$'NDVI: No Gaps'[11] <- mean(coef$pop_density_catch_100)
  #table$'NDVI: No Gaps'[13] <- mean(coef$pop_density_catch_100_2)
  table$'NDVI: No Gaps'[15] <- mean(coef$dist_market)
  table$'NDVI: No Gaps'[17] <- mean(coef$tsfirewood)
  table$'NDVI: No Gaps'[19] <- mean(coef$hh_education)
  table$'NDVI: No Gaps'[21] <- mean(coef$iddir)
  table$'NDVI: No Gaps'[23] <- mean(coef$bicycle)
  table$'NDVI: No Gaps'[25] <- mean(coef$electric)
  table$'NDVI: No Gaps'[27] <- mean(coef$tv)
  table$'NDVI: No Gaps'[31] <- mean(coef$'(Intercept)')
  
  #CALCULATE STANDARD DEV
  table$'NDVI: No Gaps'[4] <- sd(coef$e_area_h)
  table$'NDVI: No Gaps'[6] <- sd(coef$e_earatio_km)
  table$'NDVI: No Gaps'[8] <- sd(coef$elev_km)
  table$'NDVI: No Gaps'[10] <- sd(coef$e_prec_m)
  table$'NDVI: No Gaps'[12] <- sd(coef$pop_density_catch_100)
  #table$'NDVI: No Gaps'[14] <- sd(coef$pop_density_catch_100_2)
  table$'NDVI: No Gaps'[16] <- sd(coef$dist_market)
  table$'NDVI: No Gaps'[18] <- sd(coef$tsfirewood)
  table$'NDVI: No Gaps'[20] <- sd(coef$hh_education)
  table$'NDVI: No Gaps'[22] <- sd(coef$iddir)
  table$'NDVI: No Gaps'[24] <- sd(coef$bicycle)
  table$'NDVI: No Gaps'[26] <- sd(coef$electric)
  table$'NDVI: No Gaps'[28] <- sd(coef$tv)
  table$'NDVI: No Gaps'[32] <- sd(coef$'(Intercept)')

##Record Gaps Stats
  ##R-Squared and Lambda
  table$'NDVI: Gaps'[33] <- sem.lm.2[["SSE"]]
  
  ##Variable Vars
  #gen p-value binary. Tests are single-tailed, based on whether the mean for the var is + or -
  coef_g$d_ar_p <- ifelse(coef_g$d_area_h > 0, 1, 0)
  coef_g$d_ea_p <- ifelse(coef_g$d_earatio_km > 0, 1, 0)
  coef_g$d_el_p <- ifelse(coef_g$elev_km < 0, 1, 0)
  coef_g$d_pr_p <- ifelse(coef_g$e_prec_m < 0, 1, 0)
  coef_g$pop_p <- ifelse(coef_g$pop_density_catch_100 < 0, 1, 0)
  #coef_g$pop_p2 <- ifelse(coef_g$pop_density_catch_100_2 < 0, 1, 0)
  coef_g$dm_p <- ifelse(coef_g$dist_market > 0, 1, 0)
  coef_g$tsf_p <- ifelse(coef_g$tsfirewood > 0, 1, 0)
  #coef_g$tca_p <- ifelse(coef_g$total_cons_ann > 0, 1, 0)
  coef_g$he_p <- ifelse(coef_g$hh_education < 0, 1, 0)
  coef_g$id_p <- ifelse(coef_g$iddir < 0, 1, 0)
  coef_g$bi_p <- ifelse(coef_g$bicycle < 0, 1, 0)
  coef_g$el_p <- ifelse(coef_g$electric > 0, 1, 0)
  coef_g$tv_p <- ifelse(coef_g$tv > 0, 1, 0)
  coef_g$int_p <- ifelse(coef_g$'(Intercept)' < 0, 1, 0)

  #P-Values, using two-tailed form: 2*(r+1)/(n+1), where r is the number of observations which reject the null.
  table$`P-Value2`[3] <- 2*(sum(coef_g$d_ar_p)+1)/(200+1)
  table$`P-Value2`[5] <- 2*(sum(coef_g$d_ea_p)+1)/(200+1)
  table$`P-Value2`[7] <- 2*(sum(coef_g$d_el_p)+1)/(200+1)
  table$`P-Value2`[9] <- 2*(sum(coef_g$d_pr_p)+1)/(200+1)
  table$`P-Value2`[11] <- 2*(sum(coef_g$pop_p)+1)/(200+1)
  #table$`P-Value2`[13] <- 2*(sum(coef_g$pop_p2)+1)/(200+1)
  table$`P-Value2`[15] <- 2*(sum(coef_g$dm_p)+1)/(200+1)
  table$`P-Value2`[17] <- 2*(sum(coef_g$tsf_p)+1)/(200+1)
  table$`P-Value2`[19] <- 2*(sum(coef_g$he_p)+1)/(200+1)
  table$`P-Value2`[21] <- 2*(sum(coef_g$id_p)+1)/(200+1)
  table$`P-Value2`[23] <- 2*(sum(coef_g$bi_p)+1)/(200+1)
  table$`P-Value2`[25] <- 2*(sum(coef_g$el_p)+1)/(200+1)
  table$`P-Value2`[27] <- 2*(sum(coef_g$tv_p)+1)/(200+1)
  table$`P-Value2`[31] <- 2*(sum(coef_g$int_p)+1)/(200+1)
  
  #calculate mean effects
  table$'NDVI: Gaps'[3] <- mean(coef_g$d_area_h)
  table$'NDVI: Gaps'[5] <- mean(coef_g$d_earatio_km)
  table$'NDVI: Gaps'[7] <- mean(coef_g$elev_km)
  table$'NDVI: Gaps'[9] <- mean(coef_g$e_prec_m)
  table$'NDVI: Gaps'[11] <- mean(coef_g$pop_density_catch_100)
  #table$'NDVI: Gaps'[13] <- mean(coef_g$pop_density_catch_100_2)
  table$'NDVI: Gaps'[15] <- mean(coef_g$dist_market)
  table$'NDVI: Gaps'[17] <- mean(coef_g$tsfirewood)
  table$'NDVI: Gaps'[19] <- mean(coef_g$hh_education)
  table$'NDVI: Gaps'[21] <- mean(coef_g$iddir)
  table$'NDVI: Gaps'[23] <- mean(coef_g$bicycle)
  table$'NDVI: Gaps'[25] <- mean(coef_g$electric)
  table$'NDVI: Gaps'[27] <- mean(coef_g$tv)
  table$'NDVI: Gaps'[31] <- mean(coef_g$'(Intercept)')
  
  #CALCULATE STANDARD DEV
  table$'NDVI: Gaps'[4] <- sd(coef_g$d_area_h)
  table$'NDVI: Gaps'[6] <- sd(coef_g$d_earatio_km)
  table$'NDVI: Gaps'[8] <- sd(coef_g$elev_km)
  table$'NDVI: Gaps'[10] <- sd(coef_g$e_prec_m)
  table$'NDVI: Gaps'[12] <- sd(coef_g$pop_density_catch_100)
  #table$'NDVI: Gaps'[14] <- sd(coef_g$pop_density_catch_100_2)
  table$'NDVI: Gaps'[16] <- sd(coef_g$dist_market)
  table$'NDVI: Gaps'[18] <- sd(coef_g$tsfirewood)
  table$'NDVI: Gaps'[20] <- sd(coef_g$hh_education)
  table$'NDVI: Gaps'[22] <- sd(coef_g$iddir)
  table$'NDVI: Gaps'[24] <- sd(coef_g$bicycle)
  table$'NDVI: Gaps'[26] <- sd(coef_g$electric)
  table$'NDVI: Gaps'[28] <- sd(coef_g$tv)
  table$'NDVI: Gaps'[32] <- sd(coef_g$'(Intercept)')

##Record Gaps 2 Stats
  ##R-Squared and Lambda
  table$'NDVI: Gaps 2'[33] <- sem.lm.3[["SSE"]]
  
  ##Variable Vars
  #gen p-value binary. Tests are single-tailed, based on whether the mean for the var is + or -
  coef_g2$d_ar_p <- ifelse(coef_g2$d_area_h > 0, 1, 0)
  coef_g2$d_ea_p <- ifelse(coef_g2$d_earatio_km > 0, 1, 0)
  coef_g2$d_el_p <- ifelse(coef_g2$elev_km < 0, 1, 0)
  coef_g2$d_pr_p <- ifelse(coef_g2$e_prec_m < 0, 1, 0)
  coef_g2$pop_p <- ifelse(coef_g2$pop_density_catch_100 < 0, 1, 0)
  #coef_g2$pop_p2 <- ifelse(coef_g2$pop_density_catch_100_2 < 0, 1, 0)
  coef_g2$dm_p <- ifelse(coef_g2$dist_market > 0, 1, 0)
  coef_g2$tsf_p <- ifelse(coef_g2$tsfirewood > 0, 1, 0)
  #coef_g2$tca_p <- ifelse(coef_g2$total_cons_ann > 0, 1, 0)
  coef_g2$he_p <- ifelse(coef_g2$hh_education < 0, 1, 0)
  coef_g2$id_p <- ifelse(coef_g2$iddir < 0, 1, 0)
  coef_g2$bi_p <- ifelse(coef_g2$bicycle < 0, 1, 0)
  coef_g2$el_p <- ifelse(coef_g2$electric > 0, 1, 0)
  coef_g2$tv_p <- ifelse(coef_g2$tv > 0, 1, 0)
  coef_g2$gs_p <- ifelse(coef_g2$gap_size_h10 < 0, 1, 0)
  coef_g2$int_p <- ifelse(coef_g2$'(Intercept)' < 0, 1, 0)
  
  #P-Values, using two-tailed form: 2*(r+1)/(n+1), where r is the number of observations which reject the null.
  table$`P-Value3`[3] <- 2*(sum(coef_g2$d_ar_p)+1)/(200+1)
  table$`P-Value3`[5] <- 2*(sum(coef_g2$d_ea_p)+1)/(200+1)
  table$`P-Value3`[7] <- 2*(sum(coef_g2$d_el_p)+1)/(200+1)
  table$`P-Value3`[9] <- 2*(sum(coef_g2$d_pr_p)+1)/(200+1)
  table$`P-Value3`[11] <- 2*(sum(coef_g2$pop_p)+1)/(200+1)
  #table$`P-Value3`[13] <- 2*(sum(coef_g2$pop_p2)+1)/(200+1)
  table$`P-Value3`[15] <- 2*(sum(coef_g2$dm_p)+1)/(200+1)
  table$`P-Value3`[17] <- 2*(sum(coef_g2$tsf_p)+1)/(200+1)
  table$`P-Value3`[19] <- 2*(sum(coef_g2$he_p)+1)/(200+1)
  table$`P-Value3`[21] <- 2*(sum(coef_g2$id_p)+1)/(200+1)
  table$`P-Value3`[23] <- 2*(sum(coef_g2$bi_p)+1)/(200+1)
  table$`P-Value3`[25] <- 2*(sum(coef_g2$el_p)+1)/(200+1)
  table$`P-Value3`[27] <- 2*(sum(coef_g2$tv_p)+1)/(200+1)
  table$`P-Value3`[29] <- 2*(sum(coef_g2$gs_p)+1)/(200+1)
  table$`P-Value3`[31] <- 2*(sum(coef_g2$int_p)+1)/(200+1)
  
  #calculate mean effects
  table$'NDVI: Gaps 2'[3] <- mean(coef_g2$d_area_h)
  table$'NDVI: Gaps 2'[5] <- mean(coef_g2$d_earatio_km)
  table$'NDVI: Gaps 2'[7] <- mean(coef_g2$elev_km)
  table$'NDVI: Gaps 2'[9] <- mean(coef_g2$e_prec_m)
  table$'NDVI: Gaps 2'[11] <- mean(coef_g2$pop_density_catch_100)
  #table$'NDVI: Gaps 2'[13] <- mean(coef_g2$pop_density_catch_100_2)
  table$'NDVI: Gaps 2'[15] <- mean(coef_g2$dist_market)
  table$'NDVI: Gaps 2'[17] <- mean(coef_g2$tsfirewood)
  table$'NDVI: Gaps 2'[19] <- mean(coef_g2$hh_education)
  table$'NDVI: Gaps 2'[21] <- mean(coef_g2$iddir)
  table$'NDVI: Gaps 2'[23] <- mean(coef_g2$bicycle)
  table$'NDVI: Gaps 2'[25] <- mean(coef_g2$electric)
  table$'NDVI: Gaps 2'[27] <- mean(coef_g2$tv)
  table$'NDVI: Gaps 2'[29] <- mean(coef_g2$gap_size_h10)
  table$'NDVI: Gaps 2'[31] <- mean(coef_g2$'(Intercept)')
  
  #CALCULATE STANDARD DEV
  table$'NDVI: Gaps 2'[4] <- sd(coef_g2$d_area_h)
  table$'NDVI: Gaps 2'[6] <- sd(coef_g2$d_earatio_km)
  table$'NDVI: Gaps 2'[8] <- sd(coef_g2$elev_km)
  table$'NDVI: Gaps 2'[10] <- sd(coef_g2$e_prec_m)
  table$'NDVI: Gaps 2'[12] <- sd(coef_g2$pop_density_catch_100)
  #table$'NDVI: Gaps 2'[14] <- sd(coef_g2$pop_density_catch_100_2)
  table$'NDVI: Gaps 2'[16] <- sd(coef_g2$dist_market)
  table$'NDVI: Gaps 2'[18] <- sd(coef_g2$tsfirewood)
  table$'NDVI: Gaps 2'[20] <- sd(coef_g2$hh_education)
  table$'NDVI: Gaps 2'[22] <- sd(coef_g2$iddir)
  table$'NDVI: Gaps 2'[24] <- sd(coef_g2$bicycle)
  table$'NDVI: Gaps 2'[26] <- sd(coef_g2$electric)
  table$'NDVI: Gaps 2'[28] <- sd(coef_g2$tv)
  table$'NDVI: Gaps 2'[30] <- sd(coef_g2$gap_size_h10)
  table$'NDVI: Gaps 2'[32] <- sd(coef_g2$'(Intercept)')
  
##Record Forest Size Stats
  ##R-Squared and Lambda
  table$'Forest Size'[33] <- sem.lm.4[["SSE"]]
  
  ##Variable Vars
  #gen p-value binary. Tests are single-tailed, based on whether the mean for the var is + or -
  coef_g3$d_ndvi_p <- ifelse(coef_g3$d_ndvi_r_10 > 0, 1, 0)
  coef_g3$d_ea_p <- ifelse(coef_g3$d_earatio_km > 0, 1, 0)
  coef_g3$d_el_p <- ifelse(coef_g3$elev_km < 0, 1, 0)
  coef_g3$d_pr_p <- ifelse(coef_g3$e_prec_m < 0, 1, 0)
  coef_g3$pop_p <- ifelse(coef_g3$pop_density_catch_100 < 0, 1, 0)
  #coef_g3$pop_p2 <- ifelse(coef_g3$pop_density_catch_100_2 > 0, 1, 0)
  coef_g3$dm_p <- ifelse(coef_g3$dist_market < 0, 1, 0)
  coef_g3$tsf_p <- ifelse(coef_g3$tsfirewood < 0, 1, 0)
  #coef_g3$tca_p <- ifelse(coef_g3$total_cons_ann < 0, 1, 0)
  coef_g3$he_p <- ifelse(coef_g3$hh_education < 0, 1, 0)
  coef_g3$id_p <- ifelse(coef_g3$iddir < 0, 1, 0)
  coef_g3$bi_p <- ifelse(coef_g3$bicycle < 0, 1, 0)
  coef_g3$el_p <- ifelse(coef_g3$electric < 0, 1, 0)
  coef_g3$tv_p <- ifelse(coef_g3$tv > 0, 1, 0)
  coef_g3$gs_p <- ifelse(coef_g3$gap_size_h10 < 0, 1, 0)
  coef_g3$int_p <- ifelse(coef_g3$'(Intercept)' < 0, 1, 0)
  
  #P-Values, using two-tailed form: 2*(r+1)/(n+1), where r is the number of observations which reject the null.
  table$`P-Value4`[1] <- 2*(sum(coef_g3$d_ndvi_p)+1)/(200+1)
  table$`P-Value4`[5] <- 2*(sum(coef_g3$d_ea_p)+1)/(200+1)
  table$`P-Value4`[7] <- 2*(sum(coef_g3$d_el_p)+1)/(200+1)
  table$`P-Value4`[9] <- 2*(sum(coef_g3$d_pr_p)+1)/(200+1)
  table$`P-Value4`[11] <- 2*(sum(coef_g3$pop_p)+1)/(200+1)
  #table$`P-Value4`[13] <- 2*(sum(coef_g3$pop_p2)+1)/(200+1)
  table$`P-Value4`[15] <- 2*(sum(coef_g3$dm_p)+1)/(200+1)
  table$`P-Value4`[17] <- 2*(sum(coef_g3$tsf_p)+1)/(200+1)
  table$`P-Value4`[19] <- 2*(sum(coef_g3$he_p)+1)/(200+1)
  table$`P-Value4`[21] <- 2*(sum(coef_g3$id_p)+1)/(200+1)
  table$`P-Value4`[23] <- 2*(sum(coef_g3$bi_p)+1)/(200+1)
  table$`P-Value4`[25] <- 2*(sum(coef_g3$el_p)+1)/(200+1)
  table$`P-Value4`[27] <- 2*(sum(coef_g3$tv_p)+1)/(200+1)
  table$`P-Value4`[29] <- 2*(sum(coef_g3$gs_p)+1)/(200+1)
  table$`P-Value4`[31] <- 2*(sum(coef_g3$int_p)+1)/(200+1)
  
  #calculate mean effects
  table$'Forest Size'[1] <- mean(coef_g3$d_ndvi_r_10)
  table$'Forest Size'[5] <- mean(coef_g3$d_earatio_km)
  table$'Forest Size'[7] <- mean(coef_g3$elev_km)
  table$'Forest Size'[9] <- mean(coef_g3$e_prec_m)
  table$'Forest Size'[11] <- mean(coef_g3$pop_density_catch_100)
  #table$'Forest Size'[13] <- mean(coef_g3$pop_density_catch_100_2)
  table$'Forest Size'[15] <- mean(coef_g3$dist_market)
  table$'Forest Size'[17] <- mean(coef_g3$tsfirewood)
  table$'Forest Size'[19] <- mean(coef_g3$hh_education)
  table$'Forest Size'[21] <- mean(coef_g3$iddir)
  table$'Forest Size'[23] <- mean(coef_g3$bicycle)
  table$'Forest Size'[25] <- mean(coef_g3$electric)
  table$'Forest Size'[27] <- mean(coef_g3$tv)
  table$'Forest Size'[29] <- mean(coef_g3$gap_size_h10)
  table$'Forest Size'[31] <- mean(coef_g3$'(Intercept)')
  
  #CALCULATE STANDARD DEV
  table$'Forest Size'[2] <- sd(coef_g3$d_ndvi_r_10)
  table$'Forest Size'[6] <- sd(coef_g3$d_earatio_km)
  table$'Forest Size'[8] <- sd(coef_g3$elev_km)
  table$'Forest Size'[10] <- sd(coef_g3$e_prec_m)
  table$'Forest Size'[12] <- sd(coef_g3$pop_density_catch_100)
  #table$'Forest Size'[14] <- sd(coef_g3$pop_density_catch_100_2)
  table$'Forest Size'[16] <- sd(coef_g3$dist_market)
  table$'Forest Size'[18] <- sd(coef_g3$tsfirewood)
  table$'Forest Size'[20] <- sd(coef_g3$hh_education)
  table$'Forest Size'[22] <- sd(coef_g3$iddir)
  table$'Forest Size'[24] <- sd(coef_g3$bicycle)
  table$'Forest Size'[26] <- sd(coef_g3$electric)
  table$'Forest Size'[28] <- sd(coef_g3$tv)
  table$'Forest Size'[30] <- sd(coef_g3$gap_size_h10)
  table$'Forest Size'[32] <- sd(coef_g3$'(Intercept)') 
  
##Record Gap Size Stats
  ##R-Squared and Lambda
  table$'Gap Size'[33] <- sem.lm.5[["SSE"]]
  
  ##Variable Vars
  #gen p-value binary. Tests are single-tailed, based on whether the mean for the var is + or -
  coef_g4$d_ndvi_p <- ifelse(coef_g4$d_ndvi_r_10 < 0, 1, 0)
  coef_g4$d_ar_p <- ifelse(coef_g4$d_area_h < 0, 1, 0)
  coef_g4$d_ea_p <- ifelse(coef_g4$d_earatio_km < 0, 1, 0)
  coef_g4$d_el_p <- ifelse(coef_g4$elev_km < 0, 1, 0)
  coef_g4$d_pr_p <- ifelse(coef_g4$e_prec_m < 0, 1, 0)
  coef_g4$pop_p <- ifelse(coef_g4$pop_density_catch_100 < 0, 1, 0)
  #coef_g4$pop_p2 <- ifelse(coef_g4$pop_density_catch_100_2 > 0, 1, 0)
  coef_g4$dm_p <- ifelse(coef_g4$dist_market > 0, 1, 0)
  coef_g4$tsf_p <- ifelse(coef_g4$tsfirewood > 0, 1, 0)
  #coef_g4$tca_p <- ifelse(coef_g4$total_cons_ann < 0, 1, 0)
  coef_g4$he_p <- ifelse(coef_g4$hh_education < 0, 1, 0)
  coef_g4$id_p <- ifelse(coef_g4$iddir < 0, 1, 0)
  coef_g4$bi_p <- ifelse(coef_g4$bicycle > 0, 1, 0)
  coef_g4$el_p <- ifelse(coef_g4$electric < 0, 1, 0)
  coef_g4$tv_p <- ifelse(coef_g4$tv < 0, 1, 0)
  coef_g4$int_p <- ifelse(coef_g4$'(Intercept)' > 0, 1, 0)
  
  #P-Values, using two-tailed form: 2*(r+1)/(n+1), where r is the number of observations which reject the null.
  table$`P-Value5`[1] <- 2*(sum(coef_g4$d_ndvi_p)+1)/(200+1)
  table$`P-Value5`[3] <- 2*(sum(coef_g4$d_ar_p)+1)/(200+1)
  table$`P-Value5`[5] <- 2*(sum(coef_g4$d_ea_p)+1)/(200+1)
  table$`P-Value5`[7] <- 2*(sum(coef_g4$d_el_p)+1)/(200+1)
  table$`P-Value5`[9] <- 2*(sum(coef_g4$d_pr_p)+1)/(200+1)
  table$`P-Value5`[11] <- 2*(sum(coef_g4$pop_p)+1)/(200+1)
  #table$`P-Value5`[13] <- 2*(sum(coef_g4$pop_p2)+1)/(200+1)
  table$`P-Value5`[15] <- 2*(sum(coef_g4$dm_p)+1)/(200+1)
  table$`P-Value5`[17] <- 2*(sum(coef_g4$tsf_p)+1)/(200+1)
  table$`P-Value5`[19] <- 2*(sum(coef_g4$he_p)+1)/(200+1)
  table$`P-Value5`[21] <- 2*(sum(coef_g4$id_p)+1)/(200+1)
  table$`P-Value5`[23] <- 2*(sum(coef_g4$bi_p)+1)/(200+1)
  table$`P-Value5`[25] <- 2*(sum(coef_g4$el_p)+1)/(200+1)
  table$`P-Value5`[27] <- 2*(sum(coef_g4$tv_p)+1)/(200+1)
  table$`P-Value5`[31] <- 2*(sum(coef_g4$int_p)+1)/(200+1)
  
  #calculate mean effects
  table$'Gap Size'[1] <- mean(coef_g4$d_ndvi_r_10)
  table$'Gap Size'[3] <- mean(coef_g4$d_area_h)
  table$'Gap Size'[5] <- mean(coef_g4$d_earatio_km)
  table$'Gap Size'[7] <- mean(coef_g4$elev_km)
  table$'Gap Size'[9] <- mean(coef_g4$e_prec_m)
  table$'Gap Size'[11] <- mean(coef_g4$pop_density_catch_100)
  #table$'Gap Size'[13] <- mean(coef_g4$pop_density_catch_100_2)
  table$'Gap Size'[15] <- mean(coef_g4$dist_market)
  table$'Gap Size'[17] <- mean(coef_g4$tsfirewood)
  table$'Gap Size'[19] <- mean(coef_g4$hh_education)
  table$'Gap Size'[21] <- mean(coef_g4$iddir)
  table$'Gap Size'[23] <- mean(coef_g4$bicycle)
  table$'Gap Size'[25] <- mean(coef_g4$electric)
  table$'Gap Size'[27] <- mean(coef_g4$tv)
  table$'Gap Size'[31] <- mean(coef_g4$'(Intercept)')
  
  #CALCULATE STANDARD DEV
  table$'Gap Size'[2] <- sd(coef_g4$d_ndvi_r_10)
  table$'Gap Size'[4] <- sd(coef_g4$d_area_h)
  table$'Gap Size'[6] <- sd(coef_g4$d_earatio_km)
  table$'Gap Size'[8] <- sd(coef_g4$elev_km)
  table$'Gap Size'[10] <- sd(coef_g4$e_prec_m)
  table$'Gap Size'[12] <- sd(coef_g4$pop_density_catch_100)
  #table$'Gap Size'[14] <- sd(coef_g4$pop_density_catch_100_2)
  table$'Gap Size'[16] <- sd(coef_g4$dist_market)
  table$'Gap Size'[18] <- sd(coef_g4$tsfirewood)
  table$'Gap Size'[20] <- sd(coef_g4$hh_education)
  table$'Gap Size'[22] <- sd(coef_g4$iddir)
  table$'Gap Size'[24] <- sd(coef_g4$bicycle)
  table$'Gap Size'[26] <- sd(coef_g4$electric)
  table$'Gap Size'[28] <- sd(coef_g4$tv)
  table$'Gap Size'[32] <- sd(coef_g4$'(Intercept)') 
  
View(table)

#as is, the above code is a tad sloppy. would be best to program 2-tail so you don't have to manually change sign when p >1. ah well, still works
write.csv(table,"sarerr_output.csv")
  
  


#############################
# Robustness Checks
#############################
##Set up SARerr regressions
#create coordinate matrix
coords <- as.matrix(cbind(maindata$X, maindata$Y))
#create nearest neighbor pairs at 100,000m
coord.nb <- dnearneigh(coords, 0, 100000, longlat=FALSE)
coord.list <- nb2listw(coord.nb, style="W")

### Rerun Landscape SARs for sample 250 ESS forests

#no gaps SAR
lm.0.1 <- lm(e_ndvi_r ~ e_area_h + e_earatio_km + elev_km + e_prec_m + pop_density_catch_100, data=maindata)
sem.lm.0.1 <- errorsarlm(lm.0.1, listw=coord.list)

#gaps sar
lm.0.2 <- lm(d_ndvi_r ~ d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100, data=maindata)
sem.lm.0.2 <- errorsarlm(lm.0.2, listw=coord.list)

#gaps 2 sar
lm.0.3 <- lm(d_ndvi_r ~ d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + gap_size_h10, data=maindata)
sem.lm.0.3 <- errorsarlm(lm.0.3, listw=coord.list)

#forest area SAR
lm.0.4 <- lm(d_area_h ~ d_ndvi_r_10 + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100 + gap_size_h10, data=maindata)
sem.lm.0.4 <- errorsarlm(lm.0.4, listw=coord.list)

#gap size SAR
lm.0.5 <- lm(gap_size_h10 ~ d_ndvi_r_10 + d_area_h + d_earatio_km + elev_km + e_prec_m + pop_density_catch_100, data=maindata)
sem.lm.0.5 <- errorsarlm(lm.0.5, listw=coord.list)

#summaries
sem.lm.0.1 %>% summary()
sem.lm.0.2 %>% summary()
sem.lm.0.3 %>% summary()
sem.lm.0.4 %>% summary()
sem.lm.0.5 %>% summary()


### P analysis. a bit restrictive
# create p dist tables
library(vtable)
sumtable(p, out='csv', file='p.csv', digits=3,
         vars=c('e_area_h',
                'e_earatio_km',
                'elev_km',
                'e_prec_m',
                'pop_density_catch_100'),
         summ=c('mean(x)',
                'min(x)',
                'max(x)'))

sumtable(p_g, out='csv', file='p2.csv', digits=3,
         vars=c('d_area_h',
                'd_earatio_km',
                'elev_km',
                'e_prec_m',
                'pop_density_catch_100'),
         summ=c('mean(x)',
                'min(x)',
                'max(x)'))

sumtable(p_g2, out='csv', file='p3.csv', digits=3,
         vars=c('d_area_h',
                'd_earatio_km',
                'elev_km',
                'e_prec_m',
                'pop_density_catch_100',
                'gap_size_h10'),
         summ=c('mean(x)',
                'min(x)',
                'max(x)'))

sumtable(p_g3, out='csv', file='p4.csv', digits=3,
         vars=c('d_ndvi_r_10',
                'd_earatio_km',
                'elev_km',
                'e_prec_m',
                'pop_density_catch_100',
                'gap_size_h10'),
         summ=c('mean(x)',
                'min(x)',
                'max(x)'))

sumtable(p_g4, out='csv', file='p5.csv', digits=3,
         vars=c('d_ndvi_r_10',
                'd_area_h',
                'd_earatio_km',
                'elev_km',
                'e_prec_m',
                'pop_density_catch_100'),
         summ=c('mean(x)',
                'min(x)',
                'max(x)'))

#Calc p-values under 0.05

p$e_area_h_s <- ifelse(p$e_area_h < 0.05, 1, 0) 
p$e_earatio_km_s <- ifelse(p$e_earatio_km < 0.05, 1, 0) 
p$elev_km_s <- ifelse(p$elev_km < 0.05, 1, 0)
p$e_prec_m_s <- ifelse(p$e_prec_m < 0.05, 1, 0) 
p$pop_density_catch_100_s <- ifelse(p$pop_density_catch_100 < 0.05, 1, 0)

p_g$d_area_h_s <- ifelse(p_g$d_area_h < 0.05, 1, 0) 
p_g$d_earatio_km_s <- ifelse(p_g$d_earatio_km < 0.05, 1, 0) 
p_g$elev_km_s <- ifelse(p_g$elev_km < 0.05, 1, 0)
p_g$e_prec_m_s <- ifelse(p_g$e_prec_m < 0.05, 1, 0) 
p_g$pop_density_catch_100_s <- ifelse(p_g$pop_density_catch_100 < 0.05, 1, 0)

p_g2$d_area_h_s <- ifelse(p_g2$d_area_h < 0.05, 1, 0) 
p_g2$d_earatio_km_s <- ifelse(p_g2$d_earatio_km < 0.05, 1, 0) 
p_g2$elev_km_s <- ifelse(p_g2$elev_km < 0.05, 1, 0)
p_g2$e_prec_m_s <- ifelse(p_g2$e_prec_m < 0.05, 1, 0) 
p_g2$pop_density_catch_100_s <- ifelse(p_g2$pop_density_catch_100 < 0.05, 1, 0)
p_g2$gap_size_h10_s <- ifelse(p_g2$gap_size_h10 < 0.05, 1, 0)

p_g3$d_ndvi_r_10_s <- ifelse(p_g3$d_ndvi_r_10 < 0.05, 1, 0) 
p_g3$d_earatio_km_s <- ifelse(p_g3$d_earatio_km < 0.05, 1, 0) 
p_g3$elev_km_s <- ifelse(p_g3$elev_km < 0.05, 1, 0)
p_g3$e_prec_m_s <- ifelse(p_g3$e_prec_m < 0.05, 1, 0) 
p_g3$pop_density_catch_100_s <- ifelse(p_g3$pop_density_catch_100 < 0.05, 1, 0)
p_g3$gap_size_h10_s <- ifelse(p_g3$gap_size_h10 < 0.05, 1, 0)

p_g4$d_ndvi_r_10_s <- ifelse(p_g4$d_ndvi_r_10 < 0.05, 1, 0) 
p_g4$d_area_h_s <- ifelse(p_g4$d_area_h < 0.05, 1, 0) 
p_g4$d_earatio_km_s <- ifelse(p_g4$d_earatio_km < 0.05, 1, 0) 
p_g4$elev_km_s <- ifelse(p_g4$elev_km < 0.05, 1, 0)
p_g4$e_prec_m_s <- ifelse(p_g4$e_prec_m < 0.05, 1, 0) 
p_g4$pop_density_catch_100_s <- ifelse(p_g4$pop_density_catch_100 < 0.05, 1, 0)

#model6
mean(p$e_area_h_s)
mean(p$e_earatio_km_s)
mean(p$elev_km_s)
mean(p$e_prec_m_s)
mean(p$pop_density_catch_100_s)
#model7
mean(p_g$d_area_h_s)
mean(p_g$d_earatio_km_s)
mean(p_g$elev_km_s)
mean(p_g$e_prec_m_s)
mean(p_g$pop_density_catch_100_s)
#model8
mean(p_g2$d_area_h_s)
mean(p_g2$d_earatio_km_s)
mean(p_g2$elev_km_s)
mean(p_g2$e_prec_m_s)
mean(p_g2$pop_density_catch_100_s)
mean(p_g2$gap_size_h10_s)
#model9
mean(p_g3$d_ndvi_r_10_s)
mean(p_g3$d_earatio_km_s)
mean(p_g3$elev_km_s)
mean(p_g3$e_prec_m_s)
mean(p_g3$pop_density_catch_100_s)
mean(p_g3$gap_size_h10_s)
#model10
mean(p_g4$d_ndvi_r_10_s)
mean(p_g4$d_area_h_s)
mean(p_g4$d_earatio_km_s)
mean(p_g4$elev_km_s)
mean(p_g4$e_prec_m_s)
mean(p_g4$pop_density_catch_100_s)
