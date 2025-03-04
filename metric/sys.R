library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere);library(parallel);library(foreign)
setwd('E:/luoa/gamma/')
dat <- read.dbf('data/mapping/data_1d_land.dbf')
dat <- dat[,c('layer','gmm_obs')]
# colnames(dat)
# dat <- rename(dat,gmm_obs=Gmm_obs)

# load gamma EH and alpha in pixels ---------------------------------------

join.dat <- read.csv('results/100m/gammas_100m_1mDEM_pixel.csv')
colnames(join.dat)[1] <- 'layer'

dat <- left_join(dat,join.dat)
dat$gmm_GI <- dat$gmm_obs-dat$gmm_EH
dat$land <- (!is.na(dat$gmm_obs))&(!is.na(dat$a_mn))
write.dbf(dat,'data/mapping/data_1d_land.dbf')

test <- read_sf('data/mapping/data_1d_land.shp')
plot(test[,'a_mn'])
plot(test[,'gmm_GI'])

test[['gmm_obs']][!test$land%in%'T'] <- NA
plot(test[,'gmm_obs'])

plot(test[test$land%in%'T','gmm_obs'])

#load('data/beta/beta_all.rdata') # b.s

# fls  <- list.files(path = 'results/test/')
# gammas.list <- lapply(fls,function(fl){
#   #fl <- fls[fl.i]
#   gamma_EH <- read.csv(paste0('results/test/',fl))
#   
#   return(gamma_EH)
# })
# 
# beta.GI.uncertain <- do.call('rbind',gammas.list)
# beta.GI.uncertain <- beta.GI.uncertain[!beta.GI.uncertain$group%in%c(1),]
# #beta.GI.uncertain <- beta.GI.uncertain[!beta.GI.uncertain$group%in%c(1,49),]
# 
# beta.GI.uncertain.g <- beta.GI.uncertain %>% group_by(pixel) %>% 
#   mutate(
#     Gmm_EH_mid=median(gamma_EH),
#     Gmm_EH_mn=median(gamma_EH),
#     Gmm_EH_l=quantile(gamma_EH,0.05),
#     Gmm_EH_h=quantile(gamma_EH,0.95),
#     #mn=mean(gamma_EH),
#     gmm_sd=sd(gamma_EH,na.rm=T),
#     df=n()-1) %>% 
#   ungroup() %>% 
#   dplyr::select(-group,-gamma_EH) %>% 
#   distinct()
# 
# beta.GI.uncertain.g$Gmm_EH_rg <- beta.GI.uncertain.g$Gmm_EH_h - beta.GI.uncertain.g$Gmm_EH_l
# 
# colnames(beta.GI.uncertain.g)[1] <- 'layer'
# 
# dat <- read.dbf('data/mapping/data_1d_land - 副本.dbf')
# dat <- left_join(dat,beta.GI.uncertain.g)
# dat$Gmm_EH_rg_r <- dat$Gmm_EH_rg/dat$Gmm_EH_mn
# 
# df <- 47#unique(dat$df)
# t_critical = qt((1 + 0.95) / 2, df)
# dat$conf_inter <- 2*(dat$gmm_sd/sqrt(df+1))*t_critical
# 
# dat <- dat %>% mutate(Gmm_GI_mn=Gmm_obs-Gmm_EH_mn,
#                       Gmm_GI_mid=Gmm_obs-Gmm_EH_mid,
#                Gmm_GI_l=Gmm_obs-Gmm_EH_l,
#                Gmm_GI_h=Gmm_obs-Gmm_EH_h)
# 
# #write.dbf(dat,'data/mapping/data_1d_land.dbf')
# 
# 
# ##
# 
# geo <- read_sf('data/metricGeo/activetype.dbf')
# colnames(geo)[1] <- 'active2'
# xy <- st_coordinates(geo)
# geo$layer <- cellFromXY(raster(),xy)
# geo <- st_drop_geometry(geo) %>% as.data.frame() %>% na.omit()
# geo$active2 <- factor(geo$active2,levels=c(0,1,2),labels=c('Inactive','M active', 'T active')) %>% as.character()
# dat <- left_join(dat,geo)
# 
# #
# # geo <- read_sf('results/geo-deltaG/geoActive.shp')
# # colnames(geo)[2] <- 'activetype2'
# # geo <- st_drop_geometry(geo) %>% as.data.frame() %>% na.omit()
# # #geo$activetype <- factor(geo$activetype,levels=c(0,1,2),labels=c('Inactive','Morphologicaly active', 'Tectonically active'))
# # dat <- left_join(dat,geo)
# 
# write.dbf(dat,'data/mapping/data_1d_land.dbf')
