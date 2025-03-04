library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(engression)
setwd('E:/luoa/gamma/')

set.seed(302)
# chech the value of t
t <- raster('data/clim/CHELSA_bio1_1981-2010_V.2.1.tif')
t
p <- raster('data/clim/CHELSA_bio12_1981-2010_V.2.1.tif')

load('data/sPlot/3474_55_Dataset/sPlotOpen.RData')
header.oa <- data.frame(PlotObservationID=header.oa$PlotObservationID,bio1=extract(t,header.oa[,c('Longitude','Latitude')]) ,swb=extract(p,header.oa[,c('Longitude','Latitude')]) ) 
dat <- DT2.oa %>% group_by(PlotObservationID) %>% mutate(alpha=length(unique(Species))) %>% dplyr::select(PlotObservationID,alpha) %>% 
  ungroup() %>% distinct() %>%  left_join(header.oa) 

dat <- dat %>% na.omit()
X = dat[,3:4]
Y = dat[[2]]

## fit engression object
engr = engression(X,Y)

####
library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere)

rm(list= ls()[!ls()%in%'engr'])

#bio1.g <- raster('data/clim/CHELSA_bio1_1981-2010_V.2.1.tif')
#bio12.g <- raster('data/clim/CHELSA_bio12_1981-2010_V.2.1.tif')

bio1.range <- seq(-33.6,34.9,by=0.1)
bio12.range <- seq(0,6554,by=1)

bio1.bio12 <- expand.grid(bio1.range,bio12.range)
bio1.bio12.pred <- predict(engr,bio1.bio12,type="quantiles",quantitle,quantile=0.99,nsample=999)

bio1.bio12.output <- data.frame(bio1=bio1.bio12$Var1,bio12=bio1.bio12$Var2,alpha.max=bio1.bio12.pred)
save(bio1.bio12.output,file='code/alpha_model/model_table/bio1_10times_bio12_alpha_999_1.rdata')

for (i in 2:10) {
  bio1.bio12 <- expand.grid(bio1.range,bio12.range)
  bio1.bio12.pred <- predict(engr,bio1.bio12,type="quantiles",quantitle,quantile=0.99,nsample=999)
  
  bio1.bio12.output <- data.frame(bio1=bio1.bio12$Var1,bio12=bio1.bio12$Var2,alpha.max=bio1.bio12.pred)
  save(bio1.bio12.output,file=paste0('code/alpha_model/model_table/bio1_10times_bio12_alpha_999_',i,'.rdata'))
}
