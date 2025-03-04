library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere)
setwd('E:/luoa/gamma/')
rm(list= ls()[!ls()%in%'engr'])

bio1.range <- seq(-33.6,34.9,by=0.1)
bio12.range <- seq(0,6554,by=1)
bio1.bio12 <- expand.grid(bio1.range,bio12.range)

load(paste0('code/alpha_model/model_table/bio1_10times_bio12_alpha_999_',1,'.rdata'))
bio1.bio12.outputs <- bio1.bio12.output

for(i in 2:10){
  load(paste0('code/alpha_model/model_table/bio1_10times_bio12_alpha_999_',i,'.rdata'))
  bio1.bio12.outputs <- cbind(bio1.bio12.outputs,bio1.bio12.output$alpha.max)
}

bio1.bio12.pred <- apply(bio1.bio12.outputs[,-(1:2)],1,median)
bio1.bio12.pred <- data.frame(bio1=round(bio1.bio12.outputs$bio1*10),bio12=round(bio1.bio12.outputs$bio12),alpha.max=bio1.bio12.pred)
save(bio1.bio12.pred,file='code/alpha_model/bio1_10times_bio12_alpha.rdata')

r <- raster(xmn=min(bio1.range)*10, xmx=max(bio1.range)*10+1, ymn=min(bio12.range), ymx=max(bio12.range), resolution=c(1, 1))  
r[cellFromXY(r,bio1.bio12.pred[,1:2])] <- bio1.bio12.pred$alpha.max

w <- focalWeight(r, 1, "Gauss")
r_smooth <- focal(r, w=w)
plot(r_smooth,asp=diff(range(bio1.range))*10/diff(range(bio12.range)))
writeRaster(r_smooth,file='code/alpha_model/bio1_10times_bio12_alpha.tif')

###################################

library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere)
setwd('E:/luoa/gamma/')
rm(list= ls()[!ls()%in%'engr'])

bio1.range <- seq(-33.6,34.9,by=0.1)
bio12.range <- seq(0,6554,by=1)
bio1.bio12 <- expand.grid(bio1.range,bio12.range)

load(paste0('code/alpha_model/model_table_scale_30/bio1_10times_bio12_alpha_999_',1,'.rdata'))
bio1.bio12.outputs <- bio1.bio12.output

for(i in 2:10){
  load(paste0('code/alpha_model/model_table_scale_30/bio1_10times_bio12_alpha_999_',i,'.rdata'))
  bio1.bio12.outputs <- cbind(bio1.bio12.outputs,bio1.bio12.output$alpha.max)
}

bio1.bio12.pred <- apply(bio1.bio12.outputs[,-(1:2)],1,median)
bio1.bio12.pred <- data.frame(bio1=round(bio1.bio12.outputs$bio1*10),bio12=round(bio1.bio12.outputs$bio12),alpha.max=bio1.bio12.pred)
save(bio1.bio12.pred,file='code/alpha_model/bio1_10times_bio12_alpha30.rdata')

r <- raster(xmn=min(bio1.range)*10, xmx=max(bio1.range)*10+1, ymn=min(bio12.range), ymx=max(bio12.range), resolution=c(1, 1))  
r[cellFromXY(r,bio1.bio12.pred[,1:2])] <- bio1.bio12.pred$alpha.max

w <- focalWeight(r, 1, "Gauss")
r_smooth <- focal(r, w=w)
plot(r_smooth)
writeRaster(r_smooth,file='code/alpha_model/bio1_10times_bio12_alpha_scale30.tif')
