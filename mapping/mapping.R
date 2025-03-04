library(raster);library(dplyr)
setwd('E:/luoa/gamma/')

rst.rf <- raster('results/chi/relief.tif')
rst.rf[] <- rst.rf[]>quantile(rst.rf[],0.75,na.rm=T)

rst1 <- rst.chi <- raster('results/chi/maxdeltaChi.tif')

rst2 <- rst.chi
rst2[!is.na(rst.rf)&is.na(rst2)] <- 0
rst2[] <- rst2[]>quantile(rst2[],0.75,na.rm=T)
rst2[is.na(rst1)] <- NA

rst3 <- rst2*2+rst.rf
#plot(rst3,col=c('','','red'))
#plot(rst3,col=c('#F2F2F2','#00A600','#ECB176','red'))
#plot(rst3,col=c('#F2F2F2','#ECC094','#53A653','red'))
#rst4 <- rst3;rst4[is.na(rst1)] <- NA

par(mfcol=c(3,1))
plot(rst1)
title('delta Chi (max value in each pixel)')
plot(rst2)
title('top 10%, delta Chi')
plot(rst3,col=c('#F2F2F2','#ECC094','#53A653','red'))
title('geomorphically active')

writeRaster(rst3,'data/metric/tect/reclass_mor.tif',overwrite=T)
####

library(raster);library(dplyr)
setwd('D:/Works/gamma/')

rst.rf <- raster('results/chi/relief.tif')
rst.rf[] <- rst.rf[]>quantile(rst.rf[],0.6,na.rm=T)

rst1 <- rst.chi <- raster('results/chi/maxdeltaChi.tif')

rst2 <- rst.chi
rst2[!is.na(rst.rf)&is.na(rst2)] <- 0
rst2[] <- rst2[]>quantile(rst2[],0.6,na.rm=T)
rst2[is.na(rst1)] <- NA

rst3 <- rst2*2+rst.rf
#plot(rst3,col=c('','','red'))
#plot(rst3,col=c('#F2F2F2','#00A600','#ECB176','red'))
#plot(rst3,col=c('#F2F2F2','#ECC094','#53A653','red'))
#rst4 <- rst3;rst4[is.na(rst1)] <- NA

par(mfcol=c(3,1))
plot(rst1)
title('delta Chi (max value in each pixel)')
plot(rst2)
title('top 40%, delta Chi')
plot(rst3,col=c('#F2F2F2','#ECC094','#53A653','red'))
title('geomorphically active')

####

rst.rf <- raster('results/chi/relief.tif')
rst.rf[] <- rst.rf[]>quantile(rst.rf[],0.6,na.rm=T)

rst1 <- rst.chi <- raster('results/chi/chi_MAXvar_20km_closebasin.tif')

rst2 <- rst.chi
rst2[!is.na(rst.rf)&is.na(rst2)] <- 0
rst2[] <- rst2[]>quantile(rst2[],0.8,na.rm=T)
rst2[is.na(rst1)] <- NA

rst3 <- rst2*2+rst.rf
#plot(rst3,col=c('','','red'))
#plot(rst3,col=c('#F2F2F2','#00A600','#ECB176','red'))
#plot(rst3,col=c('#F2F2F2','#ECC094','#53A653','red'))
#rst4 <- rst3;rst4[is.na(rst1)] <- NA

par(mfcol=c(3,1))
plot(rst1)
title('variance of Chi (max value in each pixel)')
plot(rst2)
title('top 20%, variance of Chi')
plot(rst3,col=c('#F2F2F2','#ECC094','#53A653','red'))
title('geomorphically active')
