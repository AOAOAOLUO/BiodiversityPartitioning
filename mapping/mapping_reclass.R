library(raster);library(dplyr);library(sf)
setwd('E:/luoa/gamma/')

dat.tec <- read.csv('data/metric/tect/Tectonic_style (1).csv')

dat.mor <- raster('data/metric/tect/reclass_mor.tif')
dat.mor <- data.frame(ID=1:64800,reclass_mor=dat.mor[])
dat.mor$reclass_mor <- factor(dat.mor$reclass_mor) 
levels(dat.mor$reclass_mor) <- c('Inactve','Relief','Delta Chi','Geomorphically active')

dat <- dat.tec
dat$tectonicstring[(dat$tectonicstring%in%'Inactive')&(dat.mor$reclass_mor%in%'Geomorphically active')] <- 'Geomorphically active'
dat$tectonicstring <- factor(dat$tectonicstring)


rst <- st_as_sf(rasterToPolygons(raster()))
rst$active <- dat$tectonicstring
rst$layer <- 1:64800
write_sf(rst,'results/geo-deltaG/geoActive.shp')

#rst <- left_join(rst,p2.geo.active)
