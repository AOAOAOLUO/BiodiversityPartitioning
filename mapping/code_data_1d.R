setwd('E:/luoa/gamma/')
sapply(c('dplyr','raster','ggplot2','stringr','ggtext','sf'), require,character.only=T)
dat <- read.csv('data/alpha/alpha_1d_100mscale.csv')
colnames(dat) <- c('cell','Alpha_max_max','Alpha_max_mean','SumP')

rstland <- raster('../data/map/Map_land/landboundary.tif')
gamma <- raster('data/gamma/gamma/gamma.tif')
gamma[is.na(rstland[])] <- NA
gamma.obser <- gamma

gamma <- read.csv('results/100m/gammas_100m_scale_1mDEM_01.csv')
rst.gamma <- raster()
rst.gamma[gamma$pixel] <- gamma$gamma
rst.gamma[is.na(rstland[])] <- NA

tmp <- data.frame(cell=1:64800,Gmm_obs=gamma.obser[],Gmm_EH=rst.gamma[])
dat <- left_join(dat,tmp)# %>% na.omit()
dat$Gmm_GI <- dat$Gmm_obs - dat$Gmm_EH
dat$Gmm_GI_0 <- ifelse(dat$Gmm_GI>0,dat$Gmm_GI,0)

dat$K <- dat$Gmm_GI_0/dat$SumP;dat$K[dat$SumP==0] <- NA
dat$K[dat$K<2] <- 2
dat$beta_GI <-  (dat$K-2)/(dat$K-1)
dat$beta_GI_2 <- dat$Gmm_GI_0/dat$Alpha_max_mean 

rst[dat$cell] <- dat$K;plot(rst)
rst[dat$cell] <- dat$beta_GI;plot(rst)
rst[dat$cell] <- dat$beta_GI_2;plot(rst)
##################
# rst.shp <- raster() %>% rasterToPolygons() %>% st_as_sf()
# rst.shp$layer <- 1:64800
# 
# gamma.obser[is.na(gamma.obser[])&(1:64800%in%dat$pixel)] <- 0
# gamma.obser[is.na(rstland[])] <- NA
# rst.shp$gamma_obser <- gamma.obser[]
# rst.shp$gamma_pred <- rst.gamma[]
# 
# gamma.obser
# gamma.a <- gamma.obser - rst.gamma
# rst.shp$gamma_a <- gamma.a[]
# 
# alpha <- load('E:/luoa/gamma/code/elev_100m/model_table_scale/bio1_10times_bio12_alpha_999_1.rdata')
# write_sf(rst.shp,'data/mapping/data_1d.shp')

####################
active <- read_sf('results/geo-deltaG/geoActive.shp')
active$active <- str_replace_all(active$active,'Geomorphically active','Geomorphic active')
active <- active %>% st_drop_geometry()
active <- as.data.frame(active) %>% na.omit()

colnames(dat)[1] <- 'layer'
dat <- left_join(dat,active)

rst.1d <- read_sf('data/mapping/data_1d_land.shp')
rst.1d <- rst.1d[,1]

rst.1d <- left_join(rst.1d,dat)

rst.1d$Top_Gmm_a <- NA
rst.1d$Top_Gmm_a[rst.1d$Gmm_GI>=quantile(rst.1d$Gmm_GI,0.8,na.rm=T)] <- T
rst.1d$Top_Gmm_a[rst.1d$Gmm_GI<quantile(rst.1d$Gmm_GI,0.8,na.rm=T)] <- F
write_sf(rst.1d,'data/mapping/data_1d_land.shp')
