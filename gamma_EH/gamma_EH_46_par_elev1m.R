library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere);library(parallel)
setwd('E:/luoa/gamma/')
b <- 0.01084035
#b <- 0.0328354
# fls <- list.files(path = 'data/alpha/DEM100m_Scale/')
# fls <- fls[str_detect(fls,'.tif')]
#flag.num <- which(fls%in%flag2)
fls  <- list.files(path = 'data/subpixels/100m_scale/') %>% str_replace('.rdata','.tif')
flag.num <- 1:length(fls)

deltaE2beta <- 1-exp(-b* (0:999) ) # 0 match 0+1

fl.i <- 1
#gammas <- data.frame()

cl <- makeCluster(25)
clusterEvalQ(cl,{
  library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
  library(stringr);library(geosphere)
  library(parallel)
  setwd('E:/luoa/gamma/')
  b <- 0.01084035
  fls  <- list.files(path = 'data/subpixels/100m_scale/') %>% str_replace('.rdata','.tif')
  gamma_obs_rst <- raster('data/gamma/gamma/gamma.tif')
  deltaE2beta <- 1-exp(-b* (0:999) ) # 0 match 0+1
  NULL
})

gammas.list <- parLapply(cl,flag.num,function(fl.i){
  fl <- fls[fl.i]
  load(paste0('data/subpixels/100m_scale/',str_replace(fl,'.tif','.rdata')))
  #gamma_EH <- sum(subpx.info$alpha_weighted*subpx.info$n)
  gamma_obs <- gamma_obs_rst[unique(subpx.info$pixel)]
  if(is.na(gamma_obs)) return(data.frame(beta.GI=NA,gamma_tol=NA,pixel=unique(subpx.info$pixel)))
  
  beta.GI.list <- seq(-3,3,0.0001)
  gamma_tol <- lapply(beta.GI.list, function(beta.GI){
    k_tol <- 1-(1+1/(beta.GI-2))*(2-subpx.info$k_i_tmp)
    return(sum(subpx.info$alpha_i/k_tol*subpx.info$n))
  }) %>% unlist()
  beta.GI <- beta.GI.list[which.min(abs(gamma_tol-gamma_obs))]
  gamma_tol <- gamma_tol[which.min(abs(gamma_tol-gamma_obs))]
  return(data.frame(beta.GI=beta.GI,gamma_tol=gamma_tol,pixel=unique(subpx.info$pixel)))
  #stopCluster(cl)
})
beta.GI <- do.call('rbind',gammas.list)
#dir.create('results/100m/beta_GI')
write.csv(beta.GI,'results/100m/beta_GI/beta_GI_20241111.csv',row.names = F) 

beta.GI <- do.call('rbind',gammas.list)
rst <- raster()
rst[beta.GI$pixel] <- beta.GI$beta.GI
plot(rst)
dat <- data.frame(layer=1:64800,beta_GI=rst[])

beta.GI$beta.GI[beta.GI$beta.GI>1] <- 1
beta.GI$beta.GI[beta.GI$beta.GI<0] <- 0
rst <- raster()
rst[beta.GI$pixel] <- beta.GI$beta.GI
plot(rst)
dat$beta_GI_lim <- rst[]
dat <- na.omit(dat)

# plot(rst,col=topo.colors(100))
# rst[beta.GI$pixel] <- beta.GI$gamma_tol
# plot(rst-gamma_obs_rst[]) 

shp <- read_sf('data/mapping/data_1d_land.shp')
shp <- shp[,'layer']
shp <- left_join(shp,dat)
write_sf(shp,'data/mapping/data_test.shp')
