library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere);library(parallel)
setwd('E:/luoa/gamma/')
fls  <- list.files(path = 'data/subpixels/100m_scale/')
flag.num <- which(str_detect(fls,'.rdata'))

fl.i <- 1

cl <- makeCluster(25)
clusterEvalQ(cl,{
  library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
  library(stringr);library(geosphere)
  library(parallel)
  setwd('E:/luoa/gamma/')
  fls  <- list.files(path = 'data/subpixels/100m_scale/')
  flag.num <- which(str_detect(fls,'.rdata'))
  NULL
})

gammas.list <- parLapply(cl,flag.num,function(fl.i){
  fl <- fls[fl.i]
  load(paste0('data/subpixels/100m_scale/',fl))
  
  gamma_EH <- sum(subpx.info$alpha_weighted*subpx.info$n)
  
  tmp <- data.frame(pixel=unique(subpx.info$pixel),
                    a_mn=weighted.mean(subpx.info$alpha_i[!is.na(subpx.info$alpha_i)],
                                             w=subpx.info$n[!is.na(subpx.info$alpha_i)]),
                    a_mn=max(subpx.info$alpha_i),
                    gmm_EH=sum(gamma_EH))
  #write.csv(tmp,paste0('results/tmp/',fl.i,'.csv'),row.names = F)
  return(tmp)
})

gammas <- do.call('rbind',gammas.list)
write.csv(gammas,'results/100m/gammas_100m_1mDEM_pixel.csv',row.names=F)
stopCluster(cl)

###########

gamma_EH <- raster()
gamma_EH[gammas$pixel] <- gammas$gamma
plot(gamma_EH)

gamma_obs <- raster('data/gamma/gamma/gamma.tif')
#gamma_obs[is.na(gamma_EH[])] <- NA

gamma_GI <- gamma_obs - gamma_EH
plot(gamma_GI)
