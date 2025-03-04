library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere);library(parallel)
setwd('E:/luoa/gamma/')
b <- 0.01084035
#b <- 0.0328354
fls <- list.files(path = 'data/alpha/DEM100m_Scale/')
fls <- fls[str_detect(fls,'.tif')]
#flag.num <- which(fls%in%flag2)
fls.done  <- list.files(path = 'data/subpixels/100m_scale/') %>% str_replace('.rdata','.tif')
flag.num <- which(!fls%in%fls.done)

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
  fls <- list.files(path = 'data/alpha/DEM100m_Scale/')
  fls <- fls[str_detect(fls,'.tif')]
  
  deltaE2beta <- 1-exp(-b* (0:999) ) # 0 match 0+1
  NULL
})

#gammas.list <- parLapply(cl,1:length(fls),function(fl.i){
gammas.list <- parLapply(cl,flag.num,function(fl.i){
  #for (fl.i in 1:length(fls)) {
  fl <- fls[fl.i]
  
  fl.elev.path <- paste0('../elev/ASTGTM_100m/',fl)
  fl.alpha.path <- paste0('data/alpha/DEM100m_Scale/',fl)
  
  alpha.rst <- raster(fl.alpha.path)
  elev <- raster(fl.elev.path)
  #elev <- aggregate(elev,3,fun='mean')
  elev[] <- round(elev[])
  
  rst <- raster()
  rst[] <- 1:length(rst[])
  rst <- crop(rst,extent(alpha.rst)) 
  
  alpha <- cbind(rst[],alpha.rst[],elev[]) %>% as.data.frame()
  colnames(alpha) <- c('pixel','alpha','elev')
  alpha <- alpha %>% na.omit()
  if(dim(alpha)[1]==0) return(NULL)
  
  tmp <- alpha
  tmp$alpha <- tmp$alpha %>% round()
  #tmp$elev <- tmp$elev%/%5*5
  tmp$elev <- tmp$elev %>% round()
  
  tmp <- tmp %>%  group_by(elev,alpha) %>% mutate(n=n()) %>% 
    ungroup() %>% distinct()
  
  
  subpx.info <- lapply(1:nrow(tmp), function(i){
    #print(i)
    alpha_i <- tmp$alpha[i]
    elev_i <- tmp$elev[i]
    
    alpha_j <- tmp$alpha
    elev_j <- tmp$elev
    
    deltaElev <- abs(elev_i-elev_j)+1 #plus 1 #%>% round()
    deltaElev[deltaElev>1000] <- 1000
    beta <- deltaE2beta[deltaElev]
    #Pij.beta <- (1 + 1/(beta-2))
    Pij <- (1 + 1/(beta-2)) * ((alpha_i+alpha_j)/alpha_i)
    Pij_tmp <- (1 + 1/(beta-2)) * ((alpha_i+alpha_j)^2/alpha_i^2)
    
    # Pij <- Pij.alpha*Pij.beta
    Pij[Pij>1] <- 1
    Pij_tmp[Pij>1] <- 1
    # Pij <- sum(Pij*tmp$n)
    # alpha_weighted <- alpha_i/sum(Pij)
    #ki <- sum(Pij*tmp$n) - 1
    
    ki <- sum(Pij*tmp$n)
    ki_tmp <- sum(Pij_tmp*tmp$n)
    
    alpha_weighted <- alpha_i/sum(ki)
    data.frame(alpha_weighted=alpha_weighted,k_i=ki,k_i_tmp=ki_tmp,alpha_i=alpha_i,beta_EH_i=mean(beta),n=tmp$n[i])
    #alpha_weighted
  })
  subpx.info <- do.call('rbind',subpx.info)
  subpx.info$pixel <- rst[]
  save(subpx.info,file=paste0('data/subpixels/100m_scale/',str_replace(fl,'.tif','.rdata')))
  gamma_EH <- sum(subpx.info$alpha_weighted*subpx.info$n)
  
  tmp <- data.frame(pixel=rst[],
                    alpha_mean=weighted.mean(subpx.info$alpha_i,w=subpx.info$n),
                    alpha_mean_max=max(subpx.info$alpha_i),
                    gamma_EH=sum(gamma_EH))
  write.csv(tmp,paste0('results/tmp/',fl.i,'.csv'),row.names = F)
  return(tmp)
  #stopCluster(cl)
})
gammas <- do.call('rbind',gammas.list)
write.csv(gammas,'gamma/results/100m/gammas_100m_5mDEM_01.csv',row.names=F)
stopCluster(cl)
###########
test <- raster()
#flag <- which(test[]%in%gammas$pixel) 
test[gammas$pixel] <- gammas$gamma
#test[flag] <- gammas$gamma
#test[-flag] <- NA
#writeRaster(test,'data/gamma/raw_elev_exp.tif',overwrite=T)
plot(test)

sr <- raster('gamma/data/alpha/sr.tif')
sr <- crop(sr,extent(ext)) 
sr[is.na(test[])] <- NA

plot(sr)
plot(test)

plot(sr-test)
title('gamma_observed - gamma_predicted')

gammas$gamma_observe <- NA
gammas$gamma_observe <- sr[gammas$pixel] 