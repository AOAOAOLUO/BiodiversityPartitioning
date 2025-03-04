library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr)
setwd('D:/work/Gamma/')

# DATA1 MIREN-------------------------------------------------------------------
dat <- data.frame()

fls <- list.files('data/transects/ELEV/')[1:28]
elevs <- list()
for (fl in fls) {
  tmp <- raster(paste0('data/transects/ELEV/',fl,'/',fl,'_dem.tif')) 
  elevs <- c(elevs,list(tmp))
}
names(elevs) <- fls

cums <- read.csv('data/transects/data1_MIREN/MIREN_plant_records_data2007-2015.lat.long.csv')
lat <- (cums$lat%/%1)
lat <- paste0(ifelse(lat>=0,'N','S'),sprintf("%02d", abs(lat))) 
long <- (cums$long%/%1)
long <- paste0(ifelse(long>=0,'E','W'),sprintf("%03d", abs(long))) 
cums$fl <- paste0('ASTGTMV003_',lat,long)
cums$elev <- NA

for (i in 1:nrow(cums)) {
  print(i)
  elev <- elevs[[cums$fl[i]]]
  cums$elev[i] <- raster::extract(elev,cums[i,c('long','lat')] %>% as.matrix())
}
#write.csv(cums,'data1_MIREN/withelev.csv',row.names = T)
cums.all <- cums
write.csv(cums.all,'data/transects/data1_MIREN/cleaned_data1_MIREN.csv',row.names = F)

dat <- data.frame()
for (i in na.omit(unique(cums.all$road))) {
  cums <- cums.all[cums.all$road%in%i,]
  #cums <- cums[cums$Status%in%'Native',]
  cums$plot <- cums$plot_id %>% str_remove('\\.[0-9]$')
  #cums$plot <- cums$plot_id#paste0(cums$site,'_',cums$plot_id)
  cums.m <- dcast(cums[,c('plot','Accepted.Name.MIREN')],plot~Accepted.Name.MIREN,
                  value.var = 'Accepted.Name.MIREN',fun.aggregate = length)
  row.names(cums.m) <- cums.m[,1]
  cums.m <- cums.m[,-1]
  cums.m <- as.data.frame(cums.m)
  cums.m <- ifelse(cums.m>=1,1,0)
  
  cums.m <- cums.m[rowSums(cums.m)>1,] 
  if(nrow(cums.m)<2) next
  
  cums.d.m <- cums[,c('plot','elev')] %>% distinct()
  colnames(cums.d.m) <- c('plot','elev')
  
  tmp.beta <- beta.pair(cums.m, index.family = "jac")
  tmp.beta <- tmp.beta$beta.jac
  tmp.beta <- as.matrix(tmp.beta)
  tmp.beta <- melt(tmp.beta)[lower.tri(tmp.beta),]
  colnames(tmp.beta) <- c('plot1','plot2','beta')
  
  tmp.beta <- left_join(tmp.beta,cums.d.m,by=join_by(plot1==plot)) %>% rename(elev1=elev) %>% 
    left_join(cums.d.m,by=join_by(plot2==plot)) %>% rename(elev2=elev)
  tmp.beta$deltaElev <- abs(tmp.beta$elev1-tmp.beta$elev2)
  tmp.beta$site <- i
  print(nrow(tmp.beta))
  dat <- rbind(dat,tmp.beta[,c('deltaElev','beta','site')])
}
plot(log(dat$deltaElev),dat$beta)
write.csv(dat,'data/transects/data1_MIREN/cleaned_deltaElev.csv',row.names = F)

# DATA2 -------------------------------------------------------------------
sp <- read.csv('data/transects/data2_doi_10_5061_dryad_rbnzs7hdt__v20220815/bunya_species_list.csv')
sp <- sp[,1:3]
cums <- read.csv('data/transects/data2_doi_10_5061_dryad_rbnzs7hdt__v20220815/alison_low_east_and_low_west_plots.csv')
cums <- cums %>% left_join(sp)
cums <- cums[,c('location','plot','current_taxon')]

tmp <- read.csv('data/transects/data2_doi_10_5061_dryad_rbnzs7hdt__v20220815/butler_high_east.csv')
tmp <- tmp[,c('abbrev','x','y')]
tmp$plot <-  0
flag <- (tmp$x<=25)&(tmp$y<=25) ; tmp$plot[flag] <- 1
flag <- (tmp$x<=25)&(tmp$y>=75) ; tmp$plot[flag] <- 2
flag <- (tmp$x>=75)&(tmp$y<=25) ; tmp$plot[flag] <- 3
flag <- (tmp$x>=75)&(tmp$y>=75) ; tmp$plot[flag] <- 4
tmp <- tmp[tmp$plot>0,];tmp <- left_join(tmp,sp)
tmp$location <- 'high'
cums <- rbind(cums,tmp[,c('location','plot','current_taxon')]) 

tmp <- read.csv('data/transects/data2_doi_10_5061_dryad_rbnzs7hdt__v20220815/butler_mid_east.csv')
tmp <- tmp[,c('abbrev','x','y')]
tmp$plot <-  0
flag <- (tmp$x<=25)&(tmp$y<=25) ; tmp$plot[flag] <- 1
flag <- (tmp$x<=25)&(tmp$y>=75) ; tmp$plot[flag] <- 2
flag <- (tmp$x>=75)&(tmp$y<=25) ; tmp$plot[flag] <- 3
flag <- (tmp$x>=75)&(tmp$y>=75) ; tmp$plot[flag] <- 4
tmp <- tmp[tmp$plot>0,];tmp <- left_join(tmp,sp)
tmp$location <- 'mid_east'
cums <- rbind(cums,tmp[,c('location','plot','current_taxon')]) 

tmp <- read.csv('data/transects/data2_doi_10_5061_dryad_rbnzs7hdt__v20220815/butler_mid_west.csv')
tmp <- tmp[,c('abbrev','x','y')]
tmp$plot <-  0
flag <- (tmp$x<=25)&(tmp$y<=25) ; tmp$plot[flag] <- 1
flag <- (tmp$x<=25)&(tmp$y>=75) ; tmp$plot[flag] <- 2
flag <- (tmp$x>=75)&(tmp$y<=25) ; tmp$plot[flag] <- 3
flag <- (tmp$x>=75)&(tmp$y>=75) ; tmp$plot[flag] <- 4
tmp <- tmp[tmp$plot>0,];tmp <- left_join(tmp,sp)
tmp$location <- 'mid_west'
cums <- rbind(cums,tmp[,c('location','plot','current_taxon')]) 

cums.d.m <- matrix(c('high','both',1050,'mid_east','east',950,'mid_west','west',950,
                     'low_east','east',625,'low_west','west',595),ncol=3,byrow = T) %>% as.data.frame()
colnames(cums.d.m) <- c('location','site','elev')
cums <- cums %>% left_join(cums.d.m);cums$elev <- as.numeric(cums$elev)
cums <- cums[!is.na(cums$current_taxon),]

cums$plot <- paste0(cums$location,'_',cums$plot)
cums.d.m <- cums[,c('plot','elev')] %>% distinct();cums.d.m$elev <- as.numeric(cums.d.m$elev)

cums.m <- dcast(cums,plot~current_taxon)
row.names(cums.m) <- cums.m[,1]
cums.m <- cums.m[,-1]
cums.m <- as.data.frame(cums.m)
cums.m <- ifelse(cums.m>1,1,0)
cums.m.all <- cums.m
# cums.d.m <- cums[,c('plot','ELEV')] %>% distinct()
# colnames(cums.d.m) <- c('plot','elev')

cums.m <- cums.m.all[!stringr::str_detect(row.names(cums.m.all),'east'),]
tmp.beta <- beta.pair(cums.m, index.family = "jac")
tmp.beta <- tmp.beta$beta.jac
tmp.beta <- as.matrix(tmp.beta)
tmp.beta <- melt(tmp.beta)[lower.tri(tmp.beta),]
colnames(tmp.beta) <- c('plot1','plot2','beta')
tmp.beta <- left_join(tmp.beta,cums.d.m,by=join_by(plot1==plot)) %>% rename(elev1=elev) %>% 
  left_join(cums.d.m,by=join_by(plot2==plot)) %>% rename(elev2=elev)
tmp.beta$deltaElev <- abs(tmp.beta$elev1-tmp.beta$elev2)
tmp.beta <- tmp.beta[tmp.beta$deltaElev>0,]
tmp.beta$site <- 'Bunya_west'
dat <- rbind(dat,tmp.beta[,c('deltaElev','beta','site')])

cums.m <- cums.m.all[!stringr::str_detect(row.names(cums.m.all),'west'),]
tmp.beta <- beta.pair(cums.m, index.family = "jac")
tmp.beta <- tmp.beta$beta.jac
tmp.beta <- as.matrix(tmp.beta)
tmp.beta <- melt(tmp.beta)[lower.tri(tmp.beta),]
colnames(tmp.beta) <- c('plot1','plot2','beta')
tmp.beta <- left_join(tmp.beta,cums.d.m,by=join_by(plot1==plot)) %>% rename(elev1=elev) %>% 
  left_join(cums.d.m,by=join_by(plot2==plot)) %>% rename(elev2=elev)
tmp.beta$deltaElev <- abs(tmp.beta$elev1-tmp.beta$elev2)
tmp.beta <- tmp.beta[tmp.beta$deltaElev>0,]
tmp.beta$site <- 'Bunya_east'
dat <- rbind(dat,tmp.beta[,c('deltaElev','beta','site')])

# DATA3 -------------------------------------------------------------------
cums <- read.csv('data/transects/data3_doi_10_5061_dryad_8gr16__v20181004/Nabe-Nielsen etal 2017 Young Sund spp pres in plotgrps.csv')
row.names(cums) <- paste0(cums$site,'_',cums$plot.grp,'_',cums$alt)
for (a in c('a','b','c')) {
  for (b in unique(cums$site)) {
    cums.m <- cums[ (cums$site%in%b)&(cums$plot.grp%in%a) ,]
    
    cums.d.m <- data.frame(plot=row.names(cums.m),elev=cums.m[,'alt']) %>% distinct()
    colnames(cums.d.m) <- c('plot','elev')
    
    cums.m <- cums.m[,-(1:3)]
    cums.m <- as.data.frame(cums.m)
    cums.m <- ifelse(cums.m>1,1,0)
    
    tmp.beta <- beta.pair(cums.m, index.family = "jac")
    tmp.beta <- tmp.beta$beta.jac
    tmp.beta <- as.matrix(tmp.beta)
    tmp.beta <- melt(tmp.beta)[lower.tri(tmp.beta),]
    colnames(tmp.beta) <- c('plot1','plot2','beta')
    
    tmp.beta <- left_join(tmp.beta,cums.d.m,by=join_by(plot1==plot)) %>% rename(elev1=elev) %>% 
      left_join(cums.d.m,by=join_by(plot2==plot)) %>% rename(elev2=elev)
    tmp.beta$deltaElev <- abs(tmp.beta$elev1-tmp.beta$elev2)
    tmp.beta$site <- paste0(b,'_',a)
    dat <- rbind(dat,tmp.beta[,c('deltaElev','beta','site')])
  } 
}

# DATA4 -------------------------------------------------------------------
cums <- read.csv('data/transects/data4/PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv')
cums <- cums[cums$treatment=='C',]

cums$plot <- paste0(cums$site,'_',cums$plot_id)
cums.m <- dcast(cums,plot~taxon)
row.names(cums.m) <- cums.m[,1]
cums.m <- cums.m[,-1]
cums.m <- as.data.frame(cums.m)
cums.m <- ifelse(cums.m>1,1,0)

cums.d.m <- cums[,c('plot','elevation')] %>% distinct()
colnames(cums.d.m) <- c('plot','elev')

tmp.beta <- beta.pair(cums.m, index.family = "jac")
tmp.beta <- tmp.beta$beta.jac
tmp.beta <- as.matrix(tmp.beta)
tmp.beta <- melt(tmp.beta)[lower.tri(tmp.beta),]
colnames(tmp.beta) <- c('plot1','plot2','beta')

tmp.beta <- left_join(tmp.beta,cums.d.m,by=join_by(plot1==plot)) %>% rename(elev1=elev) %>% 
  left_join(cums.d.m,by=join_by(plot2==plot)) %>% rename(elev2=elev)
tmp.beta$deltaElev <- abs(tmp.beta$elev1-tmp.beta$elev2)
tmp.beta$site <- 'PFTC'
dat <- rbind(dat,tmp.beta[,c('deltaElev','beta','site')])

write.csv(dat,'data/transects/data1_MIREN/cleaned_deltaElev_data2-3.csv',row.names = F)
