library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(engression)
setwd('E:/luoa/gamma/')

set.seed(302)
# chech the value of t
t <- raster('data/clim/CHELSA_bio1_1981-2010_V.2.1.tif')
t
p <- raster('data/clim/CHELSA_bio12_1981-2010_V.2.1.tif')

load('data/sPlot/3474_55_Dataset/sPlotOpen.RData')
header.oa <- data.frame(PlotObservationID=header.oa$PlotObservationID,
                        bio1=extract(t,header.oa[,c('Longitude','Latitude')]) ,
                        swb=extract(p,header.oa[,c('Longitude','Latitude')]) ) 
dat <- DT2.oa %>% group_by(PlotObservationID) %>% mutate(alpha=length(unique(Species))) %>% 
  dplyr::select(PlotObservationID,alpha) %>% 
  ungroup() %>% distinct() %>%  left_join(header.oa) 

dat <- dat %>% na.omit()
X = dat[,3:4]
Y = dat[[2]]

## fit engression object
engr = engression(X,Y)


# scale to 10000 ----------------------------------------------------------
library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(engression)
setwd('E:/luoa/gamma/')

set.seed(302)
# chech the value of t
t <- raster('data/clim/CHELSA_bio1_1981-2010_V.2.1.tif')
t
p <- raster('data/clim/CHELSA_bio12_1981-2010_V.2.1.tif')

load('data/sPlot/3474_55_Dataset/sPlotOpen.RData')
header.oa <- data.frame(PlotObservationID=header.oa$PlotObservationID,area=header.oa$Releve_area,
                        t=extract(t,header.oa[,c('Longitude','Latitude')]) ,p=extract(p,header.oa[,c('Longitude','Latitude')]) ) 
dat <- DT2.oa %>% group_by(PlotObservationID) %>% mutate(alpha=length(unique(Species))) %>% dplyr::select(PlotObservationID,alpha) %>% 
  ungroup() %>% distinct() %>%  left_join(header.oa) 
dat <- dat %>% na.omit()

m <- lm(log(alpha)~log(area),data=dat)
z <- m$coefficients[2]
#alpha=dat$alpha
#area=dat$area
#m <- nls(alpha ~ c*area^z,data=dat,start=list(c=9,z=0.25),
#         control = nls.control(maxiter = 10000))
#m
dat$alpha_100 <- dat$alpha * (10000/dat$area)^z

X = dat[,c('t','p')]
#Y = dat[,'alpha_100']
Y = dat[['alpha_100']]
engr = engression(X,Y)
