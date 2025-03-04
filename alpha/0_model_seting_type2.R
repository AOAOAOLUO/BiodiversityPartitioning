library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)

# Setting -----------------------------------------------------------------
# predictor: bio1, bio12
# scaling to 100m


library(stringr);library(engression)
setwd('E:/luoa/gamma/')

set.seed(302)
# chech the value of t
t <- raster('data/clim/CHELSA_bio1_1981-2010_V.2.1.tif')
m <- raster('data/clim/CHELSA_bio12_1981-2010_V.2.1.tif')

load('data/sPlot/3474_55_Dataset/sPlotOpen.RData')
header.oa <- data.frame(PlotObservationID=header.oa$PlotObservationID,
                        area=header.oa$Releve_area,
                        t=extract(t,header.oa[,c('Longitude','Latitude')]) ,
                        m=extract(m,header.oa[,c('Longitude','Latitude')]) ) 
dat <- DT2.oa %>% group_by(PlotObservationID) %>% mutate(alpha=length(unique(Species))) %>% dplyr::select(PlotObservationID,alpha) %>% 
  ungroup() %>% distinct() %>%  left_join(header.oa) 
dat <- dat %>% na.omit()

m <- lm(log(alpha)~log(area),data=dat)
z <- m$coefficients[2]

dat$alpha_100 <- dat$alpha * (10000/dat$area)^z

X = dat[,c('t','m')]
Y = dat[['alpha_100']]
engr = engression(X,Y)


# predict quantiles -------------------------------------------------------

Yhatquant = predict(engr,X,type="quantiles",
                    quantitle,quantile=c(0.01,seq(0.1,0.9,0.1),0.99),nsample=999)
ord = order(Y)
matplot(Y[ord], Yhatquant[ord,], type="l", col=2,lty=1,xlab="observation", ylab="prediction")
points(Y[ord],Yhatquant[ord],pch=20,cex=0.5)
dev.off()

# predict -----------------------------------------------------------------

####
library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere)

rm(list= ls()[!ls()%in%'engr'])

t.range <- seq(-33.6,34.9,by=0.1)
m.range <- seq(0,6554,by=1)

t.m <- expand.grid(t.range,m.range)
t.m.pred <- predict(engr,t.m,type="quantiles",quantitle,quantile=0.99,nsample=999)

t.m.output <- data.frame(bio1=t.m$Var1,bio12=t.m$Var2,alpha.max=t.m.pred)
save(t.m.output,file='code/alpha_model/table_pred/type2/bio1_10times_bio12_alpha_999_1.rdata')

for (i in 2:10) {
  t.m <- expand.grid(t.range,m.range)
  t.m.pred <- predict(engr,t.m,type="quantiles",quantitle,quantile=0.99,nsample=999)
  
  t.m.output <- data.frame(bio1=t.m$Var1,bio12=t.m$Var2,alpha.max=t.m.pred)
  save(t.m.output,file=paste0('code/alpha_model/table_pred/type2/bio1_10times_bio12_alpha_999_',i,'.rdata'))
}

