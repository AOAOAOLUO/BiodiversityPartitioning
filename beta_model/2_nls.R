library(sf);library(betapart);library(dplyr);library(reshape2);library(raster)
library(stringr);library(geosphere);library(ggplot2)
setwd('D:/work/Gamma/')

dat <- read.csv('data/transects/beta_deltaE.csv')
# dat <- dat[dat$deltaElev>0,]
# dat <- dat[dat$beta<1,] %>% na.omit()
# dat <- dat[dat$site%in%unique(dat$site),]
b.s <- data.frame()
set.seed(1)
for(i in unique(dat$site)){
  #print(i)
  flag <- dat$site%in%i
  dat.tmp <- dat[flag,]
  dat.tmp <- dat.tmp %>% na.omit()
  dat.tmp <- dat.tmp[!is.nan(dat.tmp$beta),]# %>% na.rm=T
  
  beta <- dat.tmp$beta;DeltaE <- dat.tmp$deltaElev
  
  model <- nls(beta ~ 1 - exp(-bpar*DeltaE),start=list(bpar=0.002),
               control = nls.control(maxiter = 10000))
  m.s <- summary(model)
  
  b.s <- rbind(b.s, data.frame(b=coef(model)[1],p=m.s$coefficients[4]))
}
#b <- mean(b.s)
save(b.s,file='data/beta/beta_all_not_clean.rdata')

#
b.s <- c()
set.seed(1)
for(i in unique(dat$site)){
  #print(i)
  flag <- dat$site%in%i
  dat.tmp <- dat[flag,]
  dat.tmp <- dat.tmp %>% na.omit()
  dat.tmp <- dat.tmp[!is.nan(dat.tmp$beta),]# %>% na.rm=T
  
  beta <- dat.tmp$beta;DeltaE <- dat.tmp$deltaElev
  
  model <- nls(beta ~ 1 - exp(-bpar*DeltaE),start=list(bpar=0.002),
               control = nls.control(maxiter = 10000))
  m.s <- summary(model)
  
  #cat(m.s$coefficients[4],'   ',coef(model)[1],'/n')
  if(m.s$coefficients[4]<0.05) b.s <- c(b.s, coef(model)[1])
}
b <- mean(b.s)
save(b.s,file='data/beta/beta_all.rdata')
hist(log(b.s),main='',xlab='log(b)')
#lines(x=log(rep(b,37)),y=0:36,col='red',lty=2,lwd=3)
jpeg('outputs/sup/beta/bHist.jpg',height=300*4,width=300*5,res=300)
hist(log(b.s),main='',xlab='log(b)')
dev.off()

m.s.s <- lapply(unique(dat$site), function(i){
  #print(i)
  flag <- dat$site%in%i
  dat.tmp <- dat[flag,]
  dat.tmp <- dat.tmp %>% na.omit()
  dat.tmp <- dat.tmp[!is.nan(dat.tmp$beta),]# %>% na.rm=T
  
  beta <- dat.tmp$beta;DeltaE <- dat.tmp$deltaElev
  
  m <- nls(beta ~ 1 - exp(-bpar*DeltaE),start=list(bpar=0.002),
               control = nls.control(maxiter = 10000))
  m.s <- summary(m)
  
  # plot(dat.tmp$deltaElev,dat.tmp$beta)
  # line()
  
  rss <- sum(m.s$residuals^2)
  tss <- sum((dat.tmp$beta - mean(dat.tmp$beta))^2)

  
  m.s.table <- m.s$coefficients %>% round(4) %>% as.data.frame()
  colnames(m.s.table) <- c('b','SE','t','p') 
  m.s.table$AIC <- AIC(m) %>% round(1)
  m.s.table$Transect <- i
  m.s.table$df <-  m.s$df[2]
  return(m.s.table)
})
m.s.s <- do.call(rbind,m.s.s)
m.s.s <- m.s.s[,c('Transect','b','SE','t','p','df','AIC')]
m.s.s$t <- round(m.s.s$t,1)
m.s.s$p <- round(m.s.s$p,3)
write.csv(m.s.s,'outputs/sup/beta/modelSummary.csv',row.names = F)
