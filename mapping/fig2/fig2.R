setwd('D:/work/Gamma/')
sapply(c('dplyr','raster','metR','ggplot2','geomtextpath'),require,character.only=T)
dat <- read.csv('data/sPlots/alphamax_splots_100m_scale.csv')
load('data/sPlots/bio1_10times_bio12_alpha_scale.rdata')

dat$alpha <- dat$alpha_100
dat <- rename(dat,bio1=t,bio12=m)
bio1.bio12.pred <- rename(bio1.bio12.pred,bio1=t,bio12=m)

# im model
nseq <- 50
xx <- dat$bio1 %>% sort %>% unique() 
yy <- dat$bio12 %>% sort %>% unique() 

func <- function(x) seq(min(x),max(x),diff(range(x))/nseq)
xseq <- func(xx)
yseq <- func(yy)

xseq.int <- (xseq*10) %>% round()
yseq.int <- yseq %>% round()
# bio1.bio12.pred[bio1.bio12.pred$bio1%in%,]

im.model <- matrix(NA,nrow=nseq,ncol=nseq)
for (xi in 1:nseq) {
  print(xi)
  for (yi in 1:nseq) {
    flag <- between(bio1.bio12.pred$bio1,xseq.int[xi],xseq.int[xi+1]) & between(bio1.bio12.pred$bio12,yseq.int[yi],yseq.int[yi+1]) 
    if(sum(flag,na.rm=T)==0) next
    im.model[xi,yi] <- mean(bio1.bio12.pred$alpha[flag],na.rm=T)
  }
}

im <- expand.grid(x=xseq[-1]/2+xseq[-length(xseq)]/2,
                  y=yseq[-1]/2+yseq[-length(yseq)]/2) %>% as.data.frame()
im$z <- im.model %>% as.vector()
im.p.model <- im

# im predict
nseq <- 100
xx <- dat$bio1 %>% sort %>% unique() 
yy <- dat$bio12 %>% sort %>% unique() 

func <- function(x) seq(min(x),max(x),diff(range(x))/nseq)
xseq <- func(xx)
yseq <- func(yy)

xseq.int <- (xseq*10) %>% round()
yseq.int <- yseq %>% round()

im.splot <- matrix(NA,nrow=nseq,ncol=nseq)
for (xi in 1:nseq) {
  print(xi)
  for (yi in 1:nseq) {
    flag <- between(dat$bio1,xseq[xi],xseq[xi+1]) & between(dat$bio12,yseq[yi],yseq[yi+1]) 
    #print(sum(flag,na.rm=T))
    if(sum(flag,na.rm=T)==0) next
    #print('X')
    im.splot[xi,yi] <- mean(dat$alpha[flag],na.rm=T)
  }
}

im <- expand.grid(x=xseq[-1]/2+xseq[-length(xseq)]/2,
                  y=yseq[-1]/2+yseq[-length(yseq)]/2) %>% as.data.frame()
im$z <- im.splot %>% as.vector()
im.p.splot <- im

im.pred <- matrix(NA,nrow=nseq,ncol=nseq)
for (xi in 1:nseq) {
  print(xi)
  for (yi in 1:nseq) {
    flag <- between(dat$bio1,xseq[xi],xseq[xi+1]) & between(dat$bio12,yseq[yi],yseq[yi+1]) 
    #print(sum(flag,na.rm=T))
    if(sum(flag,na.rm=T)==0) next
    #print('X')
    im.pred[xi,yi] <- mean(dat$alpha_pred[flag],na.rm=T)
  }
}

im <- expand.grid(x=xseq[-1]/2+xseq[-length(xseq)]/2,
                  y=yseq[-1]/2+yseq[-length(yseq)]/2) %>% as.data.frame()
im$z <- im.pred %>% as.vector()
im.p.pred <- im

#### draw map
breaks<-seq(0,175,25)
breaks<-c(breaks,max(dat$alpha))
dat$alpha_lab <- as.numeric(cut(dat$alpha,breaks)) *25  -12.5

gplot <- g
gplot() + geom_raster(aes(x=x,y=y,fill=z),data = im.p.splot) +
  scale_fill_gradient(low='#FFE0C1',high='#007900',transform='log2',breaks=c(1,128),labels=c(1,128),na.value=NA)+
  geom_textcontour(aes(x=x,y=y,z=z),data = im.p.model,size=1.2,linewidth=0.1,col='gray10') +
  coord_fixed(ratio=diff(range(xseq))/diff(range(yseq)))+
  #ylim(0,6000)+
  theme_bw()
gplot
ggsave('outputs/fig2/2_1_rst.png',width = 3.5,height=3.5,dpi='print')

gplot <- ggplot() + 
  geom_point(aes(x = bio1,y = bio12,color=alpha),data=dat,pch=19,size=0.1)+#,alpha=0.33
  scale_color_gradient(name='α max',low='#FFE0C1',high='#006900',
                       transform='log2',breaks=c(1,128),labels=c(1,128))+
  geom_textcontour(aes(x=x,y=y,z=z),data = im.p.model,size=2,linewidth=0.1,col='gray10') +
  coord_fixed(ratio=diff(range(xseq))/diff(range(yseq)))+
  theme_bw() + labs(x='T',y='M')+
  theme(legend.position = c(0.15,0.7),
        #aspect.ratio = 0.75,
        panel.grid = element_blank(),
        legend.background = element_rect(fill = 'white', color = NA, linewidth = 0.5),
        axis.ticks.length = unit(-0.05, "cm")
  )

ggplot() + 
  geom_point(aes(x = bio1,y = bio12,color=alpha),data=dat,pch=19,size=0.1)+#,alpha=0.33
  scale_color_gradient(name='α',low='#FFE0C1',high='#006900',
                       transform='log2',breaks=c(1,128),labels=c(1,128))+
  geom_textcontour(aes(x=x,y=y,z=z),data = im.p.model,size=2,linewidth=0.1,col='gray10') +
  coord_fixed(ratio=diff(range(xseq))/diff(range(yseq)))+
  theme_bw() + labs(x='T',y='M')+
  theme(legend.position = 'right',
        #aspect.ratio = 0.75,
        panel.grid = element_blank(),
        legend.background = element_rect(fill = 'white', color = NA, linewidth = 0.5),
        axis.ticks.length = unit(-0.05, "cm")
  )

gplot <- ggplot() + 
  geom_point(aes(x = bio1,y = bio12,color=alpha),data=dat,pch=19,size=0.1)+#,alpha=0.33
  scale_color_gradientn(name='',colours = rev(c('#79171c','#934f24',
    '#ac7e26','#c2b055', '#cbdda1','#a5d7ca','#5cb8c9','#2d8ab6','#185da3','#0f3188')),
                       transform='log2',breaks=c(1,128),labels=c(1,128))+
  geom_textcontour(aes(x=x,y=y,z=z),data = im.p.model,size=2,linewidth=0.1,col='gray10') +
  coord_fixed(ratio=diff(range(xseq))/diff(range(yseq)))+
  theme_bw() + labs(x='T',y='M')+
  theme(legend.position = 'right',
        #aspect.ratio = 0.75,
        panel.grid = element_blank(),
        legend.background = element_rect(fill = 'white', color = NA, linewidth = 0.5),
        axis.ticks.length = unit(-0.05, "cm")
  )
gplot
ggsave('outputs/fig2/2_1_p.png',width = 3.5,height=3,dpi='print')
ggsave('outputs/fig2/2_1_p.pdf',width = 3.5,height=3,dpi='print')
