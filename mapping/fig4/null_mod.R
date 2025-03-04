sapply(c('raster','dplyr','ggplot2','RColorBrewer'),require,character.only=T)
library(raster);library(dplyr)
setwd('D:/work/Gamma/')

#### load data

dat <- foreign::read.dbf('data/metric/omega/data_1d_land.dbf') 
dat <- dat[dat$land,]
dat <- dat[dat$gmm_obs>10,]

dat$active.ori <- dat$active
dat$active[is.na(dat$gmm_GI)] <- NA

mycolor <- c('#803429','#F1B972','#F3E887','#8BCBEF','#0156FF','#CAD1DB') %>% rev()
mycolor.light <- lapply(mycolor, function(x)colorRampPalette(c(x,'white'))(5)[4]) %>% unlist()

dat$active <- dat$active.ori

#### function classfy gmm_gi (gamma anomaly) into percentiles  
func <- function(gmm,act,dat){
  geo.active <- dat
  
  geo.active$active <- factor(geo.active$active,levels = c(
    "Geomorphic active","Compression","Compressive shear",
    "Extensional shear","Extension","Inactive") %>% rev())
  
  geo.active$active2 <- factor(geo.active$active2,
                               levels = c("T active","M active","Inactive")%>% rev(),
                               labels = c("Tectonic active","Geomorphic active","Inactive")%>% rev())
  
  geo.active$gamma_a <- geo.active[,gmm]
  geo.active$active <- geo.active[,act]
  
  geo.active <- geo.active[!is.na(geo.active$active),]
  geo.active$g_a_1 <- geo.active$gamma_a>quantile(geo.active$gamma_a,0.99,na.rm=T)
  geo.active$g_a_5 <- geo.active$gamma_a>quantile(geo.active$gamma_a,0.95,na.rm=T)
  geo.active$g_a_10 <- geo.active$gamma_a>quantile(geo.active$gamma_a,0.9,na.rm=T)
  geo.active$g_a_25 <- geo.active$gamma_a>quantile(geo.active$gamma_a,0.75,na.rm=T)
  geo.active$g_a_50 <- geo.active$gamma_a>quantile(geo.active$gamma_a,0.5,na.rm=T)
  
  p.geo.active <- geo.active %>% group_by(active) %>% 
    mutate(g_a_5=sum(g_a_5,na.rm=T),g_a_10=sum(g_a_10,na.rm=T),
           g_a_25=sum(g_a_25,na.rm=T),g_a_50=sum(g_a_50,na.rm=T),
           n=n()) %>% 
    ungroup() %>% dplyr::select(active,g_a_5,g_a_10,g_a_25,g_a_50,n) %>% distinct()
  
  ######## pixel number
  p2.geo.active <- geo.active
  p2.geo.active$gamma_anomaly <- 'Low'
  p2.geo.active$gamma_anomaly[p2.geo.active$g_a_50] <- 'Top 50%'
  p2.geo.active$gamma_anomaly[p2.geo.active$g_a_25] <- 'Top 25%'
  p2.geo.active$gamma_anomaly[p2.geo.active$g_a_10] <- 'Top 10%'
  p2.geo.active$gamma_anomaly[p2.geo.active$g_a_5] <- 'Top 5%'
  p2.geo.active$gamma_anomaly[p2.geo.active$g_a_1] <- 'Top 1%'
  # p2.geo.active$gamma_anomaly <- factor(p2.geo.active$gamma_anomaly,
  #                                       levels=c('Low','Top 50%','Top 25%',
  #                                                'Top 10%','Top 5%'))
  p2.geo.active$gamma_anomaly <- factor(p2.geo.active$gamma_anomaly,
                                        levels=c('Top 1%','Top 5%',
                                                 'Top 10%','Top 25%','Top 50%','Low') %>% rev())
  
  ######## pixel percentile
  p.geo.active <- p2.geo.active %>% group_by(gamma_anomaly) %>% 
    mutate(n=n()) %>% ungroup() %>% 
    group_by(active,gamma_anomaly) %>% 
    mutate(gamma_anomaly_n=n()) %>% ungroup() %>% 
    dplyr::select(active,gamma_anomaly,gamma_anomaly_n,n) %>% distinct()
  p.geo.active$gamma_anomaly_p <- p.geo.active$gamma_anomaly_n/p.geo.active$n*100
  return(p.geo.active)
}

#### reclassfy cells

p.geo.active <- func('gmm_GI','active',dat);
levels(p.geo.active$gamma_anomaly) <- stringr::str_remove(levels(p.geo.active$gamma_anomaly),'Top ')
g <- ggplot(p.geo.active, aes(x=gamma_anomaly,y=gamma_anomaly_p))
g2 <- g + geom_bar(aes(fill = active),stat = "identity")+
  scale_fill_manual(values = mycolor) +
  #scale_y_continuous(breaks = c(0,146395*0.25,146395/2,146395*0.75,146395),labels = c(0,25,50,75,100)) + 
  xlab("") + ylab("Proportion of cells (%)")+  labs(fill = 'Catagories') +
  theme_bw()+
  theme(legend.position = "none",
        text=element_text(size=12),
        axis.text = element_text(size=6),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))
g2 <- g2+xlab('Quantile')
#ggsave('outputs/fig4/fig4_2.pdf',width = 2,height = 3,dpi='print')

# run null model ----------------------------------------------------------


dat$active <- dat$active.ori
p.geo.active <- func('gmm_GI','active',dat)
p.geo.active.s <- lapply(1:999,function(i){
  set.seed(i+35946)
  flag <- which(!is.na(dat$active))
  dat$active <- dat$active.ori
  dat$active[flag] <- dat$active[sample(flag)]
  tmp <- func('gmm_GI','active',dat)
  tmp$grp <- i
  return(tmp)
})
p.geo.active.s <- do.call(rbind,p.geo.active.s)

p.geo.active.s$obs <- F

p.geo.active.obs <- p.geo.active
p.geo.active.obs$grp <- 0
p.geo.active.obs$obs <- T

pdat <- rbind(p.geo.active.obs,p.geo.active.s)


# summary null model ------------------------------------------------------

str(pdat)
pdat2 <- pdat
pdat2$gamma_anomaly_p[pdat2$active%in%'Inactive'] <- 100 - pdat2$gamma_anomaly_p[pdat2$active%in%'Inactive']
levels(pdat2$active)[1] <- 'Active'

s.dat <- pdat2 %>% group_by(active,gamma_anomaly) %>% 
  mutate(perc_obs=gamma_anomaly_p,perc_mean = mean(gamma_anomaly_p),perc_sd = sd(gamma_anomaly_p),rank=rank(gamma_anomaly_p)) %>% 
  ungroup() %>% dplyr::select(active,gamma_anomaly,perc_obs,perc_mean,perc_sd,obs,rank) %>% filter(obs) %>% 
  mutate(ses = (perc_obs - perc_mean)/perc_sd, z = (perc_obs - perc_mean)/perc_sd/sqrt(1000),p=1-rank/1000)
s.dat$test <- s.dat$p<0.05
s.dat$p_mark <- 'ns'
s.dat$p_mark[s.dat$test] <- '*'

s.dat$p_value_z <- 2 * (1 - pnorm(abs(s.dat$z)))
s.dat$p_value_z <- round(1 - pnorm(s.dat$ses),3)
s.dat$p_value_z <- round(1 - pnorm(s.dat$z),3)

s.dat <- s.dat[,c('active',"gamma_anomaly",'ses','p','p_mark')]

s.dat <- s.dat[order(s.dat$gamma_anomaly,decreasing = T),]
s.dat <- s.dat[order(s.dat$active),]
s.dat$p <- round(s.dat$p,3)
View(s.dat)
write.csv(s.dat,'outputs/sup/table_null_model.csv',row.names = F)
