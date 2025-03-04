sapply(c('raster','dplyr','ggplot2','RColorBrewer'),require,character.only=T)
library(raster);library(dplyr)
setwd('D:/work/Gamma/')
source('code/fig4/fig4_func.R')


# figure 4 ----------------------------------------------------------------


dat <- foreign::read.dbf('data/metric/omega/data_1d_land.dbf') 

dat <- dat[dat$land,]
dat$active.ori <- dat$active
dat$active[is.na(dat$gmm_GI)] <- NA

mycolor <- c('#803429','#F1B972','#F3E887','#8BCBEF','#0156FF','#CAD1DB') %>% rev()
mycolor.light <- lapply(mycolor, function(x)colorRampPalette(c(x,'white'))(5)[4]) %>% unlist()

dat$active <- dat$active.ori
#dat$gmm_GI
p.geo.active <- func('gmm_GI','active',dat);

p.mark <- read.csv('outputs/sup/table_null_model.csv') %>% filter(active%in%"Active")
p.mark$gamma_anomaly <- factor(p.mark$gamma_anomaly)
p.mark$gamma_anomaly <- factor(p.mark$gamma_anomaly,
                                      levels=c('Top 1%','Top 5%',
                                               'Top 10%','Top 25%','Top 50%','Low') %>% rev())

levels(p.mark$gamma_anomaly) <- stringr::str_remove(levels(p.mark$gamma_anomaly),'Top ')
levels(p.geo.active$gamma_anomaly) <- stringr::str_remove(levels(p.geo.active$gamma_anomaly),'Top ')
g <- ggplot(p.geo.active, aes(x=as.numeric(gamma_anomaly),y=gamma_anomaly_p))
g2 <- g + geom_bar(aes(fill = active),stat = "identity")+
  scale_fill_manual(values = mycolor) +
  scale_x_continuous(
    breaks = c(1:7-0.5),
    labels = c(0,50,75,90,95,99,100)
  ) +
  #scale_y_continuous(breaks = c(0,146395*0.25,146395/2,146395*0.75,146395),labels = c(0,25,50,75,100)) + 
  xlab("") + ylab("Proportion of cells (%)")+  labs(fill = 'Catagories') +
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        text=element_text(size=8),
        axis.text = element_text(size=6),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))
g2 <- g2 + geom_text(dat= p.mark, aes(x=as.numeric(gamma_anomaly),y=105,label=p_mark),size=2)
g2 <- g2+xlab('quantile')
g2
ggsave('outputs/fig4/fig4_2.pdf',width = 1.6,height = 2.1,dpi='print')
