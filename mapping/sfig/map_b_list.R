sapply(c('raster','dplyr','ggplot2','RColorBrewer','patchwork'),require,character.only=T)
setwd('D:/work/Gamma/')

load('data/beta/beta_all_not_clean.rdata')
df <- b.s
df$significant <- 'p < 0.05'
df$significant[df$p>=0.05] <- 'p > 0.05' 
ggplot(df) +
  geom_histogram(aes(x = b,fill=significant), #fill = "lightblue", 
                 color = "gray50",bins=20) +
  scale_fill_manual(values=c('p < 0.05'= "lightblue",
                             'p > 0.05'= "gray75")) +
  scale_x_log10()+
  theme_classic()+
  labs(x = "b", y = "Frequency")

ggplot(df[df$p<0.05,]) +
  geom_histogram(aes(x = b),fill = "lightblue", 
                 color = "gray50",bins=20) +
  scale_x_log10()+
  theme_classic()+
  labs(x = "b", y = "Frequency")
ggsave('outputs/sup/beta/hist.jpg',width = 5,height = 3)


# mapping -----------------------------------------------------------------

load('data/gamma_EH/beta_all.rdata')
quantile(b.s,c(0.16,0.84))
fls <- list.files('data/gamma_EH/gamma_EH_all/',full.names = T)

###
ehs <- lapply(fls, function(i){
  test <- read.csv(i)
  test <- test[-1,]
  test <- test[order(b.s),]
  test$rank <- 1:48
  test
})
ehs.obs <- lapply(fls, function(i){
  test <- read.csv(i)
  test[1,]
})

eh.obs <- do.call('rbind',ehs.obs)
eh <- do.call('rbind',ehs)

# calcualate EH and GI ----------------------------------------------------


rst.eh.obs <- raster();rst.eh.obs[eh.obs$pixel] <- eh.obs$gamma_EH
rst.eh <- lapply(1:48, function(i){
  rst <- raster()
  rst[eh$pixel[eh$rank%in%i]] <- eh$gamma_EH[eh$rank%in%i]
  return(rst)
})
rst.to <- raster('data/gamma_TO/gamma/gamma.tif')
rst.gi.obs <- rst.to - rst.eh.obs
rst.gi <- lapply(rst.eh, function(rst.eh.i){
  rst.gi.i <- rst.to - rst.eh.i
  return(rst.gi.i)
})

# convert to data.frame ---------------------------------------------------


rst.gi.obs <- rst.gi.obs %>% rasterToPoints() %>% as.data.frame()
rst.gi <- lapply(rst.gi, function(rst.gi.i){
  rst.gi.i <- rst.gi.i %>% rasterToPoints() %>% as.data.frame()
  return(rst.gi.i)
})

rst.eh.obs <- rst.eh.obs %>% rasterToPoints() %>% as.data.frame()
rst.eh <- lapply(rst.eh, function(rst.eh.i){
  rst.eh.i <- rst.eh.i %>% rasterToPoints() %>% as.data.frame()
  return(rst.eh.i)
})

flag <- ceiling(quantile(1:48,c(0.95,0.5,0.25,0.05)))
rst.eh <- list(rst.eh[[flag[1]]],rst.eh.obs,rst.eh[[flag[2]]],rst.eh[[flag[3]]],rst.eh[[flag[4]]]) 
rst.gi <- list(rst.gi[[flag[1]]],rst.gi.obs,rst.gi[[flag[2]]],rst.gi[[flag[3]]],rst.gi[[flag[4]]]) 

ls.ylabs <- c('95%', 'Mean','50%','25%','5%')

eh_max <- lapply(rst.eh, function(x)max(x$layer,na.rm = T)) %>% unlist() %>% max() %>% ceiling()
eh_min <- lapply(rst.eh, function(x)min(x$layer,na.rm = T)) %>% unlist() %>% min() %>% floor()

gi_max <- lapply(rst.gi, function(x)max(x$layer,na.rm = T)) %>% unlist() %>% max() %>% ceiling()
gi_min <- lapply(rst.gi, function(x)min(x$layer,na.rm = T)) %>% unlist() %>% min() %>% floor()
#p <- list()


mypalette <- colorRampPalette(c('#79171c','#934f24','#ac7e26',
                                '#c2b055', '#cbdda1','#a5d7ca',
                                '#5cb8c9','#2d8ab6','#CAD1DB'))
mycol <- rev(mypalette(100))
colsEH <- mycol[c(seq(1,50,2),51:100)]

mypalette <- colorRampPalette(c('#803429','#F1B972','#F3E887',
                                '#8BCBEF','#CAD1DB'))
mycol <- rev(mypalette(100))
colsGI <- mycol

for (i in 1:5) {
  r_df <- rst.eh[[i]]
  p1 <- ggplot(r_df, aes(x = x, y = y, fill = layer)) +
    geom_raster() +
    scale_fill_gradientn(colours = colsEH,
                         # breaks = c(eh_min,0,1000,2000,3000,eh_max),
                         # labels = c(eh_min,0,1000,2000,3000,eh_max),
                         limits = c(eh_min, eh_max)
                         ) +
    theme_classic()+
    theme(#panel.grid = element_blank(),
      panel.border = element_rect(fill = NA,colour = 'gray'),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.position = 'none',
      aspect.ratio = 0.5,
      axis.title.x = element_blank())+
    ylab(ls.ylabs[i])
  #p1
  r_df <- rst.gi[[i]]
  p2 <- ggplot(r_df, aes(x = x, y = y, fill = layer)) +
    geom_raster() +
    scale_fill_gradientn(colours = colsGI,
                         # breaks = c(eh_min,0,1000,2000,3000,eh_max),
                         # labels = c(eh_min,0,1000,2000,3000,eh_max),
                         limits = c(gi_min, gi_max)
    ) +
    labs(fill = "Value") +
    theme_void()+
    theme(#panel.grid = element_blank(),
      panel.border = element_rect(fill = NA,colour = 'gray'),
      legend.position = 'none',
      aspect.ratio = 0.5,
      axis.title = element_blank())
  df <- rst.gi[[i]][] %>% na.omit()
  p3 <- ggplot(df) +
    geom_histogram(aes(x = layer),
                   boundary = 0,
                   fill = "#003366", 
                   color = "gray50",
                   closed = "left",
                   bins=50) +
    scale_x_continuous(breaks = seq(-2000,12000,by=2000),
                       limits = c(-2000, 12000))+
    theme_light(base_size = 6)+
    labs(x = "", y = "Frequency")
  p3
  if(i==1) p <- p1 + p2 + p3 else p <- p + p1 + p2 + p3
}
pa <- p + plot_layout(ncol = 3)
pa
{
  p1 <- p1 + labs(fill='Gamma_EH')+
    theme(legend.position = 'bottom',
          legend.key.width = unit(0.3,'in'),
          legend.key.height = unit(0.1,'in'),
          legend.title.position = 'top')
  p2 <- p2 + labs(fill='Gamma_GI')+
    theme(legend.position = 'bottom',
          legend.key.width = unit(0.3,'in'),
          legend.key.height = unit(0.1,'in'),
          legend.title.position = 'top')
  p3 <- p3 + labs(x='Gamma_GI') + theme_bw(base_size = 8)
  pb <- p1 + p2 + p3 + 
    plot_layout(ncol = 3,heights = c(1,1,1,1,1,1.25),widths = c(2,2,1.7))
}
pb
ggsave('outputs/sup/beta/b_maps_legend.jpg',width = 5.5,height = 1.5,dpi=300)
pa
ggsave('outputs/sup/beta/b_maps_main.jpg',width = 5.5,height = 6,dpi=300)
ggsave('outputs/sup/beta/b_maps.jpg',width = 5.5,height = 6,dpi=300)

#
par(mfrow=c(5,3),mar=c(3,3,1,1))
plot(rst.eh.obs);plot(rst.gi.obs);hist(rst.gi.obs,ylab='',xlab='',main='');
flag <- ceiling(quantile(1:48,c(0.9,0.95,0.99)))
for (i in flag) {
  plot(rst.eh[[i]]);plot(rst.gi[[i]]);hist(rst.gi[[i]],main='',xlab='',ylab='')
}


dev.off()
global_min <- -1432 %>% ceiling()
global_max <- 11521 %>% floor()
par(mfrow=c(5,3),mar=c(3,3,1,1))
breaks <- seq(global_min, global_max, length.out = 100) %>% round 
colors <- terrain.colors(length(breaks)-1)%>% rev()

eh_min <- 0 %>% ceiling()
eh_max <- 3469 %>% floor()
eh_breaks <- seq(eh_min, eh_max, length.out = 100) %>% round 
eh_colors <- terrain.colors(length(eh_breaks)-1)%>% rev()

par(mfrow=c(5,3),mar=c(3,3,1,1))
plot(rst.eh.obs,
     breaks = eh_breaks, col = eh_colors, 
     axis.args = list(at = c(eh_min,0,1000,2000,3000,eh_max),
                      labels = c(eh_min,0,1000,2000,3000,eh_max)),
     legend.shrink = 0.8, legend.width = 1.2);
plot(rst.gi.obs,
     breaks = breaks, col = colors, 
     axis.args = list(at = c(global_min,0,3000,6000,9000,global_max),
                      labels = c(global_min,0,3000,6000,9000, global_max)),
     legend.shrink = 0.8, legend.width = 1.2)
hist(rst.gi.obs,ylab='',xlab='',main='');
for (i in flag) {
  plot(rst.eh[[i]],
       breaks = eh_breaks, col = eh_colors, 
       axis.args = list(at = c(eh_min,0,1000,2000,3000,eh_max),
                        labels = c(eh_min,0,1000,2000,3000,eh_max)),
       legend.shrink = 0.8, legend.width = 1.2);
  plot(rst.gi[[i]],
       breaks = breaks, col = colors, 
       axis.args = list(at = c(global_min,0,3000,6000,9000,global_max),
                        labels = c(global_min,0,3000,6000,9000, global_max)),
       legend.shrink = 0.8, legend.width = 1.2);
  hist(rst.gi[[i]],main='',xlab='',ylab='')
}

#########################################################################
plot(rst.eh[[i]]);plot(rst.gi[[i]]);hist(rst.gi[[i]])


which(ehs[[1]][,3] %in% sort(ehs[[1]][-1,3])[ceiling(quantile(1:48,c(0.01,0.05,0.25,0.5,1)))] ) 
which(ehs[[2]][,3] %in% sort(ehs[[2]][-1,3])[ceiling(quantile(1:48,c(0.01,0.05,0.25,0.5)))] ) 

par(mfrow=c(5,2))
plot(rst.eh[[1]]);plot(rst.gi[[1]])
plot(rst.eh[[6]]);plot(rst.gi[[6]])
plot(rst.eh[[7]]);plot(rst.gi[[7]])
plot(rst.eh[[24]]);plot(rst.gi[[24]])
plot(rst.eh[[49]]);plot(rst.gi[[49]])


bb <- mean(log(b.s))-2*sd(log(b.s))
aa <- mean(log(b.s))+2*sd(log(b.s))
b.s[!between(log(b.s),bb,aa)]

######3
max(rst.gi.obs[],na.rm = T)
for (i in flag) {
  max(rst.gi[[i]][],na.rm = T) %>% print()
}


max(rst.eh.obs[],na.rm = T)
for (i in flag) {
  max(rst.eh[[i]][],na.rm = T) %>% print()
}

dat <- cbind(rst.ot[],
             rst.eh.obs[],
             rst.eh[[2]][],
             rst.eh[[4]][],
             rst.eh[[13]][],
             rst.eh[[25]][],
             rst.gi.obs[],
             rst.gi[[2]][],
             rst.gi[[4]][],
             rst.gi[[13]][],
             rst.gi[[25]][])
colnames(dat) <- c('GI','EH MEAN','EH 1%','EH 5%','EH 25%','EH 50%','GI MEAN','GI 1%','GI 5%','GI 25%','GI 50%')
View(dat)
