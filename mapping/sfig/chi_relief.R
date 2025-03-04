sapply(c('raster','sf','foreign','dplyr','ggplot2','RColorBrewer','patchwork'),require,character.only=T)
setwd('D:/work/Gamma/')

shp <- read_sf('data/metric/omega/data_1d_land.shp') %>% dplyr::select(layer,land)
tmp <- read.dbf('data/metric/omega/geomor/deltaChi.dbf') %>% rename(layer=ID) %>% dplyr::select(layer,q95DltC)
tmp[is.na(tmp[,2]),2] <- 0
shp <- left_join(shp,tmp) 
tmp <- read.csv('data/metric/omega/geomor/gamma_relief.csv') %>% rename(layer=id) %>% dplyr::select(layer,relief)
tmp[is.na(tmp[,2]),2] <- 0
shp <- left_join(shp,tmp)

shp$chiAct <- 'Inactive'
shp$chiAct[shp$q95DltC > quantile(shp$q95DltC,0.75)] <- 'Active'
shp$reliefAct <- 'Inactive'
shp$reliefAct[shp$relief > quantile(shp$relief,0.75)] <- 'Active'
shp$geomorAct <- 'Inactive'
shp$geomorAct[shp$chiAct%in%'Active'&shp$reliefAct%in%'Active'] <- 'Active'

shp <- cbind(shp,xyFromCell(raster(),shp$layer) %>% as.data.frame()) 

mypalette <- colorRampPalette(c('#79171c','#934f24','#ac7e26',
                                '#c2b055', '#cbdda1','#a5d7ca',
                                '#5cb8c9','#2d8ab6','#CAD1DB'))
mycol <- rev(mypalette(100))

p1 <- ggplot(shp, aes(x = x, y = y, fill = relief)) +
  geom_raster() +
  scale_fill_gradientn(colours = mycol) +
  theme_classic()+
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(0.3,'in'),
    legend.key.height = unit(0.1,'in'),
    legend.title.position = 'top',
    #panel.grid = element_blank(),
    panel.border = element_rect(fill = NA,colour = 'gray'),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    #legend.position = 'none',
    aspect.ratio = 0.5,
    axis.title = element_blank())+
  labs(title =  'a',fill = 'Relief')
p1

p2 <- ggplot(shp, aes(x = x, y = y, fill = q95DltC)) +
  geom_raster() +
  scale_fill_gradientn(colours = mycol) +
  theme_classic()+
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(0.3,'in'),
    legend.key.height = unit(0.1,'in'),
    legend.title.position = 'top',
    #panel.grid = element_blank(),
    panel.border = element_rect(fill = NA,colour = 'gray'),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    #legend.position = 'none',
    aspect.ratio = 0.5,
    axis.title = element_blank())+
  labs(title =  'b',fill = 'Δχ')
p2

p3 <- ggplot(shp, aes(x = x, y = y, fill = reliefAct)) +
  geom_raster() +
  scale_fill_manual(values = c('Active' = 'brown','Inactive' = 'gray75')) +
  theme_classic()+
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(0.3,'in'),
    legend.key.height = unit(0.1,'in'),
    legend.title.position = 'top',
    #panel.grid = element_blank(),
    panel.border = element_rect(fill = NA,colour = 'gray'),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    #legend.position = 'none',
    aspect.ratio = 0.5,
    axis.title = element_blank())+
  labs(title =  'c',fill = 'Relief-active')
p3

p4 <- ggplot(shp, aes(x = x, y = y, fill = chiAct)) +
  geom_raster() +
  scale_fill_manual(values = c('Active' = 'brown','Inactive' = 'gray75')) +
  theme_classic()+
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(0.3,'in'),
    legend.key.height = unit(0.1,'in'),
    legend.title.position = 'top',
    #panel.grid = element_blank(),
    panel.border = element_rect(fill = NA,colour = 'gray'),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    #legend.position = 'none',
    aspect.ratio = 0.5,
    axis.title = element_blank())+
  labs(title =  'd',fill = 'χ-active ')
p4

p5 <- ggplot(shp, aes(x = x, y = y, fill = geomorAct)) +
  geom_raster() +
  scale_fill_manual(values = c('Active' = 'brown','Inactive' = 'gray75')) +
  theme_classic()+
  theme(
    legend.position = 'bottom',
    legend.key.width = unit(0.3,'in'),
    legend.key.height = unit(0.1,'in'),
    legend.title.position = 'top',
    #panel.grid = element_blank(),
    panel.border = element_rect(fill = NA,colour = 'gray'),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    #legend.position = 'none',
    aspect.ratio = 0.5,
    axis.title = element_blank())+
  labs(title =  'e',fill = 'Geomorphically active ')
p5

p <- p1+p2+p3+p4+p5+plot_layout(ncol = 2)
ggsave('outputs/sup/chi/chi_relif.jpg',width = 5.5,height = 7,dpi='print')
