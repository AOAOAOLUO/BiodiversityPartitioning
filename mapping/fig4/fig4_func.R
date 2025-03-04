
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

func2 <- function(gmm,act,dat){
  geo.active <- dat
  
  geo.active$active <- factor(geo.active$active,levels = c(
    "Geomorphic active","Compression","Compressive shear",
    "Extensional shear","Extension","Inactive"))
  
  geo.active$active2 <- factor(geo.active$active2,
                               levels = c("T active","M active","Inactive"),
                               labels = c("Tectonic active","Geomorphic active","Inactive"))
  
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
  
  g <- ggplot(p.geo.active, aes(x=gamma_anomaly,y=gamma_anomaly_p))
  g2 <- g + geom_bar(aes(fill = active),stat = "identity")+
    scale_fill_manual(values = brewer.pal(3,'Reds') %>% rev()) +
    #scale_y_continuous(breaks = c(0,146395*0.25,146395/2,146395*0.75,146395),labels = c(0,25,50,75,100)) + 
    xlab("Gamma_GI") + ylab("Pixel ratio (%)")+  labs(fill = 'Catagories') +
    theme_bw()+
    theme(text=element_text(size=12),
          axis.text = element_text(size=8),
          legend.key.height = unit(0.5, "line"),
          legend.key.size = unit(0.5, "line"))
}