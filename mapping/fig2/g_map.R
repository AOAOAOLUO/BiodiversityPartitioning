sapply(c('dplyr','raster'),require,character.only=T)
gamma <- raster('AAA_Work/gamma/results/raw_elev5m_exp.tif')
gamma[gamma[]==0] <- NA
plot(gamma)

sr <- raster('AAA_Work/hotspots/data/sr_globe.tif')
gamma.ano <-  sr-gamma
plot(gamma.ano)
