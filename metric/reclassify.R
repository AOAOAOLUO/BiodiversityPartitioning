setwd('E:/luoa/gamma/')
sapply(c('sf','dplyr','raster','stringr','parallel'), require,character.only=T)
rstl <- raster('../data/map/Map_land/landboundary.tif')

load('../geomor/devide/devide_info_mark.rdata')
load('../geomor/devide/')
bio12 <- raster('data/clim/CHELSA_bio12_1981-2010_V.2.1.tif')

bs.p.info$bio12 <- extract(bio12,bs.p.info[,c('X','Y')])

# cellDeltaChi <- filter(bs.p.info,!is.na(HYBAS_ID_A))%>%group_by(cell)%>%
#   mutate(meanDeltaChi = mean(deltaChi),q95DeltaChi = quantile(deltaChi,0.95))%>%
#   ungroup()%>%dplyr::select(cell,meanDeltaChi,q95DeltaChi)%>%distinct()

cellDeltaChi <- filter(bs.p.info2,!is.na(HYBAS_ID_A))%>%group_by(cell)%>%
  mutate(meanDeltaChi = mean(deltaChi),q95DeltaChi = quantile(deltaChi,0.95))%>%
  ungroup()%>%dplyr::select(cell,meanDeltaChi,q95DeltaChi)%>%distinct()



########
# rst <- bio12
# rst[rst[]<300] <- NA
# plot(rst,colNA ='gray95')

bs.p.info2 <- bs.p.info[bs.p.info$bio12>300,]
bs.p.info2$chi_A[bs.p.info2$chi_A>35] <- 35
bs.p.info2$chi_B[bs.p.info2$chi_B>35] <- 35

thr <- quantile(bs.p.info2[,'deltaChi'],0.8,na.rm=T)

# cellDeltaChi <- filter(bs.p.info2,!is.na(HYBAS_ID_A))%>%group_by(cell)%>%
#   mutate(meanDeltaChi = mean(deltaChi),q95DeltaChi = quantile(deltaChi,0.95))%>%
#   ungroup()%>%dplyr::select(cell,meanDeltaChi,q95DeltaChi)%>%distinct()

deltaChiOverThr <- filter(bs.p.info2,!is.na(HYBAS_ID_A))%>%group_by(cell)%>%
  mutate(overThr = sum(deltaChi>thr)/n(),n=n())%>%
  ungroup()%>%dplyr::select(cell,overThr,n)%>%distinct()
deltaChiOverThr <- deltaChiOverThr[deltaChiOverThr$n>100,]

# rst <- raster()
# rst[deltaChiOverThr$cell] <- deltaChiOverThr$overThr
# rst[is.na(rstl[])] <- NA
# plot(rst)
#writeRaster(rst,'results/tmp/deltaChiRC.tif')

rst <- raster()
rst[deltaChiOverThr$cell] <- deltaChiOverThr$overThr>0.5
rst[is.na(rstl[])] <- NA
plot(rst)
