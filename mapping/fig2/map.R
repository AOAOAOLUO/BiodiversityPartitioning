sapply(c('dplyr','raster'),require,character.only=T)
dat <- read.csv('AAA_Work/gamma/data/sPlots/alphamax_splots.csv')
load('AAA_Work/gamma/data/sPlots/bio1_10times_bio12_alpha.rdata')

#dat$bio12 <- dat$bio12 %>% log1p()
nseq <- 45
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
    #print(sum(flag,na.rm=T))
    if(sum(flag,na.rm=T)==0) next
    #print('X')
    im.model[xi,yi] <- mean(bio1.bio12.pred$alpha[flag],na.rm=T)
    
    # flag <- (bio1.bio12.pred$bio1%in%xseq.int[xi])&(bio1.bio12.pred$bio12%in%yseq.int[yi])
    # if(sum(flag,na.rm=T)==0) next
    # tmp <- bio1.bio12.pred$alpha.max[flag]
    # print(tmp)
    # im.model[xi,yi] <- tmp
    #mean(dat$alpha[flag],na.rm=T)
  }
}

nseq <- 50
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

# im.s <- im.pred
# for (i in 1:nseq) {
#   flag <- !is.na(im.pred[,i])
#   im.s[flag,i] <- smooth(im.pred[flag,i])
# }

# im.s <- im.model
# for (i in 1:nseq) {
#   flag <- !is.na(im.model[,i])
#   im.s[flag,i] <- smooth(im.model[flag,i])
# }
#plot(data=dat,bio12~bio1,color=alpha_pred)

ncolors = 10
colors <- c("yellow", "red")
#colors <- c('#885F0D','#009900')
gradientColors <- grDevices::colorRampPalette(colors, space = "Lab")
ColorRamp <- (gradientColors(ncolors))
#ColorRamp <- rev(gradientColors(ncolors))
#colSeqTotal <- seq(min(rangeVals), max(rangeVals), length = ncolors)
ColorRamp_ex <- ColorRamp

breaks <- seq(min(c(im.pred,im.splot),na.rm = T), 
              max(c(im.pred,im.splot),na.rm = T), length.out = 11)  
breaks[1] <- 0
breaks[11] <- breaks[11]+1
labels <- breaks[-1] %>% round()

z_colors <- cut(dat$alpha, breaks=breaks, labels=FALSE)  
z_colors <- factor(z_colors,labels=ColorRamp_ex)
plot(x = dat$bio1,
     y = dat$bio12,
     col= z_colors %>% as.character(),
     xaxs="r", yaxs="r", xlab = "BIO1", ylab = "BIO12",
     axes=F, pch=19,cex=0.1,#alpha=0.5,
     main = "")#, asp = 1
graphics::contour(x = xseq[-1],
                  y = yseq[-1],
                  z = im.model,
                  nlevels = 10,
                  levels = breaks[-1],
                  method = "edge",
                  drawlabels = TRUE, 
                  add = TRUE)
graphics::box(which="plot")
graphics::axis(1,tcl=0.3,lwd=0.8)
graphics::axis(2, las=1, tcl=0.3,lwd=0.8)
legend("topleft", legend = labels, 
       leng,fill = ColorRamp_ex, title = "α")

graphics::image(x = xseq[-1],
                y = yseq[-1],
                z = im.splot,breaks=breaks,
                col = ColorRamp_ex, xaxs="r", yaxs="r", xlab = "BIO1", ylab = "BIO12",
                axes=F,main = "")
graphics::contour(x = xseq[-1],
                  y = yseq[-1],
                  z = im.model,
                  nlevels = 10,
                  levels = breaks[-1],
                  method = "edge",
                  drawlabels = TRUE, 
                  add = TRUE)
graphics::box(which="plot")
graphics::axis(1,tcl=0.3,lwd=0.8)
graphics::axis(2, las=1, tcl=0.3,lwd=0.8)
legend("topleft", legend = labels, fill = ColorRamp_ex, title = "α")

graphics::image(x = xseq[-1],
                y = yseq[-1],
                z = im.pred,breaks=breaks,
                col = ColorRamp_ex, xaxs="r", yaxs="r", xlab = "BIO1", ylab = "BIO12",
                axes=F,main = "")
graphics::contour(x = xseq[-1],
                  y = yseq[-1],
                  z = im.model,
                  nlevels = 10,
                  levels = breaks[-1],
                  method = "edge",
                  drawlabels = TRUE, 
                  add = TRUE)
graphics::box(which="plot")
graphics::axis(1,tcl=0.3,lwd=0.8)
graphics::axis(2, las=1, tcl=0.3,lwd=0.8)
legend("topleft", legend = labels, fill = ColorRamp_ex, title = "α max")

if(globalContour == TRUE & x$parameters$objectClass != "TPDs"){
  if(is.null(globalContour.quant)){
    globalContour.quant <- x$parameters$threshold
  }
  graphics::contour(x = unique(x$parameters$evaluation_grid[,1]),
                    y = unique(x$parameters$evaluation_grid[,2]),
                    z = x$global$images$noThreshold.quantiles,
                    levels = globalContour.quant, lwd = globalContour.lwd, lty = globalContour.lty,
                    col = globalContour.col,
                    drawlabels = FALSE, add = TRUE)
}