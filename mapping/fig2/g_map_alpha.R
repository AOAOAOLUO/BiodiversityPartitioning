sapply(c('dplyr','raster'),require,character.only=T)
dat <- read.csv('AAA_Work/gamma/results/alpha_check.csv')
rst <- raster()


par(mfrow=c(2,2))
rst[dat$pixel] <- dat$alpha
plot(rst)
title(main='(a) alpha max (mean)',adj=0,font=1)

rst[dat$pixel] <- dat$alpha.max
plot(rst)
title(main='(b) alpha max (max)',adj=0,font=1)

rst[dat$pixel] <- dat$alpha.min
plot(rst)
title(main='(c) alpha max (min)',adj=0,font=1)

rst[dat$pixel] <- dat$alpha.range
plot(rst)
title(main='(d) alpha max (range)',adj=0,font=1)

