library(raster)
library(ripums)
library(rnaturalearth)

workdir <- Sys.getenv('WORKDIR')
datadir <- 'tif'

nnodes_ex <- c(rep('129.229.26.83', 4), rep('129.229.26.102', 12), rep('129.229.26.100', 4))

## survey data
eSvi <- ripums::svi()
vSvi <- eSvi $pca $pc %*% eSvi $pca $vt[, 1]
dfrSvi <- data.frame(svi=vSvi, key=eSvi $key)
dfrIpums <- merge(dfrIpums, dfrSvi, by.x='SERIAL', by.y='key')

dfrIpums $MIG5INT <- as.numeric(dfrIpums $MIGRATE5==30)
dfrIpums $MIG5MAJ <- as.numeric(dfrIpums $MIGRATE5>19)

## calculate geolevel2 weighted means (xbar)
lWgt <- by(dfrIpums, dfrIpums $GEOLEV2, function(dfrGl2) weight('svi', 'PERWT', dfrGl2))
xbar <- sapply(lWgt, function(dfrGl2) { # geolev2 weighted means
    weighted.mean(as.numeric(dfrGl2[, 'svi']), dfrGl2 $wtFreq, na.rm=TRUE)
})

## center & scale
## if(center) {
## xbarGrand <- weighted.mean(as.numeric(xbar[, 1]), xbar[, 2], na.rm=TRUE)
## xbar <- xbar - xbarGrand
## }
## if(scale) xbar <- xbar / sd(xbar)

## raster data
## rPop <- raster::raster(file.path(workdir, datadir, 'pop.tif'))
rPopA <- raster::raster(file.path(workdir, datadir, 'popA.tif'))
rLcUrb <- raster::raster(file.path(workdir, datadir, 'reLcUrbA.tif'))
rLcAg <- raster::raster(file.path(workdir, datadir, 'reLcAgA.tif'))
rLcPad <- raster::raster(file.path(workdir, datadir, 'reLcPadA.tif'))
rAccess <- raster::raster(file.path(workdir, datadir, 'access.tif'))
rNl <- raster::raster(file.path(workdir, datadir, 'reNl.tif'))

rLcUrb <- raster::crop(rLcUrb, rPopA)
rLcAg <- raster::crop(rLcAg, rPopA)
rLcPad <- raster::crop(rLcPad, rPopA)
rAccess <- raster::crop(rAccess, rPopA)
rNl <- raster::crop(rNl, rPopA)

raster::extent(rLcUrb) <- raster::extent(rPopA)
raster::extent(rLcAg) <- raster::extent(rPopA)
raster::extent(rLcPad) <- raster::extent(rPopA)
raster::extent(rAccess) <- raster::extent(rPopA)
raster::extent(rNl) <- raster::extent(rPopA)

raster::values(rAccess)[raster::values(rAccess)==-9999] <- NA

rstack <- raster::stack(rPopA, rLcUrb, rLcAg, rLcPad, rAccess, rNl)
names(rstack) <- c('popA', 'urban', 'ag', 'paddy', 'access', 'nl')

## vector data
spGl2 <- as(stGeolev2, 'Spatial')
spGl2 $svi <- xbar[match(spGl2 $GEOLEVEL2, names(xbar))]

## clip spGl2
spPHL <- rnaturalearth::ne_states(country='Philippines')
spNCR <- spPHL[spPHL $region %in% c('National Capital Region', 'Central Luzon (Region III)', 'CALABARZON (Region IV-A)'), ]
spGl2NCR <- raster::intersect(spGl2, spNCR)

## aggregate raster to vector
cl <- parallel::makeCluster(nnodes_ex, master='129.229.26.83')
dfrRex <- parExtract(cl, rstack, spGl2, weights=TRUE, normalizeWeights=FALSE,
                     small=TRUE, cellnumbers=TRUE, df=TRUE)
parallel::stopCluster(cl)

xbarSrc <- aggregate(.~ID, dfrRex, FUN=mean, na.rm=TRUE)
namesXbar <- paste0(names(xbarSrc)[2:ncol(xbarSrc)], 'bar')
names(xbarSrc)[2:ncol(xbarSrc)] <- namesXbar
spGl2NCR <- cbind(spGl2NCR, xbarSrc)

## aggregate gl2 to gl1
spGl2NCR $GEOLEVEL1 <- substr(spGl2NCR $GEOLEVEL2, 1, 6)
spGl1NCR <- raster::aggregate(spGl2NCR, by='GEOLEVEL1',
                              sums=list(list(mean, c('svi', names(rstack)))))

## krige and dasy
kSvi <- krige(form, spGl2NCR, rstack, vgm=gstat::vgm(0.003, 'Exp', 400, 0.003),
              nnodes_ex, nnodes_vgm, master='129.229.26.83')
## save(kSvi, file=file.path(workdir, 'svi.Rdata'))

## evaluate using known data
spGl2NCR <- spGl2NCR[complete.cases(spGl2NCR @data[, c('svi', names(rstack))]), ]
spGL1NCR <- spGl1NCR[complete.cases(spGl1NCR @data[, c('svi', names(rstack))]), ]

## krige
kGl2 <- krige0(form, spGl1NCR, spGl2NCR, parVgmArea,
               vgm=gstat::vgm(0.003, 'Exp', 400, 0.003),
               nnodes=nnodes_vgm, master='129.229.26.83')

## dasy
spGl2NCR $svi1 <- spGl1NCR $svi[match(spGl2NCR $GEOLEVEL1, spGl1NCR $GEOLEVEL1)]
mod <- lm(svi1~popA+urban+access+nl, data=spGl2NCR)
spGl2NCR $svi2hat <- predict(mod)

## how to eval?  sum of squares?
