## library(automap)
library(raster)
library(rgeos)
library(ripums) # devtools::load_all('/data/ripums')
library(rnaturalearth)

## workdir <- Sys.getenv('WORKDIR')
## datadir <- 'tif'

data(dfrIpums)
data(stGeolev2)

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

rstack[['ag']] <- rstack[['ag']] + rstack[['paddy']]
rstack <- rstack[[c('popA', 'ag', 'urban', 'nl', 'access')]]

## form <- svi~popA+I(popA^2)+ag+paddy+urban+access+nl
form <- svi~popA+urban+access+nl

## dfrIpums <- ripums::setMissing()
eSvi <- ripums::svi()
vSvi <- eSvi $pca $pc %*% eSvi $pca $vt[, 1]
dfrSvi <- data.frame(svi=vSvi, key=eSvi $key)
## dfrIpums <- merge(dfrIpums, dfrSvi, by.x='SERIAL', by.y='key')
dfrIpums $svi <- dfrSvi $svi[match(dfrIpums $SERIAL, dfrSvi $key)]

dfrIpums $MIG5INT <- as.numeric(dfrIpums $MIGRATE5==30)
dfrIpums $MIG5MAJ <- as.numeric(dfrIpums $MIGRATE5>19)

## eMig5Int <- ripums::dasymetric(rstack, 'MIG5INT', rhs, dfrIpums, R=120,
##                                parallel='multicore', ncpus=22)
## save(eMig5Int, file=file.path(workdir, 'mig5Int.Rdata'))

## eMig5Maj <- ripums::dasymetric(rstack, 'MIG5MAJ', rhs, dfrIpums, R=120,
##                                parallel='multicore', ncpus=22)
## save(eMig5Maj, file=file.path(workdir, 'mig5Maj.Rdata'))

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

spGl2 <- as(stGeolev2, 'Spatial')
spGl2 $svi <- xbar[match(spGl2 $GEOLEVEL2, names(xbar))]

## clip spGl2
spPHL <- rnaturalearth::ne_states(country='Philippines')
spNCR <- spPHL[spPHL $region %in% c('National Capital Region', 'Central Luzon (Region III)', 'CALABARZON (Region IV-A)'), ]

mNCR <- rgeos::gIntersects(spGl2, spNCR, byid=TRUE, returnDense=TRUE)
vNCR <- apply(mNCR, 2, any)
spNCR2 <- spGl2[vNCR, drop=FALSE]

nnodes_ex <- c(rep('129.229.26.83', 4), rep('129.229.26.102', 12), rep('129.229.26.100', 4))
nnodes_vgm <- c(rep('129.229.26.83', 16), rep('129.229.26.102', 24), rep('129.229.26.100', 16))

krigeSvi <- ripums::krige(form, spGl2, rstack, vgm=gstat::vgm(0.003, 'Exp', 400, 0.003),
                          nnodes_ex, nnodes_vgm, master='129.229.26.83', computeVar=TRUE)
save(krigeSvi, file=file.path(workdir, 'krigeSvi.RData'))
