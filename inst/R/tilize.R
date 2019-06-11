library(meteo)
library(parallel)
library(raster)
library(snowfall)

workdir <- Sys.getenv('WORKDIR')
datadir <- 'dataDasy'

rRePop <- raster::raster(file.path(workdir, datadir, 'rePop.tif'))
rRePopA <- raster::raster(file.path(workdir, datadir, 'rePopA.tif'))
rLcAUrb <- raster::raster(file.path(workdir, datadir, 'lcAUrb.tif'))
rLcAAg <- raster::raster(file.path(workdir, datadir, 'lcAAg.tif'))
rLcAPad <- raster::raster(file.path(workdir, datadir, 'lcAPad.tif'))

rstack <- raster::stack(rRePop, rRePopA, rLcAUrb, rLcAAg, rLcAPad)
names(rstack) <- c('pop', 'popA', 'urban', 'ag', 'paddy')

rsTiles <- meteo::tiling(rstack, overlapping=0, parallel.processing=TRUE, cpus=44)

save(rsTiles, file=file.path(workdir, 'tilized.Rdata'))
