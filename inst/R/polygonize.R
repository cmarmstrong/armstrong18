library(meteo)
library(parallel)
library(raster)
library(snowfall)

workdir <- Sys.getenv('WORKDIR')
datadir <- 'dataDasy'

load(file.path(workdir, 'tilized.Rdata'))

cl <- parallel::makeForkCluster(11)
spTiles <- parallel::parLapply(cl, rsTiles, raster::rasterToPolygons)
stopCluster(cl)

save(spTiles, file=file.path(workdir, 'polygonized.Rdata'))
