library(countrycode)
library(gdalUtils)
library(raccess)
library(raster)
library(rlandscan)
library(rsrtm)
library(rvisnav)
library(rnaturalearth)
library(Rnightlights)

workdir <- getwd()
datadir <- file.path(workdir, 'armstrong18', 'inst')
tifdir <- file.path(datadir, 'tif') # c('PHL', 'ZAF')

## get NL
## nlPeriod <- '2015' # Rnightlights::nlRange('201301', '201312')
## nlType <- 'VIIRS.Y' # 'VIIRS.M'
## nlStats <- list('mean', na.rm=TRUE)

## tileList <- Rnightlights:::getNlTiles(nlType='VIIRS.Y') $name
## Rnightlights:::downloadNlTiles(nlType, nlPeriod, tileList) # imperative

## ag=file.path(tifdir, 'ag.tif'),
## urb=file.path(tifdir, 'urb.tif'),
ch <- c(lcover.tif=system.file('extdata', 'visnav.vrt', package='rvisnav'),
        dem.tif=system.file('extdata', 'srtm.vrt', package='rsrtm'),
        weiss2.tif=file.path(tifdir, 'weiss.tif'),
        lscan2.tif=file.path(tifdir, 'lscan.tif'),
        nlites.tif=file.path(tifdir, 'nlites.vrt'))

te <- c(-180, -90, 180, 90)
tr <- rep(0.00833333, 2)
tap <- TRUE
## mapply(gdalUtils, ...)
cl <- parallel::makeForkCluster(length(ch))
parSapply(cl, names(ch), function(name, ...) {
    if(name=='lcover.tif') r <- 'near'
    else r <- 'bilinear'
    gdalUtils::gdalwarp(ch[name], file.path(tifdir, name), r=r, ...)
}, te=te, tr=tr, tap=tap)
parallel::stopCluster(cl)

rLc <- raster(file.path(tifdir, 'lcover.tif'))
iLc <- c(6, 7, 8) # urban, ag-gen, ag-paddy
cl <- parallel::makeForkCluster(3)
lLc <- parallel::parLapply(cl, iLc, function(i) {
    r <- raster::mask(rLc, rLc!=i, maskvalue=1, updatevalue=0)
    r==i ## do this directly and skip mask?
})
parallel::stopCluster(cl)
rAg <- lLc[[2]] | lLc[[3]]
writeRaster(lLc[[1]], file.path(tifdir, 'urb.tif'))
writeRaster(rAg, file.path(tifdir, 'ag.tif'))


## RESAMPLE2
## datadir <- file.path('tif', 'ZAF')
## country <- 'South Africa'
## spCountry <- rnaturalearth::ne_countries(country=country)

## ## VIIRS data from Rnightlights
## iTile <- 5 # tile 5 contains ZAF, 3 contains PHL
## ctryCodes <- 'ZAF'
## ## nlPeriod <- paste0('2013', sprintf("%02d", 1:12))
## nlPeriod <- '2015' # Rnightlights::nlRange('201301', '201312')
## nlType <- 'VIIRS.Y' # 'VIIRS.M'
## nlStats <- list('mean', na.rm=TRUE)

## ## nlTile <- Rnightlights:::getNlTileTifLclNamePathVIIRS(nlPeriod, iTile, nlType)
## ## if(!file.exists(nlTile)) {
##     ## sapply(nlPeriod, function(nlPeriod, ctryCodes, nlType) {
##     ## tileList <- Rnightlights:::getCtryTileList(ctryCodes=ctryCodes, nlType=nlType)
##     tileList <- Rnightlights:::getNlTiles(nlType='VIIRS.Y') $name
##     Rnightlights:::downloadNlTiles(nlType, nlPeriod, tileList)
##     ## }, ctryCodes, nlType)
## ## }
## ## broke
## ## Rnightlights:::getCtryNlData(ctryCodes, "highest", nlType, nlPeriod, nlStats, ignoreMissing=FALSE)
## ## Rnightlights::getCtryRasterOutputFnamePath(ctryCodes, nlType, nlPeriod)

## rNltile <- raster::raster(nlTile)
## rNl <- raster::crop(rNltile, spCountry)

## ## lapply(l, do.call, list(country=country))
## rAccess <- raccess::access2cities(country=country)
## rDem <- rsrtm::srtm(country)
## rLcover <- rvisnav::visnav(country)
## rPop <- rlandscan::landscan(country=country)
## rSoils <- rsoils::soils(country)

## ## mapply(raster::writeRaster, functions, tmpfiles)
## tmpfiles <- tempfile(fileext=rep('.tif', 8))
## raster::writeRaster(rAccess, tmpfiles[1])
## raster::writeRaster(rLcover, tmpfiles[2])
## raster::writeRaster(rNl, tmpfiles[5])
## raster::writeRaster(rPop, tmpfiles[3])
## raster::writeRaster(rSoils, tmpfiles[4])
## ## DEMS
## raster::writeRaster(rDem, tmpfiles[6])
## gdalUtils::gdaldem('slope', tmpfiles[6], tmpfiles[7], s=111120)
## gdalUtils::gdaldem('roughness', tmpfiles[6], tmpfiles[8])

## names(tmpfiles) <- c('ACC_Weiss_1k.tif', 'LC_Visnav_1k.tif', 'NL_VIIRS_1k.tif', 'POP_LandScan_1k.tif', 'SO_USoils_1k.tif', 'DEM_SRTM_1k.tif', 'SL_SRTM_1k.tif', 'TR_SRTM_1k.tif')

## ## resample rasters to 1k
## ext <- raster::extent(spCountry)
## te <- c(ext[1], ext[3], ext[2], ext[4])
## tr <- rep(0.00833333, 2)
## tap <- TRUE
## r <- 'bilinear'
## ## mapply(gdalUtils, ...)
## sapply(names(tmpfiles)[-2], function(name, ...) {
##     gdalUtils::gdalwarp(tmpfiles[name], file.path(datadir, name), ...)
## }, te=te, tap=tap, tr=tr, r=r)
## gdalUtils::gdalwarp(tmpfiles[2], file.path(datadir, names(tmpfiles)[2]),
##                     te=te, tr=tr, tap=tap, r='near')
