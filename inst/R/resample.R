library(countrycode)
library(gdalUtils)
library(raccess)
library(raster)
library(rlandscan)
library(rsrtm)
library(rvisnav)
library(rnaturalearth)
library(Rnightlights)


datadir <- file.path('tif', 'ZAF')
country <- 'South Africa'
spCountry <- rnaturalearth::ne_countries(country=country)

## VIIRS data from Rnightlights
iTile <- 5 # tile 5 contains ZAF, 3 contains PHL
ctryCodes <- 'ZAF'
## nlPeriod <- paste0('2013', sprintf("%02d", 1:12))
nlPeriod <- '2015' # Rnightlights::nlRange('201301', '201312')
nlType <- 'VIIRS.Y' # 'VIIRS.M'
nlStats <- list('mean', na.rm=TRUE)

## nlTile <- Rnightlights:::getNlTileTifLclNamePathVIIRS(nlPeriod, iTile, nlType)
## if(!file.exists(nlTile)) {
    ## sapply(nlPeriod, function(nlPeriod, ctryCodes, nlType) {
    ## tileList <- Rnightlights:::getCtryTileList(ctryCodes=ctryCodes, nlType=nlType)
    tileList <- Rnightlights:::getNlTiles(nlType='VIIRS.Y') $name
    Rnightlights:::downloadNlTiles(nlType, nlPeriod, tileList)
    ## }, ctryCodes, nlType)
## }
## broke
## Rnightlights:::getCtryNlData(ctryCodes, "highest", nlType, nlPeriod, nlStats, ignoreMissing=FALSE)
## Rnightlights::getCtryRasterOutputFnamePath(ctryCodes, nlType, nlPeriod)

rNltile <- raster::raster(nlTile)
rNl <- raster::crop(rNltile, spCountry)

## lapply(l, do.call, list(country=country))
rAccess <- raccess::access2cities(country=country)
rDem <- rsrtm::srtm(country)
rLcover <- rvisnav::visnav(country)
rPop <- rlandscan::landscan(country=country)
rSoils <- rsoils::soils(country)

## mapply(raster::writeRaster, functions, tmpfiles)
tmpfiles <- tempfile(fileext=rep('.tif', 8))
raster::writeRaster(rAccess, tmpfiles[1])
raster::writeRaster(rLcover, tmpfiles[2])
raster::writeRaster(rNl, tmpfiles[5])
raster::writeRaster(rPop, tmpfiles[3])
raster::writeRaster(rSoils, tmpfiles[4])
## DEMS
raster::writeRaster(rDem, tmpfiles[6])
gdalUtils::gdaldem('slope', tmpfiles[6], tmpfiles[7], s=111120)
gdalUtils::gdaldem('roughness', tmpfiles[6], tmpfiles[8])

names(tmpfiles) <- c('ACC_Weiss_1k.tif', 'LC_Visnav_1k.tif', 'NL_VIIRS_1k.tif', 'POP_LandScan_1k.tif', 'SO_USoils_1k.tif', 'DEM_SRTM_1k.tif', 'SL_SRTM_1k.tif', 'TR_SRTM_1k.tif')

## resample rasters to 1k
ext <- raster::extent(spCountry)
te <- c(ext[1], ext[3], ext[2], ext[4])
tr <- rep(0.00833333, 2)
tap <- TRUE
r <- 'bilinear'
## mapply(gdalUtils, ...)
sapply(names(tmpfiles)[-2], function(name, ...) {
    gdalUtils::gdalwarp(tmpfiles[name], file.path(datadir, name), ...)
}, te=te, tap=tap, tr=tr, r=r)
gdalUtils::gdalwarp(tmpfiles[2], file.path(datadir, names(tmpfiles)[2]),
                    te=te, tr=tr, tap=tap, r='near')
