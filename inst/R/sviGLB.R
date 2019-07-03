library(devtools)
library(parallel)
library(raster)
library(reshape2)
library(rgdal)
library(rgeos)
## library(ripums)
devtools::load_all('/data/ripums')
## devtools::load_all('C:\\Users\\omnia\\OneDrive\\Documents\\GitHub\\ripums')
library(sp)

## SETUP
## Sys.getenv('WORKDIR')
## workdir <- file.path(.Platform $file.sep, 'home', 'cmarmstrong', 'Desktop')
workdir <- getwd()
datadir <- file.path(workdir, 'armstrong18', 'inst')
rdadir <- file.path(datadir, 'RData')
tifdir <- file.path(datadir, 'tif') # c('PHL', 'ZAF')
shpdir <- file.path(datadir, 'shp')
csvdir <- file.path(datadir, 'csv')

## HDI
hdi <- read.csv(file.path(csvdir, 'GDL-Sub-national-HDI-data.csv'))
hdi <- melt(hdi, id.vars=names(hdi)[1:5], variable.name='year', value.name='hdi')
hdi $year <- substr(hdi $year, 2, 5)
spHdi <- readOGR(file.path(shpdir, 'GDL-SHDI-SHP-2.shp'))
spHdi[which(spHdi $GDLCode=="#N/A"), ] <- NA
spHdi $iso_code <- as.character(spHdi $iso_code)
lgc <- with(spHdi @data, !is.na(GDLCode) & is.na(iso_code))
spHdi[lgc, 'iso_code'] <- substr(spHdi[lgc, ] $GDLCode, 1, 3)
spHdi $iso_code <- as.factor(spHdi $iso_code)
spHdi <- spHdi[!is.null(spHdi $iso_code), ]
spHdi <- spHdi[!is.na(spHdi $GDLCode), ]

gdlcode <- unique(spHdi $GDLCode)
srl <- sapply(unique(spHdi $GDLCode), function(i) gUnaryUnion(spHdi[spHdi $GDLCode==i, ]))
spHdi <- do.call(rbind, c(srl, makeUniqueIDs=TRUE))
pid <- sapply(slot(spHdi, "polygons"), function(x) slot(x, "ID"))
spHdi <- SpatialPolygonsDataFrame(spHdi, data.frame(GDLCode=gdlcode, row.names=pid))
spHdi $iso_code <- substr(spHdi $GDLCode, 1, 3)

spHdiPH <- spHdi[spHdi $iso_code=='PHL', ]
spHdiPH00 <- merge(spHdiPH, hdi[hdi $year==2000, ], by.x='GDLCode', by.y='GDLCODE')
spHdiPH10 <- merge(spHdiPH, hdi[hdi $year==2010, ], by.x='GDLCode', by.y='GDLCODE')

## SVI
load(file.path(rdadir, 'krgSviPHL10.RData'))
load(file.path(rdadir, 'krgSviPHL00.RData'))
krgSviPHL00 <- krgSviPHL
rm(krgSviPHL)

spHdiPH00 $svi <- raster::extract(krgSviPHL00 $r[['pred']], spHdiPH, fun=mean, na.rm=TRUE)
spHdiPH10 $svi <- raster::extract(krgSviPHL10 $r[['pred']], spHdiPH, fun=mean, na.rm=TRUE)
spHdiPH <- rbind(spHdiPH00, spHdiPH10)

## svi~hdi
## svi <- cbind(svi, spHdiPHL @data)
# dfr <- merge(hdi[hdi $year==2010, ], svi, by.x='GDLCODE', by.y='GDLCode') # svi contains only PHL
lmHdi <- lm(svi~hdi, data=spHdiPH)
hdi $svi <- predict(lmHdi, newdata=hdi)

## svi~...
hdi10 <- hdi[hdi $year==2010, ]
src <- merge(spHdi, hdi10, by.x='GDLCode', by.y='GDLCODE')

## get NL here...
## nlPeriod <- '2015' # Rnightlights::nlRange('201301', '201312')
## nlType <- 'VIIRS.Y' # 'VIIRS.M'
## nlStats <- list('mean', na.rm=TRUE)

## tileList <- Rnightlights:::getNlTiles(nlType='VIIRS.Y') $name
## Rnightlights:::downloadNlTiles(nlType, nlPeriod, tileList) # imperative

## resample
## rLc <- raster(system.file('extdata', 'visnav.vrt', package='rvisnav'))
## iLc <- c(6, 7, 8) # urban, ag-gen, ag-paddy
## cl <- parallel::makeForkCluster(3)
## lLc <- parallel::parLapply(cl, iLc, function(i) {
##     r <- raster::mask(rLc, rLc!=i, maskvalue=1, updatevalue=0)
##     r==i ## do this directly and skip mask?
## })
## parallel::stopCluster(cl)
## rAg <- lLc[[2]] | lLc[[3]]
## writeRaster(lLc[[1]], file.path(tifdir, 'urb.tif'))
## writeRaster(rAg, file.path(tifdir, 'ag.tif'))

## ag=file.path(tifdir, 'ag.tif'),
## urb=file.path(tifdir, 'urb.tif'),
ch <- c(lcover.tif=system.file('extdata', 'visnav.vrt', package='rvisnav'),
        ## dem.tif=system.file('extdata', 'srtm.vrt', package='rsrtm'),
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
