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
library(tools)

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

## SVI~HDI
## svi <- cbind(svi, spHdiPHL @data)
# dfr <- merge(hdi[hdi $year==2010, ], svi, by.x='GDLCODE', by.y='GDLCode') # svi contains only PHL
lmHdi <- lm(svi~hdi, data=spHdiPH)
hdi $svi <- predict(lmHdi, newdata=hdi)

## RASTER
tifs <- c('urb.tif', 'ag.tif', 'dem.tif', 'weiss2.tif', 'lscan2.tif', 'nlites.tif')
rstack <- raster::stack(lapply(file.path(tifdir, tifs), raster))
names(rstack) <- file_path_sans_ext(tifs)
raster::values(rstack[['weiss2']])[raster::values(rstack[['weiss2']])==-9999] <- NA
stop()
## SVI~RSTACK
form <- svi~weiss2+nlites+lscan2+urb
hdi10 <- hdi[hdi $year==2010, ]
src <- merge(spHdi, hdi10, by.x='GDLCode', by.y='GDLCODE')
krigeSvi <- ripums::krige(form, spGl2, rstack, vgm=gstat::vgm('Exp'),
                          nnodes_ex, nnodes_vgm, master='129.229.26.83', homogeneous=FALSE,
                          computeVar=TRUE) #, beta=beta)
