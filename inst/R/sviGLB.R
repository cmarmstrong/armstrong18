library(devtools)
library(parallel)
library(raster)
library(reshape2)
library(rgdal)
library(rgeos)
## library(ripums)
## devtools::load_all('/data/ripums')
devtools::load_all('C:\\Users\\omnia\\OneDrive\\Documents\\GitHub\\ripums')
library(rnaturalearth)

## SETUP
## workdir <- file.path(.Platform $file.sep, 'home', 'cmarmstrong', 'Desktop') # Sys.getenv('WORKDIR')
workdir <- getwd()
datadir <- file.path(workdir, 'armstrong18', 'inst')
rdadir <- file.path(datadir, 'RData')
tifdir <- file.path(datadir, 'tif', 'PHL') # c('PHL', 'ZAF')
shpdir <- file.path(datadir, 'shp')
csvdir <- file.path(datadir, 'csv')
country <- c('Philippines') # c('Philippines', 'South Africa')

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

srl <- sapply(unique(spHdi $GDLCode), function(i) spI <- gUnaryUnion(spHdi[spHdi $GDLCode==i, ]))
SpatialPolygons(sapply(srl, slot, 'polygons'))
spHdi <- SpatialPolygons(srl)

spHdiPHL <- spHdi[spHdi $ISO2=='PH', ]

load(file.path(rdadir, 'krgSviPHL10.RData'))
load(file.path(rdadir, 'krgSviPHL00.RData'))

svi <- raster::extract(krgSviPHL10 $r[['pred']], spHdiPHL, fun=mean, na.rm=TRUE)
svi <- cbind(svi, spHdiPHL @data)

dfr <- merge(hdi[hdi $year==2010, ], svi, by.x='GDLCODE', by.y='GDLCode') # svi contains only PHL

