## library(automap)
library(devtools)
library(parallel)
library(raster)
library(rgeos)
## library(ripums)
devtools::load_all('/data/ripums')
library(rnaturalearth)

## SETUP (note tifdir, country, dfrIpums, slGl2, and the data() function call)
workdir <- file.path(.Platform $file.sep, 'home', 'cmarmstrong', 'Desktop') # Sys.getenv('WORKDIR')
rdadir <- file.path(workdir, 'armstrong18', 'inst', 'RData')
tifdir <- file.path(workdir, 'armstrong18', 'inst', 'tif', 'ZAF') # c('PHL', 'ZAF')
country <- c('Africa') # c('Philippines', 'South Africa')
## quoSvi <- quote(dfrIpums $COUNTRY%in%c(608, 710))

## ONLY IF RK
load(file.path(rdadir, 'krgSviPHL10.RData'))
beta <- krgSviPHL10 $zHat $beta

data(ipumsZAF) # ipumsPHLYY ipumsZAF
data(stGeolev2)
## data(spGeo2PH10)
dfrIpums <- ipumsZAF # ipumsPHLYY ipumsZAF
stGl2 <- stGeolev2[stGeolev2 $CNTRY_CODE==710, ] # %in% c(608, 710)
spGl2 <- as(stGl2, 'Spatial')

## merge rasters
paths <- file.path(tifdir, list.files(tifdir, pattern='\\.tif$'))
lR <- lapply(paths, raster::raster)
aR <- sapply(lR, names)
mR <- sapply(unique(aR), '==', aR)
lR <- apply(mR, 2, function(J) {
    if(sum(J)>1) return(do.call(raster::merge, c(lR[J], tolerance=0.5)))
    else return(lR[[which(J)]])
})
## align raster
## lExt <- lapply(lR, raster::extent)
## ext <- Reduce(raster::intersect, lExt)
## lR <- lapply(lR, raster::crop, ext)
## lR <- lapply(lR, raster::setExtent, ext, keepres=TRUE)
rstack <- raster::stack(lR)

## make lc categories
## iLc <- unique(raster::values(rstack['lc']))
lc <- 'LC_Visnav_1k'
iLc <- c(6, 7, 8) # urban, ag-gen, ag-paddy
cl <- parallel::makeForkCluster(3)
lLc <- parallel::parLapply(cl, iLc, function(i) {
    r <- raster::mask(rstack[[lc]], rstack[[lc]]!=i, maskvalue=1, updatevalue=0)
    r==i ## do this directly and skip mask?
})
parallel::stopCluster(cl)

## stack rasters
nRstack <- names(rstack)
rstack <- raster::stack(rstack, lLc[[1]], lLc[[2]] | lLc[[3]]) # note OR
## c('acc', 'dem', 'lc', 'nl', 'pop', 'sl', 'so', 'tr', 'urb', 'ag')
names(rstack) <- c(nRstack, 'urban', 'ag')
## set missing values
acc <- 'ACC_Weiss_1k'
raster::values(rstack[[acc]])[raster::values(rstack[[acc]])==-9999] <- NA

## impute missing data (NOTE: this is where data might be dropped)
variable <- c('OWNERSHIP', 'ELECTRIC', 'PHONE', 'TV', 'RADIO', 'WALL', 'ROOF', 'PERSONS', 'EDATTAIN', 'MARST', 'DISABLED', 'INDGEN', 'CLASSWK')
domain_var <- c('SAMPLE', 'GEOLEV1', 'GEOLEV2') #, 'MUNIZA', 'MAGISZA') #, 'ELECTRIC', 'SEX'
iiMiss <- ripums::setMissing(dfrIpums)
iiNoMiss <- VIM::hotdeck(iiMiss, variable=variable, domain_var=domain_var)
## HAKC!!!!!
## iiMiss <- iiNoMiss[, 1:48]
## domain_var <- c('SAMPLE') #, 'GEOLEV1') #, 'GEOLEV2') ,'MUNIZA', 'MAGISZA') #, 'ELECTRIC', 'SEX'
## iiNoMiss <- VIM::hotdeck(iiMiss, variable=variable, domain_var=domain_var)

## calculate svi
## eSvi <- ripums::svi(quoSvi, TRUE) ## TRUE to select all columns
eSvi <- ripums::svi(iiNoMiss)
vSvi <- eSvi $pca $pc %*% eSvi $pca $vt[, 1]
dfrSvi <- data.frame(svi=vSvi, key=eSvi $key)
dfrSvi <- dfrSvi[complete.cases(dfrSvi), ]
## dfrIpums <- merge(dfrIpums, dfrSvi, by.x='SERIAL', by.y='key')
dfrIpums $svi <- dfrSvi $svi[match(dfrIpums $SERIAL, dfrSvi $key)]

dfrIpums <- dfrIpums[!is.na(dfrIpums $svi), ]

## calculate migration
## dfrIpums $MIG5INT <- as.numeric(dfrIpums $MIGRATE5==30)
## dfrIpums $MIG5MAJ <- as.numeric(dfrIpums $MIGRATE5>19)

## setup model
rstack <- rstack[[c('ACC_Weiss_1k', 'NL_VIIRS_1k', 'POP_LandScan_1k', 'urban')]]
names(rstack) <- c('acc', 'nl', 'pop', 'urb')
form <- svi~acc+nl+pop+urb

## aggregate svi to geolevel 2
## lWgt <- by(dfrIpums, dfrIpums $GEOLEV2, function(dfrGl2) weight('svi', dfrGl2, 'PERWT'))
## lWgtPH10 <- by(dfrIpums, dfrIpums $GEO2_PH2010, function(dfrGl2) weight('svi', dfrGl2, 'PERWT'))
lWgtGl2 <- by(dfrIpums, dfrIpums $GEOLEV2, function(dfrGl2) weight('svi', dfrGl2, 'PERWT'))
## xbar <- sapply(lWgt, function(dfrGl2) { # geolev2 weighted means
##     weighted.mean(as.numeric(dfrGl2[, 'svi']), dfrGl2 $wtFreq, na.rm=TRUE)
## })
## xbarPH10 <- sapply(lWgtPH10, function(dfrGl2) { # geolev2 weighted means
##     weighted.mean(as.numeric(dfrGl2[, 'svi']), dfrGl2 $wtFreq, na.rm=TRUE)
## })
xbarGl2 <- sapply(lWgtGl2, function(dfrGl2) { # geolev2 weighted means
    weighted.mean(as.numeric(dfrGl2[, 'svi']), dfrGl2 $wtFreq, na.rm=TRUE)
})

## center & scale
## if(center) {
## xbarGrand <- weighted.mean(as.numeric(xbar[, 1]), xbar[, 2], na.rm=TRUE)
## xbar <- xbar - xbarGrand
## }
## if(scale) xbar <- xbar / sd(xbar)

## spGl2 $svi <- xbar[match(spGl2 $GEOLEVEL2, names(xbar))]
## spGeo2PH10 $svi <- xbarPH10[match(spGeo2PH10 $IPUM2010, names(xbarPH10))]
spGl2 $svi <- xbarGl2[match(spGl2 $GEOLEVEL2, names(xbarGl2))]

## sidescale
## spGeo2PH10NoMiss <- spGeo2PH10[!is.na(spGeo2PH10 $svi), ]
## spGl2NoMiss <- spGl2[!is.na(spGl2 $svi), ]
## v <- gstat::variogram(svi~1, data=spGeo2PH10NoMiss)
## vgmSS <- gstat::fit.variogram(v, gstat::vgm('Sph'))
## krigeGeo2 <- krige0(svi~1, spGeo2PH10NoMiss, spGl2, parVgmArea, vgm=vgmSS, nnodes=28)
## missSvi <- is.na(spGl2 $svi)
## spGl2[missSvi, 'svi'] <- krigeGeo2[['pred']][missSvi]

## clip spGl2 (not necessary?)
## spClip0 <- rnaturalearth::ne_states(country=country)
## spPHL.NCR <- spClip0[spClip0 $region %in% c('National Capital Region', 'Central Luzon (Region III)', 'CALABARZON (Region IV-A)'), ]
## mNCR <- rgeos::gIntersects(spGl2, spPHL.NCR, byid=TRUE, returnDense=TRUE)
## vNCR <- apply(mNCR, 2, any)
## spNCR <- spGl2[vNCR, drop=FALSE]
## mClip0 <- rgeos::gIntersects(spGl2, spClip0, byid=TRUE, returnDense=TRUE)
## vClip0 <- apply(mClip0, 2, any)
## spClip0 <- spGl2[vClip0, drop=FALSE]

nnodes_ex <- c(rep('129.229.26.83', 12), rep('129.229.26.102', 6)) #, rep('129.229.26.100', 2))
nnodes_vgm <- c(rep('129.229.26.83', 28), rep('129.229.26.102', 22)) #, rep('129.229.26.100', 14))
stop()
krigeSvi <- ripums::krige(form, spGl2, rstack, vgm=gstat::vgm('Exp'),
                          nnodes_ex, nnodes_vgm, master='129.229.26.83', homogeneous=FALSE,
                          computeVar=TRUE, beta=beta)
## krigeSvi <- ripums::krige(form, spGeo2PH10, rstack, vgm=gstat::vgm('Exp'),
##                           nnodes_ex, nnodes_vgm, master='129.229.26.83', homogeneous=FALSE,
##                           computeVar=TRUE) #, beta=beta)
save(krigeSvi, file=file.path(workdir, 'krigeSvi.RData'))
