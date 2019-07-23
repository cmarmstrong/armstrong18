rdadir <- file.path(workdir, 'armstrong18', 'inst', 'RData')


load('svi.Rdata')
rMM <- raster::crop(eSvi $r, spBbox)
rSviHi <- rMM > 0.4
spSviHi <- raster::rasterToPolygons(rSviHi)
stSviHi <- sf::as_sf(spSviHi)
lGeolev2Hi <- sf::st_intersects(stGeolev2, stSviHi[stSviHi $layer==1, ], sparse=FALSE)
raster::plot(rMM, col=rev(brewer.pal(11, 'RdBu'))[4:11])
plot(sf::st_geometry(stGeolev2[lGeolev2Hi,]), border='grey', lwd=1, add=TRUE)


load(file.path(rdadir, 'krgSviZAF00.RData'))
colBluWhtRed <- colorRampPalette(c('blue', 'white', 'red'))
plot(krgSviZAF00 $r[['pred']], col=colBluWhtRed(256)[54:256], colNA='grey90')
plot(krgSviZAF00 $r[['pred']], col=colBluWhtRed(256)[54:256], colNA='grey90', xlim=c(24.5, 26.5), ylim=c(-34.5, -32.5))
plot(krgSviZAF00 $r[['pred']], col=colBluWhtRed(256)[54:256], colNA='grey90', xlim=c(17.5, 20), ylim=c(-35, -33))


sfGLB <- st_read('h:/tmp20190617/migza.shp/migza.shp')
sfZAF <- sfGLB[sfGLB $CNTRY_CODE==710, ]
ggMigMi <- ggplot() + geom_sf(data=sfZAF, aes(fill=MIGMI))
ggMigMa <- ggplot() + geom_sf(data=sfZAF, aes(fill=MIGMA))
gridExtra::grid.arrange(ggMigMi, ggMigMa + scale_fill_gradient(low="#2a1342", high="#b257f7"), nrow=1)
