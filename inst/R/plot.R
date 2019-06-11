load('svi.Rdata')
rMM <- raster::crop(eSvi $r, spBbox)
rSviHi <- rMM > 0.4
spSviHi <- raster::rasterToPolygons(rSviHi)
stSviHi <- sf::as_sf(spSviHi)
lGeolev2Hi <- sf::st_intersects(stGeolev2, stSviHi[stSviHi $layer==1, ], sparse=FALSE)

raster::plot(rMM, col=rev(brewer.pal(11, 'RdBu'))[4:11])
plot(sf::st_geometry(stGeolev2[lGeolev2Hi,]), border='grey', lwd=1, add=TRUE)