#' Example of armstrong18
#'
#' Dasymetrically maps and builds a surface buffer upon IPUMS-I data
#' @export
example <- function() { # this should be a shiny app that allows users to click and set start points
    data(stGeolev2) # necessary?
    spGeolev2 <- as(stGeolev2, 'Spatial')
    rPop <- rlandscan::landscan(geom=spGeolev2)
    raster::contour(rPop)
    m <- matrix(c(5,7,5,7,7,7,5,7,5)/63, nrow=3)
    rA <- raster::focal(rPop, m)
    names(rA) <- 'lspop2010A'
    rstack <- raster::stack(rPop, rA)
    rhs <- "lspop2010+I(lspop2010^2)+lspop2010A+I(lspop2010A^2)"
    r <- ripums::dasymetric(rstack, "EDATTAIN", rhs, c("0", "9"))
    p <- raster::rasterToPolygons(r)
    buffy::surfBuff(p,,)
}
