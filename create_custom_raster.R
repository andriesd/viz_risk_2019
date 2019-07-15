library(rgdal)
library(raster)

# for raster files, go to 
# http://viewfinderpanoramas.org/Coverage%20map%20viewfinderpanoramas_org3.htm

elevation <- raster("G44/N27E083.hgt")
image(elevation)
plot(elevation, col=rgb(1:100/100, 1:30/30, 0))

elevation0 <- raster("H44/N30E080.hgt")
elevation1 <- raster("H44/N30E081.hgt")
elevation2 <- raster("H44/N29E080.hgt")
elevation3 <- raster("H44/N29E081.hgt")
elevation4 <- raster("H44/N29E082.hgt")
elevation5 <- raster("H44/N29E083.hgt")
elevation6 <- raster("H44/N28E080.hgt")
elevation7 <- raster("H44/N28E081.hgt")
elevation8 <- raster("H44/N28E082.hgt")
elevation9 <- raster("H44/N28E083.hgt")
elevation10 <- raster("G44/N27E082.hgt")
elevation11 <- raster("G44/N27E083.hgt")
elevation12 <- raster("G45/N27E084.hgt")
elevation13 <- raster("G45/N27E085.hgt")
elevation14 <- raster("G45/N27E086.hgt")
elevation15 <- raster("G45/N27E087.hgt")
elevation16 <- raster("G45/N27E088.hgt")
elevation17 <- raster("G45/N26E084.hgt")
elevation18 <- raster("G45/N26E085.hgt")
elevation19 <- raster("G45/N26E086.hgt")
elevation20 <- raster("G45/N26E087.hgt")
elevation21 <- raster("G45/N26E088.hgt")
elevation22 <- raster("H45/N28E084.hgt")
elevation23 <- raster("H45/N28E085.hgt")
elevation24 <- raster("H45/N28E086.hgt")
elevation25 <- raster("G44/N26E080.hgt")
elevation26 <- raster("G44/N26E081.hgt")
elevation27 <- raster("G44/N26E082.hgt")
elevation28 <- raster("G44/N26E083.hgt")
elevation29 <- raster("G44/N27E080.hgt")
elevation30 <- raster("G44/N27E081.hgt")
elevation31 <- raster("H45/N28E084.hgt")
elevation32 <- raster("H45/N28E085.hgt")
elevation33 <- raster("H45/N28E086.hgt")
elevation34 <- raster("H45/N28E087.hgt")
elevation35 <- raster("H45/N28E088.hgt")
elevation36 <- raster("H45/N29E084.hgt")
elevation37 <- raster("H45/N29E085.hgt")
elevation38 <- raster("H45/N29E086.hgt")
elevation39 <- raster("H45/N29E087.hgt")
elevation40 <- raster("H45/N29E088.hgt")
elevation41 <- raster("H44/N30E083.hgt")
elevation42 <- raster("H45/N30E084.hgt")
elevation43 <- raster("H45/N30E085.hgt")
elevation44 <- raster("H45/N30E086.hgt")
elevation45 <- raster("H45/N30E087.hgt")
elevation46 <- raster("H45/N30E088.hgt")
elevation47 <- raster("H44/N30E082.hgt")


x <- list(elevation0, elevation1, elevation2, elevation3, elevation4, elevation5, elevation6
          , elevation7, elevation8, elevation9, elevation10, elevation11, elevation12
          , elevation13, elevation14, elevation15, elevation16, elevation17, elevation18
          , elevation19, elevation20, elevation21, elevation22, elevation23, elevation24
          , elevation25, elevation26, elevation27, elevation28, elevation29, elevation30
          , elevation31, elevation32, elevation33, elevation34, elevation35, elevation36
          , elevation37, elevation38, elevation39, elevation40, elevation41, elevation42
          , elevation43, elevation44, elevation45, elevation46, elevation47)
x$filename <- 'test'
x$overwrite <- TRUE
m <- do.call(raster::merge, x)
m <- aggregate(m, fact=15)

writeRaster(m, 'Nepal_Elevations.tiff', overwrite=TRUE)

# for precipitation data, go to
# https://data.humdata.org/dataset/nepal-historical-annual-and-monthly-rainfall-distribution-for-monsoon-months

precip <- raster("Annual_Precip")
precip_downsized <- aggregate(precip, fact=15)
writeRaster(precip_downsized, 'Nepal_Precip', overwrite=TRUE)
