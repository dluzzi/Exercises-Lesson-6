library(raster)
# Download, unzip and load the data
download.file(url = 'https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/landsat8.zip', destfile = 'data/landsat8.zip', method = 'auto')
unzip('data/landsat8.zip', exdir = 'data')
# Identify the right file
landsatPath <- list.files(path = 'data/', pattern = glob2rx('LC8*.grd'), full.names = TRUE)
wagLandsat <- brick(landsatPath)

# First part - Wageningen -------------------------------------------------


# plotRGB does not support negative values, so that they need to be removed
wagLandsat[wagLandsat < 0] <- NA
plotRGB(wagLandsat, 5, 4, 3)

# Download municipality boundaries
nlCity <- getData('GADM',country='NLD', level=3, path = 'data/')
class(nlCity)
# Investigate the structure of the object
head(nlCity@data)

wagContour <- nlCity[nlCity$NAME_2 == 'Wageningen',]

# Load rgdal library (needed to reproject data)
library(rgdal)

wagContourUTM <- spTransform(wagContour, CRS(proj4string(wagLandsat)))

wagLandsatCrop <- crop(wagLandsat, wagContourUTM)
wagLandsatSub <- mask(wagLandsat, wagContourUTM)

# Set graphical parameters (one row and two columns)
opar <- par(mfrow=c(1,2))
plotRGB(wagLandsatCrop, 5, 4, 3, main = 'Crop()')
plotRGB(wagLandsatSub, 5, 4, 3, main = 'Mask()')
plot(wagContourUTM, add = TRUE)
# Reset graphical parameters
par(opar)

download.file(url = 'https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/wageningenWater.zip', destfile = 'data/wageningenWater.zip', method = 'auto')
unzip('data/wageningenWater.zip', exdir = 'data')
# Check the names of the layers for input in readOGR()
ogrListLayers('data/Water.shp')
water <- readOGR('data/Water.shp', layer = 'Water')
waterUTM <- spTransform(water, CRS(proj4string(wagLandsat)))

wagLandsatSubW <- mask(wagLandsatSub, mask = waterUTM, inverse = TRUE)
plotRGB(wagLandsatSubW, 5, 4, 3)
plot(waterUTM, col = 'blue', add = TRUE)


#waterContour <- spTransform(water, CRS(proj4string(wagContour)))
#writeOGR(wagContour, file.path("data","wagContour.kml"), 
 #        "wagContour", driver="KML", overwrite_layer=TRUE)
#writeOGR(waterContour, file.path("data","water.kml"), 
    #     "water", driver="KML", overwrite_layer=TRUE)

# Change to the correct file path and layer name
samples <- readOGR('data/ClassificationPoints.kml', layer = 'ClassificationPoints')

# Re-project SpatialPointsDataFrame
samplesUTM <- spTransform(samples, CRS(proj4string(wagLandsatCrop)))
# The extract function does not understand why the object would have 3 coord columns, so we need to edit this field
samplesUTM@coords <- coordinates(samplesUTM)[,-3]
# Extract the surface reflectance 
calib <- extract(wagLandsatCrop, samplesUTM, df=TRUE)
# Combine the newly created dataframe to the description column of the calibration dataset
calib2 <- cbind(samplesUTM$Description, calib)
# Change the name of the first column, for convienience
colnames(calib2)[1] <- 'lc'
# Inspect the structure of the dataframe
str(calib2)

library(randomForest)

# Calibrate model
model <- randomForest(lc ~ band1 + band2 + band3 + band4 + band5 + band6 + band7, data = calib2)
# Use the model to predict land cover
lcMap <- predict(wagLandsatCrop, model = model)

library(rasterVis)

levelplot(lcMap, col.regions = c('green', 'brown', 'lightgreen', 'darkgreen', 'grey', 'blue'))


# Second part - Belgium ---------------------------------------------------------------------

# Download data
bel <- getData('alt', country='BEL', mask=TRUE, path = 'data/')
# Display metadata
bel

plot(bel)
line <- drawLine()

alt <- extract(bel, line, along = TRUE)
plot(alt[[1]], type = 'l')

library(geosphere)
# Calculate great circle distance between the two ends of the line
dist <- distHaversine(coordinates(line)[[1]][[1]][1,], coordinates(line)[[1]][[1]][2,])
# Format a vector for use as x axis index
distanceVector <- seq(0, dist, along.with = alt[[1]])

# Visualize the output
# Set graphical parameters (grid with 2 rows and 1 column)
opar <- par(mfrow = c(2,1))
plot(bel, main = 'Altitude (m)')
plot(line, add = TRUE)
plot(distanceVector/1000, alt[[1]], type = 'l',
     main = 'Altitude transect Belgium',
     xlab = 'Distance (Km)',
     ylab = 'Altitude (m)',
     las = 1)
# Reset graphical parameters to default
par(opar)

