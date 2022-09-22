library(raster)
library(rgdal)
library(randomForest)
library(mapview)

training_poly<-readOGR(dsn='.',layer = 'trainingsamples')

f.list <- list.files('.', pattern=('*.tif$'))
landsat <- stack(f.list)

plot(landsat)

B_template<-raster('b5.tif')
plot(B_template)

t_raster <- rasterize(training_poly, B_template, field = training_poly$classid)
plot(t_raster)
mapview(t_raster)

alldata <- addLayer(landsat, t_raster)
plot(alldata)

alldata.df <- as.data.frame(alldata)
cols <- c('band2', 'band3', 'band4', 'band5', 'band6', 'band7', 'y')
colnames(alldata.df) <- cols

alltraining <- na.omit(alldata.df)
plot(alltraining[,3], alltraining[,4])

rftree <- randomForest(as.factor(y) ~. , data = alltraining)
rftree.pred = predict(rftree, alldata.df)

output <- setValues(B_template, rftree.pred)
plot(output)

writeRaster(output, filename = 'landcoverNEW.tif', format = 'GTif',
            overwrite = TRUE)

answers <- data.frame(nrow = 5, ncol = 1)
new <- freq(output)
list.a <- c(0,0,0,0,0)
for(i in 1:5){
  list.a[i] <- (new[i,2] * 900) / 1E6
}

NDVI <- (landsat$b5 - landsat$b4) / (landsat$b5 + landsat$b4)
plot(NDVI)

list.a
