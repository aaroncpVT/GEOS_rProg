library(raster)
library(rgdal)
library(mapview)
library(rgeos)
library(RColorBrewer)

##Activity 1
nlcd<-raster('./raster/nlcd.tif')
plot(nlcd)
nlcd

pophu<-readOGR('./shapefile',layer='y2010_51_pophu')
plot(pophu)
pophu

pophu_p <- spTransform(pophu, crs(nlcd))

mydata<-data.frame(pophu_p)

mydata$area <- gArea(pophu_p, byid = TRUE)
for(i in 1:length(mydata$area)){
  mydata$area[i] <- mydata$area[i] / 1E6
  mydata$HD[i] <- mydata$HOUSING10[i] / mydata$area[i]
}

out<- merge(pophu_p,mydata,by='BLOCKID10')

mapview(out,zcol = "HD", col.regions=brewer.pal(9,"Pastel1"))

nlcd[nlcd==41|nlcd==42|nlcd==43|nlcd==52|nlcd==71|nlcd==90|nlcd==95]=1
nlcd[nlcd!=1]=0
plot(nlcd)

out$poly_id <-1:length(pophu_p)
out$zone <- 1:length(pophu_p)
pophu_raster <- rasterize(out, nlcd, field = out$poly_id)

plot(pophu_raster)

veg_mean<-zonal(nlcd, pophu_raster, fun='mean')
head(veg_mean)
colnames(veg_mean)

new <- merge(out, veg_mean, by = 'zone')
new$mean[is.na(new$mean)]<- -9999
wui <- new[new$mean>=0.5 & new$HD >= 6.17,]

plot(nlcd)
plot(wui,add=TRUE)
writeOGR(obj=wui, dsn=".", layer='wui', driver="ESRI Shapefile",overwrite_layer=T)

mapview(wui, zcol = 'mean', col.region=brewer.pal(9, 'YlGn'))

##Activity 2

va <- readOGR('./shapefile', layer: 'tabblock2010_51_pophu')
# albemare county = 003