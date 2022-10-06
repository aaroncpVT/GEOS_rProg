#Lab 5
#Aaron Price
#9/29/2022

library(raster)
library(rgdal)
library(mapview)
library(ggplot2)

#Generating spatial point
pt.x <- c(-80.429361) #Get long
pt.y <- c(37.229596) #Get lat

#Read .tif for CRS
bburg <- raster('MOD13Q1.A2001001.h11v05.006.2015140082007_250m_16_days_blue_reflectance.tif')
crs <- crs(bburg) #Get CRS 

sp.pt <- SpatialPoints(cbind(pt.x,pt.y),
                       proj4string = crs) #Generate spatialpoint
mapview(sp.pt) #Mapview spatial point

#Create stack of .tif files
f.list <- list.files('.', pattern = '*_EVI.tif') #List .tif files
f.stack <- stack(f.list) #Generate stack

n.val <- extract(f.stack, sp.pt) #Extract MODIS EVI value

#Create data.frame with years and EVI values
years <- seq(as.Date('2001-01-01'), by = "16 days", length.out = 391)
df <- data.frame(matrix(nrow = 391, ncol = 2))
colnames(df) <- c('Date', 'EVI')
df$Date <- years
for(i in 1:391){
  df$EVI[i] <- n.val[,i]
}

#Create plot of timeseries 
p <- (ggplot(df, aes(Date, EVI)) + geom_point() + geom_line()
      + ggtitle('EVI Timeseries'))

p + theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))


for(year in 2001:2017){
  wc = paste0('*A', year, '*EVI.tif')
  filelist <- Sys.glob(wc)
  m <- stack(filelist)
  mv <- calc(m, fun = mean)
  output <- paste0('modis', year, '.tif')
  writeRaster(mv, output)
}
plot(mv)

filelist <- Sys.glob('modis*.tif')
m <- stack(filelist)
dim(m)
df <- getValues(m)

df.1 <- as.data.frame((m))

#t <- 1:17
t <- 1:12

df.val <- na.omit(df)

y <- df.val[1,]
lm.1 <- lm(y~t)

cm <- matrix(NA, nrow = dim(df.val)[1],2)

for(i in 1:dim(df.val)[1]){
  y <- df.val[i,]
  mod.y <- lm(y~t)
  cm[i,1] <- mod.y$coefficients[1]
  cm[i,2] <- mod.y$coefficients[2]
}

#May need to change WD back

bburg[!is.na(bburg)] <- cm[,2]
plot(bburg)
mapview(bburg)
