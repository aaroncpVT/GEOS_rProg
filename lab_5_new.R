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

#Stack files with Sys.glob
for(year in 2001:2017){
  wc = paste0('*A', year, '*EVI.tif') #Form wildcard
  filelist <- Sys.glob(wc) #List files by year
  m <- stack(filelist) #Create stack of the year
  mv <- calc(m, fun = mean) #Calculate the mean for that year
  output <- paste0('modis', year, '.tif')
  writeRaster(mv, output) #Write the full year raster out
}
plot(mv) #Base plot

filelist <- Sys.glob('modis*.tif') #List the year by year files
m <- stack(filelist) #Stack the year by year files
dim(m) #View dimensions
df <- getValues(m) #Pull EVI values

df.1 <- as.data.frame((m)) #Create df object

t <- 1:17 #Set range of years

df.val <- na.omit(df) #Filter NA values

y <- df.val[1,] # Test linear model
lm.1 <- lm(y~t)

cm <- matrix(NA, nrow = dim(df.val)[1],2) #create a template df

for(i in 1:dim(df.val)[1]){ #Linear regression for the whole image
  y <- df.val[i,]
  mod.y <- lm(y~t)
  cm[i,1] <- mod.y$coefficients[1]
  cm[i,2] <- mod.y$coefficients[2]
} #End loop w/ full data frame of linear coefficients

colnames(cm) <- c('Intercept', 'Slope') #Rename

bburg[!is.na(bburg)] <- cm[,2] #Create a template and merge to cm
plot(bburg, main = 'Timeseries Trends')
mapview(bburg)

#Histogram of results
av <- round(mean(cm[,2]),3)
sd <- round(sd(cm[,2]),3)
leg <- c((paste0('Mean = ', av)), paste0('SD = ', sd))

#Plot histogram
hist(cm[,2], 
     breaks = 20,
     main = "Distribution of Regression Slopes",
     xlab = 'Slope')
legend(x = 'topright',
       legend = leg)

#View the head of the linear model dataframe
head(cm)
write.csv(cm, 'cm.csv')
