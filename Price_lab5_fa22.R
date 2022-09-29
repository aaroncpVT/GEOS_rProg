#Lab 5
#Aaron Price
#9/29/2022

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
f.list <- list.files('.', pattern = '.tif') #List .tif files
f.stack <- stack(f.list) #Generate stack

n.val <- extract(f.stack, sp.pt) #Extract MODIS EVI value


