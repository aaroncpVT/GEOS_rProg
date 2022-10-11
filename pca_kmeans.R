library(raster)
library(rgdal)
library(sp)
#r <- getData("worldclim",var="bio",res=10)

#list all tif files and then stack them to a rasterstack object
filelist<-Sys.glob('*.tif')
r<-stack(filelist)

#crop data to usa boundary
states<-readOGR('.',layer='states')
rsub<-crop(r,states)
plot(rsub)

#convert to a dataframe 
df<-getValues(rsub)
#sample data (4 variables and 1000 observations)
dfsub<-df[sample(nrow(df), 1000),c(1,5,6,12)]
dfsub<-na.omit(dfsub)
pairs(dfsub)
#Correlation matrix
cor(dfsub)

#PCA transformation for 4 variables
pca <- prcomp(dfsub, scale. = TRUE)
#PCA components are saved as pca$x
pairs(pca$x)

#PCA transformation or rotation coefficients
pca$rotation

#importance of pca components
m<-summary(pca)
mi<-m$importance
#plot cummulative fraction variance 
plot(mi[3,], type="b", col=6,
     ylab="Cummulative fraction variance", xlab="Principle Component")



#for the entire dataset (df)
df <- na.omit(df)
pca <- prcomp(df, scale. = TRUE)
m.df <- summary(pca.df)
mi.df <- m.df$importance
plot(mi.df[3,], type="b", col=6,
     ylab="Cummulative fraction variance", 
     xlab="Principle Component")

#map PCA components
template<-rsub[[1]]
#PC1 map
pc<-pca$x[,1]
template[!is.na(template)]<-pc[!is.na(pc)]
plot(template)
#PC2 map
pc<-pca$x[,2]
template[!is.na(template)]<-pc[!is.na(pc)]
plot(template)
#PC3 map
pc<-pca$x[,3]
template[!is.na(template)]<-pc[!is.na(pc)]
plot(template)

#generate a PCA stack and write it out as .tif?


#use rasterPCA for rasterstack object
#install.packages('RStoolbox')
library(RStoolbox)
m<-rasterPCA(rsub,spca=T,maskCheck = TRUE)

#k-means
n=12
k <- kmeans(pca$x[,1:8], centers = n, nstart = 25)
k_label<-k$cluster
template[!is.na(template)]<-k_label[!is.na(k_label)]
plot(template)

ras <- raster('./hyperspectral/hyperspectral.tif')
df <- getValues(ras)
df <- na.omit(df)
pca <- prcomp(df, scale. = TRUE)
