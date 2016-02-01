########
# Species Distribution models for Pin-Tailed Whydah
# Written by Robert Pecchia
# Hunter College, The City College of New York
# Winter/Spring 2015-2016

#install.packages(c("spThin","ENMeval","dismo","rJava","jsonlite","fields","maptools","devtools","scales","dplyr","ecospat"))
#install.packages('/Library/gurobi650/mac64/R/gurobi_6.5-0.tgz', repos=NULL)
#install.packages("ggbiplot")
setwd("~/Desktop/Whydah Project/whydah/Data")
#load("~/Desktop/Whydah Project/whydah/R/whydah_workspace.RData")
options(java.parameters = "-Xmx1g" )
Sys.setenv(NOAWT=TRUE)
library(rJava)
library(gurobi)
library(spThin)
library(ENMeval)
library(dismo)
library(jsonlite)
library(sp)
library(fields)
library(rgdal)
library(maptools)
library(devtools)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(rgbif)
library(dplyr)
library(tmap)
library(scales)
library(ecospat)
library(ggbiplot)


#Full occurrence dataset####
#ptw<-gbif('Vidua', 'macroura', geo=T, removeZeros = T)
#save(ptw, file="ptw.rdata")
load("ptw.rdata")
head(ptw)
dim(ptw)
ptw<-ptw[,c('lon','lat','country','species')]
ptw<-subset(ptw, !is.na(lat) & !is.na(lon))
head(ptw)
ptw.unique<- distinct(select(ptw,lon,lat,country,species)) #remove duplicates
dim(ptw.unique)
head(ptw.unique)

# Removing outliers
unique(ptw.unique$country) #Remove Taiwan Whydahs
ptw.unique<-filter(ptw.unique, country !=c("Taiwan")) #get rid of Taiwan sightings.
# From many years ago, and not over a consistent amount of years
# click()
filter(ptw.unique, lon<(-80) & lat>30 & country=="United States") #find Chicago point
which(ptw.unique$lon == -82.9989, ptw.unique$lat== 39.96110) #find it in the data.frame
ptw.unique<- ptw.unique[-4493,] #remove that row!

filter(ptw.unique, lon<(-122) & lat>35 & country=="United States") #find San Fran point
which(ptw.unique$lon == -122.511, ptw.unique$lat== 37.7777)
ptw.unique<-ptw.unique[-1312,] #remove san fran point

# only complete cases
ptw.unique<-ptw.unique[complete.cases(ptw.unique),]
dim(ptw.unique)
is.na(ptw.unique) #no NAs in df

# check map
data("wrld_simpl")
plot(wrld_simpl)
points(ptw.unique, col="red")
dim(ptw.unique)

# Clean/Organize Data####
lonzero = subset(ptw.unique, lon==0) #any points have longitude that was auto-set to 0
lonzero #All OK
duplicated(ptw.unique) #any duplicates?

# cross checking our occurence points by means of a spatial query####
# countries<-getData("countries") #maps we used in this exercise were a bit crude, so can use
# this function to get highly detailed maps
# these lines of code will create object "over" that shows all countries where our
# points fall
# coordinates(ptw.unique) <- ~lon+lat
# crs(ptw.unique) <- crs(countries)
# class(ptw.unique)
# ovr <- over(ptw.unique, countries)
# cntr <- ovr$NAME #if we see any NAs, that means they're mapped to the ocean
# AND which countires are recorded here, that are different from their record included in GBIF
# i <- which(is.na(cntr))
# i #When I switched to "countries" dataset, didn't lose any points
# j <- which(cntr != ptw.unique$country) #this asks which of our counties, doesn't align w/ GBIFs countires
# j
# for the mismatches, bind the country names of the polygons and points
# cbind(cntr, ptw.unique$country)[j,] #here's all the spots we have mismatches
# plot(wrld_simpl, add=T, border= "blue" , lwd=1)
# points(ptw.unique[j, ], col="red" , pch=20, cex=.75)

# spThin####
setwd("~/Desktop/Whydah Project/whydah/Output") #running from mac
# set coordinate system
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
# then thin
thin1 <-spThin(
  ptw.unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 10000,
  method= "gurobi", #can change to "gurobi" to make it even faster, but have to install it first
  great.circle.distance=TRUE)
summary(thin1)
str(thin1)
plot(thin1) #to visualize which records we kept after thinning
# hexagons show the distribution of all the occurrence records and the 
# red points show the location of the retained records.
thin1@call


# Saving the thinned file####
# print temporary dir
print(tempdir())
write.SpThin(
  thin1,
  coords=FALSE,
  dir=tempdir()
)
#can elect to read in .csv of all thinned points
thin_ptw2<-read.csv("thin_0001.csv", head=T)
head(thin_ptw2)
thin_ptw2_coords<-thin_ptw2[,1:2]

data("wrld_simpl")
plot(wrld_simpl)
points(thin_ptw2_coords)

#calculate bounding box
min(thin_ptw2_coords$lon)
max(thin_ptw2_coords$lon)
min(thin_ptw2_coords$lat)
max(thin_ptw2_coords$lat)
#bounding box is... y (lat) min = -34.8324, ymax=41.526  /  x (lon) min = -118.808, xmax = 55.4539

#OCW Occurrence Points####
setwd("~/Desktop/Whydah Project/whydah/Data") #set back to data directory

#OCW Points####
#ocw<-gbif('Estrilda', 'melpoda', geo=T, removeZeros = T)
#save(ocw, file="ocw.rdata")
load("ocw.rdata")
head(ocw)
dim(ocw)

ocw<-ocw[,c('lon','lat','country','species')]
ocw<-subset(ocw, !is.na(lat) & !is.na(lon))
head(ocw)
ocw.unique<- distinct(select(ocw,lon,lat,country,species)) #remove duplicates
dim(ocw.unique)
head(ocw.unique)

#Removing outliers by country
plot(wrld_simpl)
points(ocw.unique, col="red")

unique(ocw.unique$country)
ocw.unique<-filter(ocw.unique, country !="Canada") #Remove Canada
ocw.unique<-filter(ocw.unique, country !="Germany") #Remove Germany
unique(ocw.unique$country)

#Remove outliers by lon/lat
#click()
filter(ocw.unique, lon>(-100) & lon<(-89) & country=="United States") #find northern midwest point
which(ocw.unique$lon == -95.55926, ocw.unique$lat== 29.69430) #find it in the data.frame
ocw.unique<- ocw.unique[-404,] #remove that row!

filter(ocw.unique, lon>(-100) & lon<(-89) & country=="United States") #find 2nd midwest points
points(-96.67213,40.80549,col="green") #make sure it's the right one
which(ocw.unique$lon == -96.67213, ocw.unique$lat== 40.80549) #find it in the data.frame
ocw.unique<- ocw.unique[-1027,] #remove that row!

filter(ocw.unique, lon>(-100) & lon<(-89) & country=="United States") #find 3rd midwest points
points(-96.67964,40.80004,col="purple") #make sure it's the right one
which(ocw.unique$lon == -96.67964, ocw.unique$lat== 40.80004) #find it in the data.frame
ocw.unique<- ocw.unique[-1081,] #remove that row!

filter(ocw.unique, lon>(-87) & lat>(36) & country=="United States") #find DC point
points(-77.09590,38.75890,col="purple") #make sure it's the right one
which(ocw.unique$lon == -77.09590, ocw.unique$lat== 38.75890) #find it in the data.frame
ocw.unique<- ocw.unique[-607,] #remove that row!

points(-77.10650,38.75890,col="green") #make sure it's the right one
which(ocw.unique$lon == -77.10650, ocw.unique$lat== 38.75890) #find it in the data.frame
ocw.unique<- ocw.unique[-612,] #remove that row!

points(-83.14313,42.47483,col="green") #make sure it's the right one
which(ocw.unique$lon == -83.14313, ocw.unique$lat== 42.47483) #find it in the data.frame
ocw.unique<- ocw.unique[-955,] #remove that row!

filter(ocw.unique, lon<(-120) & lat>35 & country=="United States") #find 1st San Fran point
which(ocw.unique$lat == 38.57520)
points(-121.4675,38.57520,col="red")
ocw.unique<-ocw.unique[-858,]

#find 2nd San Fran point
which(ocw.unique$lat == 41.96554)
ocw.unique<-ocw.unique[-1057,] #remove san fran point

#re-check ocw points
plot(wrld_simpl)
points(ocw.unique,col="red")

#only complete cases
ocw.unique<-ocw.unique[complete.cases(ocw.unique),]
dim(ocw.unique)

#checking for any zero values
lonzero = subset(ocw.unique, lon==0) #any points have longitude that was auto-set to 0
lonzero
duplicated(ocw.unique) #any duplicates?

# Check for outliers
# coordinates(ocw.unique) <- ~lon+lat
# crs(ocw.unique) <- crs(countries)
# class(ocw.unique)
# ovr <- over(ocw.unique, countries)
# cntr <- ovr$NAME
# #i <- which(is.na(cntr))
# i
# j <- which(cntr != ocw.unique$country)
# j
# cbind(cntr, ocw.unique$country)[j,]
# plot(ocw.unique)
# plot(wrld_simpl, add=T, border= "blue" , lwd=1)
# points(ocw.unique[j, ], col="red" , pch=20, cex=.75)

#Thin OCW ####
setwd("~/Desktop/Whydah Project/whydah/Output")
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

thin_ocw <-spThin(
  ocw.unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 10000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_ocw)
str(thin_ocw)
plot(thin_ocw)

#Saving the thinned file
print(tempdir())
write.SpThin(
  thin_ocw,
  coords=FALSE,
  dir=tempdir()
)

#can elect to read in .csv of all thinned points
thin_ocw2<-read.csv("thin_0001.csv", head=T)
head(thin_ocw2) #Always check to make sure this shows correct species

#Common Waxbill Points####
setwd("~/Desktop/Whydah Project/whydah/Data") #back to data directory
#cw<-gbif('Estrilda', 'astrild', geo=T, removeZeros = T)
#save(cw, file="cw.rdata")
load("cw.rdata")
head(cw)
dim(cw)
cw<-cw[,c('lon','lat','country','species')]
cw<-subset(cw, !is.na(lat) & !is.na(lon))
cw<-subset(cw, lat%%1>0 & lon%% 1>0) #Why do we use this?
head(cw)
cw.unique<- distinct(select(cw,lon,lat,country,species)) #remove duplicates
dim(cw.unique)
head(cw.unique)

#Removing Common Waxbill outliers by country
plot(wrld_simpl)
points(cw.unique, col="red")
cw.unique<-cw.unique[complete.cases(cw.unique),]


cw.unique<-filter(cw.unique, country !="Canada")#Remove Canada
cw.unique<-filter(cw.unique, country !="United Arab Emirates") #remove UAE (listed as escapes)
unique(cw.unique$country)

#Remove Common Waxbill outliers by lon/lat
#click()
filter(cw.unique, lon<(-72) & lat>(35)) #find northern midwest point
points(-77.22568,38.97612)
which(cw.unique$lon == -77.22568, cw.unique$lat== 38.97612) #find it in the data.frame
cw.unique<- cw.unique[-6050,] #remove that row!

#Check Common Waxbill Points
plot(wrld_simpl)
points(cw.unique, col="red")

#Clean Up the Data
lonzero = subset(cw.unique, lon==0) #any points have longitude that was auto-set to 0
lonzero #all OK
duplicated(cw.unique) #any duplicates?

# coordinates(cw.unique) <- ~lon+lat
# crs(cw.unique) <- crs(countries)
# class(cw.unique)
# ovr <- over(cw.unique, countries)
# cntr <- ovr$NAME
# i <- which(is.na(cntr))
# i
# j <- which(cntr != cw.unique$country)
# j
# cbind(cntr, cw.unique$country)[j,] 
# plot(cw.unique)
# plot(wrld_simpl, add=T, border= "blue" , lwd=1)
# points(cw.unique[j, ], col="red" , pch=20, cex=.75)

#thin Common Waxbill
setwd("~/Desktop/Whydah Project/whydah/Output")
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

dim(cw.unique)
thin_cw <-spThin(
  cw.unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 10000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_cw)
str(thin_cw)
plot(thin_cw)

#Saving the thinned file
print(tempdir())
write.SpThin(
  thin_cw,
  coords=FALSE,
  dir=tempdir()
)

#can elect to read in .csv of all thinned points
thin_cw2<-read.csv("thin_0001.csv", head=T)
head(thin_cw2)

# #Nutmeg Mannakin
# nutmeg<-gbif('Lonchura', 'punctulata', geo=T, removeZeros = T)
# save(nutmeg, file="nutmeg.rdata")
load("nutmeg.rdata")
head(nutmeg)
dim(nutmeg)

nutmeg<-nutmeg[,c('lon','lat','country','species')]
nutmeg<-subset(nutmeg, !is.na(lat) & !is.na(lon))
head(nutmeg)
nutmeg.unique<- distinct(select(nutmeg,lon,lat,country,species)) #remove duplicates
dim(nutmeg.unique)
head(nutmeg.unique)

# Removing outliers by country
plot(wrld_simpl)
points(nutmeg.unique, col="red")

unique(nutmeg.unique$country)
nutmeg.unique<-filter(nutmeg.unique, country !="Canada") #Remove Canada
nutmeg.unique<-filter(nutmeg.unique, country !="Honduras") #Remove Honduras
unique(nutmeg.unique$country)

# Remove outliers by lon/lat...for nutmegs these include museum speciments and acoustic labs in US
# click()
points(-94, 38, col='green') #This is acoustic lab point
which(nutmeg.unique$lon == -94, nutmeg.unique$lat== 38) #find it in the data.frame
nutmeg.unique<- nutmeg.unique[-9269,] #remove that row!

filter(nutmeg.unique, lon>(-90) & lat>(40) & country=="United States") #find midwest points
points(-83.13383, 42.68063,col="green") #make sure it's the right one
which(nutmeg.unique$lon == -83.13383, nutmeg.unique$lat== 42.68063) #find it in the data.frame
nutmeg.unique<- nutmeg.unique[-8057,] #remove that row!
points(-83.72634, 42.27084,col="green") #make sure it's the right one
which(nutmeg.unique$lon == -83.72634, nutmeg.unique$lat== 42.27084) #find it in the data.frame
nutmeg.unique<- nutmeg.unique[-8857,] #remove that row!

filter(nutmeg.unique, lon>(-90) & lat>(38) & country=="United States") #last midwest point
points(-83.0189, 39.9961,col="green")
which(nutmeg.unique$lon == -83.0189, nutmeg.unique$lat== 39.9961) #find it in the data.frame
nutmeg.unique<- nutmeg.unique[-9114,] #remove that row!

#re-check points for more outliers
plot(wrld_simpl)
points(nutmeg.unique, col="red")

#only complete cases
nutmeg.unique<-nutmeg.unique[complete.cases(nutmeg.unique),]
dim(nutmeg.unique)

#checking for any zero values
lonzero = subset(nutmeg.unique, lon==0) #any points have longitude that was auto-set to 0
lonzero
which(duplicated(nutmeg.unique)==TRUE) #any duplicates?

# #check for outliers
# coordinates(nutmeg.unique) <- ~lon+lat
# crs(nutmeg.unique) <- crs(countries)
# class(nutmeg.unique)
# ovr <- over(nutmeg.unique, countries)
# cntr <- ovr$NAME
# i <- which(is.na(cntr))
# i
# j <- which(cntr != nutmeg.unique$country)
# j
# cbind(cntr, nutmeg.unique$country)[j,]
# plot(nutmeg.unique)
# plot(wrld_simpl, add=T, border= "blue" , lwd=1)
# points(nutmeg.unique[j, ], col="red" , pch=20, cex=.75)

#Thin Nutmeg ####
setwd("~/Desktop/Whydah Project/whydah/Output")
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

thin_nutmeg <-spThin(
  nutmeg.unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 10000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_nutmeg)
str(thin_nutmeg)
plot(thin_nutmeg)

#Saving the thinned file
print(tempdir())
write.SpThin(
  thin_nutmeg,
  coords=FALSE,
  dir=tempdir()
)

# can elect to read in .csv of all thinned points
thin_nutmeg2<-read.csv("thin_0001.csv", head=T)
head(thin_nutmeg2) #Always check to make sure this shows correct species

# Back to data directory one more time
setwd("~/Desktop/Whydah Project/whydah/Data")

# setting up background points according to Hijmans and Elith####
# Also, we have our thinned occurrence sets
thin_ptw2
thin_cw2
thin_ocw2
thin_nutmeg2

####

#Preparing Presence/Absence Rasters####

####

#Function for Presence/Absence Rasters for Host Species by Amy Whitehead
presence.absence.raster <- function (mask.raster,species.data,raster.label="") {
  require(raster)
  
  # set the background cells in the raster to 0
  mask.raster[!is.na(mask.raster)] <- 0
  
  #set the cells that contain points to 1
  speciesRaster <- rasterize(species.data,mask.raster,field=1)
  speciesRaster <- merge(speciesRaster,mask.raster)
  
  #label the raster
  names(speciesRaster) <- raster.label
  return(speciesRaster)
}

#P/A Raster for Common Waxbill
species <- "Common Waxbill"
thin_cw2<-thin_cw2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
setwd("~/Desktop/Whydah Project/whydah/Data/wc5")
myRaster <- raster( "bio1.bil") #resolution of this file is low .08333x.08333, or 10km grid cells

# create presence absence raster for Common Waxbills using pre-made function
pa_raster_cw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_cw2, raster.label=species)
pa_raster_cw
plot(pa_raster_cw, main="Common Waxbill Presence/Absence Raster File")

#P/A Raster for Orange-Cheeked Waxbill
species <- "OrangeCheekedWaxbill"
thin_ocw2<-thin_ocw2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
setwd("~/Desktop/Whydah Project/whydah/Data/wc5")
myRaster <- raster( "bio1.bil") #resolution of this file is low .08333x.08333, or 10km grid cells

# create presence absence raster for Common Waxbills using pre-made function
pa_raster_ocw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_ocw2, raster.label=species)
pa_raster_ocw
plot(pa_raster_ocw, main="Orance Cheeked Waxbill Presence/Absence Raster File")

#P/A Raster for Nutmeg
species <- "Nutmeg Mannikin"
thin_nutmeg2<-thin_nutmeg2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
setwd("~/Desktop/Whydah Project/whydah/Data/wc5")
myRaster <- raster( "bio1.bil") #resolution of this file is low .08333x.08333, or 10km grid cells

# create presence absence raster for Common Waxbills using pre-made function
pa_raster_nutmeg <- presence.absence.raster(mask.raster=myRaster, species.data=thin_nutmeg2, raster.label=species)
pa_raster_nutmeg
plot(pa_raster_nutmeg, main="Nutmeg Mannikin Presence/Absence Raster File")

####

#Environmental Variables####

####
# get the file names...these should be all of our our worldclim
files <- list.files(path="~/Desktop/Whydah Project/whydah/Data/wc5", pattern="bil", full.names=TRUE)
predictors<-stack(files)
mask <- raster(files[1]) #just sampling from 1 of the worldclim variables (since they are all from whole world)
set.seed(1963) #makes sure we're generating random numbers

#Created custom sets of predictors
files #here are all climate files
predictors_no_host<-stack(files)
predictors_cw<-stack(files, pa_raster_cw) #make a rasterstack of climate data & waxbill presence/absence
predictors_ocw<-stack(files, pa_raster_ocw)
predictors_nutmeg<-stack(files, pa_raster_nutmeg)
predictors_ocw_and_cw<-stack(files, pa_raster_cw,pa_raster_ocw)
predictors_all_hosts<-stack(files, pa_raster_cw,pa_raster_ocw,pa_raster_nutmeg)

#background points
backg <- randomPoints(predictors_no_host, n=4000, ext = (extent(-119, 55.4539,-33,23)), extf=1.25) #pull background points from specified extent

#From occurrence records...bounding box is. y (lat) min = -34.8324, ymax=41.526  /  x (lon) min = -118.808, xmax = 55.4539
#ext = extent(-90, -32, -33, 23) #to speed up how quickly everything processes, so limit our extent
#Format for extent is (xmin,xmax,ymin,ymax)
colnames(backg) = c('lon' , 'lat')
group <- kfold(backg, 4)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

ext<-extent(-119, 55.4539,-33,23)

plot(wrld_simpl,main="Background Points with \nExtent Limited to Whydah Distribution")
points(backg, cex=.3, col="purple")

####

#Traditional PCA####

####

#Example from Hijmans for calculating PCA of Rasterstack
#sr <- sampleRandom(envs.df, 1000) #could sample randomly of rasterbrick is too large

#PCA with just worldclim
#results are pretty much the same as with waxbills except axis 4!
pca_worldclim_only <- prcomp(na.omit(values(predictors)), scale=T, center=T)
summary(pca_worldclim_only) #prop. variance is similar to eigen value
print(pca_worldclim_only) #this function prints loadings for each PCA
pca_worldclim_only$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_worldclim_only$rotation #for EIGEN VECTORS
pca_predictions_worldclim_only <- na.omit(predict(predictors, pca_worldclim_only, index=1:4)) 


#PCA for Common Waxbill Predictors
pca_cw <- prcomp(na.omit(values(predictors_cw)), scale=T, center=T)
summary(pca_cw) #prop. variance is similar to eigen value
print(pca_cw) #this function prints loadings for each PCA
pca_cw$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_cw$rotation #EIGEN VECTORS
plot(pca_cw, type="l") #screeplot1
pca_predictions_cw <- na.omit(predict(predictors_cw, pca_cw, index=1:4)) #make further predictions w/ PCA results

#Creating Biplots (but they're really cluttered)
# library("devtools")
# install_github("kassambara/factoextra")
# library(factoextra)
# fviz_pca_biplot(pca_cw)

#so this should be result in a (raster stack? df?) of 4 PCs...then put into maxEnt
plot(pca_predictions_cw[,1], pca_predictions_cw[,2]) #xlim=c(-12,7), ylim=c(-12,7)

#PCA for Orange Cheeked Waxbill Predictors
pca_ocw <- prcomp(na.omit(values(predictors_ocw)), scale=T, center=T)
summary(pca_ocw) #prop. variance is similar to eigen value
print(pca_ocw) #this function prints loadings for each PCA
pca_ocw$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_ocw$rotation #for EIGEN VECTORS
plot(pca_ocw, type="l") #screeplot1
screeplot(pca_ocw, type="l")  #screeplot2
pca_predictions_ocw <- na.omit(predict(predictors_ocw, pca_ocw, index=1:4)) #make further predictions w/ PCA results
#so this should be result in a (raster stack? df?) of 4 PCs...then put into maxEnt
plot(pca_predictions_ocw[,1], pca_predictions_ocw[,2]) #xlim=c(-12,7), ylim=c(-12,7)

#PCA for Nutmeg Predictors
pca_nutmeg <- prcomp(na.omit(values(predictors_nutmeg)), scale=T, center=T)
summary(pca_nutmeg) #prop. variance is similar to eigen value
print(pca_nutmeg) #this function prints loadings for each PCA
pca_nutmeg$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_nutmeg$rotation #for EIGEN VECTORS

plot(pca_nutmeg, type="l") #screeplot1
screeplot(pca_nutmeg, type="l")  #screeplot2
pca_predictions_nutmeg <- na.omit(predict(predictors_nutmeg, pca_nutmeg, index=1:4)) #make further predictions w/ PCA results
#so this should be result in a (raster stack? df?) of 4 PCs...then put into maxEnt
plot(pca_predictions_nutmeg[,1], pca_predictions_nutmeg[,2]) #xlim=c(-12,7), ylim=c(-12,7)

#PCA for OCW AND CW Predictors 
pca_ocw_and_cw <- prcomp(na.omit(values(predictors_ocw_and_cw)), scale=T, center=T)
summary(pca_ocw_and_cw) #prop. variance is similar to eigen value
print(pca_ocw_and_cw) #this function prints loadings for each PCA
pca_ocw_and_cw$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_ocw_and_cw$rotation #for EIGEN VECTORS
plot(pca_ocw_and_cw, type="l") #screeplot1
screeplot(pca_ocw_and_cw, type="l")  #screeplot2

pca_predictions_ocw_and_cw <- na.omit(predict(predictors_ocw_and_cw, pca_ocw_and_cw, index=1:5)) #make further predictions w/ PCA results
#so this should be result in a (raster stack? df?) of 4 PCs...then put into maxEnt
plot(pca_predictions_ocw_and_cw[,1], pca_predictions_ocw_and_cw[,2]) #xlim=c(-12,7), ylim=c(-12,7)

#PCA for all hosts
pca_all_hosts <- prcomp(na.omit(values(predictors_all_hosts)), scale=T, center=T)
summary(pca_all_hosts) #prop. variance is similar to eigen value
print(pca_all_hosts) #this function prints loadings for each PCA
pca_all_hosts$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_all_hosts$rotation #for EIGEN VECTORS
plot(pca_all_hosts, type="l") #screeplot1
screeplot(pca_all_hosts, type="l")  #screeplot2

pca_predictions_all_hosts <- na.omit(predict(predictors_all_hosts, pca_all_hosts, index=1:5)) #make further predictions w/ PCA results
#so this should be result in a (raster stack? df?) of 4 PCs...then put into maxEnt
plot(pca_predictions_all_hosts[,1], pca_predictions_all_hosts[,2]) #xlim=c(-12,7), ylim=c(-12,7)

######

#ENMeval####

######
#enmeval_results_worldclim <- ENMevaluate(thin_ptw2_coords, env=predictors, bg.coords = backg, n.bg = 500 ,method="block", overlap=TRUE,
#                              bin.output=TRUE, clamp=TRUE)
#enmeval_results_pca <- ENMevaluate(thin_ptw2_coords, env = pca_predictions_worldclim_only, bg.coords = backg, n.bg = 500 ,method="block", overlap=TRUE, bin.output=TRUE, clamp=TRUE)
#save(enmeval_results_worldclim, file="enmeval_results_worldclim.rdata")
#save(enmeval_results_pca, file = "enmeval_results_pca.rdata")
# load("enmeval_results_worldclim.rdata")
# load("enmeval_results_pca.rdata")
# 
# #look at ENMeval for WorldClim
# enmeval_results_worldclim
# plot(enmeval_results_worldclim@predictions[[which (enmeval_results_worldclim@results$delta.AICc == 0) ]])
# points(enmeval_results_worldclim@occ.pts, pch=21, bg=enmeval_results_worldclim@occ.grp)
# head(enmeval_results_worldclim@results)
# enmeval_results_worldclim@results #all the results
# Q_worldclim<-enmeval_results_worldclim@results#arrange by AICc value
# QQ_worldclim<-as.data.frame(Q_worldclim)
# head(QQ_worldclim)
# QQ_worldclim<-QQ_worldclim[,c(1,2,3,14)]
# head(QQ_worldclim)
# arrange(QQ_worldclim,AICc,settings,features,rm) #this will sort ENMeval results so that we can see exact settings for model with lowest AICc
# #Shows that model with LQ ranging from .5-4.0 all had the lowest AICc
# enmeval_results_worldclim@overlap
# 
# #Very important figures
# par(mfrow=c(2,2))
# eval.plot(enmeval_results_worldclim@results, legend.position="topright")
# eval.plot(enmeval_results_worldclim@results, "Mean.AUC", )
# eval.plot(enmeval_results_worldclim@results, "Mean.AUC.DIFF", variance="Var.AUC.DIFF")
# eval.plot(enmeval_results_worldclim@results, "Mean.ORmin")
# 
# enmeval_results_worldclim@results
# specify how data should be partitioned w/ method="jackknife", "randomkfold", "user", "block", "checkerboard1", "checkerboard2".
# n.bg is The number of random background localities to draw from the study extent
#when overlap = TRUE, provides pairwise metric of niche overlap 
#bin.output appends evaluations metrics for each evaluation bin to results table

#look at ENMeval results for PCA
# enmeval_results_pca
# plot(enmeval_results_pca@predictions[[which (enmeval_results_pca@results$delta.AICc == 0) ]])
# points(enmeval_results_pca@occ.pts, pch=21, bg=enmeval_results_pca@occ.grp)
# head(enmeval_results_pca@results)
# enmeval_results_pca@results #all the results
# Q_pca<-enmeval_results_pca@results#arrange by AICc value
# QQ_pca<-as.data.frame(Q_pca)
# head(QQ_pca)
# QQ_pca<-QQ_pca[,c(1,2,3,14)]
# head(QQ_pca)
# arrange(QQ_pca,AICc,settings,features,rm) #this will sort ENMeval results so that we can see exact settings for model with lowest AICc
# #Shows that model with LQ ranging from .5-4.0 all had the lowest AICc
# enmeval_results_pca@overlap
# ?ENMeval
# par(mfrow=c(2,2))
# eval.plot(enmeval_results_pca@results, legend.position="topright")
# eval.plot(enmeval_results_pca@results, "Mean.AUC", )
# eval.plot(enmeval_results_pca@results, "Mean.AUC.DIFF", variance="Var.AUC.DIFF")
# eval.plot(enmeval_results_pca@results, "Mean.ORmin")
# #These figures are key /\.  We should relect RM and Model Setting from key when deta.AUCc is below 2
# enmeval_results_pca@results
# # specify how data should be partitioned w/ method="jackknife", "randomkfold", "user", "block", "checkerboard1", "checkerboard2".
# # n.bg is The number of random background localities to draw from the study extent
# #when overlap = TRUE, provides pairwise metric of niche overlap 
# #bin.output appends evaluations metrics for each evaluation bin to results table
# 
# 
####

#MaxEnt#####

####
#envs<-mask(envs,north.america) #mask makes all enviro cells with no data NA
#envs<-crop(envs,north.america)

#Prepare Training and Testing dataset####
folds<-kfold(thin_ptw2_coords, k=4) #this is a 4 fold test
train<-thin_ptw2_coords[folds>1,] #training has 75% of points
test<-thin_ptw2_coords[folds==1,] #testing has 25% of points
train<-train[,1:2]
test<-test[,1:2]
head(train) #just has lon/lat

#MaxEnt for Whydah No Host PCA####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
write.csv(thin_ptw2_coords,occs.path) #write a CSV of our occurrence points
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_pca_only <- maxent(pca_predictions_worldclim_only,train,a=backg_train,args=c('betamultiplier=1.5','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_pca_only) #response curves
plot(mx_pca_only) #importance of each variable in building model

#Model Evaluation
e_pca_bio_only <- evaluate(test, backg_test, mx_pca_only, pca_predictions_worldclim_only) #evalute test points, pseudo-absences (random background points), the model and predictors
e_pca_bio_only #shows number of presences/absences/AUC and cor
px_pca_bio_only <- predict(pca_predictions_worldclim_only, mx_pca_only, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_pca_bio_only, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_pca_bio_only <- threshold(e_pca_bio_only, 'spec_sens' )
plot(px_pca_bio_only > tr_pca_bio_only, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_pca_bio_only, 'ROC')

#Plotting Maxent output
map.pca.bio.only <- rasterToPoints(px_pca_bio_only) #make predictions raster a set of points for ggplot
df_pca_bio_only <- data.frame(map.pca.bio.only) #convert to data.frame
head(df_pca_bio_only)
colnames(df_pca_bio_only) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_pca_bio_only$Suitability)

#Now make the map
p_no_host_PCA<-ggplot(data=df_pca_bio_only, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith No Hosts and PCA") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_no_host_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                     na.value = "black", limits=c(0,.90))

#MaxEnt for Whydah with CW and PCA####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
write.csv(thin_ptw2_coords,occs.path) #write a CSV of our occurrence points
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_cw <- maxent(pca_predictions_cw,train,a=backg_train,args=c('betamultiplier=1.5','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_cw) #response curves
plot(mx_cw) #importance of each variable in building model

#Model Evaluation
e_cw <- evaluate(test, backg_test, mx_cw, pca_predictions_cw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_cw #shows number of presences/absences/AUC and cor
px_cw <- predict(pca_predictions_cw, mx_cw, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_cw, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_cw <- threshold(e_cw, 'spec_sens' )
plot(px_cw > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_cw, 'ROC')

#Plotting Maxent output
map.cw <- rasterToPoints(px_cw) #make predictions raster a set of points for ggplot
df_cw <- data.frame(map.cw) #convert to data.frame
head(df_cw)
colnames(df_cw) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_cw$Suitability)

#Now make the map
p_cw_PCA<-ggplot(data=df_cw, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Common Waxbills and PCA") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_cw_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black",limits=c(0,.90))

#MaxEnt for Whydah with OCW and PCA####
mx_ocw_pca <- maxent(pca_predictions_ocw,train,a=backg_train,args=c('betamultiplier=1.5','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_ocw_pca) #response curves
plot(mx_ocw_pca) #importance of each variable in building model

#Model Evaluation
e_ocw <- evaluate(test, backg_test, mx_ocw_pca, pca_predictions_ocw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_ocw #shows number of presences/absences/AUC and cor
px_ocw_pca <- predict(pca_predictions_ocw, mx_ocw_pca, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_ocw_pca, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_ocw_pca <- threshold(e_ocw, 'spec_sens' )
plot(px_ocw_pca > tr_ocw_pca, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_ocw, 'ROC')

#Plotting Maxent output
map.ocw.pca <- rasterToPoints(px_ocw_pca) #make predictions raster a set of points for ggplot
df_ocw_pca <- data.frame(map.ocw.pca) #convert to data.frame
head(df_ocw_pca)
colnames(df_ocw_pca) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_ocw_pca$Suitability)

#Now make the map
p_ocw_PCA<-ggplot(data=df_ocw_pca, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Orange Cheeked Waxbills and PCA") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_ocw_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black", limits=c(0,.90))

#MaxEnt for Whydah with Nutmeg and PCA####
mx_nutmeg_pca <- maxent(pca_predictions_nutmeg,train,a=backg_train,args=c('betamultiplier=1.5','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_nutmeg_pca) #response curves
plot(mx_nutmeg_pca) #importance of each variable in building model

#Model Evaluation
e_nutmeg <- evaluate(test, backg_test, mx_nutmeg_pca, pca_predictions_nutmeg) #evalute test points, pseudo-absences (random background points), the model and predictors
e_nutmeg #shows number of presences/absences/AUC and cor
px_nutmeg_pca <- predict(pca_predictions_nutmeg, mx_nutmeg_pca, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_nutmeg_pca, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_nutmeg_pca <- threshold(e_nutmeg, 'spec_sens' )
plot(px_nutmeg_pca > tr_nutmeg_pca, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_nutmeg, 'ROC')

#Plotting Maxent output
map.nutmeg.pca <- rasterToPoints(px_nutmeg_pca) #make predictions raster a set of points for ggplot
df_nutmeg_pca <- data.frame(map.nutmeg.pca) #convert to data.frame
head(df_nutmeg_pca)
colnames(df_nutmeg_pca) <- c('lon', 'lat', 'Suitability') #column headings
head(thin_ptw2_coords)
max(df_nutmeg_pca$Suitability) 

#Now make the map
p_nutmeg_PCA<-ggplot(data=df_nutmeg_pca, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Nutmeg Mannikins and PCA") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )

p_nutmeg_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black", limits=c(0,.90))

#MaxEnt for Whydah with OCW AND CW PCA####
mx_ocw_and_cw_pca <- maxent(pca_predictions_ocw_and_cw,train,a=backg_train,args=c('betamultiplier=1.5','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_ocw_and_cw_pca) #response curves
plot(mx_ocw_and_cw_pca) #importance of each variable in building model

#Model Evaluation
e_ocw_and_cw_pca <- evaluate(test, backg_test, mx_ocw_and_cw_pca, pca_predictions_ocw_and_cw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_ocw_and_cw_pca #shows number of presences/absences/AUC and cor
px_ocw_and_cw_pca <- predict(pca_predictions_ocw_and_cw, mx_ocw_and_cw_pca, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_ocw_and_cw_pca, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_ocw_and_cw_pca <- threshold(e_ocw_and_cw_pca, 'spec_sens' )
plot(px_ocw_and_cw_pca > tr_ocw_and_cw_pca, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_ocw_and_cw_pca, 'ROC')

#Plotting Maxent output
map.ocw.and.cw.pca <- rasterToPoints(px_ocw_and_cw_pca) #make predictions raster a set of points for ggplot
df_ocw_and_cw_pca <- data.frame(map.ocw.and.cw.pca) #convert to data.frame
head(df_ocw_and_cw_pca)
colnames(df_ocw_and_cw_pca) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_ocw_and_cw_pca$Suitability)

#Now make the map
p_ocw_and_cw_PCA<-ggplot(data=df_ocw_and_cw_pca, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Native Hosts and PCA") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_ocw_and_cw_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black", limits=c(0,.90)) +
  #coord_fixed(xlim = c(-88, -79),  ylim = c(24, 32)) #add this line to zoom into florida
  #coord_fixed(xlim = c(-125.8,-62.2), ylim = c(22.8, 50)) #add this line to zoom into USA

#MaxEnt for Whydah with all hosts PCA####
mx_all_hosts_pca <- maxent(pca_predictions_all_hosts,train,a=backg_train,args=c('betamultiplier=1.5','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_all_hosts_pca) #response curves
plot(mx_all_hosts_pca) #importance of each variable in building model

#Model Evaluation
e_all_hosts_pca <- evaluate(test, backg_test, mx_all_hosts_pca, pca_predictions_all_hosts) #evalute test points, pseudo-absences (random background points), the model and predictors
e_all_hosts_pca #shows number of presences/absences/AUC and cor
px_all_hosts_pca <- predict(pca_predictions_all_hosts, mx_all_hosts_pca, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_all_hosts_pca, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_all_hosts_pca <- threshold(e_all_hosts_pca, 'spec_sens' )
plot(px_all_hosts_pca > tr_all_hosts_pca, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_all_hosts_pca, 'ROC')

#Plotting Maxent output
map.all.hosts.pca <- rasterToPoints(px_all_hosts_pca) #make predictions raster a set of points for ggplot
df_all_hosts_pca <- data.frame(map.all.hosts.pca) #convert to data.frame
head(df_all_hosts_pca)
colnames(df_all_hosts_pca) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_all_hosts_pca$Suitability)

#Now make the map
p_all_hosts_PCA<-ggplot(data=df_all_hosts_pca, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith All Hosts and PCA") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_all_hosts_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                        na.value = "black", limits=c(0,.90))

#MaxEnt  for Whydah - No Host species / all worldclim####
mx_no_host <- maxent(predictors_no_host,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_no_host) #response curves
plot(mx_no_host) #importance of each variable in building model

#Model Evaluation 
e_no_host <- evaluate(test, backg_test, mx_no_host, predictors_no_host) #evalute test points, pseudo-absences (random background points), the model and predictors
e_no_host #shows number of presences/absences/AUC and cor
px_no_host <- predict(predictors_no_host, mx_no_host, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_no_host, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_no_host <- threshold(e_no_host, 'spec_sens' )
plot(px_no_host > tr_no_host, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_no_host, 'ROC')

#Plotting Maxent output
map.no.host <- rasterToPoints(px_no_host) #make predictions raster a set of points for ggplot
df_no_host <- data.frame(map.no.host) #convert to data.frame
head(df_no_host)
colnames(df_no_host) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_no_host$Suitability)

#Now make the map
p_no_host_all_worldclim <- ggplot(data=df_no_host, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith No Hosts and all WorldClim") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_no_host_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black",limits=c(0,.90))

#values=c(0,0.1,seq(0.100,1,length.out=7)) #I think above map is good!, can insert this if we want to change spacing
#scale_fill_gradient(low="wheat1", high="red1", limits=c(0,.90)) #this one works!

#MaxEnt for Whydah with Common Waxbill and ALL worldclim####
mx_cw_all_worldclim <- maxent(predictors_cw,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_cw_all_worldclim) #response curves
plot(mx_cw_all_worldclim) #importance of each variable in building model
#Model Evaluation 
e_cw_all_worldclim <- evaluate(test, backg_test, mx_cw_all_worldclim, predictors_cw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_cw_all_worldclim #shows number of presences/absences/AUC and cor
px_cw_all_worldclim <- predict(predictors_cw, mx_cw_all_worldclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_cw_all_worldclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_cw_all_worldclim <- threshold(e_cw_all_worldclim, 'spec_sens' )
plot(px_cw_all_worldclim > tr_cw_all_worldclim, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_cw_all_worldclim, 'ROC')

#Plotting Maxent output
map.cw.all.worldclim <- rasterToPoints(px_cw_all_worldclim) #make predictions raster a set of points for ggplot
df_cw_all_worldclim <- data.frame(map.cw.all.worldclim) #convert to data.frame
head(df_cw_all_worldclim)
colnames(df_cw_all_worldclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_cw_all_worldclim$Suitability)

p_cw_all_worldclim<-ggplot(data=df_cw_all_worldclim, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Common Waxbills and all WorldClim") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )

p_cw_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                          na.value = "black",limits=c(0,.90))


#MaxEnt for Whydah with OCW and ALL worldclim####
mx_ocw_all_worldclim <- maxent(predictors_ocw, train, a=backg_train, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_ocw_all_worldclim) #response curves
plot(mx_ocw_all_worldclim) #importance of each variable in building model

#Model Evaluation 
e_ocw_all_worldclim <- evaluate(test, backg_test, mx_ocw_all_worldclim, predictors_ocw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_ocw_all_worldclim #shows number of presences/absences/AUC and cor
px_ocw_all_worldclim <- predict(predictors_ocw, mx_ocw_all_worldclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
plot(px_ocw_all_worldclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_ocw_all_worldclim <- threshold(e_ocw_all_worldclim, 'spec_sens' )
plot(px_ocw_all_worldclim > tr_ocw_all_worldclim, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_ocw_all_worldclim, 'ROC')

#Plotting Maxent output
map.ocw.all.worldclim <- rasterToPoints(px_ocw_all_worldclim) #make predictions raster a set of points for ggplot
df_ocw_all_worldclim <- data.frame(map.ocw.all.worldclim) #convert to data.frame
head(df_ocw_all_worldclim)
colnames(df_ocw_all_worldclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_ocw_all_worldclim$Suitability)

p_ocw_all_worldclim<-ggplot(data=df_ocw_all_worldclim, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Orange CheekedWaxbills and all WorldClim") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_ocw_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                          na.value = "black",limits=c(0,.90))

#MaxEnt for Whydah with Nutmeg Mannikin and ALL worldclim####
mx_nutmeg_all_worldclim <- maxent(predictors_nutmeg,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_nutmeg_all_worldclim) #response curves
plot(mx_nutmeg_all_worldclim) #importance of each variable in building model

#Model Evaluation 
e_nutmeg_all_worldclim <- evaluate(test, backg_test, mx_nutmeg_all_worldclim, predictors_nutmeg) #evalute test points, pseudo-absences (random background points), the model and predictors
e_nutmeg_all_worldclim #shows number of presences/absences/AUC and cor
px_nutmeg_all_worldclim <- predict(predictors_nutmeg, mx_nutmeg_all_worldclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_nutmeg_all_worldclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_nutmeg_all_worldclim <- threshold(e_nutmeg_all_worldclim, 'spec_sens' )
plot(px_nutmeg_all_worldclim > tr_nutmeg_all_worldclim, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_nutmeg_all_worldclim, 'ROC')

#Plotting Maxent output
map.nutmeg.all.worldclim <- rasterToPoints(px_nutmeg_all_worldclim) #make predictions raster a set of points for ggplot
df_nutmeg_all_worldclim <- data.frame(map.nutmeg.all.worldclim) #convert to data.frame
head(df_nutmeg_all_worldclim)
colnames(df_nutmeg_all_worldclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_nutmeg_all_worldclim$Suitability)

p_nutmeg_all_worldclim<-ggplot(data=df_nutmeg_all_worldclim, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Nutmeg Mannikin and all WorldClim") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )

p_nutmeg_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black",limits=c(0,.90))

#MaxEnt for Whydah with OCW AND CW and ALL worldclim####
mx_ocw_and_cw_all_worldclim <- maxent(predictors_ocw_and_cw,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_ocw_and_cw_all_worldclim) #response curves
plot(mx_ocw_and_cw_all_worldclim) #importance of each variable in building model

#Model Evaluation 
e_ocw_and_cw_all_worldclim <- evaluate(test, backg_test, mx_ocw_and_cw_all_worldclim, predictors_ocw_and_cw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_ocw_and_cw_all_worldclim #shows number of presences/absences/AUC and cor
px_ocw_and_cw_all_worldclim <- predict(predictors_ocw_and_cw, mx_ocw_and_cw_all_worldclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_ocw_and_cw_all_worldclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_ocw_and_cw_all_worldclim <- threshold(e_ocw_and_cw_all_worldclim, 'spec_sens' )
plot(px_ocw_and_cw_all_worldclim > tr_ocw_and_cw_all_worldclim, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_ocw_and_cw_all_worldclim, 'ROC')

#Plotting Maxent output
map.ocw.and.cw.all.worldclim <- rasterToPoints(px_ocw_and_cw_all_worldclim) #make predictions raster a set of points for ggplot
df_ocw_and_cw_all_worldclim <- data.frame(map.ocw.and.cw.all.worldclim) #convert to data.frame
head(df_ocw_and_cw_all_worldclim)
colnames(df_ocw_and_cw_all_worldclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_ocw_and_cw_all_worldclim$Suitability)

p_ocw_and_cw_all_worldclim<-ggplot(data=df_ocw_and_cw_all_worldclim, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Native Hosts and all WorldClim") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_ocw_and_cw_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black",limits=c(0,.90))

#MaxEnt for Whydah with all hosts and ALL worldclim####
mx_all_hosts_all_worldclim <- maxent(predictors_all_hosts,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_all_hosts_all_worldclim) #response curves
plot(mx_all_hosts_all_worldclim) #importance of each variable in building model

#Model Evaluation 
e_all_hosts_all_worldclim <- evaluate(test, backg_test, mx_all_hosts_all_worldclim, predictors_all_hosts) #evalute test points, pseudo-absences (random background points), the model and predictors
e_all_hosts_all_worldclim #shows number of presences/absences/AUC and cor
px_all_hosts_all_worldclim <- predict(predictors_all_hosts, mx_all_hosts_all_worldclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_all_hosts_all_worldclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_all_hosts_all_worldclim <- threshold(e_all_hosts_all_worldclim, 'spec_sens' )
plot(px_all_hosts_all_worldclim > tr_all_hosts_all_worldclim, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_all_hosts_all_worldclim, 'ROC')

#Plotting Maxent output
map.all.hosts.all.worldclim <- rasterToPoints(px_all_hosts_all_worldclim) #make predictions raster a set of points for ggplot
df_all_hosts_all_worldclim <- data.frame(map.all.hosts.all.worldclim) #convert to data.frame
head(df_all_hosts_all_worldclim)
colnames(df_all_hosts_all_worldclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)
max(df_all_hosts_all_worldclim$Suitability)

p_all_hosts_all_worldclim<-ggplot(data=df_all_hosts_all_worldclim, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith All Hosts and All WorldClim") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )
p_all_hosts_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                                  na.value = "black",limits=c(0,.90))

#Combining All MaxEnt Maps to one PDF####
par(mfrow = c(4, 2))
p_cw_PCA2 <- p_cw_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                na.value = "black",limits=c(0,.90))

p_ocw_PCA2 <-p_ocw_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                 na.value = "black", limits=c(0,.90))

p_nutmeg_PCA2 <-p_nutmeg_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                    na.value = "black", limits=c(0,.90))

p_ocw_and_cw_PCA2<-p_ocw_and_cw_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                        na.value = "black", limits=c(0,.90))

p_all_hosts_PCA2<-p_all_hosts_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                                          na.value = "black", limits=c(0,.90))

p_no_host_PCA2<-p_no_host_PCA + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                     na.value = "black", limits=c(0,.90))

p_no_host_all_worldclim2<-p_no_host_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                             na.value = "black",limits=c(0,.90))

p_cw_all_worldclim2 <- p_cw_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                                                 na.value = "black",limits=c(0,.90))

p_ocw_all_worldclim2 <- p_ocw_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                         na.value = "black",limits=c(0,.90))

p_nutmeg_all_worldclim2 <- p_nutmeg_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                            na.value = "black",limits=c(0,.90))
  
p_ocw_and_cw_all_worldclim2 <- p_ocw_and_cw_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                                na.value = "black",limits=c(0,.90))

p_all_hosts_all_worldclim2 <- p_all_hosts_all_worldclim + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                                 na.value = "black",limits=c(0,.90))

multiplot(p_no_host_PCA2, p_cw_PCA2, p_ocw_PCA2, p_nutmeg_PCA2, p_ocw_and_cw_PCA2, p_no_host_all_worldclim2, p_cw_all_worldclim2, p_ocw_all_worldclim2, p_nutmeg_all_worldclim2, p_ocw_and_cw_all_worldclim2, cols=2)
dev.off()

#####
##MULTIPLOT FUNCTION####
#####

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##Making comparisons across models

####

install.packages("phyloclim")
library(phyloclim)
?phyloclim
