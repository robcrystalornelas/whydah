setwd("~/Desktop/Whydah Project/whydah/Data")
options(java.parameters = "-Xmx1g" )
Sys.setenv(NOAWT=TRUE)
library(rJava)
library(gurobi)
library(spThin)
library(ENMeval)
library(dismo)
library(jsonlite)
library(sp)
library(rgdal)
library(maptools)
library(raster)
library(devtools)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(rgbif)
library(dplyr)
library(scales)
library(ecospat)
library(gridExtra)
library(scales)
library(plyr)
library(phyloclim)
library(SDMTools)

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
ptw.unique<-filter(ptw.unique, country !=c("United Arab Emirates"))

filter(ptw.unique, lon<(-80) & lat>30 & country=="United States") #find Chicago point
which(ptw.unique$lon == -82.9989, ptw.unique$lat== 39.96110) #find it in the data.frame
ptw.unique<- ptw.unique[-4522,] #remove that row!

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
filter(ocw.unique, lon>(-100) & lon<(-89) & country=="United States") #find northern midwest point
which(ocw.unique$lon == -95.55926, ocw.unique$lat== 29.69430) #find it in the data.frame
ocw.unique<- ocw.unique[-404,] #remove that row!

filter(ocw.unique, lon>(-100) & lon<(-89) & country=="United States") #find 2nd midwest points
points(-96.67213,40.80549,col="green") #make sure it's the right one
which(ocw.unique$lon == -96.67213, ocw.unique$lat== 40.80549) #find it in the data.frame
ocw.unique<- ocw.unique[-1031,] #remove that row!

filter(ocw.unique, lon>(-100) & lon<(-89) & country=="United States") #find 3rd midwest points
points(-96.67964,40.80004,col="purple") #make sure it's the right one
which(ocw.unique$lon == -96.67964, ocw.unique$lat== 40.80004) #find it in the data.frame
ocw.unique<- ocw.unique[-1084,] #remove that row!

filter(ocw.unique, lon>(-87) & lat>(36) & country=="United States") #find DC point
points(-77.09590,38.75890,col="purple") #make sure it's the right one
which(ocw.unique$lon == -77.09590, ocw.unique$lat== 38.75890) #find it in the data.frame
ocw.unique<- ocw.unique[-607,] #remove that row!

points(-77.10650,38.75890,col="green") #make sure it's the right one
which(ocw.unique$lon == -77.10650, ocw.unique$lat== 38.75890) #find it in the data.frame
ocw.unique<- ocw.unique[-611,] #remove that row!

points(-83.14313,42.47483,col="green") #make sure it's the right one
which(ocw.unique$lon == -83.14313, ocw.unique$lat== 42.47483) #find it in the data.frame
ocw.unique<- ocw.unique[-958,] #remove that row!

filter(ocw.unique, lon<(-120) & lat>35 & country=="United States") #find 1st San Fran point
which(ocw.unique$lat == 38.57520)
points(-121.4675,38.57520,col="red")
ocw.unique<-ocw.unique[-859,]

#find 2nd San Fran point
which(ocw.unique$lat == 41.96554)
ocw.unique<-ocw.unique[-1061,] #remove san fran point

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
cw.unique<- cw.unique[-6046,] #remove that row!

#Check Common Waxbill Points
plot(wrld_simpl)
points(cw.unique, col="red")

#Clean Up the Data
lonzero = subset(cw.unique, lon==0) #any points have longitude that was auto-set to 0
lonzero #all OK
duplicated(cw.unique) #any duplicates?

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

####

#Preparing Presence/Absence Rasters

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
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_cw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_cw2, raster.label=species)
pa_raster_cw

#P/A Raster for Orange-Cheeked Waxbill
species <- "OrangeCheekedWaxbill"
thin_ocw2<-thin_ocw2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
myRaster <- raster( "bio1.bil")
pa_raster_ocw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_ocw2, raster.label=species)
pa_raster_ocw

#P/A Raster for Nutmeg
species <- "Nutmeg Mannikin"
thin_nutmeg2<-thin_nutmeg2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
myRaster <- raster( "bio1.bil")
pa_raster_nutmeg <- presence.absence.raster(mask.raster=myRaster, species.data=thin_nutmeg2, raster.label=species)
pa_raster_nutmeg

####

#Environmental Variables####

####

# get the file names...these should be all of our our worldclim
files <- list.files(path="~/Desktop/Whydah Project/whydah/Data/wc2", pattern="bil", full.names=TRUE)
predictors<-stack(files)
mask <- raster(files[1]) #just sampling from 1 of the worldclim variables (since they are all from whole world)
set.seed(1963) #makes sure we're generating random numbers

#Created custom sets of predictors
predictors_no_host<-stack(files)
predictors_ocw_and_cw <- stack(files, pa_raster_cw, pa_raster_ocw)
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

#Prepare Training and Testing dataset####
folds<-kfold(thin_ptw2_coords, k=4) #this is a 4 fold test
train<-thin_ptw2_coords[folds>1,] #training has 75% of points
test<-thin_ptw2_coords[folds==1,] #testing has 25% of points
train<-train[,1:2]
test<-test[,1:2]
head(train) #just has lon/lat

####

# MaxEnt

####

#no host####
mx_no_host_all_worldclim <- maxent(predictors, train, a=backg_train, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_no_host_all_worldclim@results
mx_no_host_all_worldclim@lambdas
response(mx_no_host_all_worldclim)
plot(mx_no_host_all_worldclim)

#Model Evaluation 
e_no_host_all_worldclim <- evaluate(test, backg_test, mx_no_host_all_worldclim, predictors) #evalute test points, pseudo-absences (random background points), the model and predictors
e_no_host_all_worldclim #shows number of presences/absences/AUC and cor
px_no_host_all_worldclim <- predict(predictors, mx_no_host_all_worldclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
plot(px_no_host_all_worldclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_no_host_all_worldclim <- threshold(e_no_host_all_worldclim2, 'spec_sens' )
tr_no_host_all_worldclim
plot(px_no_host_all_worldclim > tr_no_host_all_worldclim, main='presence/absence')
plot(e_no_host_all_worldclim, 'ROC')

#Plotting Maxent output
map.no.host.all.worldclim <- rasterToPoints(px_no_host_all_worldclim) #make predictions raster a set of points for ggplot
df_no_host_all_worldclim <- data.frame(map.no.host.all.worldclim) #convert to data.frame
head(df_no_host_all_worldclim)
colnames(df_no_host_all_worldclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_no_host_all_worldclim2$Suitability)
plot(wrld_simpl)
points(filter(df_no_host_all_worldclim, Suitability >= .6912), col="red")

#####

# Native Only####

#####
mx_native_host_all_worldclim <- maxent(predictors_ocw_and_cw, train, a=backg_train, args=c('betamultiplier=3','responsecurves=TRUE'))
mx_native_host_all_worldclim
mx_native_host_all_worldclim@results
mx_native_host_all_worldclim@lambdas
response(mx_native_host_all_worldclim)
plot(mx_native_host_all_worldclim)
mx_native_host_all_worldclim@results

#Model Evaluation 
e_native_host_all_worldclim <- evaluate(test, backg_test, mx_native_host_all_worldclim, predictors_ocw_and_cw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_native_host_all_worldclim #shows number of presences/absences/AUC and cor
px_native_host_all_worldclim <- predict(predictors_ocw_and_cw, mx_native_host_all_worldclim) #make predictions of habitat suitability can include argument ext=ext
# plot(px_native_host_all_worldclim2, main= 'Maxent, raw values')
tr_native_host_all_worldclim2 <- threshold(e_native_host_all_worldclim2, 'spec_sens' )
tr_native_host_all_worldclim2
# plot(px_native_host_all_worldclim2 > tr_native_host_all_worldclim2)
plot(e_native_host_all_worldclim2, 'ROC')

#Plotting Maxent output
map.native.all.worldclim2 <- rasterToPoints(px_native_host_all_worldclim2) #make predictions raster a set of points for ggplot
df_native_host_all_worldclim2 <- data.frame(map.native.all.worldclim2) #convert to data.frame
head(df_native_host_all_worldclim2)
colnames(df_native_host_all_worldclim2) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_native_host_all_worldclim2$Suitability)
plot(wrld_simpl)
points(filter(df_native_host_all_worldclim2, Suitability >= .7332606), col="red")

# All hosts#####
mx_all_host_all_worldclim <- maxent(predictors_all_hosts, train, a=backg_train, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_all_host_all_worldclim@results
mx_all_host_all_worldclim@lambdas
response(mx_all_host_all_worldclim)
plot(mx_all_host_all_worldclim)

# Model Evaluation
e_all_host_all_worldclim <- evaluate(test, backg_test, mx_all_host_all_worldclim, predictors_all_hosts) #evalute test points, pseudo-absences (random background points), the model and predictors
e_all_host_all_worldclim #shows number of presences/absences/AUC and cor
px_all_host_all_worldclim <- predict(predictors_all_hosts, mx_all_host_all_worldclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
plot(px_all_host_all_worldclim, main= 'Maxent, raw values')
writeRaster(px_all_host_all_worldclim, filename="allhost_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_all_host_all_worldclim <- threshold(e_all_host_all_worldclim2, 'spec_sens' )
tr_all_host_all_worldclim
plot(px_all_host_all_worldclim > tr_all_host_all_worldclim, main='presence/absence')
writeRaster(px_all_host_all_worldclim, "test.bil", format = "EHdr")

plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_all_host_all_worldclim2, 'ROC')

#Plotting Maxent output
map.all.host.all.worldclim2 <- rasterToPoints(px_all_host_all_worldclim2) #make predictions raster a set of points for ggplot
df_all_host_all_worldclim2 <- data.frame(map.all.host.all.worldclim2) #convert to data.frame
head(df_all_host_all_worldclim2)
colnames(df_all_host_all_worldclim2) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_all_host_all_worldclim2$Suitability)
plot(wrld_simpl)
points(filter(df_all_host_all_worldclim2, Suitability >= .77572), col="red")

#Combining all hosts / no hosts
combining_all_and_no_hosts <- data.frame(df_no_host_all_worldclim2$Suitability,df_all_host_all_worldclim2$Suitability)

names(combining_all_and_no_hosts) <- c('none','all')
all_and_none_combined_scaled<-scale(combining_all_and_no_hosts, center = TRUE, scale = TRUE)
all_and_none_combined_scaled<-as.data.frame(all_and_none_combined_scaled)
all_and_none_combined_scaled$subtracted <- (all_and_none_combined_scaled$all - all_and_none_combined_scaled$none)
all_and_none_combined_scaled$lon <- df_no_host_all_worldclim2$lon
all_and_none_combined_scaled$lat <- df_no_host_all_worldclim2$lat

head(all_and_none_combined_scaled)

min(all_and_none_combined_scaled$subtracted) #calbriate min and max of plots
max(all_and_none_combined_scaled$subtracted)

library(scales)

#exporting this data.frame
all_and_none_combined_scaled2<-all_and_none_combined_scaled[,c(4,5,3)]
pts<-all_and_none_combined_scaled2
names(pts)<-c("x","y","subtracted")
coordinates(pts)=~x+y
proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
pts = spTransform(pts,CRS("+init=epsg:4326"))
gridded(pts) = TRUE
r <- raster(pts)
projection(r) = CRS("+init=epsg:4326")
plot(r)
writeRaster(r,"whydah_subtracted_predictions.tif")


#####

# Heatmap of suitable regions

####
df_no_host_all_worldclim_pres <- df_no_host_all_worldclim2 %>% mutate(pres_no_host = ifelse(Suitability >= 0.4087746, 1, 0))
df_native_host_all_worldclim_pres <- df_native_host_all_worldclim2 %>% mutate(pres_native_host = ifelse(Suitability >= 0.3712534, 1, 0))
df_all_host_all_worldclim_pres <- df_all_host_all_worldclim2 %>% mutate(pres_all_host = ifelse(Suitability >= .3772382, 1, 0))

df_partial<- left_join(df_no_host_all_worldclim_pres, df_native_host_all_worldclim_pres, by = c("lon", "lat"))
head(df_partial)
df_all_presences <- left_join(df_partial, df_all_host_all_worldclim_pres, by = c("lon", "lat"))

df_all_presences$total <- rowSums(df_all_presences[, c(4, 6, 8)])
#df_all_presences$total <- as.factor(df_all_presences$total)

#converting to geotiff
df_all_presences_2<-df_all_presences[,c(1,2,9)]
head(df_all_presences_2)
coordinates(df_all_presences_2) <- ~ lon + lat
gridded(df_all_presences_2) <- TRUE
raster_of_heatmap <- raster(df_all_presences_2)
writeRaster(raster_of_heatmap, filename="heatmap_test_whydah.tif", format="GTiff", overwrite=TRUE)

#####

# Model w/ Replicates

#####
#no host
mx_no_host_all_worldclim_replicates <- maxent(predictors, thin_ptw2_coords, args=c('betamultiplier=3','responsecurves=TRUE','replicates=10','writebackgroundpredictions=TRUE'))
mx_no_host_all_worldclim_replicates
mx_no_host_all_worldclim_replicates@results

#native hosts#

mx_native_host_replicates <- maxent(predictors_ocw_and_cw, thin_ptw2_coords, args=c('betamultiplier=3','responsecurves=TRUE','replicates=10','writebackgroundpredictions=TRUE'))
mx_native_host_replicates
mx_native_host_replicates@results

#all hosts
mx_all_host_replicates <- maxent(predictors_all_hosts, thin_ptw2_coords, args=c('betamultiplier=3','responsecurves=TRUE','replicates=10','writebackgroundpredictions=TRUE'))
mx_all_host_replicates
mx_all_host_replicates@results

#####

# Calculating Schoner's D

####
#no_host_asc <- writeRaster(px_no_host_all_worldclim, filename = 'nohost.asc', format = "ascii", overwrite = TRUE)
#native_host_asc <- writeRaster(px_native_host_all_worldclim, filename = "nativehost.asc", format = "ascii")
#all_host_asc <- writeRaster(px_all_host_all_worldclim, filename = "allhost.asc", format = "ascii")

no_host_asc <- read.asc("nohost.asc")
native_host_asc <- read.asc("nativehost.asc")
all_host_asc <- read.asc("allhost.asc")
?niche.overlap

no_grid <- sp.from.asc(no_host_asc)
native_grid <- sp.from.asc(native_host_asc)
all_grid <- sp.from.asc(all_host_asc)

no <- niche.overlap(list(no.host = no_grid, native.host = native_grid, all.hosts = all_grid))
no #upper triangle is Schoner's, Lower is Hellinger's

###

# Code for ENMeval

####


