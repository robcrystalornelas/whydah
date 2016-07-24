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

####################################################################################################
####################################   Whydah Occurrences   ########################################
####################################################################################################

# Full occurrence dataset####
# ptw<-gbif('Vidua', 'macroura', geo=T, removeZeros = T)
# save(ptw, file="ptw.rdata")
load("ptw.rdata")
head(ptw)
dim(ptw)
ptw<-ptw[,c('lon','lat','country','species')]
ptw<-subset(ptw, !is.na(lat) & !is.na(lon))
head(ptw)
ptw.unique<- distinct(select(ptw,lon,lat,country,species)) #remove duplicates
dim(ptw.unique)
head(ptw.unique)
plot(wrld_simpl)
points(ptw.unique)

# Removing outliers
unique(ptw.unique$country) #Remove Taiwan Whydahs
ptw.unique<-filter(ptw.unique, country !=c("Taiwan")) #get rid of Taiwan sightings.
ptw.unique<-filter(ptw.unique, country !=c("United Arab Emirates"))

filter(ptw.unique, lon<(-80) & lat>30 & country=="United States") #find Chicago point
which(ptw.unique$lon == -82.9989, ptw.unique$lat== 39.96110) #find it in the data.frame
ptw.unique<- ptw.unique[-4530,] #remove that row!

filter(ptw.unique, lon<(-122) & lat>35 & country=="United States") #find San Fran point
which(ptw.unique$lon == -122.511, ptw.unique$lat== 37.7777)
ptw.unique<-ptw.unique[-1311,] #remove san fran point

# only complete cases
ptw.unique<-ptw.unique[complete.cases(ptw.unique),]
dim(ptw.unique)
is.na(ptw.unique) #no NAs in df

# check map
data("wrld_simpl")
plot(wrld_simpl)
points(ptw.unique, col="red")
dim(ptw.unique)

# Clean/Organize Data ####
lonzero = subset(ptw.unique, lon==0) #any points have longitude that was auto-set to 0
lonzero #All OK

# spThin ####
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

# Saving the thinned file ####
# print temporary dir
print(tempdir())
write.SpThin(
  thin1,
  coords=FALSE,
  dir=tempdir()
)

# Read in .csv of all thinned points
# thin_ptw2<-read.csv("thin_0001.csv", head=T)
head(thin_ptw2)
thin_ptw2_coords<-thin_ptw2[,1:2]

####################################################################################################
################################   Orange-cheeked Occurrences   ####################################
####################################################################################################

setwd("~/Desktop/Whydah Project/whydah/Data") #set back to data directory

# ocw<-gbif('Estrilda', 'melpoda', geo=T, removeZeros = T)
# save(ocw, file="ocw.rdata")
load("ocw.rdata")
head(ocw)
dim(ocw)

ocw<-ocw[,c('lon','lat','country','species')]
ocw<-subset(ocw, !is.na(lat) & !is.na(lon))
head(ocw)
ocw.unique<- distinct(select(ocw,lon,lat,country,species)) #remove duplicates
dim(ocw.unique)
head(ocw.unique)

# Removing outliers by country
plot(wrld_simpl)
points(ocw.unique, col="red")

unique(ocw.unique$country)
ocw.unique<-filter(ocw.unique, country !="Canada") #Remove Canada
ocw.unique<-filter(ocw.unique, country !="Germany") #Remove Germany
unique(ocw.unique$country)

# Remove outliers by lon/lat
filter(ocw.unique, lon>(-100) & lon<(-89) & country=="United States") #find northern midwest point
which(ocw.unique$lon == -95.55926, ocw.unique$lat== 29.69430) #find it in the data.frame
ocw.unique<- ocw.unique[-407,] #remove that row!

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

# Find 2nd San Fran point
which(ocw.unique$lat == 41.96554)
ocw.unique<-ocw.unique[-1061,] #remove san fran point

# Re-check ocw points
plot(wrld_simpl)
points(ocw.unique,col="red")

# Complete cases
ocw.unique<-ocw.unique[complete.cases(ocw.unique),]
dim(ocw.unique)

# OCW Thinning ####
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

# Save thinned file
print(tempdir())
write.SpThin(
  thin_ocw,
  coords=FALSE,
  dir=tempdir()
)

# Read in .csv of thinned points
# thin_ocw2<-read.csv("thin_0001.csv", head=T)
head(thin_ocw2) #Always check to make sure this shows correct species

####################################################################################################
##############################   Common Waxbill Occurrences   ######################################
####################################################################################################

setwd("~/Desktop/Whydah Project/whydah/Data") #back to data directory
# cw<-gbif('Estrilda', 'astrild', geo=T, removeZeros = T)
# save(cw, file="cw.rdata")
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

# Removing outliers by country
plot(wrld_simpl)
points(cw.unique, col="red")
cw.unique<-cw.unique[complete.cases(cw.unique),]
cw.unique<-filter(cw.unique, country !="Canada")#Remove Canada
cw.unique<-filter(cw.unique, country !="United Arab Emirates") #remove UAE (listed as escapes)
unique(cw.unique$country)

# Remove outliers by lon/lat
filter(cw.unique, lon<(-72) & lat>(35)) #find northern midwest point
points(-77.22568,38.97612)
which(cw.unique$lon == -77.22568, cw.unique$lat== 38.97612) #find it in the data.frame
cw.unique<- cw.unique[-6046,] #remove that row!

# Check Common Waxbill Points
plot(wrld_simpl)
points(cw.unique, col="red")

# Thin Common Waxbill
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

# Save thinned file
print(tempdir())
write.SpThin(
  thin_cw,
  coords=FALSE,
  dir=tempdir()
)

# Read .csv of  thinned points
# thin_cw2<-read.csv("thin_0001.csv", head=T)
head(thin_cw2)

####################################################################################################
#############################   Bronze Mannikin Occurrences   ######################################
####################################################################################################

setwd("~/Desktop/Whydah Project/whydah/Data")
# bronze <- gbif('Spermestes', 'cucullata', geo = T, removeZeros = T)
# save(bronze, file="bronze.rdata")
load("bronze.rdata")
bronze<-bronze[,c('lon','lat','country','species')]
bronze<-subset(bronze, !is.na(lat) & !is.na(lon))
bronze.unique<- distinct(select(bronze,lon,lat,country,species)) #remove duplicates
head(bronze.unique)
plot(wrld_simpl)
points(bronze.unique)
bronze.unique<-filter(bronze.unique, country !="Canada") #Remove Canada
plot(wrld_simpl)
points(bronze.unique)

# Thin Bronze Mannikin
setwd("~/Desktop/Whydah Project/whydah/Output")
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

thin_bronze <-spThin(
  bronze.unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 10000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_bronze)

# Save thinned file
print(tempdir())
write.SpThin(
  thin_bronze,
  coords=FALSE,
  dir=tempdir()
)

# Read .csv of all thinned points
# thin_bronze2<-read.csv("thin_0001.csv", head=T)
head(thin_bronze2) #Always check to make sure this shows correct species
dim(thin_bronze2)

####################################################################################################
#####################################   Nutmeg Mannikin   ##########################################
####################################################################################################

setwd("~/Desktop/Whydah Project/whydah/Data")
# nutmeg<-gbif('Lonchura', 'punctulata', geo=T, removeZeros = T)
# save(nutmeg, file="nutmeg.rdata")
load("nutmeg.rdata")
nutmeg<-nutmeg[,c('lon','lat','country','species')]
nutmeg<-subset(nutmeg, !is.na(lat) & !is.na(lon))
head(nutmeg)
nutmeg.unique<- distinct(select(nutmeg,lon,lat,country,species)) #remove duplicates
dim(nutmeg.unique)
head(nutmeg.unique)

# Remove outliers by country
plot(wrld_simpl)
points(nutmeg.unique, col="red")

unique(nutmeg.unique$country)
nutmeg.unique<-filter(nutmeg.unique, country !="Canada") #Remove Canada
nutmeg.unique<-filter(nutmeg.unique, country !="Honduras") #Remove Honduras
unique(nutmeg.unique$country)

# Remove outliers by lon/lat...for nutmegs these include museum speciments and acoustic labs in US
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

# Re-check points for more outliers
plot(wrld_simpl)
points(nutmeg.unique, col="red")

# Complete cases
nutmeg.unique<-nutmeg.unique[complete.cases(nutmeg.unique),]
dim(nutmeg.unique)

# Thin Nutmeg
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

# Save thinned file
print(tempdir())
write.SpThin(
  thin_nutmeg,
  coords=FALSE,
  dir=tempdir()
)

# Read .csv of thinned points
# thin_nutmeg2<-read.csv("thin_0001.csv", head=T)
head(thin_nutmeg2) #Always check to make sure this shows correct species

####################################################################################################
################################   Black-rumped Waxbill Occs   ######################################
####################################################################################################

setwd("~/Desktop/Whydah Project/whydah/Data")
# black_rumped_waxbill <- gbif('Estrilda', 'troglodytes', geo = T, removeZeros = T)
# save(black_rumped_waxbill, file="black_rumped_waxbill.rdata")
load("black_rumped_waxbill.rdata")
black_rumped_waxbill<-black_rumped_waxbill[,c('lon','lat','country','species')]
black_rumped_waxbill<-subset(black_rumped_waxbill, !is.na(lat) & !is.na(lon))
head(black_rumped_waxbill)
black_rumped_waxbill.unique<- distinct(select(black_rumped_waxbill,lon,lat,country,species)) #remove duplicates
dim(black_rumped_waxbill.unique)
head(black_rumped_waxbill.unique)

# Remove outliers by country
plot(wrld_simpl)
points(black_rumped_waxbill.unique, col="red")

unique(black_rumped_waxbill.unique$country)
black_rumped_waxbill.unique<-filter(black_rumped_waxbill.unique, country !="Canada") #Remove Canada
unique(black_rumped_waxbill.unique$country)

# Remove outliers by lon/lat...for nutmegs these include museum speciments and acoustic labs in US
# click()
filter(black_rumped_waxbill.unique, lon>(-84) & lat>(40) & country=="United States")
points(-83.14313, 42.47483,col="green")
which(black_rumped_waxbill.unique$lon == -83.14313, black_rumped_waxbill.unique$lat== 42.47483) #find it in the data.frame
black_rumped_waxbill.unique<- black_rumped_waxbill.unique[-222,] #remove that row!

# Re-check points for more outliers
plot(wrld_simpl)
points(black_rumped_waxbill.unique, col="red")

# Complete cases
black_rumped_waxbill.unique<-black_rumped_waxbill.unique[complete.cases(black_rumped_waxbill.unique),]
dim(black_rumped_waxbill.unique)

# Thin Nutmeg
setwd("~/Desktop/Whydah Project/whydah/Output")
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

thin_black_rumped_waxbill <-spThin(
  black_rumped_waxbill.unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 10000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_black_rumped_waxbill)
str(thin_black_rumped_waxbill)
plot(thin_black_rumped_waxbill)

# Save thinned file
print(tempdir())
write.SpThin(
  thin_black_rumped_waxbill,
  coords=FALSE,
  dir=tempdir()
)

# Read .csv of thinned points
# thin_black_rumped_waxbill2<-read.csv("thin_0001.csv", head=T)
head(thin_black_rumped_waxbill2) #Always check to make sure this shows correct species

####################################################################################################
################################   African Silverbill Occs   #######################################
####################################################################################################

setwd("~/Desktop/Whydah Project/whydah/Data")
# african_silverbill <- gbif('Euodice', 'cantans', geo = T, removeZeros = T)
# save(african_silverbill, file="african_silverbill.rdata")
load("african_silverbill.rdata")
silverbill<-african_silverbill
silverbill<-silverbill[,c('lon','lat','country','species')]
silverbill<-subset(silverbill, !is.na(lat) & !is.na(lon))
head(silverbill)
silverbill.unique<- distinct(select(silverbill,lon,lat,country,species)) #remove duplicates
dim(silverbill.unique)
head(silverbill.unique)

# Remove outliers by country
plot(wrld_simpl)
points(silverbill.unique, col="red")

# Complete cases
silverbill.unique<-silverbill.unique[complete.cases(silverbill.unique),]
dim(silverbill.unique)

# Thin Nutmeg
setwd("~/Desktop/Whydah Project/whydah/Output")
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

thin_silverbill <-spThin(
  silverbill.unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 10000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_silverbill)
str(thin_silverbill)
plot(thin_silverbill)

# Save thinned file
print(tempdir())
write.SpThin(
  thin_silverbill,
  coords=FALSE,
  dir=tempdir()
)

# Read .csv of thinned points
# thin_silverbill2<-read.csv("thin_0001.csv", head=T)
head(thin_silverbill2) #Always check to make sure this shows correct species

####################################################################################################
################################   Presence/Absence Rasters   ######################################
####################################################################################################

# Function for Presence/Absence Rasters for Host Species by Amy Whitehead
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

# P/A Raster for Common Waxbill
species <- "Common Waxbill"
thin_cw2<-thin_cw2[,1:2] #prepare only lat/lon data for pres/absence
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_cw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_cw2, raster.label=species)
pa_raster_cw

# P/A Raster for Orange-Cheeked Waxbill
species <- "OrangeCheekedWaxbill"
thin_ocw2<-thin_ocw2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
myRaster <- raster( "bio1.bil")
pa_raster_ocw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_ocw2, raster.label=species)
pa_raster_ocw

# P/A Raster for Nutmeg
species <- "Nutmeg Mannikin"
thin_nutmeg2<-thin_nutmeg2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
myRaster <- raster( "bio1.bil")
pa_raster_nutmeg <- presence.absence.raster(mask.raster=myRaster, species.data=thin_nutmeg2, raster.label=species)
pa_raster_nutmeg

# P/A Raster for Bronze Mannikin
species <- "Bronze Mannikin"
thin_bronze2<-thin_bronze2[,1:2] #prepare only lat/lon data for pres/absence
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_bronze <- presence.absence.raster(mask.raster=myRaster, species.data=thin_bronze2, raster.label=species)
pa_raster_bronze

# P/A Raster for Black-rumped
species <- "Black-rumped"
thin_black_rumped_waxbill2<-thin_black_rumped_waxbill2[,1:2] #prepare only lat/lon data for pres/absence
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_black_rumped_waxbill <- presence.absence.raster(mask.raster=myRaster, species.data=thin_black_rumped_waxbill2, raster.label=species)
pa_raster_black_rumped_waxbill

# P/A Raster for Silverbill
species <- "Silverbill"
thin_silverbill2<-thin_silverbill2[,1:2] #prepare only lat/lon data for pres/absence
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_silverbill <- presence.absence.raster(mask.raster=myRaster, species.data=thin_silverbill2, raster.label=species)
pa_raster_silverbill

####################################################################################################
################################   Downloading Environmental Vars ##################################
####################################################################################################

# get the file names...these should be all of our our worldclim
files <- list.files(path="~/Desktop/Whydah Project/whydah/Data/wc2", pattern="bil", full.names=TRUE)
files

mask <- raster(files[1]) #just sampling from 1 of the worldclim variables (since they are all from whole world)
set.seed(1) #makes sure we're generating the same random numbers

# stack predictors
predictors<-stack(files)
predictors_and_exotic_hosts <- stack(files, pa_raster_cw,pa_raster_ocw,pa_raster_nutmeg,pa_raster_bronze,pa_raster_black_rumped_waxbill,pa_raster_silverbill)
names(predictors_and_exotic_hosts)

####################################################################################################
########################### Background points from five degree buffer ##############################
####################################################################################################
setwd('/Users/rpecchia/Desktop/Whydah Project/whydah/Data')

whydah_occurrences_spdf <- SpatialPointsDataFrame(coords = thin_ptw2_coords, data = thin_ptw2_coords,
                                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

writeOGR(whydah_occurrences_spdf, dsn = ".",layer = "whydah_csv_spdf_as_shp_new", driver = "ESRI Shapefile")

#now read in the file w/ buffers from QGIS
buffered_region_five <- readGDAL("five_degree_layer_cropped.tif")

#convert buffered region to raster
buffered_region_raster_five <- raster(buffered_region_five) #convert africa map to raster
set.seed(1)
backg_five_degree <- randomPoints(buffered_region_raster_five, n=10000)

plot(wrld_simpl)
points(backg_five_degree, col = "red", cex = 0.2)

####################################################################################################
######################################### MaxEnt for No Host #######################################
####################################################################################################

# No host k-fold
mx_no_host_k_fold <- maxent(predictors, thin_ptw2_coords, a=backg_five_degree, 
                            args=c('betamultiplier=3','responsecurves=TRUE', 
                                   'replicatetype=crossvalidate', 'replicates=5',
                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_no_host_k_fold@results

# No Host all occurrences
mx_no_host_all_occs <- maxent(predictors, thin_ptw2_coords, a=backg_five_degree, 
                              args=c('betamultiplier=3','responsecurves=TRUE',
                                     'writebackgroundpredictions=TRUE'))
# mx_no_host_all_occs
mx_no_host_all_occs@results
mx_no_host_all_occs@lambdas
response(mx_no_host_all_occs)
plot(mx_no_host_all_occs)

px_no_host_all_occs <- predict(predictors, mx_no_host_all_occs, progress='window') #make predictions of habitat suitability can include argument ext=ext
plot(px_no_host_all_occs, main= 'Maxent, raw values')
# writeRaster(px_no_host_all_occs, filename="naive_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# Making MESS map -- process failed when i tried to run
reference_points <- extract(predictors, thin_ptw2_coords)
mss <- mess(x=predictors, v=reference_points, full=TRUE)
plot(mss)

# Forming Confusion Matrix
training_suitability_naive <- extract(px_no_host_all_occs, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_naive<-na.omit(training_suitability_naive)
ten_thresh_naive_model <- quantile(training_suitability_naive, 0.1, na.rm = TRUE)
ten_thresh_naive_model

pred_binary_naive <- training_suitability_naive > .2540227 #where are known presence greater than threshold?
length(pred_binary_naive[pred_binary_naive==TRUE]) # these are "a" the true positives
length(pred_binary_naive[pred_binary_naive==FALSE]) #these are "c" the false negatives

background_suitability_naive <- extract(px_no_host_all_occs, backg_five_degree)
pred_binary_background_naive <- background_suitability_naive > .2540227

length(pred_binary_background_naive[pred_binary_background_naive==TRUE]) #these are "b" the false pos
length(pred_binary_background_naive[pred_binary_background_naive==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Exotic Hosts ########################################
####################################################################################################

# k-fold
mx_exotic_model <- maxent(predictors_and_exotic_hosts, thin_ptw2_coords, a=backg_five_degree, 
                          args=c('betamultiplier=3','responsecurves=TRUE', 
                                 'replicatetype=crossvalidate', 'replicates=5',
                                 'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_exotic_model@results

# all occurrences
mx_exotic_model_all_occs <- maxent(predictors_and_exotic_hosts, thin_ptw2_coords, a=backg_five_degree, 
                                   args=c('betamultiplier=3','responsecurves=TRUE',
                                          'writebackgroundpredictions=TRUE'))

mx_exotic_model_all_occs@results
mx_exotic_model_all_occs@lambdas
response(mx_exotic_model_all_occs)
plot(mx_exotic_model_all_occs)
mx_exotic_model_all_occs@results

px_exotic_model <- predict(predictors_and_exotic_hosts, mx_exotic_model_all_occs, progress='window') #make predictions of habitat suitability can include argument ext=ext
plot(px_exotic_model, main= 'Maxent, raw values')
# writeRaster(px_exotic_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.1913834 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_five_degree)
pred_binary_background_exotic <- background_suitability_exotic > 0.1913834

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 

####################################################################################################
######################################### Heat Map #################################################
####################################################################################################

# Prepare predictions as data.frames
map_no_host_all_occs <- rasterToPoints(px_no_host_all_occs) #make predictions raster a set of points for ggplot
df_no_host_all_occs <- data.frame(map_no_host_all_occs) #convert to data.frame
colnames(df_no_host_all_occs) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings

map_exotic_host_all_occs <- rasterToPoints(px_exotic_model) #make predictions raster a set of points for ggplot
df_exotic_host_all_occs <- data.frame(map_exotic_host_all_occs) #convert to data.frame
colnames(df_exotic_host_all_occs) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings

df_no_host_pres <- df_no_host_all_occs %>% mutate(pres_no_host = ifelse(Suitability >= 0.2540, 1, 0))
df_no_host_pres <- df_no_host_pres[,c(1,2,4)] #get only binary output

df_exotic_host_pres <- df_exotic_host_all_occs %>% mutate(pres_exotic_host = ifelse(Suitability >= 0.1914, 1, 0))
head(df_exotic_host_pres)
df_exotic_host_pres <- df_exotic_host_pres[,c(1,2,4)] #get only binary output

df_all_presence_a1 <- left_join(df_no_host_pres, df_exotic_host_pres, by = c("lon", "lat"))
head(df_all_presence_a1)
df_all_presence_a1$total <- rowSums(df_all_presence_a1[, c(3, 5)])

# Converting Heatmap to geotiff
head(df_all_presence_a1)
df_all_presences_combined<-df_all_presence_a1[,c(1,2,6)]
head(df_all_presences_combined)
coordinates(df_all_presences_combined) <- ~ lon + lat
gridded(df_all_presences_combined) <- TRUE
raster_of_heatmap <- raster(df_all_presences_combined)
writeRaster(raster_of_heatmap, filename="heatmap_whydah.tif", format="GTiff", overwrite=TRUE)

# Binary map for No Hosts
coordinates(df_no_host_pres) <- ~ lon + lat
gridded(df_no_host_pres) <- TRUE
raster_no_host <- raster(df_no_host_pres)
writeRaster(raster_no_host, filename="no_host_raster.tif", format="GTiff", overwrite=TRUE)

# Binary map for exotic host
coordinates(df_exotic_host_pres) <- ~ lon + lat
gridded(df_exotic_host_pres) <- TRUE
raster_exotic_host <- raster(df_exotic_host_pres)
writeRaster(raster_exotic_host, filename="exotic_host_raster.tif", format="GTiff", overwrite=TRUE)

####################################################################################################
########################### Predicted Occupied Area for North America ##############################
####################################################################################################

# Make object for extent of North America
north_america_extent <-c(-162,-60,11.5,65)
setwd('/Users/rpecchia/Desktop/Whydah Project/whydah/Data')

antilles <- readOGR(dsn = ".", layer = "north_america_antilles_shapefile")
plot(antilles)
usa <- readOGR(dsn = ".", layer = "north_america_shapefile")
plot(usa)

antilles@polygons
usa@polygons

CNTY_ID <- "30"
length(CNTY_ID)
length
row.names(as(usa, "data.frame"))
row.names(as(antilles, "data.frame"))
usa2 <- spChFIDs(usa, as.character(30))

usa_and_antilles <- spRbind(usa2,antilles)

# No Host Raster of NA
no_host_north_america_binary <- crop(raster_no_host, north_america_extent)
plot(no_host_north_america_binary)
no_host_usa_antilles_binary <- mask(no_host_north_america_binary, usa_and_antilles)
plot(no_host_usa_antilles_binary)
sum(na.omit(no_host_usa_antilles_binary@data@values))

# Exotic Host Raster of NA
exotic_host_north_america_binary <- crop(raster_exotic_host, north_america_extent)
plot(exotic_host_north_america_binary)
exotic_host_usa_antilles_binary <- mask(exotic_host_north_america_binary, usa_and_antilles)
plot(exotic_host_usa_antilles_binary)
sum(na.omit(exotic_host_usa_antilles_binary@data@values))

####################################################################################################
#################################### Calculating Schoner's D #######################################
####################################################################################################

naive_asc <- writeRaster(px_no_host_all_occs, filename = 'naive_model.asc', format = "ascii", overwrite = TRUE)
exotic_asc <- writeRaster(px_exotic_model, filename = "exotics_model.asc", format = "ascii", overwrite = TRUE)

naive_asc <- read.asc("naive_model.asc")
exotic_asc <- read.asc("exotics_model.asc")

naive_grid <- sp.from.asc(naive_asc)
exotic_grid <- sp.from.asc(exotic_asc)

no <- niche.overlap(list(naive = naive_grid, exotic.host = exotic_grid))
no #upper triangle is Schoner's, Lower is Hellinger's

####################################################################################################
################################# Experimenting with MESS Maps #####################################
####################################################################################################

reference_points <- extract(predictors, thin_ptw2_coords)
reference_points
mss <- mess(x = predictors, v = reference_points, full = TRUE)
plot(predictors_all_hosts[[20]])
plot(mss)

# Calculate number of parameters in each model

# read lambdas file
rf <- read.table(file.path(curpath, 'species.lambdas'), sep=',', fill=TRUE)
# record no. of params (restrict df to rows with four values and no 0 in 2nd column)
p <- nrow(rf[!is.na(rf[3]) & rf[2] != 0,])