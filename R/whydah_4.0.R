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

#can elect to read in .csv of all thinned points
thin_ptw2<-read.csv("thin_0001.csv", head=T)
head(thin_ptw2)
thin_ptw2_coords<-thin_ptw2[,1:2]

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

#find 2nd San Fran point
which(ocw.unique$lat == 41.96554)
ocw.unique<-ocw.unique[-1061,] #remove san fran point

#re-check ocw points
plot(wrld_simpl)
points(ocw.unique,col="red")

#only complete cases
ocw.unique<-ocw.unique[complete.cases(ocw.unique),]
dim(ocw.unique)

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
filter(cw.unique, lon<(-72) & lat>(35)) #find northern midwest point
points(-77.22568,38.97612)
which(cw.unique$lon == -77.22568, cw.unique$lat== 38.97612) #find it in the data.frame
cw.unique<- cw.unique[-6046,] #remove that row!

#Check Common Waxbill Points
plot(wrld_simpl)
points(cw.unique, col="red")

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

# Bronze Mannakin
bronze <- gbif('Lonchura', 'cucullata', geo = T, removeZeros = T)
bronze<-bronze[,c('lon','lat','country','species')]
bronze<-subset(bronze, !is.na(lat) & !is.na(lon))
bronze.unique<- distinct(select(bronze,lon,lat,country,species)) #remove duplicates
dim(bronze.unique)
head(bronze.unique)
plot(wrld_simpl)
points(bronze.unique)
bronze.unique<-filter(bronze.unique, country !="Canada") #Remove Canada
plot(wrld_simpl)
points(bronze.unique)

#Thin Bronze Mannikin
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

#Saving the thinned file
print(tempdir())
write.SpThin(
  thin_bronze,
  coords=FALSE,
  dir=tempdir()
)

# can elect to read in .csv of all thinned points
thin_bronze2<-read.csv("thin_0001.csv", head=T)
head(thin_bronze2) #Always check to make sure this shows correct species
dim(thin_bronze2)

# #Nutmeg Mannakin
# nutmeg<-gbif('Lonchura', 'punctulata', geo=T, removeZeros = T)
# save(nutmeg, file="nutmeg.rdata")
load("nutmeg.rdata")
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

####

# Presence/Absence Rasters

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

# P/A Raster for Common Waxbill
species <- "Common Waxbill"
thin_cw2<-thin_cw2[,1:2] #prepare only lat/lon data for pres/absence
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_cw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_cw2, raster.label=species)
pa_raster_cw

# P/A Raster for Bronze Mannikin
species <- "Bronze Mannikin"
thin_bronze2<-thin_bronze2[,1:2] #prepare only lat/lon data for pres/absence
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_bronze <- presence.absence.raster(mask.raster=myRaster, species.data=thin_bronze2, raster.label=species)
pa_raster_bronze

####

# Downloading Environmental Variables ####

####

# get the file names...these should be all of our our worldclim
files <- list.files(path="~/Desktop/Whydah Project/whydah/Data/wc2", pattern="bil", full.names=TRUE)

mask <- raster(files[1]) #just sampling from 1 of the worldclim variables (since they are all from whole world)
set.seed(1) #makes sure we're generating the same random numbers

# stack predictors
files
predictors<-stack(files)
predictors_ocw_and_cw <- stack(files, pa_raster_cw, pa_raster_ocw)
predictors_two_native_one_novel<-stack(files, pa_raster_cw,pa_raster_ocw,pa_raster_nutmeg)
predictors_three_native_one_novel<-stack(files, pa_raster_cw,pa_raster_ocw,pa_raster_nutmeg,pa_raster_bronze)

# # Background from MCP
# set.seed(23)
# backg_mcp <- mcp(thin_ptw2_coords)
# plot(wrld_simpl)
# points(thin_ptw2_coords, col = "cyan4", cex = .5)
# plot(backg_mcp, add = T)
# 
# predictors2 <- predictors
# env_mask_whydah <- mask(predictors2, backg_mcp)
# env_crop_whydah <- crop(env_mask_whydah, backg_mcp)
# backg_mcp<-randomPoints(env_crop_whydah[[1]],n=10000)
# plot(wrld_simpl,main="Background Points with \nExtent Limited to Whydah Distribution")
# points(backg_mcp, cex=.3, col="purple")

#####

# Background points from five degree buffer 

#####

whydah_occurrences_spdf <- SpatialPointsDataFrame(coords = thin_ptw2_coords, data = thin_ptw2_coords,
                                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

writeOGR(whydah_occurrences_spdf, dsn = ".",layer = "whydah_csv_spdf_as_shp_new", driver = "ESRI Shapefile")

#now read in the file w/ buffers
buffered_region_five <- readGDAL("five_degree_layer_cropped.tif")

#convert buffered region to raster
buffered_region_raster_five <- raster(buffered_region_five) #convert africa map to raster
set.seed(1)
backg_five_degree <- randomPoints(buffered_region_raster_five, n=10000)

plot(wrld_simpl)
points(backg_five_degree, col = "red", cex = 0.2)

####

# MaxEnt models for no host ####

####

mx_no_host_k_fold <- maxent(predictors, thin_ptw2_coords, a=backg_five_degree, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=5','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_no_host_k_fold@results

#### No Host All occurrences for final model ####
mx_no_host_all_occs <- maxent(predictors, thin_ptw2_coords, a=backg_five_degree, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_no_host_all_occs
mx_no_host_all_occs@results
mx_no_host_all_occs@lambdas
response(mx_no_host_all_occs)
plot(mx_no_host_all_occs)

px_no_host_all_occs <- predict(predictors, mx_no_host_all_occs) #make predictions of habitat suitability can include argument ext=ext
plot(px_no_host_all_occs, main= 'Maxent, raw values')
writeRaster(px_no_host_all_occs, filename="naitve_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# Threshold
training_suitability_no_host <- extract(px_no_host_all_occs, thin_ptw2_coords) #all predicted values, all occs
ten_thresh_no_host <- quantile(training_suitability_no_host, 0.1, na.rm = TRUE)
ten_thresh_no_host

# Where is suitability highest?
map_no_host_all_occs <- rasterToPoints(px_no_host_all_occs) #make predictions raster a set of points for ggplot
df_no_host_all_occs <- data.frame(map_no_host_all_occs) #convert to data.frame
colnames(df_no_host_all_occs) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_no_host_all_occs$Suitability)
plot(wrld_simpl)
points(filter(df_no_host_all_occs, Suitability >= .8), col="red")


#####

# Maxent Models for Exotic Model ####

#####

# 5-fold validation
mx_exotic_model <- maxent(predictors_three_native_one_novel, thin_ptw2_coords, a=backg_five_degree, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=5','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_exotic_model@results

# all occurrences
mx_exotic_model_all_occs <- maxent(predictors_three_native_one_novel, thin_ptw2_coords, a=backg_five_degree, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_exotic_model_all_occs@results
mx_exotic_model_all_occs@lambdas
response(mx_exotic_model_all_occs)
plot(mx_exotic_model_all_occs)
mx_exotic_model_all_occs@results

px_exotic_model <- predict(predictors_three_native_one_novel, mx_exotic_model_all_occs) #make predictions of habitat suitability can include argument ext=ext
plot(px_exotic_model, main= 'Maxent, raw values')
writeRaster(px_exotic_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Where is suitability highest?
map_exotics_all_occs <- rasterToPoints(px_exotic_model) #make predictions raster a set of points for ggplot
df_exotics_all_occs <- data.frame(map_exotics_all_occs) #convert to data.frame
colnames(df_exotics_all_occs) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_exotics_all_occs$Suitability)
plot(wrld_simpl)
points(filter(df_exotics_all_occs, Suitability >= .84), col="red")

#####

# Prepare Full Hosts Model

#####

#import all host data 

# spot_backed_weaver <- gbif('Ploceus', 'cucullatus', geo = T, removeZeros = T)
# africa_firefinch <- gbif('Lagonosticta', 'rubricata', geo = T, removeZeros = T)
# black_bellied_firefinch <- gbif('Lagonosticta', 'rara', geo = T, removeZeros = T)
# red_bellied_firefinch <- gbif('Lagonosticta', 'senegala', geo = T, removeZeros = T)
# black_cheeked_waxbill <- gbif('Estrilda', 'erythronotos', geo = T, removeZeros = T)
# scaly_weaver <- gbif('Sporopipes', 'squamifrons', geo = T, removeZeros = T)
# grosbeak_weaver <- gbif('Amblyospiza', 'albifrons', geo = T, removeZeros = T)
# long_tailed_widowbird <- gbif('Euplectes', "progne", geo = T, removeZeros = T)
# african_quail_finch <- gbif('Ortygospiza', "atricollis", geo = T, removeZeros = T)
# red_collared_widowbird <- gbif('Euplectes', 'ardens', geo = T, removeZeros = T)
# swee_waxbill <- gbif('Coccopygia', 'melanotis', geo = T, removeZeros = T)
# yellow_bellied_waxbill <- gbif('Coccopygia' ,'quartinia', geo = T, removeZeros = T)
# fawn_breasted_waxbill<- gbif('Estrilda', 'paludicola', geo = T, removeZeros = T)
# crimson_rumped_waxbill <- gbif('Estrilda', 'rhodopyga', geo = T, removeZeros = T)
# black_rumped_waxbill <- gbif('Estrilda', 'troglodytes', geo = T, removeZeros = T)
# black_crowned_waxbill <- gbif('Estrilda', 'nonnula', geo = T, removeZeros = T)
# zebra_waxbill <- gbif('Amandava', 'subflava', geo = T, removeZeros = T)
# african_silverbill <- gbif('Euodice', 'cantans', geo = T, removeZeros = T)
# magpie_munia <- gbif('Lonchura', 'fringilloides', geo = T, removeZeros = T)
# streaky_seedeater <- gbif('Serinus', 'striolatus', geo = T, removeZeros = T)
# african_golden_breasted_bunting <- gbif('Emberiza', 'flaviventris', geo = T, removeZeros = T)

# Subset and thin host data
all_species_list <- list(spot_backed_weaver, africa_firefinch, black_bellied_firefinch, red_bellied_firefinch, black_cheeked_waxbill, scaly_weaver, grosbeak_weaver, long_tailed_widowbird, african_quail_finch, red_collared_widowbird, swee_waxbill, yellow_bellied_waxbill, fawn_breasted_waxbill, crimson_rumped_waxbill, black_rumped_waxbill, black_crowned_waxbill, zebra_waxbill, african_silverbill, magpie_munia, bronze_munia, streaky_seedeater, african_golden_breasted_bunting)
all_species_list_subset_columns <- lapply(all_species_list, function(i) {i[,c('lon','lat','country','species')]})
head(all_species_list_subset_columns[[2]])
all_species_no_duplicates <- lapply(all_species_list_subset_columns, function(i) {unique(i)})
head(all_species_no_duplicates[[1]])
all_species_no_nas<- lapply(all_species_no_duplicates, function(i) {i[!is.na(i),]})
all_species_complete_cases <- lapply(all_species_no_nas, function(i) {i[complete.cases(i),]})

# merging all host data.frames in a list
merged.data.frame <- Reduce(function(...) merge(..., all=T), all_species_complete_cases)
head(merged.data.frame)

crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# thin all hosts
all_species_thinned_after_merge <-
  spThin(
    merged.data.frame,
    x.col = "lon",
    y.col = "lat",
    dist = 10000,
    method= "gurobi",
    great.circle.distance=TRUE)

write.SpThin(all_species_thinned_after_merge,coords=FALSE,dir=tempdir())
all_hosts_thinned <- read.csv("thin_0001.csv", head=T)
all_hosts_thinned <- all_hosts_thinned[,1:2]
plot(wrld_simpl)
points(all_hosts_thinned)

# Generate PA raster for all hosts
species <- "AllOtherHosts"
head(all_hosts_thinned)
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_all_others <- presence.absence.raster(mask.raster=myRaster, species.data=all_hosts_thinned, raster.label=species)
str(pa_raster_all_others)
predictors_all_hosts_no_nutmeg <- stack(files, pa_raster_cw, pa_raster_ocw, pa_raster_all_others)
predictors_all_hosts <- stack(files, pa_raster_cw,pa_raster_ocw, pa_raster_nutmeg, pa_raster_all_others)

####

# Maxent Full Host Model ####

####

mx_full_hosts <- maxent(predictors_all_hosts, thin_ptw2_coords, a = backg_five_degree, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=5','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_full_hosts@results

## All occurrences for final model
mx_full_hosts_all_occs <- maxent(predictors_all_hosts, thin_ptw2_coords, a=backg_five_degree, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_full_hosts_all_occs@lambdas
mx_full_hosts_all_occs@results
response(mx_full_hosts_all_occs)
plot(mx_full_hosts_all_occs)

px_full_hosts_all_occs <- predict(predictors_all_hosts, mx_full_hosts_all_occs) #make predictions of habitat suitability can include argument ext=ext
plot(px_full_hosts_all_occs, main= 'Maxent, raw values')
writeRaster(px_full_hosts_all_occs, filename="full_hosts_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# Threshold
training_suitability_full_hosts_model <- extract(px_full_hosts_all_occs, thin_ptw2_coords) #all predicted values, all occs
ten_thresh_full_hosts_model <- quantile(training_suitability_full_hosts_model, 0.1, na.rm = TRUE)
ten_thresh_full_hosts_model

# Where is suitability highest?
map_full_hosts_all_occs <- rasterToPoints(px_full_hosts_all_occs)
df_full_hosts_all_occs <- data.frame(map_full_hosts_all_occs) #convert to data.frame
colnames(df_full_hosts_all_occs) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_full_hosts_all_occs$Suitability)
plot(wrld_simpl)
points(filter(df_full_hosts_all_occs, Suitability >= .87), col="red")

#####

# Heatmap of suitable regions

####

df_no_host_pres <- df_no_host_all_occs %>% mutate(pres_no_host = ifelse(Suitability >= 0.2497079, 1, 0))
df_exotic_host_pres <- df_exotics_all_occs %>% mutate(pres_two_native_one_novel_host = ifelse(Suitability >= 0.2159376, 1, 0))
df_full_host_pres <- df_full_hosts_all_occs %>% mutate(pres_all_host = ifelse(Suitability >= 0.1720696, 1, 0))

df_all_presence_a1 <- left_join(df_no_host_pres, df_exotic_host_pres, by = c("lon", "lat"))
df_all_presence_a2 <- left_join(df_all_presence_a1, df_full_host_pres, by = c("lon", "lat"))
head(df_all_presence_a2)
df_all_presence_a2$total <- rowSums(df_all_presence_a2[, c(4, 6, 8)])

#converting to geotiff
df_all_presences_combined<-df_all_presence_a2[,c(1,2,9)]
head(df_all_presences_combined)
coordinates(df_all_presences_combined) <- ~ lon + lat
gridded(df_all_presences_combined) <- TRUE
raster_of_heatmap <- raster(df_all_presences_combined)
writeRaster(raster_of_heatmap, filename="heatmap_whydah.tif", format="GTiff", overwrite=TRUE)

#####

# Calculating Schoner's D

####

naive_asc <- writeRaster(px_no_host_all_occs, filename = 'naive_model.asc', format = "ascii", overwrite = TRUE)
exotic_asc <- writeRaster(px_exotic_model, filename = "exotics_model.asc", format = "ascii", overwrite = TRUE)
full_asc <- writeRaster(px_full_hosts_all_occs, filename = "full_model.asc", format = "ascii", overwrite = TRUE)

naive_asc <- read.asc("naive_model.asc")
exotic_asc <- read.asc("exotics_model.asc")
full_asc <- read.asc("full_model.asc")

naive_grid <- sp.from.asc(naive_asc)
exotic_grid <- sp.from.asc(exotic_asc)
full_grid <- sp.from.asc(full_asc)

no <- niche.overlap(list(naive = naive_grid, exotic.host = exotic_grid, full.host = full_grid))
no #upper triangle is Schoner's, Lower is Hellinger's



#####

# Experimenting with MESS maps

#####

reference_points <- extract(predictors_all_hosts, thin_ptw2_coords)
reference_points
mss <- mess(x = predictors_all_hosts, v = reference_points, full = TRUE)
plot(predictors_all_hosts[[20]])
plot(mss)
?mess

#####

# Lockwood Lab Homework Assignment 

#####

## Where do mynas occur in cold climates
projection(plot(predictors_all_hosts[[15]]))
plot(wrld_simpl)
plot(predictors_all_hosts[[15]])
points(thin_ptw2_coords, cex = .25, col = "red")
whydah_points_with_bio_5 <- extract(predictors_all_hosts[[15]], thin_ptw2_coords)
points(thin_ptw2_coords[2377,])
which(whydah_points_with_bio_5 < 150)

#no clamping
mx_whydah_all_host_no_clamp <- maxent(predictors_all_hosts, thin_ptw2_coords, a=backg_five_degree, args=c('doclamp=FALSE','betamultiplier=3','responsecurves=TRUE','replicatetype=crossvalidate', 'replicates=5','writebackgroundpredictions=TRUE'))
mx_whydah_all_host_no_clamp

mx_whydah_all_host_all_occs_no_clamp <- maxent(predictors_all_hosts, thin_ptw2_coords, a=backg_five_degree, args=c('doclamp=FALSE','betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_whydah_all_host_all_occs_no_clamp
px_whydah_all_host_no_clamp <- predict(predictors_all_hosts, mx_whydah_all_host_all_occs_no_clamp) #make predictions of habitat suitability can include argument ext=ext
plot(px_whydah_all_host_no_clamp, main= 'Maxent, raw values')
writeRaster(px_whydah_all_host_no_clamp, filename="whydah_all_host_no_clamp.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

#no bio5
mx_whydah_all_host_no_bio5 <- maxent(predictors_no5, thin_ptw2_coords, a=backg_five_degree, args=c('doclamp=TRUE','betamultiplier=3','responsecurves=TRUE','replicatetype=crossvalidate', 'replicates=5','writebackgroundpredictions=TRUE'))
mx_whydah_all_host_no_bio5

mx_whydah_all_host_all_occs_no_bio5 <- maxent(predictors_no5, thin_ptw2_coords, a=backg_five_degree, args=c('doclamp=TRUE','betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
px_whydah_all_host_no_bio5 <- predict(predictors_no5, mx_whydah_all_host_all_occs_no_bio5) #make predictions of habitat suitability can include argument ext=ext
plot(px_whydah_all_host_no_bio5, main= 'Maxent, raw values')
writeRaster(px_whydah_all_host_no_bio5, filename="whydah_all_host_no_bio5.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

### Number of params in each model
# read lambdas file
rf <- read.table(file.path(curpath, 'species.lambdas'), sep=',', fill=TRUE)
# record no. of params (restrict df to rows with four values and no 0 in 2nd column)
p <- nrow(rf[!is.na(rf[3]) & rf[2] != 0,])

###

# Jamies MCP function

####
mcp <- function (xy) {
  # handler for spatial objects -- extracts coordinates
  if (class(xy) == "SpatialPoints" | class(xy) == "SpatialPointsDataFrame") {
    xy <- as.data.frame(coordinates(xy))
  }
  # get mcp indices
  i <- chull(xy)
  # get rows in xy for i
  xy.mcp <- xy[i,]
  # copy first row to last position to close shape
  xy.mcp <- rbind(xy.mcp[nrow(xy.mcp),], xy.mcp)
  # return polygon of mcp
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.mcp))), 1))))
}


