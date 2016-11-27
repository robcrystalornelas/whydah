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
data("wrld_simpl")
set.seed(1) #makes sure we're generating the same random numbers

####################################################################################################
####################################   Whydah Occurrences   ########################################
####################################################################################################

# Download occurrences
# ptw <- gbif('Vidua', 'macroura', geo=T, removeZeros = T)
# save(ptw, file="ptw.rdata")
load("ptw.rdata")
head(ptw)
dim(ptw)
ptw<-subset(ptw, basisOfRecord == "HUMAN_OBSERVATION")

### South Africa Points ##
ptw_south_africa<-subset(ptw, country == "South Africa")
dim(ptw_south_africa)
head(ptw_south_africa)
list(ptw_south_africa$datasetName)
count(ptw_south_africa$datasetName == "Southern African Bird Atlas Project 2")

# First, get index numbers for rows that have "SABP2" as their dataset name
rows_with_SABP2 <- which(ptw$datasetName == "Southern African Bird Atlas Project 2")
# remove those rows
ptw_no_SABP <- ptw[-rows_with_SABP2, ] 
count(ptw_no_SABP$datasetName == "Southern African Bird Atlas Project 2")

# Explore south africa more
ptw_south_africa_2 <- subset(ptw_no_SABP, country == "South Africa")
dim(ptw_south_africa_2)
head(ptw_south_africa_2)
# options(max.print = 30000)
table(ptw_south_africa_2$collectionCode)

remove_transects_rows <- which(ptw_south_africa_2$collectionCode == "EBIRD" | 
                                 ptw_south_africa_2$collectionCode == "EBIRD_AU" | 
                                 ptw_south_africa_2$collectionCode == "EBIRD_CAN" | 
                                 ptw_south_africa_2$collectionCode == "naturgucker" | 
                                 ptw_south_africa_2$collectionCode == "Observations" | 
                                 ptw_south_africa_2$collectionCode == "SAFRING")
remove_transects_rows

south_africa_no_transects <- ptw_south_africa_2[remove_transects_rows, ] 
table(south_africa_no_transects$collectionCode)

write.csv(south_africa_no_transects, file = "south_africa_no_transects.csv")

## Remove ALL South African Points
head(ptw_no_SABP$country)
ptw_no_south_africa <- filter(ptw_no_SABP, country !="South Africa")
list(ptw_no_south_africa$country)

# Merge with our much better SA whydah points
dim(ptw_no_south_africa)
dim(south_africa_no_transects)
new_ptw <- rbind(ptw_no_south_africa, south_africa_no_transects)
dim(new_ptw)

ptw_subset <- new_ptw[,c('lon','lat','country','species')]
ptw_subset<-subset(ptw_subset, !is.na(lat) & !is.na(lon))
ptw.unique<- distinct(select(ptw_subset,lon,lat,country,species)) #remove duplicates
dim(ptw.unique)
head(ptw.unique)
plot(wrld_simpl)
points(ptw.unique, cex = .2, col = "red")
# write.csv(ptw.unique, file = "ptw.unique.csv")

####################################################################################################
#######################################   Grasses Layer    #########################################
####################################################################################################
# Test Grasses points
# grasses <- gbif('Echinochloa', '*', geo=T, removeZeros = T)
# save(grasses, file="grasses.rdata")
load("grasses.rdata")
head(grasses)
dim(grasses)
grasses_subset<-subset(grasses, basisOfRecord == "HUMAN_OBSERVATION")
dim(grasses_subset)
grasses_subset<-grasses_subset[,c('lon','lat','country','species')]
grasses_subset<-subset(grasses_subset, !is.na(lat) & !is.na(lon))
head(grasses_subset)
grasses.unique<- distinct(select(grasses_subset,lon,lat,country,species)) #remove duplicates
dim(grasses.unique)
head(grasses.unique)
plot(wrld_simpl)
points(grasses.unique, cex = .2, col = "red")




# Removing outliers
unique(ptw.unique$country) #Remove Taiwan Whydahs
count(ptw.unique$country)
ptw.unique<-filter(ptw.unique, country !=c("Taiwan")) #get rid of Taiwan sightings.
ptw.unique<-filter(ptw.unique, country !=c("United Arab Emirates"))
ptw.unique<-filter(ptw.unique, country !=c("Spain"))
ptw.unique<-filter(ptw.unique, country !=c("Dominican Republic"))
ptw.unique<-filter(ptw.unique, country !=c("Portugal"))

# Can use this code to check number of records in a particular country
plot(wrld_simpl)
points(ptw.unique, cex = .2, col = "red")

ca_point <- which(ptw.unique$lon<(-122) & ptw.unique$lat>35 & ptw.unique$country=="United States") #San Fran point
ca_point
ptw.unique<-ptw.unique[-ca_point,] #remove san fran point
dim(ptw.unique)

# Remove California Point
ca_point_2 <- which(ptw.unique$lon<(-85) & ptw.unique$lon>(-90) & ptw.unique$lat>28 & ptw.unique$lat<32 & ptw.unique$country=="United States")
ca_point_2
ptw.unique <- ptw.unique[-ca_point_2,]
dim(ptw.unique)

# Remove random island in eastern atlantic
eastern_atlantic_island_point <- which(ptw.unique$lon<(1) & ptw.unique$lon>(-2) & ptw.unique$lat>(-28) & ptw.unique$lat<(-24))
eastern_atlantic_island_point
ptw.unique<-ptw.unique[-eastern_atlantic_island_point,]
dim(ptw.unique)

# Remove Reunion Points
# filter(ptw.unique, lon<(57) & lon>(53) & lat>(-24) & lat<(-19))
# which(ptw.unique$lon == 55.2873, ptw.unique$lat== -21.1819)
# ptw.unique<-ptw.unique[-45,]
# which(ptw.unique$lon == 55.4539, ptw.unique$lat== -20.8739)
# ptw.unique<-ptw.unique[-1082,]

# only complete cases
ptw.unique<-ptw.unique[complete.cases(ptw.unique),]
dim(ptw.unique)

#remove all the florida points
ptw_unique_with_florida <- ptw.unique

florida_points <- which(ptw.unique$lon<(-80) & ptw.unique$lon>(-82.5) & ptw.unique$lat<28.8 & ptw.unique$lat>25.5 & ptw.unique$country=="United States") #San Fran point
florida_points
ptw_unique_no_florida <- ptw.unique[-florida_points,]

plot(wrld_simpl)
points(ptw_unique_no_florida)

plot(wrld_simpl)
points(ptw_unique_with_florida)

write.csv(ptw_unique_no_florida, file = "ptw.unique.no.florida.qgis.csv")
write.csv(ptw_unique_with_florida, file = "ptw.unique.with.florida.qgis.csv")
# ptw.unique<-read.csv("whydah_for_editing.csv")

####################################################################################################
#################################################   spThin    ######################################
#####################################################################################################

#### PTW no florida

setwd("~/Desktop/Whydah Project/whydah/Data")
# set coordinate system
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
# then thin
thin1 <-spThin(
  ptw_unique_no_florida, 
  x.col = "lon",
  y.col = "lat",
  dist = 3000,
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
thin_ptw2<-read.csv("thin_0001.csv", head=T)
head(thin_ptw2)
thin_ptw2_coords<-thin_ptw2[,1:2]
write.csv(thin_ptw2_coords, file = "ptw.thinned.for.figure1.csv")


### PTW with Florida
setwd("~/Desktop/Whydah Project/whydah/Data")
# set coordinate system
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
# then thin
thin_ptw_with_florida <-spThin(
  ptw_unique_with_florida, 
  x.col = "lon",
  y.col = "lat",
  dist = 3000,
  method= "gurobi", #can change to "gurobi" to make it even faster, but have to install it first
  great.circle.distance=TRUE)
summary(thin_ptw_with_florida)

# Saving the thinned file ####
# print temporary dir
print(tempdir())
write.SpThin(
  thin_ptw_with_florida,
  coords=FALSE,
  dir=tempdir()
)

# Read in .csv of all thinned points
thin_ptw_with_florida_2<-read.csv("thin_0001.csv", head=T)
head(thin_ptw_with_florida_2)
thin_ptw_with_florida_coords<-thin_ptw_with_florida_2[,1:2]
write.csv(thin_ptw_with_florida_coords, file = "ptw.thinned.with.floridafor.figure1.csv")

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
  dist = 3000,
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
thin_ocw2<-read.csv("thin_0001.csv", head=T)
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
  dist = 3000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_cw)

# Save thinned file
print(tempdir())
write.SpThin(
  thin_cw,
  coords=FALSE,
  dir=tempdir()
)

# Read .csv of  thinned points
thin_cw2<-read.csv("thin_0001.csv", head=T)
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
  dist = 3000,
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
thin_bronze2<-read.csv("thin_0001.csv", head=T)
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
nutmeg.unique<- nutmeg.unique[-9196,] #remove that row!

filter(nutmeg.unique, lon>(-90) & lat>(40) & country=="United States") #find midwest points
points(-83.13383, 42.68063,col="green") #make sure it's the right one
which(nutmeg.unique$lon == -83.13383, nutmeg.unique$lat== 42.68063) #find it in the data.frame
nutmeg.unique<- nutmeg.unique[-7926,] #remove that row!
points(-83.72634, 42.27084,col="green") #make sure it's the right one
which(nutmeg.unique$lon == -83.72634, nutmeg.unique$lat== 42.27084) #find it in the data.frame
nutmeg.unique<- nutmeg.unique[-8706,] #remove that row!

filter(nutmeg.unique, lon>(-90) & lat>(38) & country=="United States") #last midwest point
points(-83.0189, 39.9961,col="green")
which(nutmeg.unique$lon == -83.0189, nutmeg.unique$lat== 39.9961) #find it in the data.frame
nutmeg.unique<- nutmeg.unique[-8952,] #remove that row!

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
  dist = 3000,
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
thin_nutmeg2<-read.csv("thin_0001.csv", head=T)
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
  dist = 3000,
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
thin_black_rumped_waxbill2<-read.csv("thin_0001.csv", head=T)
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
  dist = 3000,
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
thin_silverbill2<-read.csv("thin_0001.csv", head=T)
head(thin_silverbill2) #Always check to make sure this shows correct species

####################################################################################################
################################   Thinning Grasses   ######################################
####################################################################################################

grasses.subset <- grasses.unique[sample(nrow(grasses.unique), 10000), ] # Randomly select 10,000 rows so that spthin doesn't crash

thin_grasses <-spThin(
  grasses.subset, 
  x.col = "lon",
  y.col = "lat",
  dist = 3000,
  method= "gurobi",
  great.circle.distance=TRUE)
summary(thin_grasses)
str(thin_grasses)
plot(thin_grasses)

# Save thinned file
print(tempdir())
write.SpThin(
  thin_grasses,
  coords=FALSE,
  dir=tempdir()
)

# Read in .csv of thinned points
thin_grasses <- read.csv("thin_0001.csv", head=T)
head(thin_grasses) #Always check to make sure this shows correct species



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

setwd("~/Desktop/Whydah Project/whydah/Data/wc5")

# P/A Raster for Common Waxbill
species <- "Common Waxbill"
thin_cw2<-thin_cw2[,1:2] #prepare only lat/lon data for pres/absence
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
myRaster <- raster( "bio1.bil")
pa_raster_nutmeg <- presence.absence.raster(mask.raster=myRaster, species.data=thin_nutmeg2, raster.label=species)
pa_raster_nutmeg

# P/A Raster for Bronze Mannikin
species <- "Bronze Mannikin"
thin_bronze2<-thin_bronze2[,1:2] #prepare only lat/lon data for pres/absence
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
pa_raster_bronze <- presence.absence.raster(mask.raster=myRaster, species.data=thin_bronze2, raster.label=species)
pa_raster_bronze

# P/A Raster for Black-rumped
species <- "Black-rumped"
thin_black_rumped_waxbill2<-thin_black_rumped_waxbill2[,1:2] #prepare only lat/lon data for pres/absence
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
pa_raster_black_rumped_waxbill <- presence.absence.raster(mask.raster=myRaster, species.data=thin_black_rumped_waxbill2, raster.label=species)
pa_raster_black_rumped_waxbill

# P/A Raster for Silverbill
species <- "Silverbill"
thin_silverbill2<-thin_silverbill2[,1:2] #prepare only lat/lon data for pres/absence
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
pa_raster_silverbill <- presence.absence.raster(mask.raster=myRaster, species.data=thin_silverbill2, raster.label=species)
pa_raster_silverbill

species <- "Grasses"
thin_grasses2 <- thin_grasses[,1:2] #prepare only lat/lon data for pres/absence
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
pa_raster_grasses <- presence.absence.raster(mask.raster=myRaster, species.data=thin_grasses2, raster.label=species)
pa_raster_grasses

####################################################################################################
################################   Preparing Predictor Variables ###################################
####################################################################################################

setwd("~/Desktop/Whydah Project/whydah/Data")

#LULC Layer
LULC_layer <- readGDAL("LULC_Resampled.tif")
LULC_raster <- raster(LULC_layer)
plot(LULC_raster)

# get the file names...these should be all of our our worldclim
files <- list.files(path="~/Desktop/Whydah Project/whydah/Data/wc5", pattern="bil", full.names=TRUE)
files

# stack predictors
climate <- stack(files)
names(climate)

climate_and_hosts <- stack(files, pa_raster_cw,pa_raster_ocw,pa_raster_nutmeg,pa_raster_bronze,pa_raster_black_rumped_waxbill,pa_raster_silverbill)
names(climate_and_hosts)

climate_and_LULC <- stack(files, LULC_raster)
names(climate_and_LULC)

climate_and_grasses_occs <- stack(climate, pa_raster_grasses)

climate_and_hosts_and_grasses_occs <- stack(climate_and_hosts, pa_raster_grasses)
names(climate_and_hosts_and_grasses_occs)

climate_and_hosts_and_LULC <- stack(climate_and_hosts, LULC_raster)
names(climate_and_hosts_and_LULC)

hosts <- stack(pa_raster_cw,pa_raster_ocw,pa_raster_nutmeg,pa_raster_bronze,pa_raster_black_rumped_waxbill,pa_raster_silverbill)

####################################################################################################
########################### Background points from five degree buffer ##############################
####################################################################################################
setwd('/Users/rpecchia/Desktop/Whydah Project/whydah/Data')

whydah_occurrences_spdf <- SpatialPointsDataFrame(coords = thin_ptw2_coords, data = thin_ptw2_coords,
                                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

writeOGR(whydah_occurrences_spdf, dsn = ".",layer = "whydah_csv_spdf_as_shp", driver = "ESRI Shapefile")

#now read in the file w/ buffers from QGIS
buffered_region_five <- readGDAL("five_degree_layer_cropped.tif")

#convert buffered region to raster
buffered_region_raster_five <- raster(buffered_region_five) #convert africa map to raster
backg_five_degree <- randomPoints(buffered_region_raster_five, n=10000)

plot(wrld_simpl)
points(backg_five_degree, col = "red", cex = 0.2)

####################################################################################################
######################################### MaxEnt for Climate #######################################
####################################################################################################

# K-fold
mx_climate_k_fold <- maxent(climate, thin_ptw2_coords, a=backg_five_degree, 
                            args=c('responsecurves=TRUE', 
                                   'replicatetype=crossvalidate', 'replicates=5',
                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_k_fold@results

# Full occurrence set
mx_climate_full <- maxent(climate, thin_ptw2_coords, a=backg_five_degree, 
                          args=c('responsecurves=TRUE',
                                 'writebackgroundpredictions=TRUE'))

# response(mx_no_host_all_occs)
plot(mx_climate_full)
mx_climate_full@results

px_climate_full <- predict(climate, mx_climate_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_no_host_full, main= 'Maxent, raw values')

# Forming Confusion Matrix
training_suitability_naive <-extract(px_no_host_full, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_naive<-na.omit(training_suitability_naive)
ten_thresh_naive_model <- quantile(training_suitability_naive, 0.1, na.rm = TRUE)
ten_thresh_naive_model

pred_binary_naive <- training_suitability_naive > 0.2417432 #where are known presence greater than threshold?
length(pred_binary_naive[pred_binary_naive==TRUE]) # these are "a" the true positives
length(pred_binary_naive[pred_binary_naive==FALSE]) #these are "c" the false negatives

background_suitability_naive <- extract(px_no_host_all_occs, backg_five_degree)
pred_binary_background_naive <- background_suitability_naive > 0.2417432

length(pred_binary_background_naive[pred_binary_background_naive==TRUE]) #these are "b" the false pos
length(pred_binary_background_naive[pred_binary_background_naive==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Climate & Hosts ########################################
####################################################################################################

# k-fold
mx_climate_and_hosts_model <- maxent(climate_and_hosts, thin_ptw2_coords, a=backg_five_degree, 
                          args=c('responsecurves=TRUE', 
                                 'replicatetype=crossvalidate', 'replicates=5',
                                 'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_and_hosts_model@results

# all occurrences
mx_climate_and_hosts_full <- maxent(climate_and_hosts, thin_ptw2_coords, a=backg_five_degree, 
                                   args=c('responsecurves=TRUE',
                                          'writebackgroundpredictions=TRUE'))


response(mx_climate_and_hosts_full)
plot(mx_climate_and_hosts_full)
mx_climate_and_hosts_full@results
mx_climate_and_hosts_full@lambdas

px_climate_and_hosts <- predict(climate_and_hosts, mx_climate_and_hosts_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_and_hosts, main= 'Maxent, raw values')
writeRaster(px_climate_and_hosts, filename="climate_and_hosts_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_five_degree)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Climate & Echinchla ########################################
####################################################################################################

# k-fold
mx_climate_and_grasses_model <- maxent(climate_and_grasses_occs, thin_ptw2_coords, a=backg_five_degree, 
                          args=c('responsecurves=TRUE', 
                                 'replicatetype=crossvalidate', 'replicates=5',
                                 'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_and_grasses_model@results

# all occurrences
mx_climate_and_grasses_full <- maxent(climate_and_grasses_occs, thin_ptw2_coords, a=backg_five_degree, 
                                   args=c('responsecurves=TRUE',
                                          'writebackgroundpredictions=TRUE'))

response(mx_climate_and_grasses_full)
plot(mx_climate_and_grasses_full)
mx_climate_and_grasses_full@results
mx_climate_and_grasses_full@lambdas

px_climate_and_grasses <- predict(climate_and_grasses_occs, mx_climate_and_grasses_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_and_grasses, main= 'Maxent, raw values')
writeRaster(px_climate_and_grasses, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_five_degree)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 


####################################################################################################
################################### MaxEnt for Climate & LULC ########################################
####################################################################################################

# k-fold
mx_climate_and_LULC_model <- maxent(climate_and_LULC, thin_ptw2_coords, a=backg_five_degree, factors = "band1",
                          args=c('responsecurves=TRUE', 
                                 'replicatetype=crossvalidate', 'replicates=5',
                                 'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_and_LULC_model@results

# all occurrences
mx_climate_and_LULC_full <- maxent(climate_and_LULC, thin_ptw2_coords, a=backg_five_degree, factors = "band1",
                                   args=c('responsecurves=TRUE',
                                          'writebackgroundpredictions=TRUE'))


response(mx_climate_and_LULC_full)
plot(mx_exomx_climate_and_LULC_fulltic_model_all_occs)
mx_climate_and_LULC_full@results
mx_climate_and_LULC_full@lambdas

px_exotic_model <- predict(predictors_and_exotic_hosts, mx_exotic_model_all_occs, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_exotic_model, main= 'Maxent, raw values')
writeRaster(px_exotic_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_five_degree)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 


####################################################################################################
########################### MaxEnt for Climate & Hosts & Grasses ###################################
####################################################################################################

# k-fold
mx_climate_hosts_grasses <- maxent(climate_and_hosts_and_grasses_occs, thin_ptw2_coords, a=backg_five_degree, 
                          args=c('responsecurves=TRUE', 
                                 'replicatetype=crossvalidate', 'replicates=5',
                                 'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_hosts_grasses@results

# all occurrences
mx_climate_and_hosts_and_grasses_full <- maxent(climate_and_hosts_and_grasses_occs, thin_ptw2_coords, a=backg_five_degree, 
                                   args=c('responsecurves=TRUE',
                                          'writebackgroundpredictions=TRUE'))


response(mx_climate_and_hosts_and_grasses_full)
plot(mx_climate_and_hosts_and_grasses_full)
mx_climate_and_hosts_and_grasses_full@results
mx_climate_and_hosts_and_grasses_full@lambdas

px_climate_and_hosts_and_grasses_model <- predict(climate_and_hosts_and_grasses, mx_exotic_model_all_occs, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_exotic_model, main= 'Maxent, raw values')
writeRaster(px_exotic_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_five_degree)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 

####################################################################################################
############################ MaxEnt for Climate & Hosts & LULC #####################################
####################################################################################################

# k-fold
names(climate_and_hosts_and_LULC)
mx_climate_hosts_LULC <- maxent(climate_and_hosts_and_LULC, thin_ptw2_coords, a=backg_five_degree, factors = "band1",
                                   args=c('responsecurves=TRUE', 
                                          'replicatetype=crossvalidate', 'replicates=5',
                                          'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_hosts_LULC@results

# all occurrences
mx_climate_and_hosts_and_LULC_full <- maxent(climate_and_hosts_and_LULC, thin_ptw2_coords, a=backg_five_degree, factors = "band1",
                                                args=c('responsecurves=TRUE',
                                                       'writebackgroundpredictions=TRUE'))


response(mx_climate_and_hosts_and_LULC_full)
plot(mx_climate_and_hosts_and_LULC_full)
mx_climate_and_hosts_and_LULC_full@results
mx_climate_and_hosts_and_grasses_full@lambdas

px_climate_and_hosts_and_grasses_model <- predict(climate_and_hosts_and_grasses, mx_exotic_model_all_occs, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_exotic_model, main= 'Maxent, raw values')
writeRaster(px_exotic_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_five_degree)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 


####################################################################################################
####################################### MaxEnt for Hosts ##########################################
####################################################################################################

# k-fold
mx_hosts_model <- maxent(hosts, thin_ptw2_coords, a=backg_five_degree, 
                          args=c('responsecurves=TRUE', 
                                 'replicatetype=crossvalidate', 'replicates=5',
                                 'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_hosts_model@results

# all occurrences
mx_hosts_full_occs <- maxent(hosts, thin_ptw2_coords, a=backg_five_degree, 
                                   args=c('responsecurves=TRUE',
                                          'writebackgroundpredictions=TRUE'))


response(mx_hosts_full_occs)
plot(mx_hosts_full_occs)
mx_hosts_full_occs@results
mx_hosts_full_occs@lambdas

px_exotic_model <- predict(hosts, mx_hosts_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_exotic_model, main= 'Maxent, raw values')
writeRaster(px_exotic_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw2_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw2_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_five_degree)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

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

df_no_host_pres <- df_no_host_all_occs %>% mutate(pres_no_host = ifelse(Suitability >= 0.2417432, 1, 0))
df_no_host_pres <- df_no_host_pres[,c(1,2,4)] #get only binary output

df_exotic_host_pres <- df_exotic_host_all_occs %>% mutate(pres_exotic_host = ifelse(Suitability >= 0.2021041, 1, 0))
head(df_exotic_host_pres)
df_exotic_host_pres <- df_exotic_host_pres[,c(1,2,4)] #get only binary output

df_all_presence <- left_join(df_no_host_pres, df_exotic_host_pres, by = c("lon", "lat"))
head(df_all_presence)
df_all_presence$total <- rowSums(df_all_presence[, c(3, 4)])

# Converting Heatmap to geotiff
head(df_all_presence)
df_all_presences_combined<-df_all_presence[,c(1,2,5)]
head(df_all_presences_combined)
coordinates(df_all_presences_combined) <- ~ lon + lat
gridded(df_all_presences_combined) <- TRUE
raster_of_heatmap <- raster(df_all_presences_combined)
writeRaster(raster_of_heatmap, filename="all_presence_combined_whydah.tif", format="GTiff", overwrite=TRUE)

# Binary map for No Hosts
coordinates(df_no_host_pres) <- ~ lon + lat
gridded(df_no_host_pres) <- TRUE
raster_no_host <- raster(df_no_host_pres)
plot(raster_no_host)
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
north_america_extent <-c(-162,-55,11.5,55)
setwd('/Users/rpecchia/Desktop/Whydah Project/whydah/Data')

antilles <- readOGR(dsn = ".", layer = "north_america_antilles_shapefile")
plot(antilles)
usa <- readOGR(dsn = ".", layer = "north_america_shapefile")
plot(usa)

CNTY_ID <- "30"
length(CNTY_ID)
row.names(as(usa, "data.frame"))
row.names(as(antilles, "data.frame"))
usa2 <- spChFIDs(usa, as.character(30))

usa_and_antilles <- spRbind(usa2,antilles)

# No Host Raster of NA
no_host_north_america_binary <- crop(raster_no_host, north_america_extent)
plot(no_host_north_america_binary)
no_host_usa_antilles_binary <- mask(no_host_north_america_binary, usa_and_antilles)
plot(no_host_usa_antilles_binary)
total_cells_no_host <- sum(na.omit(no_host_usa_antilles_binary@data@values))
total_cells_no_host * 10

# Exotic Host Raster of NA
exotic_host_north_america_binary <- crop(raster_exotic_host, north_america_extent)
plot(exotic_host_north_america_binary)
exotic_host_usa_antilles_binary <- mask(exotic_host_north_america_binary, usa_and_antilles)
plot(exotic_host_usa_antilles_binary)
total_cells_exotic_host <- sum(na.omit(exotic_host_usa_antilles_binary@data@values))
total_cells_exotic_host * 10

####################################################################################################
#################################### Calculating Schoner's D #######################################
####################################################################################################

naive_asc <- writeRaster(no_host_usa_antilles_binary, filename = 'naive_model.asc', format = "ascii", overwrite = TRUE)
exotic_asc <- writeRaster(exotic_host_usa_antilles_binary, filename = "exotics_model.asc", format = "ascii", overwrite = TRUE)

naive_asc <- read.asc("naive_model.asc")
exotic_asc <- read.asc("exotics_model.asc")

naive_grid <- sp.from.asc(naive_asc)
exotic_grid <- sp.from.asc(exotic_asc)

no <- niche.overlap(list(naive = naive_grid, exotic.host = exotic_grid))
no #upper triangle is Schoner's, Lower is Hellinger's




