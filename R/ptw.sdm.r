##SDM for PTW#
#install.packages(c("spThin","ENMeval","dismo","rJava","jsonlite","fields","maptools","devtools","scales","dplyr","ecospat"))
#install.packages('/Library/gurobi650/mac64/R/gurobi_6.5-0.tgz', repos=NULL)
setwd("~/Desktop/Whydah Project/whydah/Data")
load("~/Desktop/Whydah Project/whydah/whydah_workspace.RData")
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

#Removing outliers
unique(ptw.unique$country) #Remove Taiwan Whydahs
ptw.unique<-filter(ptw.unique, country !=c("Taiwan")) #get rid of Taiwan sightings.
#From many years ago, and not over a consistent amount of years
click()
filter(ptw.unique, lon<(-80) & lat>30 & country=="United States") #find Chicago point
which(ptw.unique$lon == -82.9989, ptw.unique$lat== 39.96110) #find it in the data.frame
ptw.unique<- ptw.unique[-4493,] #remove that row!

filter(ptw.unique, lon<(-122) & lat>35 & country=="United States") #find San Fran point
which(ptw.unique$lon == -122.511, ptw.unique$lat== 37.7777)
ptw.unique<-ptw.unique[-1312,] #remove san fran point

#only complete cases
ptw.unique<-ptw.unique[complete.cases(ptw.unique),]
dim(ptw.unique)
is.na(ptw.unique) #no NAs in df

#check map
plot(wrld_simpl)
points(ptw.unique, col="red")
dim(ptw.unique)

#Clean/Organize Data####
lonzero = subset(ptw.unique, lon==0) #any points have longitude that was auto-set to 0
lonzero #All OK
duplicated(ptw.unique) #any duplicates?

#cross checking our occurence points by means of a spatial query####
#countries<-getData("countries") #maps we used in this exercise were a bit crude, so can use
#this function to get highly detailed maps
#these lines of code will create object "over" that shows all countries where our
#points fall
coordinates(ptw.unique) <- ~lon+lat
crs(ptw.unique) <- crs(countries)
class(ptw.unique)
ovr <- over(ptw.unique, countries)
cntr <- ovr$NAME #if we see any NAs, that means they're mapped to the ocean
#AND which countires are recorded here, that are different from their record included in GBIF
i <- which(is.na(cntr))
i #When I switched to "countries" dataset, didn't lose any points
j <- which(cntr != ptw.unique$country) #this asks which of our counties, doesn't align w/ GBIFs countires
j
# for the mismatches, bind the country names of the polygons and points
cbind(cntr, ptw.unique$country)[j,] #here's all the spots we have mismatches
plot(ptw.unique)

data("wrld_simpl")
plot(wrld_simpl, add=T, border= "blue" , lwd=1)
points(ptw.unique[j, ], col="red" , pch=20, cex=.75)

#spThin####
setwd("~/Desktop/Whydah Project/whydah/Output") #running from mac
# set coordinate system
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
#then spThin!
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
#hexagons show the distribution of all the occurrence records and the 
#red points show the location of the retained records.
thin1@call

#Saving the thinned file####
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
plot(thin_ptw2_coords)

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

coordinates(ocw.unique) <- ~lon+lat
crs(ocw.unique) <- crs(countries)
class(ocw.unique)
ovr <- over(ocw.unique, countries)
cntr <- ovr$NAME
i <- which(is.na(cntr))
i
j <- which(cntr != ocw.unique$country)
j
cbind(cntr, ocw.unique$country)[j,]
plot(ocw.unique)
plot(wrld_simpl, add=T, border= "blue" , lwd=1)
points(ocw.unique[j, ], col="red" , pch=20, cex=.75)

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

coordinates(cw.unique) <- ~lon+lat
crs(cw.unique) <- crs(countries)
class(cw.unique)
ovr <- over(cw.unique, countries)
cntr <- ovr$NAME
i <- which(is.na(cntr))
i
j <- which(cntr != cw.unique$country)
j
cbind(cntr, cw.unique$country)[j,] 
plot(cw.unique)
plot(wrld_simpl, add=T, border= "blue" , lwd=1)
points(cw.unique[j, ], col="red" , pch=20, cex=.75)

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

#Back to data directory one more time
setwd("~/Desktop/Whydah Project/whydah/Data")

#setting up background points according to Hijmans and Elith####
#Also, we have our thinned occurrence sets
thin_ptw2
thin_cw2
thin_ocw2

#Environmental Variables####

# get the file names...these should be all of our our bioclim
files <- list.files(path="~/Desktop/Whydah Project/whydah/Data/wc5", pattern="bil", full.names=TRUE)
predictors<-stack(files)
predictors #WC5 data, without waxbill
mask <- raster(files[1]) #just sampling from 1 of the bioclim variables (since they are all from whole world)
set.seed(1963) #makes sure we're generating random numbers

# Might want to constrain background sampling using extent() ####
ext <- extent(-119, 55.4539,-33,23) # the area of sampling using a spatial extent ... could be useful
bg2 <- randomPoints(mask, 50, ext=e)
plot(!is.na(mask), legend=FALSE)
plot(e, add=TRUE, col= 'red')
points(bg2, cex=0.5)

#Different Methods for PCA####

# Select07 From Dormann et al. 2012
select07 <- function(X, y, family="binomial", univar="gam", threshold=0.7,
                     method="pearson", sequence=NULL, ...){
  
  #.7 is recommended for threshold by Fielding and Haworth 1995
  #if data is bi-variate normal stick with pearson for method, otherwise go "spearman"
  #sequence can be used to specify the order of predictor variable importance
  
  #From Dormann:
  # selects variables based on removing correlations > 0.7, retaining those
  # variables more important with respect to y
  # when a sequence is given, this will be used instead (Damaris)
  # 1. step: cor-matrix
  # 2. step: importance vector
  # 3. step: identify correlated pairs
  # 4. step: remove less important from pairs
  #get rid of it II: seqreg, select07,maxspan
  # written by Carsten F. Dormann;
  # last changed by Tamara Münkemüller and Damaris Zurell, 12.12.2007
  # last changed by Damaris Zurell, 19.12.2008
  var.imp <- function (variable, response, univar=univar, family="gaussian"){
    # calculates the univariate (=marginal) importance of a variable for a response
    if (!univar %in% c("glm", "gam")) stop("Invalid univariate screening method:
                                           choose 'glm' or 'gam' (default).")
    if (univar=="glm"){
      fm.glm <- glm(response ~ variable, family=family)
      summary(fm.glm)$aic
    } else {
      fm.gam <- gam(response ~ s(variable, ...), family=family)
      AIC(fm.gam)
    }
  }
  cm <- cor(X, method=method)
  pairs <- which(abs(cm)>= threshold, arr.ind=T) # identifies correlated variable pairs
  index <- which(pairs[,1]==pairs[,2]) # removes entry on diagonal
  pairs <- pairs[-index,] # -"-
  exclude <- NULL
  if (NROW(pairs)!=0)
  {
    if (is.null(sequence)) {
      #importance as AIC: the lower the better!
      imp <- apply(X, 2, var.imp, response=y, family=family, univar=univar)
      for (i in 1:NROW(pairs))
      {
        a <- imp[rownames(cm)[pairs[i,1]]]
        b <- imp[rownames(cm)[pairs[i,2]]]
        exclude <- c(exclude, ifelse(a>b, names(a), names(b)))
      }
    } else {
      for (i in 1:NROW(pairs))
      {
        a <- which(pairs[i,1]==sequence)
        b <- which(pairs[i,2]==sequence)
        exclude <- c(exclude, ifelse(a>b, rownames(cm)[pairs[i,1]],
                                     rownames(cm)[pairs[i,2]]))
      }
    }
  }
  X <- X[,!(colnames(X) %in% unique(exclude)),drop=F]
  return(X)
  }
#select07(X=LE, y=LanExc12[,"c"], family="binomial", threshold=0.7,method="spearman")[1:10,]
formula.maker <- function(dataframe, y.col=1, quadratic=TRUE, interactions=TRUE) {
  # makes a formula for GLM from dataframe column names,
  # including quadratic effects and first-order interactions
  # by default, first column is taken to be the response (y); else, an integer giving the column with the response in "dataframe"
  # by Carsten F. Dormann
  if (quadratic && interactions) {
    f <- as.formula(paste(colnames(dataframe)[y.col], " ~ (",
                          paste(colnames(dataframe[,-y.col]), collapse=" + ", sep=""), ")^2 + ", paste("I(",
                                                                                                       colnames(dataframe[,-y.col]), "^2)", collapse="+", sep="")))
  }
  if (quadratic & !interactions){
    f <- as.formula(paste(colnames(dataframe)[y.col], " ~ (",
                          paste(colnames(dataframe[,-y.col]), collapse=" + ", sep=""), ") + ", paste("I(",
                                                                                                     colnames(dataframe[,-y.col]), "^2)", collapse="+", sep="")))
  }
  if (!quadratic & !interactions){
    f <- as.formula(paste(colnames(dataframe)[y.col], " ~ ", paste(colnames(dataframe[,
                                                                                      -y.col]), collapse=" + ", sep="") ))
  }
  if (!quadratic & interactions){
    f <- as.formula(paste(colnames(dataframe)[y.col], " ~ (",
                          paste(colnames(dataframe[,-y.col]), collapse=" + ", sep=""), ")^2"))
    # + ", paste("I(", colnames(dataframe[,-1]), "^2)", collapse="+", sep="")))
  }
  f
}


head(envtrain)
envtrain.just.bioclim<-envtrain[,3:21]
envtrain.all<-envtrain[,3:22]
N<-scale(envtrain.just.bioclim) #Maybe all climate variables need to be all on same scale?

g<-cor(N, use="complete")
g #this is the corrleation matrix
suit<-extract(px, envtrain[,1:2]) #if we also need the suitability scores for our response
suit2<-cbind(suit,N)
head(suit2)
suit3<-suit2[complete.cases(suit2),]
h<-select07(suit3[,2:20],suit3[,1], family="gaussian",univar="glm",threshold=.7, method="pearson")
#predictors can be continuous & Categorical, response must be any type of variable accepted by glm
#threshold...when higher, means less conservative. Returns data.frame where r < .7
head(h) #this suggests bio1, 16, 18, 19

#with categorical variable included
suit4<-cbind(suit,envtrain.all)
suit5<-suit4[complete.cases(suit4),]
dim(suit5)
h2<-select07(suit5[,2:21],suit5[,1], family="gaussian",univar="glm",threshold=.7, method="pearson") #sequence = c(20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2))
head(h2) #when waxbill is included, suggest bio1, 16, 18, 19 & Waxbill

# Traditional PCA####
#Example from Hijmans for calculating PCA of Rasterstack
#sr <- sampleRandom(envs.df, 1000) #could sample randomly of rasterbrick is too large

#PCA for Common Waxbill Predictors
pca_cw <- prcomp(na.omit(values(predictors_cw)), scale=T, center=T)
summary(pca_cw) #prop. variance is similar to eigen value
print(pca_cw) #this function prints loadings for each PCA
pca_cw$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_cw$rotation #for EIGEN VECTORS
plot(pca_cw, type="l") #screeplot1
screeplot(pca_cw, type="l")  #screeplot2
biplot(pca_cw, cex=0.7, choices=c(1:2)) #Making a biplot, can used choices=c(...) to specify which axes we're looking at
#z3<- predict(z2)
pca_predictions_cw <- na.omit(predict(predictors_cw, pca_cw, index=1:4)) #make further predictions w/ PCA results
#so this should be result in a (raster stack? df?) of 4 PCs...then put into maxEnt
plot(pca_predictions_cw[,1], pca_predictions_cw[,2]) #xlim=c(-12,7), ylim=c(-12,7)

#PCA for Orange Cheeked Waxbill Predictors
pca_ocw <- prcomp(na.omit(values(predictors_ocw)), scale=T, center=T)
summary(pca_cw) #prop. variance is similar to eigen value
print(pca_cw) #this function prints loadings for each PCA
pca_cw$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
pca_ocw$rotation #for EIGEN VECTORS
plot(pca_ocw, type="l") #screeplot1
screeplot(pca_ocw, type="l")  #screeplot2
biplot(pca_ocw, cex=0.7, choices=c(1:2)) #Making a biplot, can used choices=c(...) to specify which axes we're looking at
#z3<- predict(z2)
pca_predictions_ocw <- na.omit(predict(predictors_ocw, pca_ocw, index=1:4)) #make further predictions w/ PCA results
#so this should be result in a (raster stack? df?) of 4 PCs...then put into maxEnt
plot(pca_predictions_ocw[,1], pca_predictions_ocw[,2]) #xlim=c(-12,7), ylim=c(-12,7)

#PCA with just bioclim
#results are pretty much the same as with waxbills except axis 4!
pca_bioclim_only <- prcomp(na.omit(values(predictors)), scale=T, center=T)
summary(pca_bioclim_only) #prop. variance is similar to eigen value
print(pca_bioclim_only) #this function prints loadings for each PCA
pca_bioclim_only$sdev^2 #but this truly gives our eigen values for each PC.  Suggests PC 1-4
<<<<<<< HEAD
pca_bioclim_only$rotation #for EIGEN VECTORS
pca_predictions_bioclim_only <- na.omit(predict(predictors, pca_bioclim_only, index=1:4)) 
pca2_bioclim_only$rotation #for EIGEN VECTORS
=======
<<<<<<< HEAD
pca_bioclim_only$rotation #for EIGEN VECTORS
pca_predictions_bioclim_only <- na.omit(predict(predictors, pca_bioclim_only, index=1:4)) 
=======
pca2_bioclim_only$rotation #for EIGEN VECTORS
>>>>>>> 2185184c6ffb345fbe0cd096a41f248d57023090
>>>>>>> ff9014a3bb28f68eacbe5895331e0316af763d93

#Preparing Host/Climate Rasters####
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

#Now, onto bioclim data
files #here are all climate files
predictors_no_host<-stack(files)
predictors_cw<-stack(files, pa_raster_cw) #make a rasterstack of climate data & waxbill presence/absence
predictors_ocw<-stack(files, pa_raster_ocw)
plot(predictors_cw)
plot(predictors_ocw)

#background points
backg <- randomPoints(predictors_no_host, n=1000, ext = (extent(-119, 55.4539,-33,23)), extf=1.25) #pull background points from specified extent
#From occurrence records...bounding box is. y (lat) min = -34.8324, ymax=41.526  /  x (lon) min = -118.808, xmax = 55.4539
#ext = extent(-90, -32, -33, 23) #to speed up how quickly everything processes, so limit our extent
#Format for extent is (xmin,xmax,ymin,ymax)
colnames(backg) = c('lon' , 'lat')
group <- kfold(backg, 4)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

ext<-extent(-119, 55.4539,-33,23)
#get a rasterstack that is full of cropped rasters
backg_cropped<-crop(predictors_no_host,ext)


plot(wrld_simpl,main="Background Points for MaxEnt Model\nExtent Matches Whydah Distribution")
points(backg, cex=.3, col="purple")

#SDMs using MaxEnt#####
#envs<-mask(envs,north.america) #mask makes all enviro cells with no data NA
#envs<-crop(envs,north.america)
#Prepare Training and Testing dataset####
folds<-kfold(thin_ptw2_coords, k=4) #this is a 4 fold test
train<-thin_ptw2_coords[folds>1,] #training has 75% of points
test<-thin_ptw2_coords[folds==1,] #testing has 25% of points
train<-train[,1:2]
test<-test[,1:2]
head(train) #just has lon/lat

#MaxEnt  for Whydah with CW and PCA####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
write.csv(thin_ptw2_coords,occs.path) #write a CSV of our occurrence points
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_cw <- maxent(pca_predictions_cw,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
#additional possible arguments for maxent:
#a = is an argument providing background points, but only works if training data isn't a vector
#factors = are any variables categorical?
#removeDuplicates = if true, then presence points within same raster cell are removed
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

#Now make the map
p<-ggplot(data=df_cw, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Common Waxbills & PCA") +
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
p + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black")

#values=c(0,0.1,seq(0.100,1,length.out=7)) #I think above map is good!, can insert this if we want to change spacing
?scale_fill_gradientn
#scale_fill_gradient(low="wheat1", high="red1", limits=c(0,.9)) #this one works!
scale_fill_brewer(palette = "PRGn") #didn't work because data sent over must be discrete

#MaxEnt  for Whydah with OCW and PCA####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
write.csv(thin_ptw2_coords,occs.path) #write a CSV of our occurrence points
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_ocw_pca <- maxent(pca_predictions_ocw,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
#additional possible arguments for maxent:
#a = is an argument providing background points, but only works if training data isn't a vector
#factors = are any variables categorical?
#removeDuplicates = if true, then presence points within same raster cell are removed
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

#Now make the map
p<-ggplot(data=df_ocw_pca, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith Orange Cheeked Waxbills & PCA") +
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
p + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black")

<<<<<<< HEAD
#MaxEnt for Whydah PCA No Host####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
write.csv(thin_ptw2_coords,occs.path) #write a CSV of our occurrence points
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_bioclim_only <- maxent(pca_predictions_bioclim_only,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
#additional possible arguments for maxent:
#a = is an argument providing background points, but only works if training data isn't a vector
#factors = are any variables categorical?
#removeDuplicates = if true, then presence points within same raster cell are removed
response(mx_bioclim_only) #response curves
plot(mx_bioclim_only) #importance of each variable in building model

#Model Evaluation
e_pca_bio_only <- evaluate(test, backg_test, mx_bioclim_only, pca_predictions_bioclim_only) #evalute test points, pseudo-absences (random background points), the model and predictors
e_pca_bio_only #shows number of presences/absences/AUC and cor
px_pca_bio_only <- predict(pca_predictions_bioclim_only, mx_bioclim_only, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
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

#Now make the map
p<-ggplot(data=df_pca_bio_only, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith PCA and NO HOSTS") +
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
p + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black")

#MaxEnt  for Whydah - No Host species / all bioclim####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
write.csv(thin_ptw2_coords,occs.path) #write a CSV of our occurrence points
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_no_host <- maxent(predictors_no_host,train,a=backg,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE','linear=TRUE','quadratic=TRUE','product=FALSE','hinge=FALSE'))
#additional possible arguments for maxent:
#a = is an argument providing background points, but only works if training data isn't a vector
#factors = are any variables categorical?
#removeDuplicates = if true, then presence points within same raster cell are removed
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
?maxent

#Plotting Maxent output
map.no.host <- rasterToPoints(px_no_host) #make predictions raster a set of points for ggplot
df_no_host <- data.frame(map.no.host) #convert to data.frame
head(df_no_host)
colnames(df_no_host) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)

#Now make the map
p<-ggplot(data=df_no_host, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith No Host & All 19 Bioclim") +
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
p + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black")

#values=c(0,0.1,seq(0.100,1,length.out=7)) #I think above map is good!, can insert this if we want to change spacing
#scale_fill_gradient(low="wheat1", high="red1", limits=c(0,.9)) #this one works!

#MaxEnt for Whydah with OCW and ALL BIOCLIM####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_ocw_all_bioclim <- maxent(predictors_ocw,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
#additional possible arguments for maxent:
#a = is an argument providing background points, but only works if training data isn't a vector
#factors = are any variables categorical?
#removeDuplicates = if true, then presence points within same raster cell are removed
response(mx_ocw_all_bioclim) #response curves
plot(mx_ocw_all_bioclim) #importance of each variable in building model

#Model Evaluation 
e_ocw_all_bioclim <- evaluate(test, backg_test, mx_ocw_all_bioclim, predictors_ocw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_ocw_all_bioclim #shows number of presences/absences/AUC and cor
px_ocw_all_bioclim <- predict(predictors_ocw, mx_ocw_all_bioclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_ocw_all_bioclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_ocw_all_bioclim <- threshold(e_ocw_all_bioclim, 'spec_sens' )
plot(px_ocw_all_bioclim > tr_ocw_all_bioclim, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_ocw_all_bioclim, 'ROC')

#Plotting Maxent output
map.ocw.all.bioclim <- rasterToPoints(px_ocw_all_bioclim) #make predictions raster a set of points for ggplot
df_ocw_all_bioclim <- data.frame(map.ocw.all.bioclim) #convert to data.frame
head(df_ocw_all_bioclim)
colnames(df_ocw_all_bioclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)

p<-ggplot(data=df_ocw_all_bioclim, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith OCW & All 19 Bioclim") +
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
p + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black")

#MaxEnt for Whydah with Common Waxbill and ALL BIOCLIM####
outdir<-("~/Desktop/Whydah Project/whydah/Data")
occs.path<- file.path(outdir,'ptw.csv')
#extr <- extract(envs[[1]],occs) #vector of positions where we have occurrence points
dim(train) #make sure our training set is the thinned set
mx_cw_all_bioclim <- maxent(predictors_cw,train,a=backg_train,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
#additional possible arguments for maxent:
#a = is an argument providing background points, but only works if training data isn't a vector
#factors = are any variables categorical?
#removeDuplicates = if true, then presence points within same raster cell are removed
response(mx_cw_all_bioclim) #response curves
plot(mx_cw_all_bioclim) #importance of each variable in building model

#Model Evaluation 
e_cw_all_bioclim <- evaluate(test, backg_test, mx_cw_all_bioclim, predictors_cw) #evalute test points, pseudo-absences (random background points), the model and predictors
e_cw_all_bioclim #shows number of presences/absences/AUC and cor
px_cw_all_bioclim <- predict(predictors_cw, mx_cw_all_bioclim, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px_cw_all_bioclim, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_cw_all_bioclim <- threshold(e_cw_all_bioclim, 'spec_sens' )
plot(px_cw_all_bioclim > tr_cw_all_bioclim, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_cw_all_bioclim, 'ROC')

#Plotting Maxent output
map.cw.all.bioclim <- rasterToPoints(px_cw_all_bioclim) #make predictions raster a set of points for ggplot
df_cw_all_bioclim <- data.frame(map.cw.all.bioclim) #convert to data.frame
head(df_cw_all_bioclim)
colnames(df_cw_all_bioclim) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
head(thin_ptw2_coords)

p<-ggplot(data=df_cw_all_bioclim, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs\nwith CW & All 19 Bioclim") +
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
p + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black")

###ENMeval###
#enmeval_results <- ENMevaluate(thin_ptw2_coords, , method="block", n.bg=500, overlap=TRUE,
#                              bin.output=TRUE, clamp=TRUE, parallel = TRUE)
#save(enmeval_results, file="enmeval_results.rdata")
load("enmeval_results.rdata")
enmeval_results
plot(enmeval_results@predictions[[which (enmeval_results@results$delta.AICc == 0) ]])
points(enmeval_results@occ.pts, pch=21, bg=enmeval_results@occ.grp)
head(enmeval_results@results)
enmeval_results@results #all the results
Q<-enmeval_results@results#arrange by AICc value
QQ<-as.data.frame(Q)
head(QQ)
QQ<-QQ[,c(1,2,3,14)]
head(QQ)
arrange(QQ,AICc,settings,features,rm) #this will sort ENMeval results so that we can see exact settings for model with lowest AICc
#Shows that model with LQ ranging from .5-4.0 all had the lowest AICc
enmeval_results@overlap

par(mfrow=c(2,2))
eval.plot(enmeval_results@results, legend.position="topright")
eval.plot(enmeval_results@results, "Mean.AUC", )
eval.plot(enmeval_results@results, "Mean.AUC.DIFF", variance="Var.AUC.DIFF")
eval.plot(enmeval_results@results, "Mean.ORmin")
#These figures are key /\.  We should relect RM and Model Setting from key when deta.AUCc is below 2
enmeval_results@results
# specify how data should be partitioned w/ method="jackknife", "randomkfold", "user", "block", "checkerboard1", "checkerboard2".
# n.bg is The number of random background localities to draw from the study extent
#when overlap = TRUE, provides pairwise metric of niche overlap 
#bin.output appends evaluations metrics for each evaluation bin to results table

#ENMeval for cropped rasterstak
enmeval_results_cropped <- ENMevaluate(thin_ptw2_coords, backg_cropped, method="block", n.bg=500, overlap=TRUE,bin.output=TRUE, clamp=TRUE, parallel = TRUE)

<<<<<<< HEAD
#ENMeval for PCA results
enmeval_results_pca_bioclim_only <- ENMevaluate(thin_ptw2_coords, pca_predictions_bioclim_only, method="block", n.bg=500, overlap=TRUE,bin.output=TRUE, clamp=TRUE, parallel = TRUE)

=======
<<<<<<< HEAD
#ENMeval for PCA results
enmeval_results_pca_bioclim_only <- ENMevaluate(thin_ptw2_coords, pca_predictions_bioclim_only, method="block", n.bg=500, overlap=TRUE,bin.output=TRUE, clamp=TRUE, parallel = TRUE)

=======
>>>>>>> 2185184c6ffb345fbe0cd096a41f248d57023090
>>>>>>> ff9014a3bb28f68eacbe5895331e0316af763d93
#SAVE WORKSPACE!####
save.image("~/Desktop/Whydah Project/whydah/R/whydah_workspace.RData")
