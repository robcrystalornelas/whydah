##SDM for PTW#
#install.packages(c("spThin","ENMeval","dismo","rJava","jsonlite","fields","maptools","devtools","scales","dplyr"))
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
library(dplyr)
library(tmap)
library(scales)

#Full occurrence dataset####
#ptw<-gbif('Vidua', 'macroura', geo=T, removeZeros = T)
#save(ptw, file="ptw.rdata")
load("ptw.rdata")
head(ptw)
ptw<-ptw[,c('lon','lat','country','species')]
ptw<-subset(ptw, !is.na(lat) & !is.na(lon))
ptw<-subset(ptw, lat%%1>0 & lon%% 1>0) #Why do we use this?
head(ptw)
ptw.unique<- distinct(select(ptw,lon,lat,country,species)) #remove duplicates
dim(ptw.unique)
head(ptw.unique)

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

#OCW Points####
setwd("~/Desktop/Whydah Project/whydah/Data") #set back to data directory
plot(thin_ptw2_coords)

#OCW Points####
#ocw<-gbif('Estrilda', 'melpoda', geo=T, removeZeros = T)
#save(ocw, file="ocw.rdata")
load("ocw.rdata")
head(ocw)

ocw<-ocw[,c('lon','lat','country','species')]
ocw<-subset(ocw, !is.na(lat) & !is.na(lon))
ocw<-subset(ocw, lat%%1>0 & lon%% 1>0) #Why do we use this?
head(ocw)
ocw.unique<- distinct(select(ocw,lon,lat,country,species)) #remove duplicates
dim(ocw.unique)
head(ocw.unique)
#Clean Up the Data####
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

cw<-cw[,c('lon','lat','country','species')]
cw<-subset(cw, !is.na(lat) & !is.na(lon))
cw<-subset(cw, lat%%1>0 & lon%% 1>0) #Why do we use this?
head(cw)
cw.unique<- distinct(select(cw,lon,lat,country,species)) #remove duplicates
dim(cw.unique)
head(cw.unique)
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

# get the file names...these should be our bioclim
files <- list.files(path="~/Desktop/Whydah Project/whydah/Data/wc5", pattern="bil", full.names=TRUE)
files
mask <- raster(files[1]) #just sampling from 1 of the bioclim variables (since they are all from whole world)
# select 500 random points
# set seed to assure that the examples will always have the same random sample.
set.seed(1963)

bg <- randomPoints(mask, 500) #background points from all over world
#And inspect the results by plotting
plot(!is.na(mask), legend=FALSE)
points(bg, cex=0.5)

# constraining background sampling using extent() ####
#Definitely want to work on this!!
# the area of sampling using a spatial extent ... could be useful
#e <- extent(-80, -53, -39, -22)
#bg2 <- randomPoints(mask, 50, ext=e)
#plot(!is.na(mask), legend=FALSE)
#plot(e, add=TRUE, col= 'red')
#points(bg2, cex=0.5)

#Prepare Host/Climate Rasters####

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

species <- "Common Waxbil"
thin_cw2<-thin_cw2[,1:2] #prepare only lat/lon data for pres/absence
# read in a raster of the world
setwd("~/Desktop/Whydah Project/whydah/Data/wc5")
myRaster <- raster( "bio1.bil") #resolution of this file is failt low .08333x.08333, or 10km grid cells

# create presence absence raster for Waxbills using pre-made function
pa_raster_cw <- presence.absence.raster(mask.raster=myRaster, species.data=thin_cw2, raster.label=species)
pa_raster_cw
plot(pa.raster, main="Common Waxbill Presence/Absence Raster File")

#Now, onto climate data
files #here are all climate files
predictors<-stack(files, pa_raster_cw) #make a rasterstack of climate data & waxbill presence/absence
names(predictors)
plot(predictors)

# extract climate data ####
#We need to extract climate (predictor values) at our point locations
#But we can skip this if we use any SDM methods in dismo
ptw.unique.for.presvals<-ptw.unique[,1:2]
presvals <- extract(predictors, ptw.unique.for.presvals)
# setting random seed to always create the same random set of points for this example
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
absvals #matrxi of 500 points in each bioclim layers
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals))) #this is vector of known presences and pseudo-absences
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
str(sdmdata)
#customize next line for a categorical variable in rasterstack
sdmdata[, biome ] = as.factor(sdmdata[, biome ])
#need next line if using categorical variable
head(sdmdata)

#Practice calculating AUC from random data####
#create 2 fake variables w/ really different distributions
#we can see that presence values are generally higher than absences
p <- rnorm(50, mean=0.7, sd=0.3)
a <- rnorm(50, mean=0.4, sd=0.4)
par(mfrow=c(1, 2))
plot(sort(p), col= 'red' , pch=21)
points(sort(a), col= 'blue' , pch=24)
legend(1, 0.95 * max(a,p), c('presence' ,  'absence'), pch=c(21,24), col=c('red','blue'))
comb = c(p,a)
group = c(rep('presence' , length(p)), rep('absence' , length(a)))
boxplot(comb~group, col=c('blue' ,  'red' ))
#Then calculate correlation coefficient and AUC
group = c(rep(1, length(p)), rep(0, length(a)))
cor.test(comb, group)$estimate #correlation coefficient
mv <- wilcox.test(p,a)
auc <- as.numeric(mv$statistic) / (length(p) * length(a))
auc
#But all this can be done much quicker and easier w/ the evaluate function
e <- evaluate(p=p, a=a)
class(e)
par(mfrow=c(1, 2))
density(e)
boxplot(e, col=c('blue' ,  'red'))

#Divide occurrences into  training and testing ####
samp <- sample(nrow(sdmdata), round(0.75 * nrow(sdmdata)))
traindata <- sdmdata[samp,]
traindata <- traindata[traindata[,1] == 1, 2:9]
testdata <- sdmdata[-samp,]
bc <- bioclim(traindata)
e <- evaluate(testdata[testdata==1,], testdata[testdata==0,], bc) 
e
plot(e, 'ROC')
#However, when using real data, use k-fold paritioning, instead of a single
#random sample like above use kfold() in 'dismo'
#do this this, create presence and background datasets
pres <- sdmdata[sdmdata[,1] == 1, 2:9]
back <- sdmdata[sdmdata[,1] == 0, 2:9]
#we only use background for model testing, so doesn't need to be partitioned
#we WILL partition our data into 5 groups
k <- 5
group <- kfold(pres, k)
group[1:10]
unique(group)
#now we're ready to both fit and test our model 5 times
#during each run, model is fit using 4 of 5 groups, and tested with 1
#store the results from all 5 runs in object e
e <- list()
for (i in 1:k) {
  train <- pres[group != i,];
  test <- pres[group == i,];
  bc <- bioclim(train);
  e[[i]] <- evaluate(p=test, a=back, bc)
}
#Now we can extract several pieces of info
auc <- sapply(e, function(x){slot(x,'auc')} ) #AUC for all 5 runs of model
auc
mean(auc)
sapply( e, function(x){ x@t[which.max(x@TPR + x@TNR)] } ) #maximum sum of sensitivity & specificity
#Removing spatial soring bias (Hijmans 2012)####
nr <- nrow(ptw.unique)
s <- sample(nr, 0.25 * nr)
pres_train <- ptw.unique[-s, 1:2]
pres_test <- ptw.unique[s, 1:2]
nrow(pres_train)
nrow(pres_test)
nr <- nrow(backgr)
s <- sample(nr, 0.25 * nr)
back_train <- backgr[-s, ]
back_test <- backgr[s, ]
sb <- ssb(pres_test, back_test, pres_train)
sb[,1] / sb[,2] #if these is NO SSB, p should = 1...our data indicates strong SSB

#How to remove spatial sorting bias#### 
i <- pwdSample(pres_test[,1:2], back_test, pres_train[,1:2], n=1, tr=0.1)
pres_test_pwd <- pres_test[!is.na(i[,1]), 1:2]
back_test_pwd <- back_test[na.omit(as.vector(i)), 1:2]
sb2 <- ssb(pres_test_pwd, back_test_pwd, pres_train)
sb2[1]/ sb2[2] #much better!! Very little SSB
#How has the AUC changed?
bc <- bioclim(predictors, pres_train)
evaluate(bc, p=pres_test, a=back_test, x=predictors) #with SSB
evaluate(bc, p=pres_test_pwd, a=back_test_pwd, x=predictors) #SSB Removed

#chater 8 - algorithm selection####
class(predictors) #we have our rasterstack of bioclim variables
head(ptw.unique) #had to re-create this up top
ptw.occ.only<-ptw.unique[,-3]
ptw.occ.only<-ptw.occ.only[,-3]
head(ptw.occ.only) #also need data.frame of just occ points
presvals <- extract(predictors, ptw.occ.only)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))

#make training and testing data again
group <- kfold(ptw.occ.only, 5)
pres_train <- ptw.occ.only[group != 1, 1:2]
pres_test <- ptw.occ.only[group == 1, 1:2]
ext = extent(-90, -32, -33, 23) #to speed up how quickly everything processes, so limit our extent

backg <- randomPoints(predictors, n=1000, ext=ext, extf = 1.25) #pull background points from specified extent
colnames(backg) = c('lon' ,  'lat' )
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

r = raster(predictors, 1) #we use the first layer in the raster as a "mask" to ensure random points occur only in this area 
plot(!is.na(r), col=c( 'white' ,  'light grey' ), legend=FALSE)
plot(ext, add=TRUE, col= 'red' , lwd=2)
points(backg_train, pch= '-' , cex=0.5, col= 'yellow' ) #background train
points(backg_test, pch= '-' ,  cex=0.5, col= 'black' ) #background test
points(pres_train, pch=  '+' , col= 'green' ) #shows our train presences
points(pres_test, pch= '+' , col= 'blue' ) #shows our test presences

#examples of linear regression SDMs####
train <- rbind(pres_train, backg_train)
pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
envtrain <- extract(predictors, train)
envtrain <- data.frame( cbind(pa=pb_train, envtrain) )
head(envtrain) #extract enviro data for our training presnece points
testpres <- data.frame( extract(predictors, pres_test) )#enviro for test presence
testbackg <- data.frame( extract(predictors, backg_test) ) #enviro for test background
pa<-pb_train


#SDM using MaxEnt (Hijmans/Elith) WORKS! #####
xm <- maxent(predictors, pres_train)
plot(xm) #variable contribution plot
response(xm) #response curves
e <- evaluate(pres_test[,1:2], backg_test, xm, predictors) #evalute test points, pseudo-absences, the model and predictors
e #shows number of presences/absences/AUC and cor
px <- predict(predictors, xm, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
tr <- threshold(e,  'spec_sens' )
plot(px > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(pres_train, pch= '+' )
plot(e, 'ROC')

#Elith Tutorial Over, Now cobbled together resources####
data(wrld_simpl)
plot(wrld_simpl)
#from Max Ent Class
#wrld_simpl@data[,c(5,9)][order(wrld_simpl@data[,9]),] #country list
#centralAmerica<-wrld_simpl[wrld_simpl$SUBREGION == 13,] #central america
#southAmerica<-wrld_simpl[wrld_simpl$SUBREGION == 5,] #south america
#csAmerica<-spRbind(southAmerica,centralAmerica) #combining central and south in one map
#plot(csAmerica)
usa.island<-wrld_simpl[wrld_simpl$SUBREGION == 29,]
usa.north.america<-wrld_simpl[wrld_simpl$SUBREGION == 21,]
north.america<-csAmerica<-spRbind(usa.island,usa.north.america)
plot(north.america)

#PTW Points on World Map####
plot(wrld_simpl, col="light yellow", axes=TRUE)
points(ptw.unique, col="red",pch=16, cex=.2) #graph of whydah distribution
click() #tells us coordinates on map
#prepare Enviro Variables
envs <- stack(getData('worldclim',var='bio',res=5), pa_raster_cw)
#envs<-mask(envs,north.america) #mask makes all enviro cells with no data NA
?mask
plot(envs[[1]])
#envs<-crop(envs,north.america)

#Prepare Folds####
folds<-kfold(thin_ptw2_coords, k=4)
train<-thin_ptw2_coords[folds>1,]
test<-thin_ptw2_coords[folds==1,]
train<-train[,1:2]
test<-test[,1:2]
head(train)
trainmatrix<-as.matrix(train)


#MaxEnt - Works now!####
mxdir<-("~/Desktop/Whydah Project/whydah/Data")
outdir<-("~/Desktop/Whydah Project/whydah/Data")

occs.path<- file.path(outdir,'ptw.csv')
write.csv(thin_ptw2_coords,occs.path)
#ptw_none_less_than_zero<-ptw_thin2_coords[ptw_thin2_coords$lon <0,] #why would we use this?
occs.sp<-SpatialPointsDataFrame(thin_ptw2_coords[,1:2],data.frame(ptw.unique[,3])) #do we really need this?
occs<-thin_ptw2_coords[,1:2]
head(occs)
dim(occs) #this is our thinned occurrence data
extr <- extract(envs[[1]],occs)
dim(train) #make sure our training set is the thinned set
mx <- maxent(envs,train,a=backg,args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
#additional possible arguments for maxent:
#a = is an argument providing background points, but only works if training data isn't a vector
#factors = are any variables categorical?
#removeDuplicates = if true, then presence points within same raster cell are removed
mx <- maxent(envs,train) #works!
response(mx) #these are response curves
plot(mx) #this shows importance of each variable in building model

#Model Evaluation
e <- evaluate(test, backg_test, mx, envs) #evalute test points, pseudo-absences (random background points), the model and predictors
e #shows number of presences/absences/AUC and cor
px <- predict(envs, xm, progress= '' ) #make predictions of habitat suitability can include argument ext=ext
par(mfrow=c(1,2))
plot(px, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
tr <- threshold(e, 'spec_sens' )
plot(px > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e, 'ROC')

#Make predictions with model output
r1<-predict(mx, envs)
#add some parameters to predictions
r2 <- predict(mx, envs, args=c("outputformat=raw"), progress='text', 
              filename='maxent_prediction.grd', overwrite=TRUE)
plot(r1, main="MaxEnt Predictions for PTW")
points(train, pch=16, cex=.15, col="cadetblue3") #this is training point
points(test, pch=16, cex=.15, col="purple") 
#these are testing points
#alpha command gives purple points some transparency

#Plotting Maxent output
map.p <- rasterToPoints(r1)
df <- data.frame(map.p)
head(df)
#Make appropriate column headings
colnames(df) <- c('lon', 'lat', 'Suitability')
head(thin_ptw2_coords)
#Now make the map
p<-ggplot(data=df, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  #geom_point(data=thin_ptw2_coords, aes(x=lon, y=lat), color='thistle3', size=1, shape=4) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Model for Whydahs with Common Waxbills") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank()
  )
p + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                         na.value = "black")

#values=c(0,0.1,seq(0.100,1,length.out=7)) #I think above map is good!, can inset this if we want to change spacing

?scale_fill_gradientn
#scale_fill_gradient(low="wheat1", high="red1", limits=c(0,.9)) #this one works!
scale_fill_brewer(palette = "PRGn") #didn't work because data sent over must be discrete


#test the maxent model
#accumulate background points
bg<-randomPoints(envs,1000)
#simplest way to eval
e1<-evaluate(mx, p=test, a=bg, x=envs)
?maxent

#Try evaluation method to get ROC
pvtest <- data.frame(extract(envs, test))
avtest <- data.frame(extract(envs, bg))
testp<-predict(mx,pvtest)
head(testp)
testa<-predict(mx,avtest)

e3<-evaluate(p=testp, a=testa)
e3
threshold(e3)

plot(e3, 'ROC')

#from Hijmans & Elith 2015#
e<-evaluate(p=testp, a=testa)
px <- predict(mx, envs, args=c("outputformat=raw"), progress='text', 
              filename='maxent_prediction.grd', overwrite=TRUE)
par(mfrow=c(1,2))
plot(px, main='maxent, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr<-threshold(e, 'spec_sens')
plot(px>tr, main = 'presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(train, pch="+", cex=.2)

###ENMeval###
#enmeval_results <- ENMevaluate(thin_ptw2_coords, predictors, method="block", n.bg=500, overlap=TRUE,
 #                              bin.output=TRUE, clamp=TRUE, parallel = TRUE)

#save(enmeval_results, file="enmeval_results.rdata")
load("enmeval_results.rdata")
enmeval_results
plot(enmeval_results@predictions[[which (enmeval_results@results$delta.AICc == 0) ]])
points(enmeval_results@occ.pts, pch=21, bg=enmeval_results@occ.grp)
head(enmeval_results@results)
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

#Testing Methods for PCA####
??vif()

#SAVE WORKSPACE!####
save.image("~/Desktop/Whydah Project/whydah/R/whydah_workspace.RData")