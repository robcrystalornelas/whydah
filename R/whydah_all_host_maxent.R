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

#ptw<-gbif('Vidua', 'macroura', geo=T, removeZeros = T)
spot_backed_weaver <- gbif('Ploceus', 'cucullatus', geo = T, removeZeros = T)
africa_firefinch <- gbif('Lagonosticta', 'rubricata', geo = T, removeZeros = T)
black_bellied_firefinch <- gbif('Lagonosticta', 'rara', geo = T, removeZeros = T)
red_bellied_firefinch <- gbif('Lagonosticta', 'senegala', geo = T, removeZeros = T)
black_cheeked_waxbill <- gbif('Estrilda', 'erythronotos', geo = T, removeZeros = T)
scaly_weaver <- gbif('Sporopipes', 'squamifrons', geo = T, removeZeros = T)
grosbeak_weaver <- gbif('Amblyospiza', 'albifrons', geo = T, removeZeros = T)
long_tailed_widowbird <- gbif('Euplectes', "progne", geo = T, removeZeros = T)
african_quail_finch <- gbif('Ortygospiza', "atricollis", geo = T, removeZeros = T)
red_collared_widowbird <- gbif('Euplectes', 'ardens', geo = T, removeZeros = T)
swee_waxbill <- gbif('Coccopygia', 'melanotis', geo = T, removeZeros = T)
yellow_bellied_waxbill <- gbif('Coccopygia' ,'quartinia', geo = T, removeZeros = T)
fawn_breasted_waxbill<- gbif('Estrilda', 'paludicola', geo = T, removeZeros = T)
crimson_rumped_waxbill <- gbif('Estrilda', 'rhodopyga', geo = T, removeZeros = T)
black_rumped_waxbill <- gbif('Estrilda', 'troglodytes', geo = T, removeZeros = T)
black_crowned_waxbill <- gbif('Estrilda', 'nonnula', geo = T, removeZeros = T)
zebra_waxbill <- gbif('Amandava', 'subflava', geo = T, removeZeros = T)
african_silverbill <- gbif('Euodice', 'cantans', geo = T, removeZeros = T)
magpie_munia <- gbif('Lonchura', 'fringilloides', geo = T, removeZeros = T)
bronze_munia <- gbif('Spermestes', 'cucullatus', geo = T, removeZeros = T)
streaky_seedeater <- gbif('Serinus', 'striolatus', geo = T, removeZeros = T)
african_golden_breasted_bunting <- gbif('Emberiza', 'flaviventris', geo = T, removeZeros = T)

# script for subsetting and thinning
all_species_list <- list(spot_backed_weaver, africa_firefinch, black_bellied_firefinch, red_bellied_firefinch, black_cheeked_waxbill, scaly_weaver, grosbeak_weaver, long_tailed_widowbird, african_quail_finch, red_collared_widowbird, swee_waxbill, yellow_bellied_waxbill, fawn_breasted_waxbill, crimson_rumped_waxbill, black_rumped_waxbill, black_crowned_waxbill, zebra_waxbill, african_silverbill, magpie_munia, bronze_munia, streaky_seedeater, african_golden_breasted_bunting)
all_species_list_subset_columns <- lapply(all_species_list, function(i) {i[,c('lon','lat','country','species')]})
head(all_species_list_subset_columns[[2]])
all_species_no_duplicates <- lapply(all_species_list_subset_columns, function(i) {unique(i)})
head(all_species_no_duplicates[[1]])
all_species_no_nas<- lapply(all_species_no_duplicates, function(i) {i[!is.na(i),]})
all_species_complete_cases <- lapply(all_species_no_nas, function(i) {i[complete.cases(i),]})

#merging all data.frames in a list
merged.data.frame <- Reduce(function(...) merge(..., all=T), all_species_complete_cases)
head(merged.data.frame)

crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

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

#PA raster for "all others"
species <- "AllOtherHosts"
head(all_hosts_thinned)
setwd("~/Desktop/Whydah Project/whydah/Data/wc2")
myRaster <- raster( "bio1.bil") #resolution of 5 second is .08333x.08333, or 10km grid cells. resolution of 2 second is .04166 x .04166
# create presence absence raster for Common Waxbills using pre-made function
pa_raster_all_others <- presence.absence.raster(mask.raster=myRaster, species.data=all_hosts_thinned, raster.label=species)
pa_raster_all_others
plot(pa_raster_all_others)

predictors_all_hosts <- stack(files, pa_raster_cw,pa_raster_ocw, pa_raster_nutmeg, pa_raster_all_others)
predictors_all_hosts_no_nutmeg <- stack(files, pa_raster_cw,pa_raster_ocw, pa_raster_all_others)

####

# Maxent All hosts NO NUTMEG ####

####

mx_all_hosts_no_nutmeg <- maxent(predictors_all_hosts_no_nutmeg, train, a=backg_train, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_all_hosts_no_nutmeg@results
mx_all_hosts_no_nutmeg@lambdas
response(mx_all_hosts_no_nutmeg)
plot(mx_all_hosts_no_nutmeg)

#Model Evaluation 
e_all_hosts_no_nutmeg <- evaluate(test, backg_test, mx_all_hosts_no_nutmeg, predictors_all_hosts_no_nutmeg)
e_all_hosts_no_nutmeg
px_all_hosts_no_nutmeg <- predict(predictors_all_hosts_no_nutmeg, mx_all_hosts) #make predictions of habitat suitability can include argument ext=ext
plot(px_all_hosts_no_nutmeg, main= 'Maxent, raw values')
tr_all_host_no_nutmeg <- threshold(e_all_hosts_no_nutmeg, 'spec_sens' )
tr_all_host_no_nutmeg
plot(px_all_hosts_no_nutmeg > tr_all_host)
plot(e_all_hosts_no_nutmeg, 'ROC')

#Plotting Maxent output
map_all_hosts <- rasterToPoints(px_all_hosts) #make predictions raster a set of points for ggplot
df_all_hosts <- data.frame(map.native.all.worldclim2) #convert to data.frame
head(df_all_hosts)
colnames(df_all_hosts) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_all_hosts$Suitability)
plot(wrld_simpl)
points(filter(df_all_hosts, Suitability >= .7332606), col="red")

## All other hosts (with k-fold)
mx_all_hosts_no_nutmeg_k_fold <- maxent(predictors_all_hosts_no_nutmeg, thin_ptw2_coords, a = backg, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=4','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_all_hosts_no_nutmeg_k_fold@results

## All occurrences, no splitting
mx_all_host_no_nutmeg_all_occs <- maxent(predictors_all_hosts_no_nutmeg, thin_ptw2_coords, a=backg, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_all_host_no_nutmeg_all_occs@results
mx_all_host_no_nutmeg_all_occs@lambdas
response(mx_all_host_no_nutmeg_all_occs)
plot(mx_all_host_no_nutmeg_all_occs)

####

#Maxent Model Including all 26 hosts of the PTW and Novel Host ####

####

mx_all_hosts <- maxent(predictors_all_hosts, train, a=backg_train, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_all_hosts@results
mx_all_hosts@lambdas
response(mx_all_hosts)
plot(mx_all_hosts)

#Model Evaluation 
e_all_hosts <- evaluate(test, backg_test, mx_all_hosts, predictors_all_hosts)
e_all_hosts
px_all_hosts <- predict(predictors_all_hosts, mx_all_hosts) #make predictions of habitat suitability can include argument ext=ext
plot(px_all_hosts, main= 'Maxent, raw values')
tr_all_host <- threshold(e_all_hosts, 'spec_sens' )
tr_all_host
plot(px_all_hosts > tr_all_host)
plot(e_all_hosts, 'ROC')
writeRaster(px_all_hosts, filename="all25_hosts_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

#Plotting Maxent output
map_all_hosts <- rasterToPoints(px_all_hosts) #make predictions raster a set of points for ggplot
df_all_hosts <- data.frame(map.native.all.worldclim2) #convert to data.frame
head(df_all_hosts)
colnames(df_all_hosts) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_all_hosts$Suitability)
plot(wrld_simpl)
points(filter(df_all_hosts, Suitability >= .7332606), col="red")

## All other hosts (with k-fold)
mx_all_hosts_k_fold <- maxent(predictors_all_hosts, thin_ptw2_coords, a = backg, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=4','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_all_hosts_k_fold@results

## All occurrences, no splitting
mx_all_host_all_occs <- maxent(predictors_all_hosts, thin_ptw2_coords, a=backg, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_all_host_all_occs@results
mx_all_host_all_occs@lambdas
response(mx_all_host_all_occs)
plot(mx_all_host_all_occs)

