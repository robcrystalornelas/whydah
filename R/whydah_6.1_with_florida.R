####################################################################################################
############################ Whyda Models Using Florida Points #####################################
####################################################################################################
setwd("~/Desktop/Whydah Project/whydah/Data")

head(thin_ptw_with_florida_coords)

#now read in the file w/ buffers from QGIS
buffered_region_florida <- readGDAL("ptw_with_florida_buffers_2_cropped.tif")

#convert buffered region to raster
buffer_florida_raster <- raster(buffered_region_florida) #convert africa map to raster
backg_with_florida <- randomPoints(buffer_florida_raster, n=10000)

plot(wrld_simpl)
points(backg_with_florida, col = "purple", cex = 0.2)
points(backg_five_degree, col = "green", cex = 0.2)

####################################################################################################
######################################### MaxEnt for Climate #######################################
####################################################################################################

mx_climate_florida <- maxent(climate, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                    args=c('responsecurves=TRUE', 
                                           'replicatetype=crossvalidate', 'replicates=5',
                                           'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_florida@results

# Full occurrence set
mx_climate_full_florida <- maxent(climate, thin_ptw_with_florida_coords, a=backg_with_florida, 
                          args=c('responsecurves=TRUE',
                                 'writebackgroundpredictions=TRUE'))

# response(mx_no_host_all_occs)
plot(mx_climate_full_florida)
mx_climate_full_florida@results
mx_climate_full_florida@lambdas

px_climate_full_florida <- predict(climate, mx_climate_full_florida, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_full_florida, main= 'Maxent, raw values')

# Forming Confusion Matrix
training_suitability_naive <-extract(px_no_host_full, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_naive<-na.omit(training_suitability_naive)
ten_thresh_naive_model <- quantile(training_suitability_naive, 0.1, na.rm = TRUE)
ten_thresh_naive_model

pred_binary_naive <- training_suitability_naive > 0.2417432 #where are known presence greater than threshold?
length(pred_binary_naive[pred_binary_naive==TRUE]) # these are "a" the true positives
length(pred_binary_naive[pred_binary_naive==FALSE]) #these are "c" the false negatives

background_suitability_naive <- extract(px_no_host_all_occs, backg_with_florida)
pred_binary_background_naive <- background_suitability_naive > 0.2417432

length(pred_binary_background_naive[pred_binary_background_naive==TRUE]) #these are "b" the false pos
length(pred_binary_background_naive[pred_binary_background_naive==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Climate & Hosts ########################################
####################################################################################################

# k-fold
mx_climate_and_hosts_florida <- maxent(climate_and_hosts, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                             args=c('responsecurves=TRUE', 
                                                    'replicatetype=crossvalidate', 'replicates=5',
                                                    'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_and_hosts_florida@results

# all occurrences
mx_climate_and_hosts_full_florida <- maxent(climate_and_hosts, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                              args=c('responsecurves=TRUE',
                                                     'writebackgroundpredictions=TRUE'))


response(mx_climate_and_hosts_full_florida)
plot(mx_climate_and_hosts_full_florida)
mx_climate_and_hosts_full_florida@results
mx_climate_and_hosts_full_florida@lambdas

px_climate_and_hosts_full_florida <- predict(climate_and_hosts, mx_climate_and_hosts_model_all_occs, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_and_hosts_model, main= 'Maxent, raw values')
writeRaster(px_climate_and_hosts_model, filename="climate_and_hosts_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_climate_and_hosts_model <- extract(px_climate_and_hosts_model, thin_ptw_with_florida_coords) #all predicted values, all occs
training_suitability_climate_and_hosts_model <- na.omit(training_suitability_climate_and_hosts_model)
ten_thresh_climate_and_hosts_model <- quantile(training_suitability_climate_and_hosts_model, 0.1, na.rm = TRUE)
ten_thresh_climate_and_hosts_model

# Confusion Matrix
training_suitability_climate_and_hosts <- extract(px_climate_and_hosts_model, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_climate_and_hosts <- na.omit(training_suitability_exotic)
pred_binary_climate_and_hosts <- training_suitability_climate_and_hosts > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_climate_and_hosts[pred_binary_climate_and_hosts==TRUE]) # these are "a" the true positives
length(pred_binary_climate_and_hosts[pred_binary_climate_and_hosts==FALSE]) #these are "c" the false negatives

background_suitability_climate_and_hosts <- extract(px_climate_and_hosts_model, backg_with_florida)
pred_binary_background_climate_and_hosts <- background_suitability_climate_and_hosts > 0.2021041

length(pred_binary_background_climate_and_hosts[pred_binary_background_climate_and_hosts==TRUE]) #these are "b" the false pos
length(pred_binary_background_climate_and_hosts[pred_binary_background_climate_and_hosts==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Climate & Grasses ###################################
####################################################################################################

# k-fold
mx_climate_and_grasses_florida <- maxent(climate_and_grasses_occs, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                         args=c('responsecurves=TRUE', 
                                                'replicatetype=crossvalidate', 'replicates=5',
                                                'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_climate_and_grasses_florida@results
names(mx_climate_and_grasses_florida)

# all occurrences
mx_climate_and_grasses_full_florida <- maxent(climate_and_grasses_occs, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                                          args=c('responsecurves=TRUE',
                                                                 'writebackgroundpredictions=TRUE'))

response(mx_climate_and_grasses_full_occurrences)
plot(mx_climate_and_grasses_full_occurrences)
mx_climate_and_grasses_full_occurrences@results
mx_climate_and_grasses_full_occurrences@lambdas

px_climate_and_grasses_model <- predict(climate_and_grasses_occurrences, mx_climate_and_grasses_full_occurrences, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_and_grasses_model, main= 'Maxent, raw values')
writeRaster(px_climate_and_grasses_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_climate_and_grasses_model <- extract(px_climate_and_grasses_model, thin_ptw_with_florida_coords) #all predicted values, all occs
training_suitability_climate_and_grasses_model <- na.omit(training_suitability_climate_and_grasses_model)
ten_thresh_climate_and_grasses_model <- quantile(training_suitability_climate_and_grasses_model, 0.1, na.rm = TRUE)
ten_thresh_climate_and_grasses_model

# Confusion Matrix
training_suitability_climate_and_grasses <- extract(px_climate_and_grasses_model, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_climate_and_grasses <- na.omit(training_suitability_climate_and_grasses)
pred_binary_climate_and_grasses <- training_suitability_climate_and_grasses > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_climate_and_grasses[pred_binary_climate_and_grasses==TRUE]) # these are "a" the true positives
length(pred_binary_climate_and_grasses[pred_binary_climate_and_grasses==FALSE]) #these are "c" the false negatives

background_suitability_climate_and_grasses <- extract(px_climate_and_grasses_model, backg_with_florida)
pred_binary_background_climate_and_grasses <- background_suitability_climate_and_grasses > 0.2021041

length(pred_binary_background_climate_and_grasses[pred_binary_background_climate_and_grasses==TRUE]) #these are "b" the false pos
length(pred_binary_background_climate_and_grasses[pred_binary_background_climate_and_grasses==FALSE]) # these are "d" the true neg 


####################################################################################################
################################### MaxEnt for Climate & LULC ########################################
####################################################################################################

# k-fold
mx_climate_and_LULC_model_florida <- maxent(climate_and_LULC, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1", 
                                            args=c('responsecurves=TRUE', 
                                                   'replicatetype=crossvalidate', 'replicates=5',
                                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_climate_and_LULC_model_florida@results


# all occurrences
mx_climate_and_LULC_model_full_florida <- maxent(climate_and_LULC, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1",
                                                     args=c('responsecurves=TRUE',
                                                            'writebackgroundpredictions=TRUE'))


response(mx_climate_and_LULC_model_full_occurrences)
plot(mx_climate_and_LULC_model_full_occurrences)
mx_climate_and_LULC_model_full_occurrences@results
mx_climate_and_LULC_model_full_occurrences@lambdas

px_climate_and_LULC_model_full_occurrences <- predict(predictors_and_exotic_hosts, mx_climate_and_LULC_model_full_occurrences, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_and_LULC_model_full_occurrences, main= 'Maxent, raw values')
writeRaster(px_climate_and_LULC_model_full_occurrences, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_climate_and_LULC <- extract(px_climate_and_LULC_model_full_occurrences, thin_ptw_with_florida_coords) #all predicted values, all occs
training_suitability_climate_and_LULC <- na.omit(training_suitability_climate_and_LULC)
ten_thresh_climate_and_LULC <- quantile(training_suitability_climate_and_LULC, 0.1, na.rm = TRUE)
ten_thresh_climate_and_LULC

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_with_florida)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 


####################################################################################################
################################### MaxEnt for Climate & Hosts & Veg Occs ########################################
####################################################################################################

# k-fold
mx_climate_hosts_grasses_florida <- maxent(climate_and_hosts_and_grasses_occs, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                           args=c('responsecurves=TRUE', 
                                                  'replicatetype=crossvalidate', 'replicates=5',
                                                  'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_hosts_grasses_florida@results

# all occurrences
mx_climate_hosts_and_grasses_florida_full <- maxent(climate_and_hosts_and_grasses_occs, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                       args=c('responsecurves=TRUE',
                                              'writebackgroundpredictions=TRUE'))


response(mx_climate_hosts_and_grasses)
plot(mx_climate_hosts_and_grasses)
mx_climate_hosts_and_grasses@results
mx_climate_hosts_and_grasses@lambdas

px_climate_hosts_and_grasses <- predict(climate_and_hosts_and_grasses_occurrences, mx_climate_hosts_and_grasses, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_hosts_and_grasses, main= 'Maxent, raw values')
writeRaster(px_climate_hosts_and_grasses, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_climate_hosts_and_grasses <- extract(px_climate_hosts_and_grasses, thin_ptw_with_florida_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_with_florida)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Climate & Hosts & LULC ########################################
####################################################################################################

# k-fold
mx_climate_hosts_LULC_florida <- maxent(climate_and_hosts_and_LULC, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1", 
                                        args=c('responsecurves=TRUE', 
                                               'replicatetype=crossvalidate', 'replicates=5',
                                               'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_climate_hosts_LULC_florida@results
names(climate_and_hosts_and_LULC)

# all occurrences
mx_climate_hosts_LULC_florida_full_occs <- maxent(climate_and_hosts_and_LULC, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1",
                                          args=c('responsecurves=TRUE',
                                                 'writebackgroundpredictions=TRUE'))


response(mx_climate_hosts_LULC_florida_full_occs)
plot(mx_climate_hosts_LULC_florida_full_occs)
mx_climate_hosts_LULC_florida_full_occs@results
mx_climate_hosts_LULC_florida_full_occs@lambdas

px_climate_hosts_LULC_model <- predict(climate_and_hosts_and_LULC, mx_climate_hosts_LULC_florida_full_occs, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_hosts_LULC_model, main= 'Maxent, raw values')
writeRaster(px_climate_hosts_LULC_model, filename="climate_hosts_LULC_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_climate_hosts_LULC_model, thin_ptw_with_florida_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_with_florida)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 


####################################################################################################
####################################### MaxEnt for Hosts ##########################################
####################################################################################################

# k-fold
names(hosts)
mx_hosts_florida <- maxent(hosts, thin_ptw_with_florida_coords, a=backg_with_florida, 
                   args=c('responsecurves=TRUE', 
                          'replicatetype=crossvalidate', 'replicates=5',
                          'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_hosts_florida@results

# all occurrences
mx_hosts_model_full_florida <- maxent(hosts, thin_ptw_with_florida_coords, a=backg_with_florida, 
                                          args=c('responsecurves=TRUE',
                                                 'writebackgroundpredictions=TRUE'))


response(mx_hosts_model_full_occurrences)
plot(mx_exotic_model_all_occs)
mx_exotic_model_all_occs@results
mx_exotic_model_all_occs@lambdas

px_exotic_model <- predict(predictors_and_exotic_hosts, mx_exotic_model_all_occs, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_exotic_model, main= 'Maxent, raw values')
writeRaster(px_exotic_model, filename="exotic_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_exotic_model <- extract(px_exotic_model, thin_ptw_with_florida_coords) #all predicted values, all occs
training_suitability_exotic_model <- na.omit(training_suitability_exotic_model)
ten_thresh_exotic_model <- quantile(training_suitability_exotic_model, 0.1, na.rm = TRUE)
ten_thresh_exotic_model

# Confusion Matrix
training_suitability_exotic <- extract(px_exotic_model, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_exotic <- na.omit(training_suitability_exotic)
pred_binary_exotic <- training_suitability_exotic > 0.2021041 #where are known presence greater than threshold?
length(pred_binary_exotic[pred_binary_exotic==TRUE]) # these are "a" the true positives
length(pred_binary_exotic[pred_binary_exotic==FALSE]) #these are "c" the false negatives

background_suitability_exotic <- extract(px_exotic_model, backg_with_florida)
pred_binary_background_exotic <- background_suitability_exotic > 0.2021041

length(pred_binary_background_exotic[pred_binary_background_exotic==TRUE]) #these are "b" the false pos
length(pred_binary_background_exotic[pred_binary_background_exotic==FALSE]) # these are "d" the true neg 

