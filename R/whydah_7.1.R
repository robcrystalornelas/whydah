# Re-running Models w/ a CALIBRATION dataset

# Divide ptw data into training and testing

folds<-kfold(thin_ptw_with_florida_coords, k=4) #this is a 4 fold test
train<-thin_ptw_with_florida_coords[folds>1,] #training has 75% of points
test<-thin_ptw_with_florida_coords[folds==1,] #testing has 25% of points
train<-train[,1:2]
test<-test[,1:2]
dim(train)
dim(test)

####################################################################################################
######################################### MaxEnt for Climate #######################################
####################################################################################################

# mx_climate_florida <- maxent(climate, train, a=backg_with_florida, 
#                              args=c('responsecurves=TRUE', 
#                                     'replicatetype=crossvalidate', 'replicates=5',
#                                     'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# mx_climate_florida@results

# Full occurrence set
mx_climate_train <- maxent(climate, train, a=backg_with_florida, 
                                  args=c('responsecurves=TRUE',
                                         'writebackgroundpredictions=TRUE'))
mx_climate_train@results

# response(mx_no_host_all_occs)
plot(mx_climate_train)
mx_climate_train@results
mx_climate_train@lambdas

px_climate_train <- predict(climate, mx_climate_train, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_train, main= 'Maxent, raw values')

## DONT CHANGE
# Forming Confusion Matrix
test_suit_climate <-extract(px_climate_train, test) # extract predicted values, at all presence points
test_suit_climate<-na.omit(all_suit_climate)
ten_thresh_climate_train <- quantile(all_suit_climate, 0.1, na.rm = TRUE)
ten_thresh_climate_train


## CHANGE
# Calculating Omission
pred_binary_climate_test <- test_suit_climate > ten_thresh_climate_train #where are known presence greater than threshold?
length(pred_binary_climate_test[pred_binary_climate_test==TRUE]) # these are "a" the true positives
length(pred_binary_climate_test[pred_binary_climate_test==FALSE]) #these are "c" the false negatives

# Calculating Comission
# background_suitability_climate <- extract(px_climate_full_florida, backg_with_florida)
# pred_binary_background_climate <- background_suitability_climate > ten_thresh_climate
# 
# length(pred_binary_background_climate[pred_binary_background_climate==TRUE]) #these are "b" the false pos
# length(pred_binary_background_climate[pred_binary_background_climate==FALSE]) # these are "d" the true neg 

####################################################################################################
####################################### MaxEnt for Hosts ##########################################
####################################################################################################

# k-fold
# names(hosts)
# mx_hosts_florida_train <- maxent(hosts, thin_ptw_with_florida_coords, a=backg_with_florida, 
#                            args=c('responsecurves=TRUE', 
#                                   'replicatetype=crossvalidate', 'replicates=5',
#                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# mx_hosts_florida@results

# all occurrences
mx_hosts_model_train <- maxent(hosts, train, a=backg_with_florida, 
                                      args=c('responsecurves=TRUE',
                                             'writebackgroundpredictions=TRUE'))

mx_hosts_model_train@results
response(mx_hosts_model_train)
plot(mx_hosts_model_train)
dim(thin_ptw_with_florida_coords)
px_hosts_model_train <- predict(hosts, mx_hosts_model_train, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_hosts_model_train, main= 'Maxent, raw values')
writeRaster(px_hosts_model_train, filename="hosts_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
test_suit_hosts <- extract(px_hosts_model_train, train) #all predicted values, all occs
test_suit_hosts <- na.omit(test_suit_hosts)
ten_thresh_hosts_test <- quantile(test_suit_hosts, 0.1, na.rm = TRUE)
ten_thresh_hosts_test
###
ten_thresh_hosts_test <- .036

# Confusion Matrix
test_suitability_hosts <- extract(px_hosts_model_train, test) # extract predicted values, at known presence points
test_suitability_hosts <- na.omit(test_suitability_hosts)
pred_binary_hosts_test <- test_suit_hosts > ten_thresh_hosts_model #where are known presence greater than threshold?
length(pred_binary_hosts_test[pred_binary_hosts_test==TRUE]) # these are "a" the true positives
length(pred_binary_hosts_test[pred_binary_hosts_test==FALSE]) #these are "c" the false negatives

background_suitability_hosts <- extract(px_hosts_model, backg_with_florida)
pred_binary_background_hosts <- background_suitability_hosts > ten_thresh_hosts_model

length(pred_binary_background_hosts[pred_binary_background_hosts==TRUE]) #these are "b" the false pos
length(pred_binary_background_hosts[pred_binary_background_hosts==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for LULC ########################################
####################################################################################################
# k-fold
# names(LULC)
# mx_LULC_model_florida <- maxent(LULC, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1",
#                                 args=c('responsecurves=TRUE', 
#                                        'replicatetype=crossvalidate', 'replicates=5',
#                                        'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# mx_LULC_model_florida@results

# all occurrences
mx_LULC_train <- maxent(LULC, train, a=backg_with_florida, factors = "band1",
                                     args=c('responsecurves=TRUE',
                                            'writebackgroundpredictions=TRUE'))

response(mx_LULC_train)
mx_LULC_train@results
mx_LULC_train@path

px_LULC_train <- predict(LULC, mx_LULC_train, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_LULC_train, main= 'Maxent, raw values')
writeRaster(px_LULC_train, filename="LULC_model_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
test_suitability_LULC <- extract(px_LULC_train, test) #all predicted values, all occs
test_suitability_LULC <- na.omit(test_suitability_LULC)
ten_thresh_train_LULC <- quantile(test_suitability_LULC, 0.1, na.rm = TRUE)
ten_thresh_train_LULC

# Confusion Matrix
test_suitability_LULC <- extract(px_LULC_train, test) # extract predicted values, at known presence points
test_suitability_LULC <- na.omit(test_suitability_LULC)
pred_binary_test_LULC <- test_suitability_LULC > ten_thresh_train_LULC #where are known presence greater than threshold?
length(pred_binary_test_LULC[pred_binary_test_LULC==TRUE]) # these are "a" the true positives
length(pred_binary_test_LULC[pred_binary_test_LULC==FALSE]) #these are "c" the false negatives

# background_suitability_LULC <- extract(px_LULC_model_full_occurrences, backg_with_florida)
# pred_binary_background_LULC <- background_suitability_LULC > ten_thresh_LULC
# 
# length(pred_binary_background_LULC[pred_binary_background_LULC==TRUE]) #these are "b" the false pos
# length(pred_binary_background_LULC[pred_binary_background_LULC==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Climate & Hosts ########################################
####################################################################################################
# k-fold
# mx_climate_and_hosts_florida <- maxent(climate_and_hosts, thin_ptw_with_florida_coords, a=backg_with_florida, 
#                                        args=c('responsecurves=TRUE', 
#                                               'replicatetype=crossvalidate', 'replicates=5',
#                                               'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# mx_climate_and_hosts_florida@results

# all occurrences
mx_climate_and_hosts_train <- maxent(climate_and_hosts, train, a=backg_with_florida, 
                                            args=c('responsecurves=TRUE',
                                                   'writebackgroundpredictions=TRUE'))
plot(mx_climate_and_hosts_train)
mx_climate_and_hosts_train@results
mx_climate_and_hosts_train@lambdas

px_climate_and_hosts_train <- predict(climate_and_hosts, mx_climate_and_hosts_train, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_and_hosts_train, main= 'Maxent, raw values')
writeRaster(px_climate_and_hosts_train, filename="climate_and_hosts_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
test_suit_climate_and_hosts <- extract(px_climate_and_hosts_train, train) #all predicted values, all occs
test_suit_climate_and_hosts <- na.omit(test_suit_climate_and_hosts)
ten_thresh__test_climate_and_hosts <- quantile(test_suit_climate_and_hosts, 0.1, na.rm = TRUE)
ten_thresh_test_climate_and_hosts

## Confusion Matrix

## Omission
test_suitability_climate_and_hosts <- extract(px_climate_and_hosts_train, test) # extract predicted values, at known presence points
test_suitability_climate_and_hosts <- na.omit(test_suitability_climate_and_hosts)
pred_binary_test_climate_and_hosts <- test_suitability_climate_and_hosts > ten_thresh_test_climate_and_hosts #where are known presence greater than threshold?
length(pred_binary_test_climate_and_hosts[pred_binary_test_climate_and_hosts==TRUE]) # these are "a" the true positives
length(pred_binary_test_climate_and_hosts[pred_binary_test_climate_and_hosts==FALSE]) #these are "c" the false negatives

## Commission 

# background_suitability_climate_and_hosts <- extract(px_climate_and_hosts, backg_with_florida)
# pred_binary_background_climate_and_hosts <- background_suitability_climate_and_hosts > ten_thresh_climate_and_hosts
# 
# length(pred_binary_background_climate_and_hosts[pred_binary_background_climate_and_hosts==TRUE]) #these are "b" the false pos
# length(pred_binary_background_climate_and_hosts[pred_binary_background_climate_and_hosts==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Climate & LULC ########################################
####################################################################################################

# k-fold
# mx_climate_and_LULC_model_florida <- maxent(climate_and_LULC, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1", 
#                                             args=c('responsecurves=TRUE', 
#                                                    'replicatetype=crossvalidate', 'replicates=5',
#                                                    'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# 
# mx_climate_and_LULC_model_florida@results

# all occurrences
mx_climate_and_LULC_train <- maxent(climate_and_LULC, train, a=backg_with_florida, factors = "band1",
                                                 args=c('responsecurves=TRUE',
                                                        'writebackgroundpredictions=TRUE'))
plot(mx_climate_and_LULC_train)
mx_climate_and_LULC_train@results
mx_climate_and_LULC_train@lambdas

px_climate_and_LULC_train <- predict(climate_and_LULC, mx_climate_and_LULC_train, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_and_LULC_train, main= 'Maxent, raw values')
writeRaster(px_climate_and_LULC_train, filename="climate_and_LULC_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
test_suitability_climate_and_LULC <- extract(px_climate_and_LULC_train, train) #all predicted values, all occs
test_suitability_climate_and_LULC <- na.omit(test_suitability_climate_and_LULC)
ten_thresh_climate_and_LULC <- quantile(test_suitability_climate_and_LULC, 0.1, na.rm = TRUE)
ten_thresh_climate_and_LULC

# Confusion Matrix
# Omission
test_suitability_climate_and_LULC <- extract(px_climate_and_LULC_train, test) # extract predicted values, at known presence points
test_suitability_climate_and_LULC <- na.omit(test_suitability_climate_and_LULC)
test_binary_climate_and_LULC <- test_suitability_climate_and_LULC > ten_thresh_climate_and_LULC #where are known presence greater than threshold?
length(test_binary_climate_and_LULC[test_binary_climate_and_LULC==TRUE]) # these are "a" the true positives
length(test_binary_climate_and_LULC[test_binary_climate_and_LULC==FALSE]) #these are "c" the false negatives

# Commission
# background_suitability_climate_and_LULC <- extract(px_climate_and_LULC_model, backg_with_florida)
# pred_binary_background_climate_and_LULC <- background_suitability_climate_and_LULC > ten_thresh_climate_and_LULC
# 
# length(pred_binary_background_climate_and_LULC[pred_binary_background_climate_and_LULC==TRUE]) #these are "b" the false pos
# length(pred_binary_background_climate_and_LULC[pred_binary_background_climate_and_LULC==FALSE]) # these are "d" the true neg 

####################################################################################################
################################### MaxEnt for Hosts & Habitat ########################################
####################################################################################################

# k-fold
# names(hosts_and_habitat)
# mx_hosts_and_habitat <- maxent(hosts_and_habitat, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1", 
#                                args=c('responsecurves=TRUE', 
#                                       'replicatetype=crossvalidate', 'replicates=5',
#                                       'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# 
# mx_hosts_and_habitat@results

# all occurrences
mx_hosts_and_habitat_train <- maxent(hosts_and_habitat, train, a=backg_with_florida, factors = "band1",
                                    args=c('responsecurves=TRUE',
                                           'writebackgroundpredictions=TRUE'))
plot(mx_hosts_and_habitat_train)
mx_hosts_and_habitat_train@results
mx_hosts_and_habitat_train@lambdas

px_hosts_and_habitat_train <- predict(hosts_and_habitat, mx_hosts_and_habitat_train, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_hosts_and_habitat_train, main= 'Maxent, raw values')
writeRaster(px_hosts_and_habitat_train, filename="hosts_and_habitat_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_hosts_and_habitat <- extract(px_hosts_and_habitat_train, train) #all predicted values, all occs
training_suitability_hosts_and_habitat <- na.omit(training_suitability_hosts_and_habitat)
ten_thresh_hosts_and_habitat <- quantile(training_suitability_hosts_and_habitat, 0.1, na.rm = TRUE)
ten_thresh_hosts_and_habitat

# Confusion Matrix
training_suitability_hosts_and_habitat <- extract(px_hosts_and_habitat, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_hosts_and_habitat <- na.omit(training_suitability_hosts_and_habitat)
pred_binary_hosts_and_habitat <- training_suitability_hosts_and_habitat > ten_thresh_hosts_and_habitat #where are known presence greater than threshold?
length(pred_binary_hosts_and_habitat[pred_binary_hosts_and_habitat==TRUE]) # these are "a" the true positives
length(pred_binary_hosts_and_habitat[pred_binary_hosts_and_habitat==FALSE]) #these are "c" the false negatives

background_suitability_hosts_and_habitat <- extract(px_hosts_and_habitat, backg_with_florida)
pred_binary_background_hosts_and_habitat <- background_suitability_hosts_and_habitat > ten_thresh_hosts_and_habitat

length(pred_binary_background_hosts_and_habitat[pred_binary_background_hosts_and_habitat==TRUE]) #these are "b" the false pos
length(pred_binary_background_hosts_and_habitat[pred_binary_background_hosts_and_habitat==FALSE]) # these are "d" the true neg 


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
mx_climate_hosts_LULC_florida_full <- maxent(climate_and_hosts_and_LULC, thin_ptw_with_florida_coords, a=backg_with_florida, factors = "band1",
                                             args=c('responsecurves=TRUE',
                                                    'writebackgroundpredictions=TRUE'))


plot(mx_climate_hosts_LULC_florida_full)
mx_climate_hosts_LULC_florida_full@results
mx_climate_hosts_LULC_florida_full@lambdas

px_climate_hosts_LULC_florida <- predict(climate_and_hosts_and_LULC, mx_climate_hosts_LULC_florida_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_climate_hosts_LULC_florida, main= 'Maxent, raw values')
writeRaster(px_climate_hosts_LULC_florida, filename="climate_hosts_LULC_with_florida_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

# 10% Min. Training Pres Threshold
training_suitability_climate_host_LULC <- extract(px_climate_hosts_LULC_florida, train) #all predicted values, all occs
training_suitability_climate_host_LULC <- na.omit(training_suitability_climate_host_LULC)
ten_thresh_climate_host_LULC <- quantile(training_suitability_climate_host_LULC, 0.1, na.rm = TRUE)
ten_thresh_climate_host_LULC

# Confusion Matrix
training_suitability_climate_hosts_LULC <- extract(px_climate_hosts_LULC_florida, thin_ptw_with_florida_coords) # extract predicted values, at known presence points
training_suitability_climate_hosts_LULC <- na.omit(training_suitability_climate_hosts_LULC)
pred_binary_climate_hosts_LULC <- training_suitability_climate_hosts_LULC > ten_thresh_climate_host_LULC #where are known presence greater than threshold?
length(pred_binary_climate_hosts_LULC[pred_binary_climate_hosts_LULC==TRUE]) # these are "a" the true positives
length(pred_binary_climate_hosts_LULC[pred_binary_climate_hosts_LULC==FALSE]) #these are "c" the false negatives

background_suitability_climate_hosts_LULC <- extract(px_climate_hosts_LULC_florida, backg_with_florida)
pred_binary_background_climate_hosts_LULC <- background_suitability_climate_hosts_LULC > ten_thresh_climate_host_LULC

length(pred_binary_background_climate_hosts_LULC[pred_binary_background_climate_hosts_LULC==TRUE]) #these are "b" the false pos
length(pred_binary_background_climate_hosts_LULC[pred_binary_background_climate_hosts_LULC==FALSE]) # these are "d" the true neg 


