####################################################################################################
################### Models built with HOST SUITABILITY rather than occurrences #####################
##################                      INCLUDES FLORIDA POINTS                #####################
####################################################################################################


##### Read in all background
cw_buffer <- readGDAL("cw_buffer.tif")
cw_buffer <- raster(cw_buffer) #convert africa map to raster
cw_backg <- randomPoints(cw_buffer, n=10000)

ocw_buffer <- readGDAL("ocw_buffer.tif")
ocw_buffer <- raster(ocw_buffer)
ocw_backg <- randomPoints(ocw_buffer, n=10000)

bronze_buffer <- readGDAL("bronze_buffer.tif")
bronze_buffer <- raster(bronze_buffer)
bronze_backg <- randomPoints(bronze_buffer, n=10000)

nutmeg_buffer <- readGDAL("nutmeg_buffer.tif")
nutmeg_buffer <- raster(nutmeg_buffer)
nutmeg_backg <- randomPoints(nutmeg_buffer, n=10000)

black_rumped_buffer <- readGDAL("black_rumped_buffer.tif")
black_rumped_buffer <- raster(black_rumped_buffer)
black_rumped_backg <- randomPoints(black_rumped_buffer, n=10000)

silverbill_buffer <- readGDAL("silverbill_buffer.tif")
silverbill_buffer <- raster(silverbill_buffer)
silverbill_backg <- randomPoints(silverbill_buffer, n=10000)

grasses_buffer <- readGDAL("silverbill_buffer.tif")

####################################################################################################
####################################   CW   ###########################################
####################################################################################################
write.csv(thin_cw2, file = "cw_occurrences.csv")

thin_cw <- thin_cw2[,1:2]
mx_common_waxbill <- maxent(climate, thin_cw, a=cw_backg, 
                                             args=c('responsecurves=TRUE', 
                                                    'replicatetype=crossvalidate', 'replicates=5',
                                                    'writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_common_waxbill@results

mx_cw_full <- maxent(climate, thin_cw, a=cw_backg, 
                          args=c('responsecurves=TRUE',
                                 'writebackgroundpredictions=TRUE'))

px_cw_full <- predict(climate, mx_cw_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_cw_full, main= 'Maxent, raw values')

####################################################################################################
#####################################   OCW   ####################################################
####################################################################################################
write.csv(thin_ocw2, file = "ocw_occurrences.csv")

thin_ocw3 <- thin_ocw2[,1:2]
head(thin_ocw3)
mx_orange_waxbill <- maxent(climate, thin_ocw3, a= ocw_backg, 
                            args=c('responsecurves=TRUE', 
                                   'replicatetype=crossvalidate', 'replicates=5',
                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_orange_waxbill@results

mx_ocw_full <- maxent(climate, thin_ocw3, a=ocw_backg, 
                     args=c('responsecurves=TRUE',
                            'writebackgroundpredictions=TRUE'))

px_ocw_full <- predict(climate, mx_ocw_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_ocw_full, main= 'Maxent, raw values')

####################################################################################################
##########################################    Bronze Mannikin ######################################################
####################################################################################################
write.csv(thin_bronze2, file = "bronze_occurrences.csv")

head(thin_bronze2)
thin_bronze3 <- thin_bronze2[,1:2]
mx_bronze <- maxent(climate, thin_bronze3, a= bronze_backg, 
                            args=c('responsecurves=TRUE', 
                                   'replicatetype=crossvalidate', 'replicates=5',
                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_bronze@results

mx_bronze_full <- maxent(climate, thin_bronze3, a=bronze_backg, 
                      args=c('responsecurves=TRUE',
                             'writebackgroundpredictions=TRUE'))

px_bronze_full <- predict(climate, mx_bronze_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_bronze_full, main= 'Maxent, raw values')
####################################################################################################
#########################################    Nutmeg Mannikin ###############################################
####################################################################################################
write.csv(thin_nutmeg2, file = "nutmeg_occurrences.csv")

head(thin_nutmeg2)

mx_nutmeg_mannikin <- maxent(climate, thin_nutmeg2, a= nutmeg_backg, 
                            args=c('responsecurves=TRUE', 
                                   'replicatetype=crossvalidate', 'replicates=5',
                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_nutmeg_mannikin@results

mx_nutmeg_full <- maxent(climate, thin_nutmeg2, a=nutmeg_backg, 
                         args=c('responsecurves=TRUE',
                                'writebackgroundpredictions=TRUE'))

px_nutmeg_full <- predict(climate, mx_nutmeg_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_nutmeg_full, main= 'Maxent, raw values')

####################################################################################################
#########################################   Black-rumped waxbill ############################################
####################################################################################################
write.csv(thin_black_rumped_waxbill2, file = "black_rumped_occurrences.csv")

head(thin_black_rumped_waxbill2)
mx_black_rumped <- maxent(climate, thin_black_rumped_waxbill2, a= black_rumped_backg, 
                            args=c('responsecurves=TRUE', 
                                   'replicatetype=crossvalidate', 'replicates=5',
                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_black_rumped@results

mx_black_rumped_full <- maxent(climate, thin_black_rumped_waxbill2, a=black_rumped_backg, 
                         args=c('responsecurves=TRUE',
                                'writebackgroundpredictions=TRUE'))

px_black_rumped_full <- predict(climate, mx_black_rumped_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_black_rumped_full, main= 'Maxent, raw values')

####################################################################################################
#########################################    African Silverbill ###############################################
####################################################################################################
write.csv(thin_silverbill2, file = "silverbill_occurrences.csv")

head(thin_silverbill2)

mx_silverbill <- maxent(climate, thin_silverbill2, a= silverbill_backg, 
                            args=c('responsecurves=TRUE', 
                                   'replicatetype=crossvalidate', 'replicates=5',
                                   'writebackgroundpredictions=TRUE','outputgrids=TRUE'))

mx_silverbill_full <- maxent(climate, thin_silverbill2, a=silverbill_backg, 
                               args=c('responsecurves=TRUE',
                                      'writebackgroundpredictions=TRUE'))

px_silverbill_full <- predict(climate, mx_silverbill_full, progress='text') #make predictions of habitat suitability can include argument ext=ext
plot(px_silverbill_full, main= 'Maxent, raw values')




####################################################################################################
####################################################################################################
####################################################################################################
climate_and_host_suit_and_LULC <- stack(climate, LULC_raster, px_cw_full, px_ocw_full, px_bronze_full, px_nutmeg_full, px_black_rumped_full, px_silverbill_full)
climate_and_host_suit <- stack(climate, px_cw_full, px_ocw_full, px_bronze_full, px_nutmeg_full, px_black_rumped_full, px_silverbill_full)
climate_and_host_suit_and_echino_suit <- stack
hosts_suit <- stack(px_cw_full, px_ocw_full, px_bronze_full, px_nutmeg_full, px_black_rumped_full, px_silverbill_full)
climate_and_echino_suit <- stack()

####################################################################################################
####################################################################################################
####################################################################################################


####################################################################################################
####################################################################################################
####################################################################################################