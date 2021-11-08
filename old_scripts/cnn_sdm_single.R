###############################################
####  Convolutional Neural Networks (CNN)  ####
####               1D-CNN                  ####
###############################################
# https://d2l.ai/

# CNN: A particular case of NN
# 1D-CNN means that the convolution kernel is convolved with the layer input 
# over a single spatial (or temporal) dimension to produce a tensor of outputs
# In other words, the input layer has only one dimension, in our case one pixel
# and its punctual values of environmental variables


library(raster)
library(sf)
library(keras)
library(dplyr)

getwd()
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_test/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_single/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/cnn_sdm_single/"
setwd(wd)

bioscrop <- brick("../dl_sdm_test/bioscrop.tif")    # 10 km
bioscrop@crs <- sp::CRS("+init=EPSG:4326")
bioscrop
dev.off()
plot(bioscrop[[1]])


worldclim_path <- "/Users/xavi_rp/Documents/MinBA_models/wc5"

worldclim_files <- list.files(worldclim_path, full.names = TRUE)
worldclim_files <- worldclim_files[grepl("bil$", worldclim_files)]
worldclim_files

worldclim_all <- stack(worldclim_files)
plot(worldclim_all[[1]])

worldclim_all <- crop(worldclim_all, bioscrop)
worldclim_all
plot(worldclim_all[[1]])


bioscrop <- worldclim_all
rm(worldclim_all); gc()


#bioscrop_array <- as.array(bioscrop)
#dim(bioscrop_array)



load("../dl_sdm_test/sprecords.RData", verbose = TRUE)
sprecords  
length(sprecords$species) # 1064 occurrences of Linaria alpina

#linaria_pres <- st_as_sf(x = sprecords, coords = c("decimalLongitude", "decimalLatitude"))
linaria_pres <- SpatialPointsDataFrame(coords = sprecords[, c("decimalLongitude", "decimalLatitude")],
                                       data = data.frame(linaria_alpina = rep(1, nrow(sprecords))),
                                       proj4string = CRS("+init=EPSG:4326"))

plot(linaria_pres, add = TRUE, col = "purple")


linaria_pres_rstr <- rasterize(linaria_pres, bioscrop, background = 0) 
#names(linaria_pres_rstr) <- "linaria_alpina"
linaria_pres_rstr <- linaria_pres_rstr[["linaria_alpina"]]
linaria_pres_rstr <- mask(linaria_pres_rstr, bioscrop[[1]])

plot(bioscrop[[1]])
plot(linaria_pres_rstr, col = c("red", "black"), legend = FALSE, add = TRUE)

res(linaria_pres_rstr) == res(bioscrop)


#linaria_pres_data <- extract(bioscrop, linaria_pres)
#linaria_pres_data

linaria_data <- stack(linaria_pres_rstr, bioscrop)
plot(linaria_data[[1]], add = TRUE, legend = FALSE)
plot(bioscrop[[1]])

linaria_data <- as.data.frame(linaria_data)
linaria_data$raster_position <- 1:nrow(linaria_data)
head(linaria_data)
tail(linaria_data)
nrow(linaria_data)
sum(!is.na(linaria_data$bioscrop.1))
unique(linaria_data$linaria_alpina)
sum(linaria_data$linaria_alpina, na.rm = TRUE)


# data set for the presences
linaria_data_presences <- linaria_data[linaria_data$linaria_alpina == 1, ]
linaria_data_presences <- linaria_data_presences[complete.cases(linaria_data_presences), ]
head(linaria_data_presences)
nrow(linaria_data_presences)

# data set from where to extract background/pseudo-absences
linaria_data_NoPresences <- linaria_data[linaria_data$linaria_alpina == 0, ]
linaria_data_NoPresences <- linaria_data_NoPresences[complete.cases(linaria_data_NoPresences), ]
head(linaria_data_NoPresences)
nrow(linaria_data_NoPresences)


# For now, each point is associated to a one (the same) point of the explanatory variables.
# Later, following Botella's approach (?), each point (presence and absence) can be associated to a 
# matrix of pixels surrounding the point (e.g. 3x3 or 5x5). This is to capture the influence of 
# the "micro-habitat" of the plant.

head(linaria_data_presences[, 2:length(linaria_data_presences)])
dim(linaria_data_presences[, 2:length(linaria_data_presences)])
is.array(linaria_data_presences[, 2:length(linaria_data_presences)])
is.data.frame(linaria_data_presences[, 2:length(linaria_data_presences)]) 


# Creating a full data set with presences and absences where 'linaria_alpina' = 0 for absences
# and 'linaria_alpina' = 1 for absences, with some level of prevalence; although prevalence can't be known
# in not structured surveys (i.e. using GBIF data).
# (e.g. prevalence = 0.5 means absences = 2*presences)
# abs   pres
# 100 + 50
# prevalence (0.5) 
# 50/100

# Let's assume all "linaria_data_NoPresences" are real absences
nrow(linaria_data_presences) / nrow(linaria_data_NoPresences)
# our prevalence = 0.0051


# If the data set is strongly class imbalanced (very diff number of presences vs absences), 
# models can be biassed towards the prediction of majority classes and the reliability of 
# preformance metrics (e.g. accuracy)

## Let's subset a balanced data set for training the model (we set aside 20% of presences for validation)
n2sample <- round(nrow(linaria_data_presences) * 80 / 100)

linaria_pres_train <- sample_n(linaria_data_presences, n2sample)
head(linaria_pres_train)
nrow(linaria_pres_train)

linaria_pres_test <- linaria_data_presences[!linaria_data_presences$raster_position %in% linaria_pres_train$raster_position, ]
head(linaria_pres_test)
nrow(linaria_pres_test)


## Balanced number of absences
#linaria_abs_train <- sample_n(linaria_data_NoPresences, n2sample)
#head(linaria_abs_train)
#
#linaria_abs_test <- sample_n(linaria_data_NoPresences, nrow(linaria_pres_test))
#sum(linaria_abs_train$raster_position %in% linaria_abs_test$raster_position)  # has to be 0 for not repeating absences


## or better, unbalanced pseudo-absences to better capture unsuitable environments for the species 
linaria_abs_train <- sample_n(linaria_data_NoPresences, 10000)
head(linaria_abs_train)
nrow(linaria_abs_train)

linaria_abs_test <- sample_n(linaria_data_NoPresences[!linaria_data_NoPresences$raster_position %in% linaria_abs_train$raster_position, ], 10000)
sum(linaria_abs_train$raster_position %in% linaria_abs_test$raster_position)  # has to be 0 for not repeating absences
head(linaria_abs_test)
nrow(linaria_abs_test)


linaria_train <- rbind(linaria_pres_train[, 1:(length(linaria_pres_train) - 1)], linaria_abs_train[, 1:(length(linaria_abs_train) - 1)])
head(linaria_train)
tail(linaria_train)
nrow(linaria_train)

linaria_test <- rbind(linaria_pres_test[, 1:(length(linaria_pres_test) - 1)], linaria_abs_test[, 1:(length(linaria_abs_test) - 1)])
nrow(linaria_test)
head(linaria_test)
#


# Creating arrays
linaria_train_array_x <- array(as.matrix(linaria_train[, -1]), dim = c(nrow(linaria_train), ncol(linaria_train[, -1]), 1))
dim(linaria_train_array_x)

#linaria_train_array_y <- to_categorical(as.matrix(linaria_train[, 1]))
linaria_train_array_y <- as.matrix(linaria_train[, 1, drop = FALSE])
dim(linaria_train_array_y)
head(linaria_train_array_y)
tail(linaria_train_array_y)


linaria_test_array_x <- array(as.matrix(linaria_test[, -1]), dim = c(nrow(linaria_test), ncol(linaria_test[, -1]), 1))
dim(linaria_test_array_x)
#linaria_test_array_y <- to_categorical(as.matrix(linaria_test[, 1]))
linaria_test_array_y <- as.matrix(linaria_test[, 1, drop = FALSE])
dim(linaria_test_array_y)


# I have to create an array where for each pixel (pres and abs) I have a tensor of, say, 3x3 pixels around it
# Then, dim of this array will be 'number of pixels', 3, 3, 'number of variables'
# This array will be 'x', which will be divided into 'x_train' and 'x_test' (e.g. 70/30)
# Finally, I will have another array for labels with dim equal to 'number of pixels' and 1 (one column per species with 1s and 0s for pres and absences, respectivelly)


#

inpt_shpe <- length(linaria_train) - 1  # input_shape = c(ncol(dataTrain_x), nrow(dataTrain_x))) 
rm(model_c); gc()

model_c <- keras_model_sequential()

model_c %>% 
  layer_conv_1d(filters = 9, kernel_size = 3, padding = "valid", input_shape = c(inpt_shpe, 1), activation = "relu") %>%
  layer_max_pooling_1d() %>%
  layer_conv_1d(filters = 18, kernel_size = 3, padding = "valid", activation = "relu") %>%
  layer_max_pooling_1d() %>%
  layer_conv_1d(filters = 64, kernel_size = 5, padding = "causal", activation = "relu") %>%
  layer_max_pooling_1d() %>%
  layer_flatten() %>%
  layer_dense(units = 1, activation = "sigmoid")

#model_c %>% 
#  layer_conv_1d(filters = 64, kernel_size = 3, padding = "valid", input_shape = c(inpt_shpe, 1), activation = "relu") %>%
#  #layer_max_pooling_1d() %>%
#  layer_average_pooling_1d() %>%
#  layer_conv_1d(filters = 128, kernel_size = 5, padding = "valid", input_shape = c(inpt_shpe, 1), activation = "relu") %>%
#  layer_average_pooling_1d() %>%
#  layer_flatten() %>%
#  layer_dense(units = 1, activation = "sigmoid")


# Why to use Pooling Layers?
# Pooling layers are used to reduce the dimensions of the feature maps. Thus, it reduces the 
# number of parameters to learn and the amount of computation performed in the network. The 
# pooling layer summarises the features present in a region of the feature map generated by 
# a convolution layer. So, further operations are performed on summarised features instead 
# of precisely positioned features generated by the convolution layer. This makes the model 
# more robust to variations in the position of the features in the input image. 

# padding = "valid" means no padding (seems to give slightly better results than "same" and "causal")
#           "same" results in padding the input such that the output has the same length as the original input. 
#           "causal" results in causal (dilated) convolutions, e.g. output[t] does not depend on input[t+1:]. Useful when modeling temporal data where the model should not violate the temporal order

# filters: the dimensionality of the output space (i.e. the number of output filters in the convolution).
#          https://towardsdatascience.com/a-beginners-guide-to-convolutional-neural-networks-cnns-14649dbddce8
#          Usually the smaller the better, and prefearible odd numbers. 3x3 convolution filters work in 
#          general, and is often the popular choice!

# https://smltar.com/dlcnn.html
# Having a small kernel size in the first layer will let the model detect low-level features locally.
# Larger kernels learn larger and less frequent patterns, while smaller kernels will find fine-grained features.

# layer_max_pooling_1d() between the convolutional layers. This layer performs a pooling operation that 
# calculates the maximum values in its pooling window; in this model, that is set to 2. This is done in the 
# hope that the pooled features will be able to perform better by weeding out the small weights.

# stride: The step size as the filter slides across the image

model_c %>% 
  compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_sgd(lr = 1e-5, momentum = 0.9),
    metrics = c(tensorflow::tf$keras$metrics$AUC(), metric_binary_accuracy)
  )


model_c %>% fit(linaria_train_array_x, linaria_train_array_y, 
                batch_size = 128, 
                epochs = 100, 
                verbose = 2)

score_train <- model_c %>% evaluate(linaria_train_array_x, linaria_train_array_y, batch_size = 128)
score_train

score <- model_c %>% evaluate(linaria_test_array_x, linaria_test_array_y, batch_size = 128)
score


score #for test data
#       loss           auc    binary_accuracy 
# 0.0650057       0.9652347       0.9791071 


# for training data (score_train)
#       loss            auc    binary_accuracy 
# 0.09203751      0.96858549      0.96816850 
# The gap between training accuracy and test accuracy is an example of "overfitting" 
# train_auc () < test_auc ()  --> No overfitting
# train_accuracy () < test_accuracy ()  --> No overfitting
# Overfitting is when a machine learning model performs worse on new data than on their training data



## Make predictions over the whole extent
bioscrop_data <- as.data.frame(bioscrop)
bioscrop_data <- bioscrop_data[complete.cases(bioscrop_data), ]
head(bioscrop_data)
nrow(bioscrop_data)

dim(linaria_train_array_x)
dim(as.matrix(bioscrop_data))

bioscrop_data_array_x <- array(as.matrix(bioscrop_data), dim = c(nrow(bioscrop_data), ncol(bioscrop_data), 1))
dim(bioscrop_data_array_x)


predictions <- model_c %>% predict_proba(bioscrop_data_array_x, batch_size = 128)
head(predictions)
nrow(predictions)
sum(predictions[, 1])

length(unique(predictions[, 1]))
range(predictions[, 1])
max(predictions[, 1])
summary(predictions[, 1])


## Mapping predictions
head(predictions)
nrow(predictions)

bioscrop
head(as.data.frame(bioscrop))
nrow(as.data.frame(bioscrop))

bioscrop_data <- as.data.frame(bioscrop)
bioscrop_data$raster_position <- 1:nrow(bioscrop_data)
head(bioscrop_data)

bioscrop_data_1 <- bioscrop_data[complete.cases(bioscrop_data), ]
bioscrop_data_1 <- bioscrop_data_1[, length(bioscrop_data_1), drop = FALSE]
head(bioscrop_data_1)

bioscrop_data_1$linaria_prediction <- predictions
head(bioscrop_data_1)

bioscrop_data <- bioscrop_data[, length(bioscrop_data), drop = FALSE]
bioscrop_data <- merge(bioscrop_data, bioscrop_data_1, by = "raster_position", all.x = TRUE)
head(bioscrop_data)

linaria_predictions <- bioscrop[[1]]
linaria_predictions <- setValues(linaria_predictions, bioscrop_data$linaria_prediction)
names(linaria_predictions) <- "linaria_predictions"
linaria_predictions

#dev.off()

pdf("linaria_predictions.pdf", width = 20, height = 15)
par(mfrow = c(1, 2))
plot(linaria_predictions, zlim = c(0, 1))
plot(linaria_pres, add = TRUE, col = "black")
plot(linaria_predictions, zlim = c(0, 1))
dev.off()






## Evaluations ####

## AUC ##

# CNN
score #for test data
#       loss             auc    binary_accuracy 
# 0.0650057       0.9652347       0.9791071


# MaxEnt
load("../dl_sdm_single/evaluations.RData", verbose = TRUE)
score_dl
auc_mxnt
BI_dl$Spearman.cor
BI_mxnt$Spearman.cor
BI$Spearman.cor


score_mxnt@auc                      # 0.9848272 (MaxEnt)
score[grepl("auc", names(score))]   # 0.9652347  (CNN)



## Boyce Index ##

# CNN
linaria_pres_test_coords <- xyFromCell(object = linaria_predictions, 
                                       cell = linaria_pres_test$raster_position, 
                                       spatial = FALSE)

BI <- ecospat::ecospat.boyce(fit = linaria_predictions,
                             obs = linaria_pres_test_coords, 
                             nclass = 0, 
                             window.w = "default", 
                             res = 100, 
                             PEplot = TRUE)


BI$Spearman.cor  # 0.982 (CNN)

# MaxEnt
BI_mxnt$Spearman.cor  # 0.993 (MaxEnt)
BI$Spearman.cor       # 0.982 (CNN)



## Plotting both maps ####

linaria_predictions_dl <- raster("../dl_sdm_single/linaria_predictions_DL.tif")
linaria_predictions_maxent <- raster("../dl_sdm_single/linaria_predictions_maxent.tif")



pdf("linaria_predictions_DL_MaxEnt_CNN.pdf", width = 20, height = 20)
par(mfrow = c(2, 2))
par(mar = c(8, 4, 4, 5))
plot(linaria_predictions_maxent, col = "grey", main = "Linaria alpina", cex.main = 3.5, legend = FALSE)
plot(linaria_pres, add = TRUE, col = "black")
#plot(linaria_predictions_maxent, col = "white", legend = FALSE)
plot(linaria_predictions, zlim = c(0, 1), main = "DL (Convolutional NN)", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("CNN: Boyce Index = ", round(BI$Spearman.cor, 3), "; AUC = ", round(score[grepl("auc", names(score))], 3)))
plot(linaria_predictions_dl, zlim = c(0, 1), main = "DL (MultiLayer Perceptrons)", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("MLP: Boyce Index = ", round(BI_dl$Spearman.cor, 3), "; AUC = ", round(score_dl[grepl("auc", names(score_dl))], 3)))
plot(linaria_predictions_maxent, zlim = c(0, 1), main = "MaxEnt", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("MaxEnt: Boyce Index = ", round(BI_mxnt$Spearman.cor, 3), "; AUC = ", round(auc_mxnt, 3)))
dev.off()




score_cnn <- score
BI_cnn <- BI

lst2safe <- c("score_cnn", "BI_cnn")
save(list = lst2safe, file = "evaluations.RData")
#load("evaluations.RData", verbose = TRUE)


writeRaster(linaria_predictions, filename = "linaria_predictions_CNN.tif", overwrite = TRUE)



