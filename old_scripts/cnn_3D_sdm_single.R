###############################################
####  Convolutional Neural Networks (CNN)  ####
####               3D-CNN                  ####
###############################################
# https://d2l.ai/

# CNN: A particular case of NN
# 3D-CNN means here that the convolution kernel is convolved with the layer input 
# over a 2D spatial (or temporal) dimensions to produce a tensor of outputs.
# The 3rd dimension is the number of variables.
# In other words, the input layer has 2 dimensions to capture spatial patterns 
# which can affect species presence/distribution, in our case one pixel and its 
# neighbours, and a 3rd dimension with the values of the environmental variables.

library(raster)
library(sf)
library(keras)
library(dplyr)

getwd()
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_test/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_single/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/cnn_sdm_single/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/cnn_3D_sdm_single/"
setwd(wd)

bioscrop <- brick("../dl_sdm_test/bioscrop.tif")    # 10 km
bioscrop@crs <- sp::CRS("+init=EPSG:4326")
bioscrop
dev.off()
plot(bioscrop[[1]])


worldclim_path <- "/Users/xavi_rp/Documents/MinBA_models/wc5"
worldclim_path0.5 <- "/Users/xavi_rp/Documents/MinBA_models/wc0.5"

worldclim_files <- list.files(worldclim_path, full.names = TRUE)
worldclim_files <- worldclim_files[grepl("bil$", worldclim_files) | grepl("tif$", worldclim_files)]
worldclim_files

worldclim_all <- stack(worldclim_files)
plot(worldclim_all[[1]])

## elevation (aggregated from 30'' to 5')
#worldclim_files <- list.files(worldclim_path, full.names = TRUE)
#worldclim_files <- worldclim_files[grepl("tif$", worldclim_files)]
#elev5 <- raster(worldclim_files)
#worldclim_all <- stack(worldclim_all, elev5)
#worldclim_all


worldclim_all <- crop(worldclim_all, bioscrop)
worldclim_all
plot(worldclim_all[[1]])


## elevation (from Worldclim 30'')
#worldclim_files <- list.files(worldclim_path0.5, full.names = TRUE)
#worldclim_files <- worldclim_files[grepl("tif$", worldclim_files)]
#worldclim_files
#elev <- raster(worldclim_files)
#elev
#plot(elev)
#elev5 <- aggregate(elev, fact = 10, fun = mean, expand = FALSE, na.rm = TRUE)
#elev5 <- crop(elev5, worldclim_all)
#elev5
#writeRaster(elev5, filename = paste0(worldclim_path, "/elev_aggregated.tif"), overwrite = TRUE)



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
plot(linaria_pres_rstr, col = c("red", "black"), legend = FALSE)#, add = TRUE)

res(linaria_pres_rstr) == res(bioscrop)



## Extracting "tensors" from variables around the presences and absences
# Following Botella's approach (?), each point (presence and absence) can be associated to a 
# matrix of pixels surrounding the point (e.g. 3x3 or 5x5). This is to capture the influence of 
# the "micro-habitat" of the plant.
# I have to create an array where for each pixel (pres and abs) I have a tensor of, say, 3x3 pixels around it
# Then, dim of this array will be 'number of pixels', 3, 3, 'number of variables'

## for presences:
#Which(linaria_pres_rstr == 1, cells = TRUE)
matrix_dim <- 7
adj_cells <- matrix(1, nrow = matrix_dim, ncol = matrix_dim) # 49 pixels
adj_cells[ceiling(matrix_dim / 2), ceiling(matrix_dim / 2)] <- 0
View(adj_cells)

x <- adjacent(linaria_pres_rstr, 
              cells = Which(linaria_pres_rstr == 1, cells = TRUE),
              #directions = 8, # 8 for a matrix 3x3; 16 for a matrix 5x5
              directions = adj_cells, # 7x7
              pairs = TRUE, 
              #target = NULL, 
              sorted = TRUE, 
              include = TRUE, 
              id = FALSE)

head(as.data.frame(x), 20)
length(unique(as.data.frame(x)$from))

linaria_pres_neighb <- as.data.frame(x)
head(linaria_pres_neighb)

linaria_pres_tensor <- bioscrop[linaria_pres_neighb$to]
linaria_pres_tensor <- cbind(linaria_pres_neighb, linaria_pres_tensor)
head(linaria_pres_tensor, 20)
View(linaria_pres_tensor)


# Creating an array (number_presences x 3 x 3 x number_variables)
length(unique(linaria_pres_tensor$from))

linaria_pres_tensor_grouped <- linaria_pres_tensor %>% group_by(from) %>% group_split()
linaria_pres_tensor_grouped
length(linaria_pres_tensor_grouped)

linaria_pres_tensor_grouped[[700]]
linaria_pres_tensor_grouped[[1]]



linaria_pres_tensor_array <- aperm(array(unlist(linaria_pres_tensor_grouped[[1]][, 3:length(linaria_pres_tensor)]), 
                                         dim = c(matrix_dim, matrix_dim, (length(linaria_pres_tensor) - 2))),
                                   c(2, 1, 3))


for(i in 2:length(linaria_pres_tensor_grouped)){
  linaria_pres_tensor_array2 <- aperm(array(unlist(linaria_pres_tensor_grouped[[i]][, 3:length(linaria_pres_tensor)]), 
                                            dim = c(matrix_dim, matrix_dim, (length(linaria_pres_tensor) - 2))), 
                                      c(2, 1, 3))
  linaria_pres_tensor_array <- array(c(linaria_pres_tensor_array, linaria_pres_tensor_array2), 
                                     dim = c(matrix_dim, matrix_dim, dim(linaria_pres_tensor_array)[3], i))
}


linaria_pres_tensor_array <- aperm(linaria_pres_tensor_array, c(4, 1, 2, 3))
dim(linaria_pres_tensor_array)
linaria_pres_tensor_array[1, , ,1]
linaria_pres_tensor_array[734, , ,1]
linaria_pres_tensor_array2[,,1]



## for absences:
#Which(linaria_pres_rstr == 0, cells = TRUE)
#length(Which(linaria_pres_rstr == 0, cells = TRUE))
#ncell(linaria_pres_rstr)

# Selecting a subset of pseudo-absences for training
num_PeudoAbs <- 10000

pseudoAbs_train_cells <- sample(Which(linaria_pres_rstr == 0, cells = TRUE), num_PeudoAbs)
head(sort(pseudoAbs_train_cells))

y <- adjacent(linaria_pres_rstr, 
              cells = pseudoAbs_train_cells,
              directions = adj_cells, # 8 for a matrix 3x3; 16 for a matrix 5x5
              pairs = TRUE, 
              #target = NULL, 
              sorted = TRUE, 
              include = TRUE, 
              id = FALSE)

head(as.data.frame(y), 20)
length(unique(as.data.frame(y)$from))

linaria_abs_neighb <- as.data.frame(y)
head(linaria_abs_neighb, 30)

linaria_abs_tensor <- bioscrop[linaria_abs_neighb$to]
linaria_abs_tensor <- cbind(linaria_abs_neighb, linaria_abs_tensor)
head(linaria_abs_tensor, 20)


# Creating an array (number_absences x 3 x 3 x number_variables)
length(unique(linaria_abs_tensor$from))

linaria_abs_tensor_grouped <- linaria_abs_tensor %>% group_by(from) %>% group_split()
#linaria_abs_tensor_grouped
length(linaria_abs_tensor_grouped)

linaria_abs_tensor_grouped[[700]]
linaria_abs_tensor_grouped[[1]]



linaria_abs_tensor_array <- aperm(array(unlist(linaria_abs_tensor_grouped[[1]][, 3:length(linaria_abs_tensor)]), 
                                         dim = c(matrix_dim, matrix_dim, (length(linaria_abs_tensor) - 2))),
                                   c(2, 1, 3))

for(i in 2:length(linaria_abs_tensor_grouped)){
  linaria_abs_tensor_array2 <- aperm(array(unlist(linaria_abs_tensor_grouped[[i]][, 3:length(linaria_abs_tensor)]), 
                                            dim = c(matrix_dim, matrix_dim, (length(linaria_abs_tensor) - 2))), 
                                      c(2, 1, 3))
  linaria_abs_tensor_array <- array(c(linaria_abs_tensor_array, linaria_abs_tensor_array2), 
                                    dim = c(matrix_dim, matrix_dim, dim(linaria_abs_tensor_array)[3], i))
}


linaria_abs_tensor_array <- aperm(linaria_abs_tensor_array, c(4, 1, 2, 3))
dim(linaria_abs_tensor_array)
linaria_abs_tensor_array[1, , ,]
linaria_abs_tensor_array[1, , ,1]


# Selecting a subset of pseudo-absences for testing
pseudoAbs_test_cells <- sample(Which(linaria_pres_rstr == 0, cells = TRUE), num_PeudoAbs)
head(sort(pseudoAbs_test_cells))
head(sort(pseudoAbs_train_cells))
sum(pseudoAbs_test_cells %in% pseudoAbs_train_cells) # number of pseudo-abs which are selected both for train and test
                                                     # it should tend to 0

y1 <- adjacent(linaria_pres_rstr, 
              cells = pseudoAbs_test_cells,
              directions = adj_cells, # 8 for a matrix 3x3; 16 for a matrix 5x5
              pairs = TRUE, 
              #target = NULL, 
              sorted = TRUE, 
              include = TRUE, 
              id = FALSE)

head(as.data.frame(y1), 20)
length(unique(as.data.frame(y1)$from))

linaria_abs_neighb_test <- as.data.frame(y1)
head(linaria_abs_neighb_test, 30)

linaria_abs_tensor_test <- bioscrop[linaria_abs_neighb_test$to]
linaria_abs_tensor_test <- cbind(linaria_abs_neighb_test, linaria_abs_tensor_test)
head(linaria_abs_tensor_test, 20)


# Creating an array (number_presences x 3 x 3 x number_variables)
length(unique(linaria_abs_tensor_test$from))

linaria_abs_tensor_test_grouped <- linaria_abs_tensor_test %>% group_by(from) %>% group_split()
#linaria_abs_tensor_test_grouped
length(linaria_abs_tensor_test_grouped)

linaria_abs_tensor_test_grouped[[700]]
linaria_abs_tensor_test_grouped[[1]]



linaria_abs_tensor_array_test <- aperm(array(unlist(linaria_abs_tensor_test_grouped[[1]][, 3:length(linaria_abs_tensor_test)]), 
                                        dim = c(matrix_dim, matrix_dim, (length(linaria_abs_tensor_test) - 2))),
                                  c(2, 1, 3))

for(i in 2:length(linaria_abs_tensor_test_grouped)){
  linaria_abs_tensor_array_test2 <- aperm(array(unlist(linaria_abs_tensor_test_grouped[[i]][, 3:length(linaria_abs_tensor_test)]), 
                                           dim = c(matrix_dim, matrix_dim, (length(linaria_abs_tensor_test) - 2))), 
                                     c(2, 1, 3))
  linaria_abs_tensor_array_test <- array(c(linaria_abs_tensor_array_test, linaria_abs_tensor_array_test2), 
                                         dim = c(matrix_dim, matrix_dim, dim(linaria_abs_tensor_array_test)[3], i))
}


linaria_abs_tensor_array_test <- aperm(linaria_abs_tensor_array_test, c(4, 1, 2, 3))
dim(linaria_abs_tensor_array_test)
#linaria_abs_tensor_array_test[1, , ,]
linaria_abs_tensor_array_test[1, , ,1]

#


## Let's subset data set for training the model (we set aside 20% of presences for validation)
dim(linaria_pres_tensor_array)
dim(linaria_pres_tensor_array)[1]
n2sample <- round(dim(linaria_pres_tensor_array)[1] * 80 / 100)

pres_all <- c(1:dim(linaria_pres_tensor_array)[1])
pres_train <- sort(sample(pres_all, n2sample))
pres_test <- pres_all[!c(1:dim(linaria_pres_tensor_array)[1]) %in% pres_train]

linaria_train_array_x <- linaria_pres_tensor_array[pres_train, , , , drop = FALSE]
dim(linaria_train_array_x)

linaria_test_array_x <- linaria_pres_tensor_array[pres_test, , , , drop = FALSE]
dim(linaria_test_array_x)


# labels
linaria_train_array_y <- matrix(1, nrow = dim(linaria_train_array_x)[1], ncol = 1)
dim(linaria_train_array_y)
head(linaria_train_array_y)

linaria_abs_tensor_array_y <- matrix(0, nrow = dim(linaria_abs_tensor_array)[1], ncol = 1)
dim(linaria_abs_tensor_array_y)
head(linaria_abs_tensor_array_y)

linaria_test_array_y <- matrix(1, nrow = dim(linaria_test_array_x)[1], ncol = 1)
dim(linaria_test_array_y)
head(linaria_test_array_y)

linaria_abs_tensor_array_test_y <- matrix(0, nrow = dim(linaria_abs_tensor_array_test)[1], ncol = 1)
dim(linaria_abs_tensor_array_test_y)
head(linaria_abs_tensor_array_test_y)


# merging presences and absences
library(abind)
dim(linaria_train_array_x)
dim(linaria_abs_tensor_array)
linaria_train_array_all_x <- abind(linaria_train_array_x, linaria_abs_tensor_array, along = 1)
dim(linaria_train_array_all_x)

linaria_train_array_y
linaria_abs_tensor_array_y
linaria_train_array_all_y <- abind(linaria_train_array_y, linaria_abs_tensor_array_y, along = 1)
dim(linaria_train_array_all_y)


dim(linaria_test_array_x)
dim(linaria_abs_tensor_array_test)
linaria_test_array_all_x <- abind(linaria_test_array_x, linaria_abs_tensor_array_test, along = 1)
dim(linaria_test_array_all_x)

dim(linaria_test_array_y)
dim(linaria_abs_tensor_array_test_y)
linaria_test_array_all_y <- abind(linaria_test_array_y, linaria_abs_tensor_array_test_y, along = 1)
dim(linaria_test_array_all_y)


## Building the model
inpt_shpe <- dim(linaria_pres_tensor_array)[2:4]
rm(model_c3d); gc()

model_c3d <- keras_model_sequential()

model_c3d %>% 
  #layer_conv_2d(filters = 32, kernel_size = c(3, 3), data_format = "channels_last", padding = "valid", input_shape = inpt_shpe, activation = "relu") %>%
  #layer_average_pooling_2d(pool_size = c(2, 2)) %>%
  #layer_conv_2d(filters = 64, kernel_size = c(5, 5), data_format = "channels_last", padding = "valid", activation = "relu") %>%
  #layer_average_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), data_format = "channels_last", padding = "valid", input_shape = inpt_shpe, activation = "relu") %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), data_format = "channels_last", padding = "valid", activation = "relu") %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), data_format = "channels_last", padding = "valid", activation = "relu") %>%
  #
  #layer_dropout(0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")



# Why to use Pooling Layers?
# Pooling layers are used to reduce the dimensions of the feature maps. Thus, it reduces the 
# number of parameters to learn and the amount of computation performed in the network. The 
# pooling layer summarises the features present in a region of the feature map generated by 
# a convolution layer. So, further operations are performed on summarised features instead 
# of precisely positioned features generated by the convolution layer. This makes the model 
# more robust to variations in the position of the features in the input image. 

# padding = "valid" means no padding (seems to give slightly better results than "same" and "causal")

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


model_c3d %>% 
  compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_sgd(lr = 1e-5, momentum = 0.9),
    metrics = c(tensorflow::tf$keras$metrics$AUC(), metric_binary_accuracy) # AUC calculation doesn't work if there are NAs in the tensors
  )

# Removing NAs from tensors (otherwise, AUC gives error and the model can't be fitted)
# Not only AUC, but the model itself is not fitted, so nothing works afterwards (i.e. predictions...) if there are NAs in the tensors
linaria_train_array_all_x[is.na(linaria_train_array_all_x)] <- 0 
sum(is.na(linaria_train_array_all_x))

model_c3d %>% fit(linaria_train_array_all_x, linaria_train_array_all_y,
                batch_size = 128, 
                epochs = 100, 
                verbose = 2)


score_train <- model_c3d %>% evaluate(linaria_train_array_all_x, linaria_train_array_all_y, batch_size = 128)
score_train

linaria_test_array_all_x[is.na(linaria_test_array_all_x)] <- 0 
sum(is.na(linaria_test_array_all_x))
score <- model_c3d %>% evaluate(linaria_test_array_all_x, linaria_test_array_all_y, batch_size = 128)
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

#bioscrop_data <- as.data.frame(bioscrop)
#bioscrop_data <- bioscrop_data[complete.cases(bioscrop_data), ]
#head(bioscrop_data)
#nrow(bioscrop_data)

#dim(linaria_train_array_x)
#dim(as.matrix(bioscrop_data))

bioscrop

x1 <- adjacent(bioscrop, 
               cells = Which(!is.na(linaria_pres_rstr), cells = TRUE),
               directions = adj_cells, # 8 for a matrix 3x3; 16 for a matrix 5x5
               pairs = TRUE, 
               #target = NULL, 
               sorted = TRUE, 
               include = TRUE, 
               id = FALSE)

head(as.data.frame(x1), 20)
length(unique(as.data.frame(x1)$from))

bioscrop_neighb <- as.data.frame(x1)
head(bioscrop_neighb)

bioscrop_tensor <- bioscrop[bioscrop_neighb$to]
bioscrop_tensor <- cbind(bioscrop_neighb, bioscrop_tensor)

head(bioscrop_tensor, 20)
View(bioscrop_tensor)


# Creating an array (number_presences x 3 x 3 x number_variables)
length(unique(bioscrop_tensor$from))

bioscrop_tensor_grouped <- bioscrop_tensor %>% group_by(from) %>% group_split()
#bioscrop_tensor_grouped
length(bioscrop_tensor_grouped)

bioscrop_tensor_grouped[[700]]
bioscrop_tensor_grouped[[1]]

bioscrop_tensor_array <- aperm(array(unlist(bioscrop_tensor_grouped[[1]][, 3:length(bioscrop_tensor)]), 
                                     dim = c(matrix_dim, matrix_dim, (length(bioscrop_tensor) - 2))),
                               c(2, 1, 3))

for(i in 2:length(bioscrop_tensor_grouped)){
  cat("\r", paste0(i, "/", length(bioscrop_tensor_grouped)))
  bioscrop_tensor_array2 <- aperm(array(unlist(bioscrop_tensor_grouped[[i]][, 3:length(bioscrop_tensor)]), 
                                        dim = c(matrix_dim, matrix_dim, (length(bioscrop_tensor) - 2))), 
                                  c(2, 1, 3))
  bioscrop_tensor_array <- array(c(bioscrop_tensor_array, bioscrop_tensor_array2), 
                                 dim = c(matrix_dim, matrix_dim, dim(bioscrop_tensor_array)[3], i))
}

bioscrop_tensor_array <- aperm(bioscrop_tensor_array, c(4, 1, 2, 3))
dim(bioscrop_tensor_array)
bioscrop_tensor_array[1, , ,1]
bioscrop_tensor_array[734, , ,1]
bioscrop_tensor_array2[,,1]
saveRDS(bioscrop_tensor_array, file = "bioscrop_tensor_array.rds")
bioscrop_tensor_array <- readRDS("bioscrop_tensor_array.rds")

#bioscrop_tensor_array[is.na(bioscrop_tensor_array)] <- 0 
#sum(is.na(bioscrop_tensor_array))
dim(bioscrop_tensor_array)


predictions <- model_c3d %>% predict_proba(bioscrop_tensor_array, batch_size = 128)
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
nrow(bioscrop_data)
str(bioscrop_data)

bioscrop_data_1 <- bioscrop_data[complete.cases(bioscrop_data), ]
bioscrop_data_1 <- bioscrop_data_1[, length(bioscrop_data_1), drop = FALSE]
head(bioscrop_data_1)
nrow(bioscrop_data_1)

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
# 0.05661052      0.95970947      0.98068392


# MaxEnt
load("../dl_sdm_single/evaluations.RData", verbose = TRUE)
score_dl
auc_mxnt
BI_dl$Spearman.cor
BI_mxnt$Spearman.cor
BI$Spearman.cor

# CNN
load("../cnn_sdm_single/evaluations.RData", verbose = TRUE)
score_cnn[grepl("auc", names(score_cnn))]   # 0.9652347 (CNN)
BI_cnn$Spearman.cor  # 0.982 (CNN)


auc_mxnt                            # 0.9848272 (MaxEnt)
score[grepl("auc", names(score))]   # 0.9597095  (3D-CNN)



## Boyce Index ##

# CNN

# raster position of presences for testing
Which(linaria_pres_rstr == 1, cells = TRUE)
pres_test
pres_test_boolean <- 1:sum(getValues(linaria_pres_rstr) == 1, na.rm = TRUE)
pres_test_boolean <- pres_test_boolean %in% pres_test
pres_test_raster_position <- Which(linaria_pres_rstr == 1, cells = TRUE)[pres_test_boolean] 


linaria_pres_test_coords <- xyFromCell(object = linaria_predictions, 
                                       cell = pres_test_raster_position, 
                                       spatial = FALSE)

BI <- ecospat::ecospat.boyce(fit = linaria_predictions,
                             obs = linaria_pres_test_coords, 
                             nclass = 0, 
                             window.w = "default", 
                             res = 100, 
                             PEplot = TRUE)


BI$Spearman.cor  # 0.979 (3D-CNN)

# MaxEnt
BI_mxnt$Spearman.cor  # 0.993 (MaxEnt)
BI$Spearman.cor       # 0.979 (3D-CNN)




## Plotting both maps ####

linaria_predictions_cnn <- raster("../cnn_sdm_single/linaria_predictions_CNN.tif")
linaria_predictions_dl <- raster("../dl_sdm_single/linaria_predictions_DL.tif")
linaria_predictions_maxent <- raster("../dl_sdm_single/linaria_predictions_maxent.tif")



pdf("linaria_predictions_DL_MaxEnt_CNN_3DCNN.pdf", width = 25, height = 20)
par(mfrow = c(2, 3))
par(mar = c(8, 4, 4, 5))
plot(linaria_predictions_maxent, col = "grey", main = "Linaria alpina", cex.main = 3.5, legend = FALSE)
plot(linaria_pres, add = TRUE, col = "black")
#plot(linaria_predictions_maxent, col = "white", legend = FALSE)
plot(linaria_predictions_maxent, zlim = c(0, 1), main = "MaxEnt", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("MaxEnt: Boyce Index = ", round(BI_mxnt$Spearman.cor, 3), "; AUC = ", round(auc_mxnt, 3)))
plot(linaria_predictions_dl, zlim = c(0, 1), main = "DL (MultiLayer Perceptrons)", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("MLP: Boyce Index = ", round(BI_dl$Spearman.cor, 3), "; AUC = ", round(score_dl[grepl("auc", names(score_dl))], 3)))
plot(linaria_predictions_cnn, zlim = c(0, 1), main = "DL (Convolutional NN)", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("CNN: Boyce Index = ", round(BI_cnn$Spearman.cor, 3), "; AUC = ", round(score_cnn[grepl("auc", names(score_cnn))], 3)))
plot(linaria_predictions, zlim = c(0, 1), main = "DL (3D-CNN)", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("3D-CNN: Boyce Index = ", round(BI$Spearman.cor, 3), "; AUC = ", round(score[grepl("auc", names(score))], 3)))
dev.off()




score_3Dcnn <- score
BI_3Dcnn <- BI

lst2safe <- c("score_3Dcnn", "BI_3Dcnn")
save(list = lst2safe, file = "evaluations.RData")
#load("evaluations.RData", verbose = TRUE)


writeRaster(linaria_predictions, filename = "linaria_predictions_3D-CNN.tif", overwrite = TRUE)



