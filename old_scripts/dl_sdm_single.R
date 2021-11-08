########################################
####  Multilayer Perceptrons (MLP)  ####
####    Fully-Connected Deep NN     ####
########################################
# https://d2l.ai/

# Multilayer Perceptrons: The simplest deep networks, they consist of multiple layers of neurons each 
# fully connected to those in the layer below (from which receive input) and those above (which they influence).
# 

library(raster)
library(sf)
library(keras)
library(dplyr)

getwd()
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_test/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_single/"
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
linaria_abs_train <- sample_n(linaria_data_NoPresences, n2sample)
head(linaria_abs_train)

linaria_abs_test <- sample_n(linaria_data_NoPresences, nrow(linaria_pres_test))
sum(linaria_abs_train$raster_position %in% linaria_abs_test$raster_position)  # has to be 0 for not repeating absences


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
nrow(linaria_train)

linaria_test <- rbind(linaria_pres_test[, 1:(length(linaria_pres_test) - 1)], linaria_abs_test[, 1:(length(linaria_abs_test) - 1)])
nrow(linaria_test)
head(linaria_test)
#



# build a model
nrow(linaria_train) # 1174 (balanced) or 10587 (unbalanced)
head(linaria_train)
rm(model); gc()

model <- keras_model_sequential()
inpt_shpe <- length(linaria_train) - 1


model %>%
  #layer_flatten(input_shape = c(1174, 3)) %>%            # transforms the format of the images from a 2d-array (of 28 by 28 pixels), to a 1d-array of 28 * 28 = 784 pixels. This layer has no parameters to learn; it only reformats the data.
  # this is probably needed if tensors are used instead of punctual environmental vector
  layer_dense(units = 128, activation = 'relu', input_shape = c(inpt_shpe)) %>%     #
  #layer_dropout(rate = 0.1) %>%   
  layer_dense(units = 128, activation = 'relu') %>%     #
  layer_dense(units = 128, activation = 'relu') %>%     #
  layer_dense(units = 128, activation = 'relu') %>%     #
  layer_dense(units = 128, activation = 'relu') %>%     #
  layer_dense(units = 128, activation = 'relu') %>%     #
  #layer_dense(units = 10, activation = 'softmax')
  layer_dense(units = 1, activation = 'sigmoid')

model
# The batch size defines the number of samples that will be propagated through the network
# e.g. we have 1050 training samples and you want to set up a batch_size equal to 100. The algorithm takes the first 100 
# samples (from 1st to 100th) from the training dataset and trains the network. Next, it takes the second 100 samples 
# (from 101st to 200th) and trains the network again.
# Advantage: mmore emory efficient and faster 
# Disadvantage: the smaller the batch the less accurate the estimate of the gradient will be

# Neural network terminology:
# one epoch = one forward pass and one backward pass of all the training examples
# batch size = the number of training examples in one forward/backward pass. The higher the batch size, the more memory space you'll need.
# number of iterations = number of passes, each pass using [batch size] number of examples. To be clear, one pass = one forward pass + one backward pass (we do not count the forward pass and backward pass as two different passes).
# Example: if you have 1000 training examples, and your batch size is 500, then it will take 2 iterations to complete 1 epoch.

# If you pass both batch_size=32 and input_shape=c(6, 8) to a layer, it will then expect every 
# batch of inputs to have the batch shape (32, 6, 8)

# Capinha et al (2021) generate 20 random models and chose the one with the best architecture by training them with a small balanced
# data set. To determine the best, the models are validated with another data set.
# Then, when model with best architecture is chosen, they train it with a bigger training data set.
# To determine the best number of epochs, they check how the performance (AUC over testing data set) of the final model trained 
# with the full training data set increases with the number of epochs, until the increase stabilizes during 25 epochs.


# Activation function (aka Transfer Function; https://towardsdatascience.com/activation-functions-neural-networks-1cbd9f8d91d6):
# used to determine the output of neural network like yes or no. It maps the resulting values in 
# between 0 to 1 or -1 to 1 etc. (depending upon the function).
# 1) Linear or Identity Activation Function: the output of the functions will not be confined between 
#    any range.
# 2) Nonlinear Activation Functions are the most used activation functions.
#    a) Sigmoid Function curve looks like a S-shape: especially used for models where we have to predict 
#       the probability as an output. Since probability of anything exists only between the range of 0 and 1, sigmoid is the right choice.
#    b) The softmax function is a more generalized logistic activation function which is used for multiclass classification.
#       maps our output to a [0,1] range but also maps each output in such a way that the total sum is 1. 
#       The output of Softmax is therefore a probability distribution.
#    a-b) In conclusion, Softmax is used for multi-classification in logistic regression model (multivariate) 
#         whereas Sigmoid is used for binary classification in logistic regression model.
#    c) ReLU is the most used activation function right now (used in almost all the convolutional neural networks or deep learning)
#       Range: [ 0 to infinity)
#       But the issue is that all the negative values become zero immediately which decreases the ability of the 
#       model to fit or train from the data properly. That means any negative input given to the ReLU activation 
#       function turns the value into zero immediately in the graph, which in turns affects the resulting graph 
#       by not mapping the negative values appropriately.


# Dropout involves injecting noise while computing each internal layer during forward propagation, 
# and it has become a standard technique for training neural networks.
# Typically, we disable dropout at test time. Given a trained model and a new example, we do not drop out
# any nodes and thus do not need to normalize. However, there are some exceptions: some researchers use 
# dropout at test time as a heuristic for estimating the uncertainty of neural network predictions: 
# if the predictions agree across many different dropout masks, then we might say that the network is 
# more confident.



# Compile the model
# Optimizers are algorithms or methods used to change the attributes of your neural network such as weights 
# and learning rate in order to reduce the losses and to provide the most accurate results possible.
# 1) Gradient Descent is the most basic but most used optimization algorithm. It’s used heavily in linear regression and classification algorithms. Backpropagation in neural networks also uses a gradient descent algorithm.
#    It requires a lot of memory and long time to converge for big data sets. 
# 2) Stochastic Gradient Descent. e.g. optimizer = optimizer_rmsprop(lr = 0.002), where 'lr' is initial learning rate. 
#    A too big lr may lead to training loss divergence; too small, learning can be very slow. Botella, 2018, used lr = 1e-8.
#    It converges in less time and requires less memory.
# 3) Mini-Batch Gradient Descent. Difficult to choose an optimum value of the learning rate. If the learning rate is too small than gradient descent may take ages to converge.   
# 4) Momentum was invented for reducing high variance in SGD and softens the convergence. It accelerates the convergence towards the relevant direction and reduces the fluctuation to the irrelevant direction.
# 5) Adam. Fast and converges rapidly, but computationally costly.



model %>% compile(
  #optimizer = 'adam',    # This is how the model is updated based on the data it sees and its loss function.
  #optimizer = "rmsprop",
  #optimizer = optimizer_rmsprop(lr = 1e-8),  # for a binary classification problem
  #optimizer = optimizer_rmsprop(),  # for a binary classification problem
  optimizer = optimizer_sgd(#lr = 1e-8, 
                            #lr = 1e-6,
                            #lr = 1e-4,
                            lr = 1e-5,
                            momentum = 0.9),
  #loss = 'sparse_categorical_crossentropy',   # This measures how accurate the model is during training. We want to minimize this function to “steer” the model in the right direction.
  loss = "binary_crossentropy",  # for a binary classification proble
  #metrics = c('accuracy')   # Used to monitor the training and testing steps. e.g. "accuracy", the fraction of the images that are correctly classified.
  #metrics = metric_binary_accuracy  # for a binary classification problems
  metrics = c(tensorflow::tf$keras$metrics$AUC(), metric_binary_accuracy)   # AUC from tensorflow + binary accuracy
)
model


## Training the model
linaria_train_data <- as.matrix(linaria_train[, -1])
head(linaria_train_data)
dim(linaria_train_data)
nrow(linaria_train_data)


linaria_train_labels <- as.matrix(linaria_train[, 1, drop = FALSE])
head(linaria_train_labels)
unique(linaria_train_labels)
nrow(linaria_train_labels)


t0 <- Sys.time()
model %>% fit(linaria_train_data,
              linaria_train_labels, 
              epochs = 200,  # The number of "epochs" is a hyperparameter that defines the number times that the learning algorithm will work through the ENTIRE training dataset.
              # The "batch" size is a hyperparameter that defines the NUMBER OF SAMPLES to work through before updating the internal model parameters.
              batch_size = 128,
              verbose = 2
              )
Sys.time() - t0



## Test the model

head(linaria_test)
rm(score)
score_dl <- model %>% evaluate(as.matrix(linaria_test[, -1]), as.matrix(linaria_test[, 1, drop = FALSE]), 
                               batch_size = 128
                               )
lst2safe <- c("score_dl")
save(list = lst2safe, file = "evaluations.RData")
#load("evaluations.RData", verbose = TRUE)
score_dl #for test data
#       loss           auc    binary_accuracy 
# 0.03891767      0.97014868      0.98521733


# for training data (last epoch)
# loss: 0.0958 - auc: 0.9564 - binary_accuracy: 0.9651
# The gap between training accuracy and test accuracy is an example of "overfitting" 
# train_auc (0.9564) < test_auc (0.97014868)  --> No overfitting
# train_accuracy (0.9651) < test_accuracy (0.98521733)  --> No overfitting
# Overfitting is when a machine learning model performs worse on new data than on their training data



## Make predictions over the whole extent
bioscrop_data <- as.data.frame(bioscrop)
bioscrop_data <- bioscrop_data[complete.cases(bioscrop_data), ]
head(bioscrop_data)
nrow(bioscrop_data)


predictions <- model %>% predict_proba(as.matrix(bioscrop_data), batch_size = 128)
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




## MaxEnt ####
library(maxnet)
?maxnet

head(linaria_train_data)
head(as.data.frame((linaria_train_data)))
head(linaria_train_labels)
head(as.vector(linaria_train_labels))

model_maxent <- maxnet(as.vector(linaria_train_labels), 
                       as.data.frame(linaria_train_data),
                       f = maxnet.formula(p = as.vector(linaria_train_labels),
                                          data = as.data.frame(linaria_train_data),
                                          classes = "default")
                       )

pdf("linaria_maxent_curves.pdf")
plot(model_maxent)
dev.off()

bioscrop_data_mxnt <- as.data.frame(bioscrop)
bioscrop_data_mxnt <- bioscrop_data_mxnt[complete.cases(bioscrop_data_mxnt), ]
head(bioscrop_data_mxnt)
nrow(bioscrop_data_mxnt)


linaria_predictions_maxent <- predict(object = model_maxent, 
                                      newdata = bioscrop_data_mxnt, 
                                      clamp = T,
                                      type = c("logistic")
                                      )

head(linaria_predictions_maxent)


bioscrop_data_mxnt <- as.data.frame(bioscrop)
bioscrop_data_mxnt$raster_position <- 1:nrow(bioscrop_data_mxnt)
head(bioscrop_data_mxnt)

bioscrop_data_mxnt_1 <- bioscrop_data_mxnt[complete.cases(bioscrop_data_mxnt), ]
bioscrop_data_mxnt_1 <- bioscrop_data_mxnt_1[, length(bioscrop_data_mxnt_1), drop = FALSE]
head(bioscrop_data_mxnt_1)

bioscrop_data_mxnt_1$linaria_predictions_maxent <- linaria_predictions_maxent
head(bioscrop_data_mxnt_1)

bioscrop_data_mxnt <- bioscrop_data_mxnt[, length(bioscrop_data_mxnt), drop = FALSE]
bioscrop_data_mxnt <- merge(bioscrop_data_mxnt, bioscrop_data_mxnt_1, by = "raster_position", all.x = TRUE)
head(bioscrop_data_mxnt)

linaria_predictions_maxent <- bioscrop[[1]]
linaria_predictions_maxent <- setValues(linaria_predictions_maxent, bioscrop_data_mxnt$linaria_predictions_maxent)
names(linaria_predictions_maxent) <- "linaria_predictions_maxent"
linaria_predictions_maxent

pdf("linaria_predictions_maxent.pdf", width = 20, height = 15)
par(mfrow = c(1, 2))
plot(linaria_predictions_maxent, zlim = c(0, 1))
plot(linaria_pres, add = TRUE, col = "black")
plot(linaria_predictions_maxent, zlim = c(0, 1))
dev.off()




## Evaluations ####

## AUC ##

# DL
score_dl #for test data
#       loss             auc    binary_accuracy 
# 0.03891767      0.97014868      0.98521733


# MaxEnt
# Although dismo::evaluate is for presence/absence data

linaria_pres_test_coords <- xyFromCell(object = linaria_predictions, 
                                       cell = linaria_pres_test$raster_position, 
                                       spatial = FALSE)
head(linaria_pres_test_coords)
nrow(linaria_pres_test_coords)

linaria_abs_test_coords <- xyFromCell(object = linaria_predictions, 
                                       cell = linaria_abs_test$raster_position, 
                                       spatial = FALSE)
head(linaria_abs_test_coords)
nrow(linaria_abs_test_coords)


score_mxnt <- dismo::evaluate(model = model_maxent, 
                              p = linaria_pres_test_coords, 
                              a = linaria_abs_test_coords, 
                              x = bioscrop)

score_mxnt@auc                      # 0.9848272 (MaxEnt)
score_dl[grepl("auc", names(score_dl))]   # 0.9701487  (DL)



## Boyce Index ##

# DL
BI_dl <- ecospat::ecospat.boyce(fit = linaria_predictions,
                                obs = linaria_pres_test_coords, 
                                nclass = 0, 
                                window.w = "default", 
                                res = 100, 
                                PEplot = TRUE)


BI_dl$Spearman.cor  # 0.877 (DL)

# MaxEnt
BI_mxnt <- ecospat::ecospat.boyce(fit = linaria_predictions_maxent,
                                  obs = linaria_pres_test_coords, 
                                  nclass = 0, 
                                  window.w = "default", 
                                  res = 100, 
                                  PEplot = TRUE)


BI_mxnt$Spearman.cor  # 0.998 (MaxEnt)
BI_dl$Spearman.cor       # 0.877 (DL)

lst2safe <- c(lst2safe, "score_mxnt", "BI_dl", "BI_mxnt")
save(list = lst2safe, file = "../dl_sdm_single/evaluations.RData")
#load("evaluations.RData", verbose = TRUE)

## Plotting both maps ####


pdf("linaria_predictions_DL_MaxEnt.pdf", width = 20, height = 20)
par(mfrow = c(2, 2))
par(mar = c(8, 4, 4, 5))
plot(linaria_predictions_maxent, col = "grey", main = "Linaria alpina", cex.main = 3.5, legend = FALSE)
plot(linaria_pres, add = TRUE, col = "black")
plot(linaria_predictions_maxent, col = "white", legend = FALSE)
plot(linaria_predictions, zlim = c(0, 1), main = "Deep Learning", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("Deep Learning: Boyce Index = ", round(BI$Spearman.cor, 3), "; AUC = ", round(score_dl[grepl("auc", names(score_dl))], 3)))
plot(linaria_predictions_maxent, zlim = c(0, 1), main = "MaxEnt", cex.main = 3.5, cex.sub = 2.5,
     sub = paste0("MaxEnt: Boyce Index = ", round(BI_mxnt$Spearman.cor, 3), "; AUC = ", round(score_mxnt@auc, 3)))
dev.off()



writeRaster(linaria_predictions, filename = "linaria_predictions_DL.tif", overwrite = TRUE)
writeRaster(linaria_predictions_maxent, filename = "linaria_predictions_maxent.tif", overwrite = TRUE)




