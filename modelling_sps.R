###############################################
####      Modelling distributions of       ####
####                species                ####
###############################################




if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local") {
  wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/DL-SDM_benchmarking_data"
  gbif_creds <- "/Users/xavi_rp/Dropbox/GBIF_credentials/"
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03", "jeodpp-terminal-dev-12")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/DL-SDM_benchmarking_data/")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/DL-SDM_benchmarking_data/")
  if(!dir.exists("/scratch/rotllxa/DL-SDM_benchmarking_data/")) 
    dir.create("/scratch/rotllxa/DL-SDM_benchmarking_data/")
  if(!dir.exists("/scratch/rotllxa/DL-SDM_benchmarking_data_dev/")) 
    dir.create("/scratch/rotllxa/DL-SDM_benchmarking_data_dev/")
  dir2save <- "/eos/jeodpp/home/users/rotllxa/DL-SDM_benchmarking_data/"
  if(Sys.info()[4] == "jeodpp-terminal-jd001-03"){
    wd <- "/scratch/rotllxa/DL-SDM_benchmarking_data/"
  }else{
    wd <- "/scratch/rotllxa/DL-SDM_benchmarking_data_dev/"
  }
  gbif_creds <- "/home/rotllxa/Documents/"
}else{
  wd <- ""
  gbif_creds <- ""
}

setwd(wd)

library(tidyr)
library(ENMeval)
library(raster)
library(dplyr)
library(dismo)
library(data.table)
library(virtualspecies)
library(terra)
library(sf)
#
library(keras)
# https://tensorflow.rstudio.com/reference/keras/install_keras/
# https://annie-wangliu.medium.com/how-to-run-keras-and-tensorflow-rstudio-46645b0d87d7
##install_keras(tensorflow = "cpu", method = "conda", envname = "CONDA_R_ENVIRONMENT_NAME")
#install_keras(tensorflow = "cpu", method = "conda", envname = "conda_Renv_2", version = "2.7.0")
install_keras(tensorflow = "cpu", method = "conda", envname = "conda_R_env", version = "2.7.0")
#Sys.setenv(RETICULATE_PYTHON = "/home/rotllxa/.conda/envs/CONDA_R_ENVIRONMENT_NAME/bin/python")
library(reticulate)
##use_condaenv('CONDA_R_ENVIRONMENT_NAME')
#use_condaenv('conda_Renv_2')
use_condaenv('conda_R_env')
library(tensorflow)
library("keras")
keras_model_sequential()



## Predictors ####

preds_dir <- "/eos/jeodpp/home/users/rotllxa/European_butterflies_SDMs_data/"

#worldclim_path0.5 <- "/Users/xavi_rp/Documents/MinBA_models/wc0.5" #  1km

#worldclim_files <- list.files(worldclim_path0.5, full.names = TRUE)
#worldclim_files <- worldclim_files[grepl("bil$", worldclim_files) | grepl("tif$", worldclim_files)]
#worldclim_files

#worldclim_all <- stack(worldclim_files)

#eur_coords <- c(-13, 48, 35, 72)
#worldclim_all <- crop(worldclim_all, eur_coords)
#writeRaster(worldclim_all, filename = "worldclim_all.tif")

worldclim_all <- stack(paste0(preds_dir, "worldclim_all.tif"))
worldclim_all
#plot(worldclim_all[[3]])
worldclim_all_names <- c("wc2.1_30s_bio_1", "wc2.1_30s_bio_10", "wc2.1_30s_bio_11", "wc2.1_30s_bio_12", "wc2.1_30s_bio_13", "wc2.1_30s_bio_14",
                         "wc2.1_30s_bio_15", "wc2.1_30s_bio_16", "wc2.1_30s_bio_17", "wc2.1_30s_bio_18", "wc2.1_30s_bio_19", "wc2.1_30s_bio_2",
                         "wc2.1_30s_bio_3", "wc2.1_30s_bio_4", "wc2.1_30s_bio_5", "wc2.1_30s_bio_6", "wc2.1_30s_bio_7", "wc2.1_30s_bio_8",
                         "wc2.1_30s_bio_9",  "wc2.1_30s_elev") 
worldclim_all_names <- gsub("wc2.1_30s_", "", worldclim_all_names)
names(worldclim_all) <- worldclim_all_names
#worldclim_all <- stack(worldclim_all)
worldclim_all



lc_dir <- "/eos/jeodpp/home/users/rotllxa/land_cover/"
lc1km_files <- list.files(lc_dir, full.names = TRUE)[grepl("lc1km_", list.files(lc_dir))]
lc1km_files
lc1km_all <- stack(lc1km_files)
lc1km_all


worldclim_all <- stack(worldclim_all, lc1km_all)
names(worldclim_all)
worldclim_all
#plot(worldclim_all[[5]])


worldclim_all_data <- fread(paste0(preds_dir, "worldclim_all_data.csv"), header = TRUE)
names(worldclim_all_data) <- names(worldclim_all)
worldclim_all_data


# removing collinearity
#vables_NoC <- removeCollinearity(worldclim_all,
#                                 multicollinearity.cutoff = 0.70,
#                                 #multicollinearity.cutoff = 0.85,
#                                 #multicollinearity.cutoff = 0.40,
#                                 select.variables = TRUE,  # if TRUE, randomly select one variable of the group. If FALSE, returns a list with the groups
#                                 sample.points = TRUE,
#                                 nb.points = 10^6,
#                                 plot = TRUE)
#vables_NoC
#save(vables_NoC, file = "vables_NoC_070.RData")
#dev.copy(png, "multicollinearity_070.png")
#save(vables_NoC, file = "vables_NoC_040.RData")
##dev.copy(png, "multicollinearity_040.png")
#dev.off()
#
##load("vables_NoC_040.RData", verbose = TRUE); vables_NoC
#worldclim_all <- subset(worldclim_all, vables_NoC)
#worldclim_all <- rast(worldclim_all)
#terra::writeRaster(worldclim_all, filename = "worldclim_all_NoCor_070.tif", 
#                   names = vables_NoC,
#                   overwrite = TRUE)
#
#worldclim_all <- brick(paste0(preds_dir, "worldclim_all_NoCor_070.tif"))
##worldclim_all <- brick("worldclim_all_NoCor_040.tif")
#worldclim_all
##load("vables_NoC.RData", verbose = TRUE) ; vables_NoC
#load("vables_NoC_070.RData", verbose = TRUE) ; vables_NoC
##load("vables_NoC_040.RData", verbose = TRUE) ; vables_NoC
##names(worldclim_all) <- vables_NoC
#
#
#names(worldclim_all)
#worldclim_all_data <- as.data.frame(worldclim_all)
#worldclim_all_data <- worldclim_all_data[complete.cases(worldclim_all_data), ]
#head(worldclim_all_data)
#nrow(worldclim_all_data)
#
##write.csv(worldclim_all_data, "worldclim_all_data_NoCor.csv", row.names = FALSE)
#write.csv(worldclim_all_data, "worldclim_all_data_NoCor_070.csv", row.names = FALSE)
##write.csv(worldclim_all_data, "worldclim_all_data_NoCor_040.csv", row.names = FALSE)
##write.csv(worldclim_all_data, "worldclim_all_data.csv", row.names = FALSE)
#names(worldclim_all_data)



## Background points ####

bckgr <- randomPoints(worldclim_all[[1]], 
                      #n = 10000)
                      n = 15000)
bckgr <- as.data.frame(bckgr)
head(bckgr)
nrow(bckgr)

write.csv(bckgr, "background_points.csv", row.names = FALSE)
bckgr <- read.csv(paste0(preds_dir, "background_points.csv"), header = TRUE)

# checking coverage
pdf("background_15k.pdf")
plot(worldclim_all[[1]])
points(bckgr, pch = 20, cex = 0.2)
dev.off()



## Virtual Occurrences ####
virt_sp_dir <- "/eos/jeodpp/home/users/rotllxa/DL-SDM_benchmarking_data/virtualspecies/"
occs_all <- fread(paste0(virt_sp_dir, "virtSp_occurrences.csv"), header = TRUE)
occs_all[, sp2 := species]
setnames(occs_all, c("lon", "lat"), c("decimalLongitude", "decimalLatitude"))
head(occs_all)
sort(table(occs_all$species))
tbl <- table(occs_all$species)
ordr <- names(sort(tbl))
spcies <- data.frame(taxons = unique(occs_all$sp2), sps = unique(occs_all$species))
spcies <- spcies[match(ordr, spcies$sps),]
spcies
#spcies <- spcies[-c(1:6), ]
taxons <- spcies$taxons
taxons


## Modelling with MaxEnt ####

info_models_maxent <- c()
info_models_mlp <- c()     # for MultiLayer Perceptrons
info_models_cnn <- c()     # for Convolutional NN


# Threshold to use for converting to presence/absence
# Options: kappa,  spec_sens, no_omission, prevalence, equal_sens_spec, sensitivity

threshold2use <- "sensitivity"    # deffault 0.9
#threshold2use <- "no_omission"    # keeping all presences
Sys.time()

for (t in taxons){
  #print(t)
  t0 <- Sys.time()
  sps <- spcies[spcies$taxons == t, "sps"]
  
  print(paste0("running... ", sps))
  
  dir2save_maxent <- paste0("models_maxent_", t, "/")
  if(!dir.exists(paste0("models_maxent_", t))) {
    dir.create(dir2save_maxent)
  }
  
  dir2save_presences <- paste0("pres4modelling", "/")
  if(!dir.exists(paste0("pres4modelling"))) {
    dir.create(dir2save_presences)
  }
  
  occs_i <- occs_all[occs_all$sp2 %in% t, c("decimalLongitude", "decimalLatitude")]
  occurrences_raw <- nrow(occs_i)
  
  occs_i_shp <- SpatialPointsDataFrame(coords = occs_i[, c("decimalLongitude", "decimalLatitude")],
                                       data = data.frame(sp = rep(1, nrow(occs_i))),
                                       proj4string = CRS("+init=EPSG:4326"))
  #names(occs_i_shp) <- t
  occs_i_rstr <- rasterize(occs_i_shp, worldclim_all[[1]], field = "sp", background = 0)
  #names(occs_i_rstr) <- t
  #occs_i_rstr <- occs_i_rstr[[2]]
  occs_i_rstr <- mask(occs_i_rstr, worldclim_all[[1]])
  
  #assign(paste0(t, "_rstr"), occs_i_rstr)
  #print(sum(getValues(occs_i_rstr) == 1, na.rm = T))
  
  
  ## occurrences for training/testing
  sps_data <- stack(occs_i_rstr, worldclim_all) 
  sps_data <- as.data.table(as.data.frame(sps_data))
  sps_data[, raster_position := 1:nrow(sps_data)]
  
  # data set for presences
  sps_data_presences <- sps_data[layer == 1, ]
  sps_data_presences <- sps_data_presences[complete.cases(sps_data_presences), ]
  occurrences_1km <- nrow(sps_data_presences)
  rm(sps_data); gc()
  
  # data set for pseudo-absences
  sps_data_absences <- as.data.table(as.data.frame(raster::extract(worldclim_all, bckgr, cellnumbers = TRUE)))
  sps_data_absences <- sps_data_absences[!sps_data_absences$cells %in% sps_data_presences$raster_position, ]
  names(sps_data_absences)
  
  nrow(sps_data_presences)
  nrow(sps_data_absences)
  
  prop4test <- 0.3
  prop4train <- 1 - prop4test
  
  sps_data_presences_train <- sample_n(sps_data_presences, ceiling(nrow(sps_data_presences) * prop4train))
  sps_data_presences_test <- sps_data_presences[!sps_data_presences$raster_position %in% sps_data_presences_train$raster_position, ]
  
  write.csv(sps_data_presences_train, paste0(dir2save_presences, "/sps_data_presences_train_", t, ".csv"), row.names = FALSE)
  write.csv(sps_data_presences_test, paste0(dir2save_presences, "/sps_data_presences_test_", t, ".csv"), row.names = FALSE)
  write.csv(sps_data_absences, paste0(dir2save_presences, "/sps_data_absences_", t, ".csv"), row.names = FALSE)

  #sps_data_presences_train <- fread(paste0(dir2save_presences, "/sps_data_presences_train_", t, ".csv"), header = TRUE)
  #sps_data_presences_test <- fread(paste0(dir2save_presences, "/sps_data_presences_test_", t, ".csv"), header = TRUE)
  #sps_data_absences <- fread(paste0(dir2save_presences, "/sps_data_absences_", t, ".csv"), header = TRUE)  
  
  
  ## Running ENMeval (https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0.0-vignette.html)
  ## Including a tryCatch to avoid stop process if there's an error because of a bug with "H" transformation or something
  
  library(dismo)
  library(ENMeval)
  
  dir_func <- function(sps_data_presences, worldclim_all, sps_data_absences, fc){ # to avoid stop modelling if low number of background points or other errors
    res <- tryCatch(
      {
        #modl1 <- ENMevaluate(occs = sps_data_presences[1:3000, .SD, .SDcols = names(worldclim_all)], 
        modl1 <- ENMevaluate(occs = sps_data_presences_train[, .SD, .SDcols = names(worldclim_all)], 
                             envs = NULL, 
                             bg = sps_data_absences[, .SD, .SDcols = names(worldclim_all)], 
                             algorithm = 'maxnet', 
                             #partitions = 'block', 
                             partitions = "testing",
                             #occs.testing = sps_data_presences[3001:3750, .SD, .SDcols = names(worldclim_all)],  # occurrences for testing; only when partitions = 'testing'
                             occs.testing = sps_data_presences_test[, .SD, .SDcols = names(worldclim_all)],  # occurrences for testing; only when partitions = 'testing'
                             tune.args = list(
                               #fc = c("L","LQ","LQH","H"),
                               #fc = c("L","LQ","LQH"),  # removed "H" because there is some bug in maxnet that gives error for some species
                               #fc = c("L","LQ"),  # removed "H" and "LQH" because there is some bug in maxnet that gives error for some species
                               #fc = c("L","LQ"),
                               fc = fc,
                               rm = c(1, 2, 5)
                               #rm = 1:2
                             ),
                             parallel = TRUE,
                             numCores = 12
                             #numCores = 4
        )
        
      },
      error = function(con){
        message(con)
        return(NULL)
      }
    )
    if(exists("modl1")){ return(modl1) }else{ return(NULL) }
  } #end of dir_func
  
  
  fc_opts <- list(c("L","LQ","LQH","H"), c("L","LQ","LQH"), c("L","LQ"), "L")
  
  for(fc in fc_opts){
    modl <- dir_func(sps_data_presences, worldclim_all, sps_data_absences, fc)
    if(!is.null(modl)) break
  }
  
  
  #rm(sps_data_absences); gc()
  modl
  modl@results
  #View(modl@results)
  write.csv(modl@results, file = paste0(dir2save_maxent, "ENMeval_results_", t, ".csv"))
  save(modl, file = paste0(dir2save_maxent, "models_", t, ".RData"))
  #evalplot.stats(e = modl, stats = "or.mtp", color = "fc", x.var = "rm")
  #load(paste0(dir2save_maxent, "models_", t, ".RData"), verbose = TRUE)
  
  occurrences_train <- nrow(modl@occs)
  occurrences_test <- nrow(modl@occs.testing)  # none because cross-validation
  background_points <- nrow(modl@bg)
  
  
  # selecting optimal model
  results <- eval.results(modl)
  results
  #View(results)
  optimal <- results %>% filter(delta.AICc == 0)
  optimal
  if(nrow(optimal) > 1) optimal <- optimal[1, ]
  
  modl_args <- eval.models(modl)[[optimal$tune.args]]
  modl_args$betas
  #str(modl_args)
  
  #dev.off()
  pdf(paste0(dir2save_maxent, "opt_model_RespCurves_", t, ".pdf"))
  plot(modl_args, type = "cloglog")
  # And these are the marginal response curves for the predictor variables wit non-zero 
  # coefficients in our model. We define the y-axis to be the cloglog transformation, which
  # is an approximation of occurrence probability (with assumptions) bounded by 0 and 1
  # (Phillips et al. 2017).
  dev.off()
  
  modl <- modl@models[[optimal$tune.args]]
  gc()
  
  #save(modl, file = paste0(dir2save_maxent, "opt_model_", t, ".RData"))
  
  # making predictions
  #worldclim_all_data <- fread("worldclim_all_data_NoCor.csv", header = TRUE)
  #worldclim_all_data <- fread("worldclim_all_data_NoCor_070.csv", header = TRUE)
  #worldclim_all_data <- fread("worldclim_all_data_NoCor_040.csv", header = TRUE)
  #worldclim_all_data <- fread(paste0(preds_dir, "worldclim_all_data.csv"), header = TRUE)
  #names(worldclim_all_data) <- names(worldclim_all)
  #names(worldclim_all_data) <- gsub("wc2.1_30s_bio_", "worldclim_all.", names(worldclim_all_data))
  #names(worldclim_all_data) <- gsub("wc2.1_30s_elev", "worldclim_all.20", names(worldclim_all_data))
  #names(worldclim_all_data) <- gsub("worldclim_all", "worldclim_all", names(worldclim_all_data))
  #worldclim_all_data <- worldclim_all_data[complete.cases(worldclim_all_data), ]
  sps_predictions_maxent <- predict(object = modl, 
                                    newdata = worldclim_all_data, 
                                    clamp = TRUE,
                                    type = c("cloglog")
  )
  #rm(worldclim_all_data); gc()
  sps_predictions_maxent <- as.data.table(sps_predictions_maxent)
  head(sps_predictions_maxent)
  range(sps_predictions_maxent)
  nrow(sps_predictions_maxent)
  
  worldclim_all_data0 <- as.data.table(as.data.frame(worldclim_all[[1]]))
  worldclim_all_data0$raster_position <- 1:nrow(worldclim_all_data0)
  
  worldclim_all_data1 <- worldclim_all_data0
  worldclim_all_data1 <- worldclim_all_data1[complete.cases(worldclim_all_data1), ]
  
  worldclim_all_data0 <- worldclim_all_data0[, .SD, .SDcols = "raster_position"]
  
  worldclim_all_data1[, predictions := sps_predictions_maxent$V1]
  
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions")], 
                               by = "raster_position", all.x = TRUE)
  
  #rm(worldclim_all_data1); gc()
  
  sps_preds_rstr <- worldclim_all[[1]]
  sps_preds_rstr <- setValues(sps_preds_rstr, worldclim_all_data0$predictions)
  
  #rm(worldclim_all_data0); gc()
  
  
  #pdf("sps_predictions_maxent_kk.pdf", width = 20, height = 15)
  #par(mfrow = c(1, 2))
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #plot(occs_i_shp, add = TRUE, col = "black")
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #dev.off()
  
  
  #BI_mxnt <- ecospat::ecospat.boyce(fit = sps_preds_rstr,
  #                                  obs = linaria_pres_test_coords, 
  #                                  nclass = 0, 
  #                                  window.w = "default", 
  #                                  res = 100, 
  #                                  PEplot = TRUE)
  
  ## Creating presence/absence map
  # Threshold: minimum presence
  
  #threshold1 <- min(extract(sps_preds_rstr, occs_i_shp))
  #threshold1 <- quantile(extract(sps_preds_rstr, occs_i_shp), 0.1)#, na.rm = TRUE) # sensitivity = 0.9
  #threshold1
  
  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr, occs_i_shp), 
                                                 extract(sps_preds_rstr, bckgr))) # sensitibity default 0.9
  thresholds
  #threshold2 <- as.numeric(thresholds$sensitivity)
  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
  threshold_used <- threshold2
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr, rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  sps_preds_rstr_pres_abs_all <- brick(sps_preds_rstr_pres_abs)
  names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt")
  
  #plot(sps_preds_rstr_pres_abs)
  
  pdf(paste0(dir2save_maxent, "sps_predictions_maxent_", t, ".pdf"), width = 18, height = 15)
  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
  par(mfrow = c(2, 2))
  plot(sps_preds_rstr, zlim = c(0, 1), main = "Occurrences (1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr, zlim = c(0, 1), main = "MaxEnt predictions (cloglog)", cex.main = 2, cex.sub = 1.5)
  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
       sub = paste0("Threshold: '", threshold2use, "'"), 
       cex.main = 2, cex.sub = 1.5, legend = FALSE)
  title(list(paste0(sps),
             cex = 4), 
        line = 1, outer = TRUE)
  
  dev.off()
  
  
  running_time <- as.vector(Sys.time() - t0)
  
  data2save <- (data.frame(species = t, occurrences_raw, occurrences_1km, occurrences_train,
                           occurrences_test, background_points, optimal,
                           thresholds, threshold_used))
  rownames(data2save) <- t
  
  info_models_maxent <- rbind(info_models_maxent, data2save)
  #write.csv(info_models_maxent, "info_modelling_all_species.csv", row.names = FALSE)
  #write.csv(info_models_maxent, "info_modelling_all_species_085.csv", row.names = FALSE)
  write.csv(info_models_maxent, "info_modelling_maxent_all_VirtualSpecies.csv", row.names = FALSE)
  write.csv(info_models_maxent, paste0(dir2save_maxent, "info_modelling_maxent_all_VirtualSpecies.csv"), row.names = FALSE)

  print(paste0(t, " run in: ", running_time))
  
  
  
  ## Modelling with Multilayer perceptrons ####
  
  # Multilayer Perceptrons: The simplest deep networks, they consist of multiple layers of neurons each 
  # fully connected to those in the layer below (from which receive input) and those above (which they influence).
  # 
  library(reticulate)
  use_condaenv('conda_R_env')
  library(tensorflow)
  library("keras")
  
  #print(t)
  t0 <- Sys.time()
  sps <- spcies[spcies$taxons == t, "sps"]
  
  print(paste0("running Multilayer Perceptrons for... ", sps))
  
  dir2save_mlp <- paste0("models_MLP_", t, "/")
  if(!dir.exists(paste0("models_MLP_", t))) {
    dir.create(dir2save_mlp)
  }
  
  # For now, each point is associated to a one (the same) point of the explanatory variables.
  # Later, following Botella's approach (?), each point (presence and absence) can be associated to a 
  # matrix of pixels surrounding the point (e.g. 3x3 or 5x5). This is to capture the influence of 
  # the "micro-habitat" of the plant.
  
  
  nrow(sps_data_absences)
  #nrow(sps_data_presences)
  nrow(sps_data_presences_train)
  nrow(sps_data_presences_test)  
  
  
  # build a model
  if(exists("model")) rm(model); gc()
  
  
  
  model <- keras_model_sequential()
  #inpt_shpe <- length(sps_data_presences_train[, names(worldclim_all)]) - 1
  inpt_shpe <- length(sps_data_presences_train[, names(worldclim_all)])      # number of variables
  
  
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
  
  
  
  ## Compile the model
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
  
  sps_train_data_0 <- sps_data_absences[, .SD, .SDcols = names(worldclim_all)]
  sps_train_data_0$layer <- 0
  sps_train_data_0 <- sps_train_data_0[, .SD, .SDcols = c("layer", names(worldclim_all))]
  
  sps_train_data <- rbind(sps_data_presences_train[, .SD, .SDcols = c("layer", names(worldclim_all))], sps_train_data_0)
  unique(sps_train_data$layer)
  nrow(sps_train_data)
  
  sps_test_data <- rbind(sps_data_presences_test[, .SD, .SDcols = c("layer", names(worldclim_all))], sps_train_data_0)
  nrow(sps_test_data)
  head(sps_test_data)
  unique(sps_test_data$layer)
  
  sps_train_data_1 <- as.matrix(sps_train_data[, .SD, .SDcols = c(names(worldclim_all))])
  head(sps_train_data_1)
  dim(sps_train_data_1)
  nrow(sps_train_data_1)
  

  sps_train_labels <- as.matrix(sps_train_data[, "layer", drop = FALSE])
  head(sps_train_labels)
  unique(sps_train_labels)
  nrow(sps_train_labels)
  dim(sps_train_labels)
  
  
  #t0 <- Sys.time()
  model %>% fit(sps_train_data_1,
                sps_train_labels, 
                epochs = 200,  # The number of "epochs" is a hyperparameter that defines the number times that the learning algorithm will work through the ENTIRE training dataset.
                # The "batch" size is a hyperparameter that defines the NUMBER OF SAMPLES to work through before updating the internal model parameters.
                batch_size = 128,
                verbose = 2
  )
  #Sys.time() - t0
  
  # https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_save_and_restore/
  save_model_hdf5(model, 
                  filepath = paste0(dir2save_mlp, "model_MLP.h5"), 
                  overwrite = TRUE, 
                  include_optimizer = TRUE)

  
  
  ## Test the model
  
  score_mlp_test <- model %>% evaluate(as.matrix(sps_test_data[, .SD, .SDcols = names(worldclim_all)]), 
                                 as.matrix(sps_test_data[, "layer", drop = FALSE]), 
                                 batch_size = 128
                                 )
  lst2safe <- c("score_mlp_test")
  save(list = lst2safe, file = paste0(dir2save_mlp, "/evaluations.RData"))
  #load("evaluations.RData", verbose = TRUE)
  score_mlp_test #for test data
  #       loss           auc    binary_accuracy 
  # 0.01116585      0.99933493      0.99636084
  
  
  # for training data (last epoch)
  # loss: 0.0958 - auc: 0.9564 - binary_accuracy: 0.9651
  # The gap between training accuracy and test accuracy is an example of "overfitting" 
  # train_auc (0.9564) < test_auc (0.97014868)  --> No overfitting
  # train_accuracy (0.9651) < test_accuracy (0.98521733)  --> No overfitting
  # Overfitting is when a machine learning model performs worse on new data than on their training data
  
  
  ## Make predictions over the whole extent
  nrow(worldclim_all_data)
  sum(complete.cases(worldclim_all_data))
  
  #predictions <- model %>% predict_proba(as.matrix(worldclim_all_data), batch_size = 128)
  predictions_mlp <- model %>% predict(as.matrix(worldclim_all_data), batch_size = 128)
  head(predictions_mlp)
  nrow(predictions_mlp)
  sum(predictions_mlp[, 1])
  
  length(unique(predictions_mlp[, 1]))
  range(predictions_mlp[, 1])
  max(predictions_mlp[, 1])
  summary(predictions_mlp[, 1])
  
  ## Mapping predictions
  worldclim_all_data1[, predictions_mlp := as.vector(predictions_mlp[, 1])]
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions_mlp")], 
                               by = "raster_position", all.x = TRUE)
  
  sps_preds_rstr <- brick(sps_preds_rstr)
  sps_preds_rstr <- setValues(sps_preds_rstr, 
                              values = worldclim_all_data0$predictions_mlp,
                              layer = 2)
  names(sps_preds_rstr) <- c("predictions_maxent", "predictions_MLP")
  sps_preds_rstr

  #pdf("sps_predictions_maxent_kk.pdf", width = 20, height = 15)
  #par(mfrow = c(1, 2))
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #plot(occs_i_shp, add = TRUE, col = "black")
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #dev.off()
  
  
  ## Boyce Index
  
  sps_data_presences_test_coords <- xyFromCell(object = sps_preds_rstr, cell = sps_data_presences_test$raster_position)
  
  BI_mlp <- ecospat::ecospat.boyce(fit = sps_preds_rstr[["predictions_MLP"]],
                                  obs = sps_data_presences_test_coords, 
                                  nclass = 0, 
                                  window.w = "default", 
                                  res = 100, 
                                  PEplot = TRUE)
  
  
  BI_mlp$cor  # 0.962 (DL)
  
  
  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr[["predictions_MLP"]], occs_i_shp), 
                                                 extract(sps_preds_rstr[["predictions_MLP"]], bckgr))) # sensitibity default 0.9
  thresholds
  #threshold2 <- as.numeric(thresholds$sensitivity)
  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
  threshold_used <- threshold2
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr[["predictions_MLP"]], rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  sps_preds_rstr_pres_abs_all <- stack(sps_preds_rstr_pres_abs_all, sps_preds_rstr_pres_abs)
  names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt", "Pres_Abs_MLP")
  #plot(sps_preds_rstr_pres_abs)
  
  pdf(paste0(dir2save_mlp, "sps_predictions_MLP_", t, ".pdf"), width = 18, height = 15)
  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
  par(mfrow = c(2, 2))
  plot(sps_preds_rstr[["predictions_MLP"]], zlim = c(0, 1), main = "Occurrences (1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr[["predictions_MLP"]], zlim = c(0, 1), main = "MLP predictions", cex.main = 2, cex.sub = 1.5)
  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
       sub = paste0("Threshold: '", threshold2use, "'"), 
       cex.main = 2, cex.sub = 1.5, legend = FALSE)
  title(list(paste0(sps),
             cex = 4), 
        line = 1, outer = TRUE)
  
  dev.off()
  
  
  
  running_time <- as.vector(Sys.time() - t0)
  
  data2save <- (data.frame(species = t, occurrences_raw, occurrences_1km, occurrences_train,
                           occurrences_test, background_points, 
                           auc.val = as.vector(score_mlp_test[grepl("auc", names(score_mlp_test))]),
                           cbi.val = BI_mlp$cor,
                           thresholds, threshold_used))
  rownames(data2save) <- t
  
  info_models_mlp <- rbind(info_models_mlp, data2save)
  write.csv(info_models_mlp, "info_models_MLP_all_VirtualSpecies.csv", row.names = FALSE)
  write.csv(info_models_mlp, paste0(dir2save_mlp, "info_models_MLP_all_VirtualSpecies.csv"), row.names = FALSE)
  
  print(paste0(t, " run in: ", running_time))
  
  
  
  ## Convolutional Neural Networks ####
  # https://d2l.ai/
  
  # CNN: A particular case of NN
  # 1D-CNN means that the convolution kernel is convolved with the layer input 
  # over a single spatial (or temporal) dimension to produce a tensor of outputs
  # In other words, the input layer has only one dimension, in our case one pixel
  # and its punctual values of environmental variables
  
  t0 <- Sys.time()
  sps <- spcies[spcies$taxons == t, "sps"]
  
  print(paste0("running Convolutional neural Networks for... ", sps))
  
  dir2save_cnn <- paste0("models_CNN_", t, "/")
  if(!dir.exists(paste0("models_CNN_", t))) {
    dir.create(dir2save_cnn)
  }
  
  nrow(sps_data_absences)
  nrow(sps_data_presences_train)
  nrow(sps_data_presences_test)  
  
  sps_train_data
  table(sps_train_data$layer)
  sps_test_data
  table(sps_test_data$layer)
  sps_train_data_1
  dim(sps_train_data_1)
  sps_train_labels
  dim(sps_train_labels)
  
  
  sps_test_data_1 <- sps_test_data[, -c("layer")]
  dim(sps_test_data_1)
  
  
  # Creating arrays
  sps_train_array_x <- array(as.matrix(sps_train_data_1), dim = c(nrow(sps_train_data_1), ncol(sps_train_data_1), 1))
  dim(sps_train_array_x)
  
  #linaria_train_array_y <- to_categorical(as.matrix(linaria_train[, 1]))
  sps_train_array_y <- as.matrix(sps_train_labels)
  dim(sps_train_array_y)
  head(sps_train_array_y)
  tail(sps_train_array_y)
  
  
  sps_test_array_x <- array(as.matrix(sps_test_data_1), dim = c(nrow(sps_test_data_1), ncol(sps_test_data_1), 1))
  dim(sps_test_array_x)
  #linaria_test_array_y <- to_categorical(as.matrix(linaria_test[, 1]))
  sps_test_array_y <- as.matrix(sps_test_data[, "layer", drop = FALSE])
  dim(sps_test_array_y)
  
  
  # We have to create an array where for each pixel (pres and abs) I have a tensor of, say, 3x3 pixels around it
  # Then, dim of this array will be 'number of pixels', 3, 3, 'number of variables'
  # This array will be 'x', which will be divided into 'x_train' and 'x_test' (e.g. 70/30)
  # Finally, I will have another array for labels with dim equal to 'number of pixels' and 1 (one column per species with 1s and 0s for pres and absences, respectivelly)
  
  # build a model
  if(exists("model_c")) rm(model); gc()
  
  inpt_shpe <- length(sps_test_data_1)  # input_shape = c(ncol(dataTrain_x), nrow(dataTrain_x))) 
  
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
  
  
  model_c %>% fit(sps_train_array_x, sps_train_array_y, 
                  batch_size = 128, 
                  epochs = 100, 
                  verbose = 2)
  
  save_model_hdf5(model_c, 
                  filepath = paste0(dir2save_cnn, "model_CNN.h5"), 
                  overwrite = TRUE, 
                  include_optimizer = TRUE)
  
  
  score_cnn_train <- model_c %>% evaluate(sps_train_array_x, sps_train_array_y, batch_size = 128)
  score_cnn_train
  
  score_cnn_test <- model_c %>% evaluate(sps_test_array_x, sps_test_array_y, batch_size = 128)
  score_cnn_test
  
  
  score_cnn_test #for test data
  #       loss           auc    binary_accuracy 
  # 0.01756423      0.99896616      0.99355167 
  
  
  # for training data (score_train)
  #       loss            auc    binary_accuracy 
  # 0.02106769      0.99900067      0.99371600
  
  
  ## Making predictions over the whole area
  nrow(worldclim_all_data)
  
  worldclim_all_data_array_x <- array(as.matrix(worldclim_all_data), dim = c(nrow(worldclim_all_data), ncol(worldclim_all_data), 1))
  dim(worldclim_all_data_array_x)
  
  
  predictions_cnn <- model_c %>% predict(worldclim_all_data_array_x, batch_size = 128)
  head(predictions_cnn)
  nrow(predictions_cnn)
  sum(predictions_cnn[, 1])
  
  length(unique(predictions_cnn[, 1]))
  range(predictions_cnn[, 1])
  max(predictions_cnn[, 1])
  summary(predictions_cnn[, 1])
  
  
  ## Mapping predictions
  worldclim_all_data1[, predictions_cnn := as.vector(predictions_cnn[, 1])]
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions_cnn")], 
                               by = "raster_position", all.x = TRUE)
  
  #sps_preds_rstr <- brick(sps_preds_rstr)
  sps_preds_rstr <- setValues(sps_preds_rstr, 
                              values = worldclim_all_data0$predictions_cnn,
                              layer = 3)
  names(sps_preds_rstr) <- c("predictions_maxent", "predictions_MLP", "predictions_CNN")
  sps_preds_rstr
  
  
  ## Boyce Index
  
  sps_data_presences_test_coords
  
  BI_cnn <- ecospat::ecospat.boyce(fit = sps_preds_rstr[["predictions_CNN"]],
                                   obs = sps_data_presences_test_coords, 
                                   nclass = 0, 
                                   window.w = "default", 
                                   res = 100, 
                                   PEplot = TRUE)
  
  
  BI_cnn$cor  # 0.89 (CNN)
  
  
  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr[["predictions_CNN"]], occs_i_shp), 
                                                 extract(sps_preds_rstr[["predictions_CNN"]], bckgr))) # sensitibity default 0.9
  thresholds
  #threshold2 <- as.numeric(thresholds$sensitivity)
  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
  threshold_used <- threshold2
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr[["predictions_CNN"]], rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  sps_preds_rstr_pres_abs_all <- stack(sps_preds_rstr_pres_abs_all, sps_preds_rstr_pres_abs)
  names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt", "Pres_Abs_MLP", "Pres_Abs_CNN")
  
  
  #plot(sps_preds_rstr_pres_abs)
  
  pdf(paste0(dir2save_cnn, "sps_predictions_CNN_", t, ".pdf"), width = 18, height = 15)
  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
  par(mfrow = c(2, 2))
  plot(sps_preds_rstr[["predictions_CNN"]], zlim = c(0, 1), main = "Occurrences (1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr[["predictions_CNN"]], zlim = c(0, 1), main = "CNN predictions", cex.main = 2, cex.sub = 1.5)
  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
       sub = paste0("Threshold: '", threshold2use, "'"), 
       cex.main = 2, cex.sub = 1.5, legend = FALSE)
  title(list(paste0(sps),
             cex = 4), 
        line = 1, outer = TRUE)
  
  dev.off()
  
  ## saving results
  running_time <- as.vector(Sys.time() - t0)
  
  data2save <- (data.frame(species = t, occurrences_raw, occurrences_1km, occurrences_train,
                           occurrences_test, background_points,
                           auc.train = as.vector(score_cnn_train[grepl("auc", names(score_cnn_train))]),
                           auc.val = as.vector(score_cnn_test[grepl("auc", names(score_cnn_test))]),
                           cbi.val = BI_cnn$cor,
                           thresholds, threshold_used))
  rownames(data2save) <- t
  
  info_models_cnn <- rbind(info_models_cnn, data2save)
  write.csv(info_models_cnn, "info_models_CNN_all_VirtualSpecies.csv", row.names = FALSE)
  write.csv(info_models_cnn, paste0(dir2save_cnn, "info_models_CNN_all_VirtualSpecies.csv"), row.names = FALSE)
  
  writeRaster(sps_preds_rstr_pres_abs_all, paste0(dir2save_cnn, "sps_preds_rstr_pres_abs_all.tif"))
  writeRaster(sps_preds_rstr, paste0(dir2save_cnn, "sps_preds_rstr.tif"))

  print(paste0(t, " run in: ", running_time))
  
  
  ## Plotting all predictions together ####
  
  sps_preds_rstr_pres_abs_all
  #names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt", "Pres_Abs_MLP", "Pres_Abs_CNN")
  
  
  pdf(paste0(dir2save_cnn, "Pres-Abs_MaxEnt_MLP_CNN_", t, ".pdf"), width = 20, height = 20)
  par(mfrow = c(2, 2))
  par(mar = c(8, 4, 4, 5))
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_MaxEnt"]], col = "grey", 
       main = paste0("Simulated species: ", t), cex.main = 3.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_MaxEnt"]], zlim = c(0, 1), main = "MaxEnt", cex.main = 3.5, cex.sub = 2.5,
       sub = paste0("MaxEnt: Boyce Index = ", round(as.vector(optimal$cbi.val), 3), "; AUC = ", round(as.vector(optimal$auc.val), 3)))
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_MLP"]], zlim = c(0, 1), main = "MultiLayer Perceptrons (MLP))", cex.main = 3.5, cex.sub = 2.5,
       sub = paste0("MLP: Boyce Index = ", round(BI_mlp$cor, 3), "; AUC = ", round(score_mlp_test[grepl("auc", names(score_mlp_test))], 3)))
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_CNN"]], zlim = c(0, 1), main = "Convolutional Neural Network (CNN)", cex.main = 3.5, cex.sub = 2.5,
       sub = paste0("CNN: Boyce Index = ", round(BI_cnn$cor, 3), "; AUC = ", round(score_cnn_test[grepl("auc", names(score_cnn_test))], 3)))
  dev.off()
  
  
  
  if(exists("model")) rm(model)
  if(exists("model_c")) rm(model_c)
  
  
  files <- list.files(tempdir(), full.names = TRUE)
  unlink(files, recursive = TRUE); rm(files)
  
  files <- list.files("/scratch/rotllxa", full.names = TRUE)
  files <- files[grepl("py", files)]
  unlink(files, recursive = TRUE); rm(files)
  
 
  detach(package:reticulate,unload=TRUE)
  detach(package:keras,unload=TRUE)
  
  #
  
}



## Checking results (MaxEnt) ####

#info_models_maxent <- read.csv("info_modelling_all_species.csv", header = TRUE)
info_models_maxent <- read.csv("info_modelling_all_VirtualSpecies.csv", header = TRUE)
View(info_models_maxent)

mean(info_models_maxent$auc.val.avg)
mean(info_models_maxent$auc.train)
mean(info_models_maxent$cbi.val.avg)
mean(info_models_maxent$cbi.train)








for (t in taxons){
  
  
  
  #


    

  
  
  
   
}



