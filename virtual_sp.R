###############################################
####     Creating virtual species for      ####
####             modelling                 ####
###############################################

# 
# 
library(raster)
library(sf)
library(virtualspecies)
library(dplyr)
library(data.table)

getwd()
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_test/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/dl_sdm_single/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/cnn_sdm_single/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/cnn_3D_sdm_single/"
wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/virtualspecies/"
setwd(wd)

#bioscrop <- brick("../dl_sdm_test/bioscrop.tif")    # 10 km
#bioscrop@crs <- sp::CRS("+init=EPSG:4326")
#bioscrop
#dev.off()
#plot(bioscrop[[1]])


worldclim_path <- "/Users/xavi_rp/Documents/MinBA_models/wc5"      # 10km
worldclim_path0.5 <- "/Users/xavi_rp/Documents/MinBA_models/wc0.5" #  1km

worldclim_files <- list.files(worldclim_path0.5, full.names = TRUE)
worldclim_files <- worldclim_files[grepl("bil$", worldclim_files) | grepl("tif$", worldclim_files)]
worldclim_files

worldclim_all <- stack(worldclim_files)
plot(worldclim_all[[20]])

## elevation (aggregated from 30'' to 5')
#worldclim_files <- list.files(worldclim_path0.5, full.names = TRUE)
#worldclim_files <- worldclim_files[grepl("tif$", worldclim_files)]
#elev5 <- raster(worldclim_files[[2]])
#elev5 <- crop(elev5, worldclim_all)
#writeRaster(elev5, paste0(worldclim_path0.5, "/wc2.1_30s_elev_Eur.tif"))
#worldclim_all <- stack(worldclim_all, elev5)
#worldclim_all

eur_coords <- c(-13, 48, 35, 72)

worldclim_all <- crop(worldclim_all, eur_coords)
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



## Creating Simulated Species ####
# http://borisleroy.com/virtualspecies_tutorial/index.html

?generateSpFromFun
?generateRandomSp

virtSps <- list()  #list of virtual species complete information to be saved
virtSps_pos <- 0
virtSp_occurrences <- as.data.frame(matrix(nrow = 0, ncol = 0))    #data frame with coordinates of occurrences of all virtual species
repetitions <- 1  # Number of species to be created
repetitions <- 25  # Number of species to be created
repetitions <- 24  # Number of species to be created

prevalences <- c(0.001, 
                 0.01,     #  1%
                 0.1,      # 10%
                 0.5#, 
                 #0.75, 
                 #0.9
                 )
prevalences <- sort(rep_len(prevalences, repetitions))
table(prevalences)

repeat{
  virtSps_pos <- virtSps_pos + 1
  virtSps[[virtSps_pos]] <- generateRandomSp(raster.stack = worldclim_all[[c(1, 3, 4, 9, 20)]], # using these variables to create the sps
                                             convert.to.PA = TRUE,
                                             sample.points = TRUE,  # for large rasters, pick nb.points 
                                             #nb.points = 10000,
                                             #nb.points = 8153664, # 5%  # sum(!is.na(getValues(worldclim_all))) * 0.05
                                             nb.points = 1630733, # 1%  # sum(!is.na(getValues(worldclim_all))) * 0.001
                                             relations = c("logistic"),
                                             realistic.sp = TRUE,
                                             PA.method = "threshold",
                                             #PA.method = "probability",
                                             #alpha = -0.1,
                                             #adjust.alpha = TRUE,
                                             beta = "random",
                                             species.prevalence = prevalences[virtSps_pos],
                                             plot = FALSE)
  #
  print(paste0("virtSp_", virtSps_pos, " at ", Sys.time()))
  #assign(paste0("virtSp", vsp), virtSpi)
  if(virtSps_pos == repetitions) break
  
}

head(virtSp_occurrences)
nrow(virtSp_occurrences)

str(virtSps[[1]])

## Plotting all virtual species maps
for(i in 1:repetitions){
  pdf(paste0("virtual_sp_", i, ".pdf"), height = 7, width = 9)
  par(mar = c(3, 6, 8, 6), bty = "n")
  #par(xpd = TRUE)
  par(mfrow = c(2, 2))
  plot(virtSps[[i]]$suitab.raster, main = "Suitability.raster")
  plot(virtSps[[i]]$probability.of.occurrence, main = "Probability.of.occurrence.raster")
  plot(virtSps[[i]]$pa.raster, main = "Presence.absence.raster")
  plot(virtSps[[i]]$pa.raster, main = "", col = "white", legend = FALSE, axes = FALSE)
  title(list(paste0("virtual_sp ", i),
             cex = 1.8), 
        line = -2, outer = TRUE)
  mtext(paste0("Prevalence = ", round(as.numeric(virtSps[[i]]$PA.conversion[["species.prevalence"]]), 4)), 
        side = 3, line = -5, outer = FALSE)
  dev.off()
  cat("\r", paste0("virtual_sp ", i, "... plotted"))
}


## Getting presence points 
virtSps[[1]]
for(i in 1:repetitions){
  # sample n2sample occurrences; where n2sample is randomly generated between 1 and total occurrences of the sp
  n2sample <- sample(1:sum(values(virtSps[[i]]$pa.raster) == 1, na.rm = TRUE), 1)
  
  assign("virtSpi", virtSps[[i]])
  virtSpi_occ <- as.data.frame(coordinates(virtSpi$pa.raster))
  virtSpi_occ$spec <- values(virtSpi$pa.raster)
  
  virtSpi_occ <- virtSpi_occ[virtSpi_occ$spec == 1, ]
  virtSpi_occ <- virtSpi_occ[!is.na(virtSpi_occ$spec), ]
  virtSpi_occ$spec <- paste0("virtSp_", i)
  names(virtSpi_occ) <- c("lon", "lat", "species")
  
  virtSpi_occ <- sample_n(virtSpi_occ, n2sample)
  #head(virtSpi_occ)
  #nrow(virtSpi_occ)
  virtSp_occurrences <- rbind(virtSp_occurrences, virtSpi_occ)
}

head(virtSp_occurrences)
nrow(virtSp_occurrences)
table(virtSp_occurrences$species)


#presence.points <- sampleOccurrences(virtSps[[i]],
#                                     n = n2sample, # The number of points to sample
#                                     type = "presence only")
#head(presence.points)
#presence.points$sample.points
#
#
#PA.points <- sampleOccurrences(virtSps[[1]],
#                               #n = 3000,
#                               #sample.prevalence = 0.01,
#                               n = 30,
#                               sample.prevalence = 0.5,
#                               type = "presence-absence",
#                               extract.probability = TRUE, # You may be interested in extracting the true probability of occurrence at each sampled point, to compare with your SDM results
#                               plot = TRUE)
#PA.points
#PA.points$sample.points
#sum(PA.points$sample.points$Real == 1)
#sum(PA.points$sample.points$Real == 0)




#
write.csv(virtSp_occurrences, "virtSp_occurrences.csv", row.names = FALSE)
save(virtSps, file = "virtSp_allinfo.RData")






