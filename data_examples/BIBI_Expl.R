library(keras)
library(dplyr)
library(readxl)
library(ONAM)
#load data
trainData <- read_xlsx("BIBITrain.xlsx")
head(trainData)

#define model formula
BIBI_formula <-
  Prediction ~ mod(Latitude) + mod(Longitude) +
  mod(BioregUpstream2) + mod(AreaSqKM) +
  mod(dep_so4_2011) + mod(elevation) +
  mod(hydrogroup_d4) + mod(percent_sandy) +
  mod(surfcoarse) + mod(deg_barr_all_local) +
  mod(upstream.total_precip) + mod(Agriculture) +
  mod(Development) + mod(Forest) + mod(Openwater) +
  mod(Barren) + mod(Grass) + mod(Woodywetland) +
  mod(Herbwetland) +
  mod(Development, elevation) +
  mod(Agriculture, elevation) +
  mod(Agriculture, Development) +
  mod(.)
#Specify model architecture in a named list where the name corresponds to the model name in the model formula
list_of_mods_BIBI <- list(mod = ONAM:::get_submodel)
categorical_features <- c("BioregUpstream2")
#Fit PHONAM model
trainData <- as.matrix(trainData)
BIBIExpl <-
  onam(
    BIBI_formula,
    list_of_mods_BIBI,
    categorical_features = categorical_features,
    trainData,
    n_ensemble = 20,
    epochs = 500,
    progresstext = TRUE,
    verbose = 0
  )
#generate prediction object for saving
BIBI_Res <- predict(BIBIExpl)
saveRDS(BIBI_Res, "BIBI_Res.RDS")
