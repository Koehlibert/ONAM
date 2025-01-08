# Achieving interpretable machine learning by functional decomposition of black-box models into explainable predictor effects
Code to reproduce results from Köhler, Rügamer and Schmid (2024):
https://arxiv.org/abs/2407.18650

## Install package:
``` r
install.packages(devtools)  
library(devtools)  
build()  
library(ONAM)
```

## Fit model on Chesapeake Bay watershed biotic integrity dataset

``` r
library(keras)
#define model architectures
deep_model1 <- function(inputs)
{
  outputs <- inputs %>%
    keras::layer_dense(units = 64, activation = "relu",
                       use_bias = TRUE) %>%
    layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 32, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dense(units = 16, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dense(units = 8, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dense(units = 1, activation = "linear",
                       use_bias = TRUE)
  subModel <- keras::keras_model(inputs, outputs)
  return(subModel)
}
deep_model2 <- function(inputs)
{
  outputs <- inputs %>%
    keras::layer_dense(units = 128, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 64, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dense(units = 32, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dense(units = 16, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dense(units = 8, activation = "relu",
                       use_bias = TRUE) %>%
    keras::layer_dense(units = 1, activation = "linear",
                       use_bias = TRUE)
  subModel <- keras::keras_model(inputs, outputs)
  return(subModel)
}
#define model formula
BIBIformula <- Prediction ~ deep_model1(Latitude) + deep_model1(Longitude) +  
  deep_model1(BioregUpstream2) + deep_model1(AreaSqKM) +  
  deep_model1(dep_so4_2011) + deep_model1(elevation) +  
  deep_model1(hydrogroup_d4) + deep_model1(percent_sandy) +  
  deep_model1(surfcoarse) + deep_model1(deg_barr_all_local) +  
  deep_model1(upstream.total_precip) + deep_model1(Agriculture) +  
  deep_model1(Development) + deep_model1(Forest) + deep_model1(Openwater) +  
  deep_model1(Barren) + deep_model1(Grass) + deep_model1(Woodywetland) +  
  deep_model1(Herbwetland) + 
  deep_model2(Development, elevation) +  
  deep_model2(Agriculture, elevation) + 
  deep_model2(Agriculture, Development) +  
  deep_model2(Latitude, Longitude,  
              BioregUpstream2, AreaSqKM, dep_so4_2011, elevation, hydrogroup_d4,  
              percent_sandy, surfcoarse, deg_barr_all_local, Openwater, Barren,  
              upstream.total_precip, Agriculture, Development, Forest, Grass,  
              Woodywetland, Herbwetland)  
#load data  
library(readxl)
trainData <- read_xlsx("BIBITrain.xlsx")
trainData <- as.matrix(trainData)
#Define Deep Models  
list_of_deep_models_BIBI <- list(deep_model1 = deep_model1,
                                 deep_model2 = deep_model2)
#Fit PHONAM model  
BIBIExpl <-  
  fitPHOModel(BIBIformula, list_of_deep_models_BIBI,  
              trainData, nEnsemble = 2, progresstext = TRUE, verbose = 1)  
#get all predictions  
BIBIEvalData <- evaluateModel(BIBIExpl)

BIBIExpl <-  
  fitPHOModel(BIBIformula, list_of_deep_models_BIBI,  
              trainData, 10, progresstext = TRUE, verbose = 1)  
#get all predictions  
BIBIEvalData <- evaluateModelGeneric(BIBIExpl)
#BIBIEvalData$finalTotalPredictions contains orthogonalized feature effects.
#Example for effect plot:
plot(trainData[,"Forest"], BIBIEvalData$finalTotalPredictions[,"Forest"])
```


