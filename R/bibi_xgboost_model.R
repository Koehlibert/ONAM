library(xgboost)
library(dplyr)
library(ONAM)
load("BIBItrainData.RData")
bibi_data <-
  train[, c(2:5, 7, 8, 9, 10, 12, 15, 16, 17:21, 23, 24, 25, 26)] %>%
  mutate(BioregUpstream2 = as.numeric(as.factor(BioregUpstream2)))

train <- bibi_data %>% select(-CalculatedBiologicalMetricValue)
label <-
  bibi_data %>% select(CalculatedBiologicalMetricValue) %>% unlist()

xgb_train <- xgb.DMatrix(as.matrix(train),
                         label = label)

xgb_mod <-
  xgboost(
    xgb_train,
    nrounds = 10000,
    verbose = 1,
    print_every_n = 100
  )

formula <-
  CalculatedBiologicalMetricValue ~ mod(Latitude) +
  mod(Longitude) + mod(BioregUpstream2) + mod(AreaSqKM) +
  mod(dep_so4_2011) + mod(elevation) +
  mod(hydrogroup_d4) + mod(percent_sandy) +
  mod(surfcoarse) + mod(deg_barr_all_local) +
  mod(upstream.total_precip) + mod(Agriculture) +
  mod(Development) + mod(Forest) + mod(Openwater) +
  mod(Barren) + mod(Grass) + mod(Woodywetland) +
  mod(Herbwetland) +
  mod(Development, elevation) +
  mod(Forest, elevation) + mod(Forest, Development) +
  mod(.)
categorical_features <- c("BioregUpstream2")

list_of_deep_models = list(mod = ONAM:::get_submodel)
bibi_expl_xgboost <-
  onam(
    formula,
    list_of_deep_models,
    model = xgb_mod,
    model_data = xgb_train,
    categorical_features = categorical_features,
    n_ensemble = 10,
    epochs = 500,
    verbose = 1
  )
bibi_expl_xg_res <- predict(bibi_expl_xgboost)
saveRDS(BIBIEvalData, "BIBIEval_xgb.RDS")
