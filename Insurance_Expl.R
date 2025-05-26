library(insuranceData)
library(xgboost)
library(dplyr)
library(caret)
data("dataCar")
dataCar_subset <- dataCar %>%
  select(veh_value, veh_body, veh_age, gender, area, agecat, clm) %>%
  mutate_all(as.numeric)

dataCar_train <-
  dataCar_subset[sample(1:nrow(dataCar_subset), 10000),]

train_insurance <- dataCar_train %>% select(-clm)
label_insurance <- dataCar_train %>% select(clm) %>% unlist()

xgb_train_insurance <- xgb.DMatrix(as.matrix(train_insurance),
                                   label = label_insurance)

xgb_mod <-
  xgboost(
    xgb_train_insurance,
    nrounds = 10000,
    verbose = 1,
    print_every_n = 100,
    objective = "binary:logistic",
    eval_metric = "logloss"
  )
summary(xgb_mod)
preds <- as.numeric(predict(xgb_mod, xgb_train_insurance) > 0.5)

confusionMatrix(as.factor(preds), as.factor(unlist(label_insurance)))

Insurance_formula <-
  clm ~ mod(veh_value) + mod(veh_body) + mod(veh_age) +
  mod(gender) + mod(area) + mod(agecat) + mod(veh_age, veh_value) +
  mod(.)

list_of_deep_models_Insurance <- list(mod = ONAM:::get_submodel)

categorical_features <- c("veh_body", "agecat", "area", "gender")

xgb_expl <-
  onam(
    Insurance_formula,
    list_of_deep_models_Insurance,
    train_insurance,
    target = "binary",
    model = xgb_mod,
    model_data = xgb_train_insurance,
    categorical_features = categorical_features,
    n_ensemble = 20,
    epochs = 500,
    progresstext = TRUE,
    verbose = 0
  )

xgb_res <- predict(xgb_expl)
saveRDS(xgb_res, "xgb_res.RDS")
