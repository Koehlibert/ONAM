library(xgboost)
library(caret)
library(MASS)
library(dplyr)
data("Boston")
# Boston_subset <- Boston %>%
  # select(STATE, CLASS, GENDER, AGE, PAID)

# Boston_train <- Bost
  # Boston_subset#[sample(1:nrow(Boston_subset), 10000),]

train_Boston <- Boston %>% dplyr::select(-medv)
label_Boston <- Boston %>% dplyr::select(medv) %>% unlist()

xgb_train_Boston <- xgb.DMatrix(as.matrix(train_Boston),
                                   label = label_Boston)

xgb_mod <-
  xgboost(
    xgb_train_Boston,
    nrounds = 10000,
    verbose = 1,
    print_every_n = 100,
    objective = "reg:squarederror",
    eval_metric = "rmse"
  )
summary(xgb_mod)
preds <- predict(xgb_mod, xgb_train_Boston)

cor(preds, Boston$medv)

# confusionMatrix(as.factor(preds), as.factor(unlist(label_insurance)))

Boston_formula <-
  medv ~ mod(crim) + mod(zn) + mod(indus) + mod(chas) +
  mod(nox) + mod(rm) + mod(age) + mod(dis) + mod(rad) +
  mod(tax) + mod(ptratio) + mod(black) + mod(lstat) +
  mod(crim, dis) + mod(rm, age) + mod(zn, indus) +
  mod(.)

list_of_deep_models_Boston <- list(mod = ONAM:::get_submodel)

categorical_features <- c("chas")

xgb_expl <-
  onam(
    Boston_formula,
    list_of_deep_models_Boston,
    Boston,
    # target = "binary",
    model = xgb_mod,
    model_data = xgb_train_Boston,
    categorical_features = categorical_features,
    n_ensemble = 20,
    epochs = 1000,
    progresstext = TRUE,
    verbose = 0
  )

xgb_res <- predict(xgb_expl)
saveRDS(xgb_res, "xgb_res.RDS")
