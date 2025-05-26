library(mlbench)
library(dplyr)
library(e1071)
library(caret)
data(PimaIndiansDiabetes)
diabetes_data <- PimaIndiansDiabetes %>%
  mutate(diabetes = as.factor(ifelse(diabetes == "pos", 1, 0))) %>%
  mutate(across(!diabetes, as.numeric)) %>%
  filter(mass > 0)
# Define a grid of parameters to search
tune_grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.5, 1))

# Train the model using cross-validation
svm_tuned <- train(diabetes ~ ., data = diabetes_data,
                   method = "svmRadial",
                   trControl = trainControl(method = "cv", number = 10),
                   tuneGrid = tune_grid)
svm_best_params <- svm_tuned$bestTune
svm_mod <- svm(diabetes ~ ., data = diabetes_data, kernel = "radial",
               cost = svm_best_params$C,
               sigma = svm_best_params$sigma)

Diabetes_formula <- diabetes ~ mod(glucose) + mod(insulin) +
  mod(pregnant) + mod(pressure) + mod(triceps) + mod(pedigree) +
  mod(mass) + mod(age) + mod(glucose, insulin) + mod(age, mass) +
  mod(.)
list_of_deep_models_Diabetes <- list(mod = ONAM:::get_submodel)
svm_expl <- onam(Diabetes_formula, list_of_deep_models_Diabetes,
                 diabetes_data, svm_mod, target = "binary",
                 n_ensemble = 20, progresstext = TRUE,
                 epochs = 500, verbose = 0)
summary(svm_expl)

#generate prediction object for saving
svm_res <- predict(svm_expl)
saveRDS(svm_res, "svm_res.RDS")
