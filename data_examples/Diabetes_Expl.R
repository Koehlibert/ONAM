library(mlbench)
library(dplyr)
library(gbm)
library(ONAM)
data(PimaIndiansDiabetes)
diabetes_data <- PimaIndiansDiabetes %>%
  mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
  mutate(across(!diabetes, as.numeric)) %>%
  filter(mass > 0)
# Train the GBM model (binary classification)
gbm_model <- gbm(diabetes ~ .,
                 data = diabetes_data,
                 distribution = "bernoulli",  # Binary classification task
                 n.trees = 500,               # Number of boosting iterations
                 interaction.depth = 3,       # Maximum depth of the tree
                 shrinkage = 0.01,            # Learning rate
                 cv.folds = 5)                # Cross-validation folds for tuning

# Print model summary
summary(gbm_model)

predicted_status <- predict(gbm_model, diabetes_data)
caret::confusionMatrix(factor(as.numeric(predicted_status > 0.5)), factor(diabetes_data$diabetes))

Diabetes_formula <- diabetes ~ mod(glucose) + mod(insulin) +
  mod(pregnant) + mod(pressure) + mod(triceps) + mod(pedigree) +
  mod(mass) + mod(age) + mod(glucose, insulin) + mod(age, mass) +
  mod(.)
list_of_deep_models_Diabetes <- list(mod = ONAM:::get_submodel)
gbm_expl <- onam(Diabetes_formula, list_of_deep_models_Diabetes,
                 diabetes_data, gbm_model,
                 prediction_function = function(model, data) {
                   predict(model, data, n.trees = 500, type = "response")
                 },
                 target = "binary",
                 n_ensemble = 10, progresstext = TRUE,
                 epochs = 500, verbose = 0)
summary(gbm_expl)

#generate prediction object for saving
gbm_res <- predict(gbm_expl)
saveRDS(gbm_res, "gbm_res.RDS")
saveRDS(summary(gbm_expl), "diabetes_summary.rds")
