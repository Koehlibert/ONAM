rm(list = ls())
library(batchtools)

setwd("/home/koehler/IML")
cluster.functions <-
  makeClusterFunctionsSlurm(
    template = "batchtools.slurm.tmpl",
    array.jobs = TRUE,
    scheduler.latency = 1,
    fs.latency = 65
  )
set.seed(1)

library(MASS)
library(dplyr)
library(ONAM)
library(bench)
#Hyperparameters####
n_sim <- 10
p <- 10
p_inf <- 3
n_inter <- factorial(p_inf) / (2 * factorial(p_inf - 2))
n_vals <- c(2000, 5000)
Effects <- c(1, 2, 3)
sim_setting <- expand.grid(n_vals, Effects)
sim_setting <-
  sim_setting[rep(seq_len(nrow(sim_setting)), each = n_sim),]
sim_setting <- cbind(sim_setting, 1:n_sim)
sim <- function(i) {
  library(MASS)
  library(dplyr)
  library(ONAM)
  setwd("/home/koehler/IML")
  n_sim <- 10
  p <- 10
  p_inf <- 3
  n_inter <- factorial(p_inf) / (2 * factorial(p_inf - 2))
  n_vals <- c(2000, 5000)
  Effects <- c(1, 2, 3)
  sim_setting <- expand.grid(n_vals, Effects)
  sim_setting <-
    sim_setting[rep(seq_len(nrow(sim_setting)), each = n_sim),]
  sim_setting <- cbind(sim_setting, 1:n_sim)
  j <- sim_setting[i, 3]
  n <- sim_setting[i, 1]
  nonLinFIdx <- 3 * (sim_setting[i, 2] - 1) + 1:3
  interFIdx <- 3 * (sim_setting[i, 2] - 1) + 1:3
  trueFileName <-
    paste("./UniformFeatureResults/",
          sim_setting[i, 2],
          "_TRUE.RDS",
          sep = "")
  trueData <- readRDS(trueFileName)
  tmp <- trueData[[j]][[as.character(n)]]
  X_Big <- trueData[[n_sim + 1]]$data
  rm(trueDat)
  originalData <- tmp$data
  #Generate data####
  Y <- rep(0, n)
  for (idx in 1:p_inf)
  {
    nonLinString <- paste("X", idx, sep = "")
    Y <- Y + tmp$finalTotalPredictions[, nonLinString]
  }
  for (idx in 1:n_inter)
  {
    interString <- paste("X",
                         combn(1:p_inf, 2)[, idx],
                         sep = "",
                         collapse = "_")
    Y <- Y + tmp$finalTotalPredictions[, interString]
  }
  globalString = paste("X", 1:p, sep = "",
                       collapse = "_")
  Y <- Y + tmp$finalTotalPredictions[, globalString]
  originalData <- cbind(originalData, Y)
  colnames(originalData) <- c(paste("X", 1:p, sep = ""), "Y")
  modelFormula <-
    formula(
      Y ~
        deep_model(X1) + deep_model(X2) + deep_model(X3) +
        deep_model(X1, X2) + deep_model(X1, X3) + deep_model(X2, X3) +
        deep_model(.)
    )
  list_of_deep_models =
    list(deep_model = ONAM:::get_submodel)
  # callback <-
  #   keras::keras$callbacks$EarlyStopping(monitor = "loss",
  #                                        patience = 10)
  modelRunTime <-
    system.time({
      modelRes <-
        onam(
          modelFormula,
          list_of_deep_models,
          originalData,
          n_ensemble = 4,
          epochs = 300,
          # callback = callback,
          progresstext = FALSE,
          verbose = 0
        )
    })
  modelEvalData <-
    predict(modelRes, X_Big)
  modelEvalData <- c(modelEvalData,
                     modelRunTime = list(modelRunTime))
  SettingString <- paste("n_", n, "_Eff_",
                         paste(nonLinFIdx, collapse = "_"),
                         sep = "")
  resFileName <-
    paste("./UniformFeatureResults/",
          SettingString,
          "_run_",
          j,
          ".RDS",
          sep = "")
  saveRDS(modelEvalData, file = resFileName)
}

reg <- makeRegistry(file.dir = paste0("/home/koehler/IML/results"))

ids <-
  batchMap(fun = sim,
           args = list(i = 1:nrow(sim_setting)),
           reg = reg)

submitJobs(
  ids,
  res = list(
    memory = 25000,
    partition = "batch",
    walltime = 25000
  ),
  reg = reg
)

while (!waitForJobs(ids = ids)) {
  Sys.sleep(1)
}

# results <- reduceResultsList(reg = reg)
# save(results, file = "test.rds")
