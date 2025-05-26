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
nSim <- 10
p <- 10
p_inf <- 3
n_inter <- factorial(p_inf) / (2 * factorial(p_inf - 2))
nVals <- c(2000, 5000)
Effects <- c(1, 2, 3)
simSetting <- expand.grid(nVals, Effects)
sim_setting <-
  sim_setting[rep(seq_len(nrow(sim_setting)), each = n_sim), ]
sim <- function(i) {
  library(MASS)
  library(dplyr)
  library(ONAM)
  nSim <- 10
  p <- 10
  p_inf <- 3
  n_inter <- factorial(p_inf) / (2 * factorial(p_inf - 2))
  nVals <- c(2000, 5000)
  Effects <- c(1, 2, 3)
  simSetting <- expand.grid(nVals, Effects)
  sim_setting <-
    sim_setting[rep(seq_len(nrow(sim_setting)), each = n_sim), ]
  n <- sim_setting[i, 1]
  nonLinFIdx <- 3 * (simSetting[i_setting, 2] - 1) + 1:3
  interFIdx <- 3 * (simSetting[i_setting, 2] - 1) + 1:3
  trueFileName <-
    paste("./UniformFeatureResults/",
          (i_setting + 1) %/% 2,
          "_TRUE.RDS",
          sep = "")
  trueData <- readRDS(trueFileName)
  tmp <- trueData[[j]][[as.character(n)]]
  X_Big <- trueData[[nSim + 1]]$data
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
  callback <-
    keras::keras$callbacks$EarlyStopping(monitor = "loss",
                                         patience = 10)
  modelRunTime <-
    system.time({
      modelRes <-
        onam(
          modelFormula,
          list_of_deep_models,
          originalData,
          n_ensemble = 4,
          epochs = 200,
          callback = callback,
          progresstext = FALSE,
          verbose = 1
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
    memory = 10000,
    partition = "batch",
    walltime = 10000
  ),
  reg = reg
)

while (!waitForJobs(ids = ids)) {
  Sys.sleep(1)
}

# results <- reduceResultsList(reg = reg)
# save(results, file = "test.rds")
