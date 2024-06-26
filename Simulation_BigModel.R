#Initial Setup####
rm(list = ls())
gc()
set.seed(111)
library(mvtnorm)
library(MASS)
setwd("//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/ONAM/")
#Hyperparameters####
nSim <- 10
p <- 10
n_Big <- 10000
p_inf <- 3
n_inter <- factorial(p_inf)/(2*factorial(p_inf - 2))
# AllNet <- TRUE
# noisy <- TRUE
# regularize <- TRUE
nVals <- c(2000, 5000)
Effects <- c(1,2,3)
# noiseVals <- c(0.1,0.5,1)
# simSetting <- expand.grid(nVals, Effects, noiseVals)
simSetting <- expand.grid(nVals, Effects)
#DGP####
# betasToSample <- seq(-3, 3, by = 0.5)[-7]
source("SimulationSettings.R")
varEstimations <- readRDS("varEstimations.rds")
# Sigma <- matrix(0.5, ncol = p, nrow = p)
# diag(Sigma) <- 1
# Z <- mvrnorm(1000000, mu = rep(0,p), Sigma = Sigma)
# X <- (pnorm(Z) - 0.5) * 6
# lotFVars <-
#   sapply(1:length(lotf), function(i)
#     var(lotf[[i]](X[,i])))
# interFVars <- sapply(1:length(interf), function(i)
#   var(interf[[i]](X[,switch(i, 1,1,2,4,4,5,7,7,8)],
#                   X[,switch(i, 2,3,3,5,6,6,8,9,9)])))
# globalTermVar <- var(apply(X, 1, globalTerm))
# cor(X) # ca. 0.48
#Model regularization####

#Run Simulation####
for(iSetting in 1:nrow(simSetting))
# for(iSetting in 5:nrow(simSetting))
{
  #Simulation setting####
  n <- simSetting[iSetting, 1]
  nonLinFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
  interFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
  trueFileName <- paste("./UniformFeatureResults_New/", (iSetting + 1) %/% 2,
                        "_TRUE.RDS", sep = "")
  trueData <- readRDS(trueFileName)
  effectVars <- diag(var(trueData[[nSim + 1]][,11:18]))
  for(j in 1:nSim)
  {
    if(iSetting == 5 & j < 6)
    {
      next
    }
    progressPercent <- ((iSetting - 1)*nSim + j)/
      (nrow(simSetting) * nSim)*100
    cat('\r',paste0(progressPercent, "% complete"))
    flush.console()
    #create data####
    # x_sample <- 1:n_Big
    # x_sample <- sample(1:n_Big, n)
    # originalData = X_Big[x_sample,]
    tmp <- trueData[[j]][[as.character(n)]]
    originalData <- tmp$data
    #Generate data####
    Y <- rep(0, n)
    # Y <- X %*% trueBetas
    tmp <- trueData[[nSim]][[as.character(n)]]
    for(idx in 1:p_inf)
    {
      nonLinString <- paste("X", idx, sep = "")
      Y <- Y + tmp$finalTotalPredictions[, nonLinString] / sqrt(effectVars[nonLinString])
    }
    for(idx in 1:n_inter)
    {
      interString <- paste("X", combn(1:p_inf, 2)[,idx], sep = "",
                           collapse = "_")
      Y <- Y + tmp$finalTotalPredictions[, interString] / sqrt(effectVars[interString])
    }
    globalString = paste("X", 1:p, sep = "",
                       collapse = "_")
    Y <- Y + tmp$finalTotalPredictions[, globalString] / sqrt(effectVars[globalString])
    # if(noisy)
    # {
    #   noise <- rnorm(n, sd = noisesd)
    #   Y <- Y + noise
    # }
    originalData <- cbind(originalData, Y)
    colnames(originalData) <- c(paste("X", 1:p, sep = ""), "Y")
    modelFormula <-
      formula(Y ~
                deep_model(X1) + deep_model(X2) + deep_model(X3) +
                deep_model(X1, X2) + deep_model(X1, X3) + deep_model(X2, X3) +
                deep_model(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10))
    list_of_deep_models =
      list(deep_model = ONAM:::getSubModel,
           deep_model1 = function(inputs)
             ONAM:::getSubModel(inputs, regularizer = keras::regularizer_l1_l2(0.001, 0.001)),
           deep_model2 = function(inputs)
             ONAM:::getSubModel(inputs, regularizer = keras::regularizer_l1_l2(0.0125, 0.0125)),
           deep_model10 = function(inputs)
             ONAM:::getSubModel(inputs, regularizer = keras::regularizer_l1_l2(0.05, 0.05))
      )
    modelRunTime <-
      system.time({modelRes <-
        ONAM:::fitPHOModel(modelFormula, list_of_deep_models,
                           originalData, 10)})
    X_Big <- tmp$data
    modelEvalData <-
      ONAM:::evaluateModelSimulation(modelRes, X_Big, Y)
    modelEvalData <- c(modelEvalData,
                       modelRunTime = list(modelRunTime))
    SettingString <- paste("n_", n, "_Eff_",
                           paste(nonLinFIdx, collapse = "_"),
                           sep = "")
    resFileName <- paste("./UniformFeatureResults_New/", SettingString, "_run_", j, ".RDS", sep = "")
    saveRDS(modelEvalData, file = resFileName)
  }
}
