#Initial Setup####
rm(list = ls())
gc()
set.seed(111)
library(mvtnorm)
library(MASS)
setwd("//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/ONAM/")
source("SimulationSettings.R")
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
# source("SimulationSettings.R")
# varEstimations <- readRDS("varEstimations.rds")
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
  # for(iSetting in 3:3)
{
  #Simulation setting####
  n <- simSetting[iSetting, 1]
  nonLinFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
  interFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
  sigmaMat <- matrix(0.5, nrow = p, ncol = p)
  diag(sigmaMat) <- 1
  # X_tmp <- mvrnorm(n_Big, rep(0, p), sigmaMat)
  # X_Big <-
  #   apply(X_tmp, 2,
  #         function(x) pnorm(x) * 6 - 3)
  # colnames(X_Big) <- paste("X", 1:p, sep = "")
  # #purify effects to get only nonlinear part#####
  # interFPre <- lapply(1:n_inter, function(i)
  # {
  #   function(x1, x2) interf[[interFIdx[i]]](x1, x2) #/ sqrt(interFVars[interFIdx[i]])
  #   # ONAM:::purifyInterFunction(i, interf = interf,
  #   #                            interFIdx = interFIdx, X = X_Big)
  # })
  # nonLinFPre <- lapply(1:p_inf, function(i)
  # {
  #   function(x) lotf[[nonLinFIdx[i]]](x) #/ sqrt(lotFVars[interFIdx[i]])
  #   # ONAM:::purifyNonLinFunction(i, lotf = lotf, nonLinFIdx = nonLinFIdx,
  #   #                             X = X_Big)
  # })
  # PHOList <-
  #   ONAM:::stackedOrthFunction(nonLinFPre, interFPre, 0.1,
  #                              X_Big, globalTerm)
  # nonLinF <- lapply(1:p_inf,
  #                   function(i)
  #                     scale(PHOList$nonLinF[[i]] /
  #                             sqrt(varEstimations[nonLinFIdx[i]]), scale = FALSE))
  # interF <- lapply(1:n_inter,
  #                  function(i)
  #                    scale(PHOList$interF[[i]] /
  #                            sqrt(varEstimations[9 + interFIdx[i]]), scale = FALSE))
  # globalF <- scale(PHOList$globalF / sqrt(varEstimations[19 + iSetting %/% 3]),
  #                  scale = FALSE)
  # # noisesd <- simSetting[iSetting, 3]
  # # SettingString <- paste("n_", n, "_Eff_",
  # #                        paste(nonLinFIdx, collapse = "_"),
  # #                        "_sd_", noisesd, sep = "")
  # trueDF <-
  #   data.frame(matrix(c(X_Big[,1:3],
  #                       unlist(nonLinF), unlist(interF), globalF),
  #                     ncol = 10))
  # SettingString <- paste("n_", n, "_Eff_",
  #                        paste(nonLinFIdx, collapse = "_"),
  #                        sep = "")
  # trueFileName <- paste("./UniformFeatureResults_New/", SettingString,
  #                       "_TRUE.RDS", sep = "")
  # saveRDS(trueDF, trueFileName)
  trueFileName <- paste("./UniformFeatureResults_New/", (iSetting + 1) %/% 2,
                        "_TRUE.RDS", sep = "")
  trueData <- readRDS(trueFileName)
  for(j in 1:nSim)
  {
    # if(iSetting == 17 & j < 5)
    # {
    #   next
    # }
    progressPercent <- ((iSetting - 1)*nSim + j)/
      (nrow(simSetting) * nSim)*100
    # progressPercent <- ((iSetting - 1)*nSim + j)/
    # (3 * nSim)*100
    cat('\r',paste0(progressPercent, "% complete"))
    flush.console()
    #create data####
    # x_sample <- 1:n_Big
    # x_sample <- sample(1:n_Big, n)
    tmp <- trueData[[j]][[as.character(n)]]
    originalData <- cbind(tmp$data, Y = tmp$totalPredictions)
    #Generate data####
    # Y <- rep(0, n_Big)
    # # Y <- X %*% trueBetas
    # for(idx in 1:p_inf)
    # {
    #   Y <- Y + nonLinF[[idx]]
    # }
    # for(idx in 1:n_inter)
    # {
    #   Y <- Y + interF[[idx]]
    # }
    # Y <- Y + globalF
    # Y_all <- Y
    # Y <- Y[x_sample]
    # if(noisy)
    # {
    #   noise <- rnorm(n, sd = noisesd)
    #   Y <- Y + noise
    # }
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
    modelEvalData <-
      ONAM:::evaluateModelSimulation(modelRes, tmp$data, tmp$totalPredictions)
    modelEvalData <- c(modelEvalData,
                       modelRunTime = list(modelRunTime))
    SettingString <- paste("n_", n, "_Eff_",
                           paste(nonLinFIdx, collapse = "_"),
                           sep = "")
    resFileName <- paste("./UniformFeatureResults_New/", SettingString, "_run_", j, ".RDS", sep = "")
    saveRDS(modelEvalData, file = resFileName)
  }
}
