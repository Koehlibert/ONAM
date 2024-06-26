rm(list = ls())
gc()
library(MASS)
library(dplyr)
setwd("//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/ONAM/")
source("SimulationSettings.R")
n_big <- 20000
n_sim <- 10
p <- 10
n_inter <- 3
p_inf <- 3
Sigma <- matrix(0.5, ncol = p, nrow = p)
diag(Sigma) <- 1
mu_vec <- rep(0,p)
evaluateModelNewData <- function(PHOModelList, X_Big)
{
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(X_Big)
  nEnsemble <- length(PHOModelList$PHOEnsemble)
  separatePredictions <-
    lapply(PHOModelList$PHOEnsemble, ONAM:::evaluateSingleModel,
           data = X_Big, modelInfoList = modelInfoList)
  effectNames <- names(separatePredictions[[1]][[1]])
  nEffects <- length(effectNames)
  predictionsData <-
    data.frame(y = unlist(separatePredictions),
               Effect =
                 rep(rep(effectNames,
                         each = n),
                     2 * nEnsemble),
               PHO = rep(rep(c("After", "Before"),
                             each = n * nEffects),
                         nEnsemble),
               Observation = rep(1:n, nEffects * 2 * nEnsemble),
               Model = rep(1:nEnsemble, each = n * 2 * nEffects))
  totalFeaturePredsPost <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(PHO == "After", Effect == effect) %>%
               dplyr::group_by(Observation) %>%
               dplyr::summarise(totalEffect = mean(y)) %>%
               dplyr::select(totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPost) <-
    effectNames
  finalTotalPredictions <-
    unlist(totalFeaturePredsPost) %>%
    matrix(nrow = n) %*%
    PHOModelList$finalW
  colnames(finalTotalPredictions) <-
    effectNames
  totalPredictions <-
    predictionsData %>%
    dplyr::filter(PHO == "After") %>%
    dplyr::group_by(Observation) %>%
    dplyr::summarise(Prediction = sum(y)/nEnsemble) %>%
    dplyr::select(Prediction) %>% unlist()
  totalFeaturePredsPre <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(PHO == "Pre", Effect == effect) %>%
               dplyr::group_by(Observation) %>%
               dplyr::summarise(totalEffect = mean(y)) %>%
               dplyr::select(totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPre) <-
    effectNames
  Var <- var(PHOModelList$finalOutputs)
  totalVar <- sum(Var)
  resVar <- Var[1,1]
  interVar <- sum(Var[2:4, 2:4])
  mainVar <- sum(Var[5:7,5:7])
  interVarPercent <- interVar/totalVar
  mainVarPercent <- mainVar/totalVar
  interpretPercent <- mainVarPercent + interVarPercent
  interpretMeasureList <- list(mainVarPercent,
                               interVarPercent,
                               interpretPercent)
  return(list(data = cbind(X_Big),
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre,
              finalTotalPredictions = finalTotalPredictions,
              interpretMeasures = interpretMeasureList))
}
for(iSetting in 1:3)
{
  nonLinFIdx <- 3 * (iSetting - 1) + 1:3
  interFIdx <- 3 * (iSetting - 1) + 1:3
  Z <- mvrnorm(n_big, mu = mu_vec, Sigma = Sigma)
  X <- (pnorm(Z) - 0.5) * 6
  Y <- 0
  interFPre <- lapply(1:n_inter, function(i)
  {
    function(x1, x2) interf[[interFIdx[i]]](x1, x2) #/ sqrt(interFVars[interFIdx[i]])
    # ONAM:::purifyInterFunction(i, interf = interf,
    #                            interFIdx = interFIdx, X = X_Big)
  })
  nonLinFPre <- lapply(1:p_inf, function(i)
  {
    function(x) lotf[[nonLinFIdx[i]]](x) #/ sqrt(lotFVars[interFIdx[i]])
    # ONAM:::purifyNonLinFunction(i, lotf = lotf, nonLinFIdx = nonLinFIdx,
    #                             X = X_Big)
  })
  PHOList <-
    ONAM:::stackedOrthFunction(nonLinFPre, interFPre, 0.1,
                               X, globalTerm)
  nonLinF <- lapply(1:p_inf, function(i) PHOList$nonLinF[[i]])
  interF <- lapply(1:n_inter, function(i) PHOList$interF[[i]])
  globalF <- PHOList$globalF
  Y <- rep(0, n_big)
  # Y <- X %*% trueBetas
  for(idx in 1:p_inf)
  {
    Y <- Y + nonLinF[[idx]]
  }
  for(idx in 1:n_inter)
  {
    Y <- Y + interF[[idx]]
  }
  Y <- Y + globalF
  originalData <- cbind(X, Y)
  colnames(originalData) <- c(paste("X", 1:p, sep = ""), "Y")
  modelFormula <-
    formula(Y ~
              deep_model(X1) + deep_model(X2) + deep_model(X3) +
              deep_model(X1, X2) + deep_model(X1, X3) + deep_model(X2, X3) +
              deep_model(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10))
  list_of_deep_models =
    list(deep_model = ONAM:::getSubModel)
  modelRunTime <-
    system.time({modelRes <-
      ONAM:::fitPHOModel(modelFormula, list_of_deep_models,
                         originalData, 5, progresstext = TRUE,
                         verbose = 1)})
  trainDataList <- list()
  modelEvalData <-
    ONAM:::evaluateModelSimulation(modelRes, originalData, Y)
  # effectVars <- diag(var(modelEvalData$finalTotalPredictions))
  for (i_sim in 1:n_sim)
  {
    trainDataList[[i_sim]] <- list()
    for(n_val in c(2000, 5000))
    {
      X_tmp <- mvrnorm(n_val, mu = mu_vec, Sigma = Sigma)
      X_Train <- (pnorm(X_tmp) - 0.5) * 6
      colnames(X_Train) <- paste("X", 1:p, sep = "")
      Train_Data <- evaluateModelNewData(modelRes, X_Train)
      trainDataList[[i_sim]][[as.character(n_val)]] <- Train_Data
    }
  }
  trueDF <- cbind(originalData, modelEvalData$finalTotalPredictions)
  trainDataList[[n_sim + 1]] <- trueDF
  trueFileName <- paste("./UniformFeatureResults_New/", iSetting,
                        "_TRUE.RDS", sep = "")
  saveRDS(trainDataList, trueFileName)
}
