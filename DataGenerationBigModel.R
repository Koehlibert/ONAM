rm(list = ls())
gc()
library(MASS)
library(dplyr)
library(ONAM)
source("SimulationSettings.R")
n_big <- 50000
n_sim <- 10
p <- 10
n_inter <- 3
p_inf <- 3
Sigma <- matrix(0.5, ncol = p, nrow = p)
diag(Sigma) <- 1
mu_vec <- rep(0, p)
evaluateModelNewData <- function(PHOModelList, X_Big)
{
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(X_Big)
  nEnsemble <- length(PHOModelList$PHOEnsemble)
  separatePredictions <-
    lapply(
      PHOModelList$PHOEnsemble,
      ONAM:::evaluateSingleModel,
      data = X_Big,
      modelInfoList = modelInfoList
    )
  effectNames <- names(separatePredictions[[1]][[1]])
  nEffects <- length(effectNames)
  predictionsData <-
    data.frame(
      y = unlist(separatePredictions),
      Effect =
        rep(rep(effectNames,
                each = n),
            2 * nEnsemble),
      PHO = rep(rep(c("After", "Before"),
                    each = n * nEffects),
                nEnsemble),
      Observation = rep(1:n, nEffects * 2 * nEnsemble),
      Model = rep(1:nEnsemble, each = n * 2 * nEffects)
    )
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
    dplyr::summarise(Prediction = sum(y) / nEnsemble) %>%
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
  resVar <- Var[1, 1]
  interVar <- sum(Var[2:4, 2:4])
  mainVar <- sum(Var[5:7, 5:7])
  interVarPercent <- interVar / totalVar
  mainVarPercent <- mainVar / totalVar
  interpretPercent <- mainVarPercent + interVarPercent
  interpretMeasureList <- list(mainVarPercent,
                               interVarPercent,
                               interpretPercent)
  return(
    list(
      data = cbind(X_Big),
      totalPredictions = totalPredictions,
      predictionsData = predictionsData,
      totalFeaturePredsPost = totalFeaturePredsPost,
      totalFeaturePredsPre = totalFeaturePredsPre,
      finalTotalPredictions = finalTotalPredictions,
      interpretMeasures = interpretMeasureList
    )
  )
}
stackedOrthFunction <-
  function(nonLinFPre,
           interFPre,
           r,
           original_data,
           globalTerm) {
    nonLinMatList <-
      lapply(seq_along(nonLinFPre),
             function(idx)
             {
               nonLinFPre[[idx]](original_data[, idx])
             })
    nonLinMat <-
      matrix(unlist(nonLinMatList), ncol = length(nonLinMatList))
    IntMatList <-
      lapply(seq_along(interFPre),
             function(idx)
             {
               relData <- original_data[, combn(1:p_inf, 2)[, idx]]
               x1 <- relData[, 1]
               x2 <- relData[, 2]
               y <- interFPre[[idx]](x1, x2)
               mod <- mgcv::gam(y ~ 1 + s(x1) + s(x2),
                                data = data.frame(
                                  y = interf[[interFIdx[idx]]](x1, x2),
                                  x1 = x1,
                                  x2 = x2
                                ))
               return(mod$residuals)
             })
    IntMat <- matrix(unlist(IntMatList), ncol = length(IntMatList))
    globalTermValues <- apply(original_data, 1, globalTerm)
    DesMat <- cbind(1, nonLinMat, IntMat)
    P <- DesMat %*% solve(t(DesMat) %*% DesMat) %*% t(DesMat)
    z1z2 <-
      solve(t(DesMat) %*% DesMat) %*% t(DesMat) %*% globalTermValues
    PHOglobalTermValues <- globalTermValues - P %*% globalTermValues
    PHODesMat <- DesMat + sweep(DesMat, 2, rowSums(z1z2), "*")
    #Orthog Inter Vs NonLin
    IntMat2 <- PHODesMat[, 5:7]
    DesMat2 <- PHODesMat[, 1:4]
    P2 <- DesMat2 %*% solve(t(DesMat2) %*% DesMat2) %*% t(DesMat2)
    z1z2_2 <-
      solve(t(DesMat2) %*% DesMat2) %*% t(DesMat2) %*% IntMat2
    PHOIntMat2 <- IntMat2 - P2 %*% IntMat2
    PHODesMat2 <- DesMat2 + sweep(DesMat2, 2, rowSums(z1z2_2), "*")
    finalPHOMat <-
      cbind(PHODesMat2[, 2:4], PHOIntMat2, PHOglobalTermValues)
    nonLinF <-
      lapply(seq_along(nonLinFPre),
             function(idx)
               finalPHOMat[, idx])
    interF <-
      lapply(seq_along(interFPre),
             function(idx)
               finalPHOMat[, 3 + idx])
    return(list(
      nonLinF = nonLinF,
      interF = interF,
      globalF = finalPHOMat[, 7]
    ))
  }
for (i_setting in 1:3)
{
  nonLinFIdx <- 3 * (i_setting - 1) + 1:3
  interFIdx <- 3 * (i_setting - 1) + 1:3
  Z <- mvrnorm(n_big, mu = mu_vec, Sigma = Sigma)
  X <- (pnorm(Z) - 0.5) * 6
  Y <- 0
  interFPre <- lapply(1:n_inter, function(i)
  {
    function(x1, x2)
      interf[[interFIdx[i]]](x1, x2) #/ sqrt(interFVars[interFIdx[i]])
    # ONAM:::purifyInterFunction(i, interf = interf,
    #                            interFIdx = interFIdx, X = X_Big)
  })
  nonLinFPre <- lapply(1:p_inf, function(i)
  {
    function(x)
      lotf[[nonLinFIdx[i]]](x) #/ sqrt(lotFVars[interFIdx[i]])
    # ONAM:::purifyNonLinFunction(i, lotf = lotf, nonLinFIdx = nonLinFIdx,
    #                             X = X_Big)
  })
  PHOList <-
    stackedOrthFunction(nonLinFPre, interFPre, 0.1,
                        X, globalTerm)
  nonLinF <- lapply(1:p_inf, function(i)
    PHOList$nonLinF[[i]])
  interF <- lapply(1:n_inter, function(i)
    PHOList$interF[[i]])
  globalF <- PHOList$globalF
  Y <- rep(0, n_big)
  # Y <- X %*% trueBetas
  for (idx in 1:p_inf)
  {
    Y <- Y + nonLinF[[idx]]
  }
  for (idx in 1:n_inter)
  {
    Y <- Y + interF[[idx]]
  }
  Y <- Y + globalF
  original_data <- cbind(X, Y)
  colnames(original_data) <- c(paste("X", 1:p, sep = ""), "Y")
  model_formula <-
    formula(
      Y ~
        deep_model(X1) + deep_model(X2) + deep_model(X3) +
        deep_model(X1, X2) + deep_model(X1, X3) + deep_model(X2, X3) +
        deep_model(.)
    )
  list_of_deep_models =
    list(deep_model = ONAM:::get_submodel)
  modelRunTime <-
    system.time({
      modelRes <-
        onam(
          model_formula,
          list_of_deep_models,
          original_data,
          n_ensemble = 1,
          epochs = 5,
          progresstext = TRUE,
          verbose = 1
        )
    })
  trainDataList <- list()
  modelEvalData <-
    predict(modelRes, original_data)
  # effectVars <- diag(var(modelEvalData$finalTotalPredictions))
  for (i_sim in 1:n_sim)
  {
    trainDataList[[i_sim]] <- list()
    for (n_val in c(2000, 5000))
    {
      X_tmp <- mvrnorm(n_val, mu = mu_vec, Sigma = Sigma)
      X_Train <- (pnorm(X_tmp) - 0.5) * 6
      colnames(X_Train) <- paste("X", 1:p, sep = "")
      Train_Data <- predict(modelRes, X_Train)
      trainDataList[[i_sim]][[as.character(n_val)]] <- Train_Data
    }
  }
  trainDataList[[n_sim + 1]] <- modelEvalData
  trueFileName <- paste("./UniformFeatureResults/", i_setting,
                        "_TRUE.RDS", sep = "")
  saveRDS(trainDataList, trueFileName)
}
