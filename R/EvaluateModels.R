compareEstimationsPHOEffect <- function(modelEvalData,
                                        FeatureNumber)
{
  originalData <- modelEvalData$data
  feature <- paste("X", FeatureNumber, sep = "")
  plotData <-
    data.frame(x = rep(modelEvalData$data[,feature], 3),
               y = c(modelEvalData$trueEffects[[feature]],
                     modelEvalData$totalFeaturePredsPost[[feature]],
                     modelEvalData$totalFeaturePredsPre[[feature]]),
               Type = rep(c("True", "Post PHO", "Pre PHO"),
                          each = nrow(originalData)))
  ggplot(plotData, aes(x = x, y = y, color = Type)) + geom_line()
}
predict_PHO <- function(PHOModelList, data = NULL)
{
  if(is.null(data))
  {
    data <-
      ONAM:::prepareData(PHOModelList$data,
                         PHOModelList$modelInfoList)
  }
  modelIdxList <-
    ONAM:::getModelIdxList(PHOModelList$modelInfoList)
  sepPredictions <-
    lapply(PHOModelList$PHOEnsemble,
           function(PHOModel)
           {
             U_Object <-
               ONAM:::getU(PHOModel$modelList, modelIdxList,
                           PHOModelList$modelInfoList, data)
             W <- PHOModel$W_List %>% unlist() %>%
               matrix(ncol = length(PHOModel$W_List)) %>%
               rowSums()
             output <- U_Object$U %*% W
             return(output)
           })
  allPreds <- sepPredictions %>% unlist() %>%
    matrix(ncol = length(PHOModelList$PHOEnsemble)) %>%
    rowMeans()
  return(allPreds)
}
evaluateSingleModel <- function(PHOModel, data, modelInfoList)
{
  fitData <- ONAM:::prepareData(data, modelInfoList)
  modelIdxList <- ONAM:::getModelIdxList(modelInfoList)
  U_Object <-
    getU(PHOModel$modelList, modelIdxList, modelInfoList, fitData)
  subModelPredictions <-
    lapply(PHOModel$W_List,
           function(W)
             U_Object$U %*% W)
  subModelPredictionsOld <-
    lapply(PHOModel$W_List_old,
           function(W)
             U_Object$U %*% W)
  modelNames <- list()
  for(orderIdx in seq_along(modelInfoList$theta))
  {
    for(modelIdx in seq_along(modelInfoList$theta[[orderIdx]]))
    {
      tmpName <-
        paste(unlist(modelInfoList$theta[[orderIdx]][[modelIdx]]),
              collapse = "_")
      if(names(modelInfoList$theta)[orderIdx] == "Linear")
      {
        tmpName <- paste(tmpName, "_Linear", sep = "")
      }
      modelNames <- c(modelNames, tmpName)
    }
  }
  names(subModelPredictions) <- modelNames
  names(subModelPredictionsOld) <- modelNames
  return(list(subModelPredictions = subModelPredictions,
              subModelPredictionsOld = subModelPredictionsOld))
}
#' Evaluate orthogonal neural additive model
#' @param PHOModelList Orthogonal neural additive model ensemble object to be evaluated
#' @returns Returns a list containing data, model output for each observation in `data` and main and interaction effects obtained by the model
#' @export
evaluateModel <- function(PHOModelList)
{
  data <- PHOModelList$data
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(data)
  nEnsemble <- length(PHOModelList$PHOEnsemble)
  separatePredictions <-
    lapply(PHOModelList$PHOEnsemble, ONAM:::evaluateSingleModel,
           data = data, modelInfoList = modelInfoList)
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
  return(list(data = data,
              totalPredictions = totalPredictions,
              finalTotalPredictions = finalTotalPredictions))
}
evaluateModelSimulation <- function(PHOModelList, X_Big, Y)
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
  return(list(data = cbind(X_Big, Y),
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre,
              finalTotalPredictions = finalTotalPredictions,
              interpretMeasures = interpretMeasureList))
}
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
