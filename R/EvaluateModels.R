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
evaluateModel <- function(PHOModelList, nonLinF, simSetting,
                          modelRunTime)
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
               filter(PHO == "After", Effect == effect) %>%
               group_by(Observation) %>%
               summarise(totalEffect = mean(y)) %>%
               select(totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPost) <-
    effectNames
  totalPredictions <-
    predictionsData %>%
    filter(PHO == "After") %>%
    group_by(Observation) %>%
    summarise(Prediction = sum(y)/nEnsemble) %>%
    select(Prediction) %>% unlist()
  totalFeaturePredsPre <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               filter(PHO == "Pre", Effect == effect) %>%
               group_by(Observation) %>%
               summarise(totalEffect = mean(y)) %>%
               select(totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPre) <-
    effectNames
  FeatureNames <- c("X1", "X2", "X3")
  effectErrorsPost <-
    lapply(seq_along(FeatureNames),
           function(featureIdx)
           {
             tmpErr <-
               (nonLinF[[featureIdx]] -
                  totalFeaturePredsPost[[FeatureNames[[featureIdx]]]])^2
           })
  effectErrorsPre <-
    lapply(seq_along(FeatureNames),
           function(featureIdx)
           {
             tmpErr <-
               (nonLinF[[featureIdx]] -
                  totalFeaturePredsPre[[FeatureNames[[featureIdx]]]])^2
           })
  names(nonLinF) <- FeatureNames
  return(list(data = data,
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre,
              effectErrorsPost = effectErrorsPost,
              effectErrorsPre = effectErrorsPre,
              trueEffects = nonLinF,
              simSetting = simSetting,
              modelRunTime = modelRunTime))
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
#' @export
evaluateModelGenericPre <- function(PHOModelList)
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
  return(list(data = data,
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre))
}
#' @export
evaluateModelGeneric <- function(PHOModelList)
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
  return(list(data = data,
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre,
              finalTotalPredictions = finalTotalPredictions))
}
#' @export
compareEstimationsPHOEffectGeneric <- function(modelEvalData,
                                               feature)
{
  originalData <- modelEvalData$data
  plotData <-
    data.frame(x = rep(modelEvalData$data[,feature], 2),
               y = c(modelEvalData$totalFeaturePredsPost[[feature]],
                     modelEvalData$totalFeaturePredsPre[[feature]]),
               Type = rep(c("Post PHO", "Pre PHO"),
                          each = nrow(originalData)))
  ggplot(plotData, aes(x = x, y = y, color = Type)) + geom_line()
}
#' @export
plotEffectGenericPost <- function(modelEvalData,
                                  feature)
{
  originalData <- modelEvalData$data
  plotData <-
    data.frame(x = modelEvalData$data[,feature],
               y = modelEvalData$finalTotalPredictions[,feature])
  ggplot(plotData, aes(x = x, y = y)) + geom_line()
}
#' @export
plotEffectGenericPre <- function(modelEvalData,
                                 feature)
{
  originalData <- modelEvalData$data
  plotData <-
    data.frame(x = modelEvalData$data[,feature],
               y = modelEvalData$totalFeaturePredsPre[[feature]])
  ggplot(plotData, aes(x = x, y = y)) + geom_line()
}
#' @export
plotInteractionEffectGenericPost <- function(modelEvalData,
                                             feature1, feature2)
{
  modelName <-
    paste(feature1, feature2,
          sep = "_")
  originalData <- modelEvalData$data
  plotData <-
    data.frame(x = modelEvalData$data[,feature1],
               y = modelEvalData$data[,feature2],
               Prediction =
                 modelEvalData$finalTotalPredictions[,modelName])
  ggplot(plotData, aes(x = x, y = y, color = Prediction)) +
    geom_point(size = 0.75) +
    scale_color_gradientn(colors = c("red", "yellow", "green"),
                          values =
                            scales::rescale(c(min(plotData$Prediction),
                                              mean(plotData$Prediction),
                                              max(plotData$Prediction))),
                          guide = "colorbar",
                          limits = c(min(plotData$Prediction),
                                     max(plotData$Prediction))
    )
}
#' @export
plotInteractionEffectGenericPre <- function(modelEvalData,
                                            feature1, feature2)
{
  modelName <-
    paste(feature1, feature2,
          sep = "_")
  originalData <- modelEvalData$data
  plotData <-
    data.frame(x = modelEvalData$data[,feature1],
               y = modelEvalData$data[,feature2],
               Prediction =
                 modelEvalData$totalFeaturePredsPre[[modelName]])
  ggplot(plotData, aes(x = x, y = y, color = Prediction)) +
    geom_point(size = 0.75) +
    scale_color_gradientn(colors = c("red", "yellow", "green"),
                          values =
                            rescale(c(min(plotData$Prediction),
                                      mean(plotData$Prediction),
                                      max(plotData$Prediction))),
                          guide = "colorbar",
                          limits = c(min(plotData$Prediction),
                                     max(plotData$Prediction))
    )
}
#' @export
plotSingleModels <- function(modelEvalData, feature)
{
  originalData <- modelEvalData$data
  n <- nrow(originalData)
  singlePredictions <- modelEvalData$predictionsData %>%
    filter(Effect == feature, PHO == "After") %>%
    select(y) %>% unlist()
  nEnsemble <- length(singlePredictions) / n
  plotData <-
    data.frame(x = rep(modelEvalData$data[,feature], nEnsemble + 1),
               y = c(c(singlePredictions),
                     modelEvalData$finalTotalPredictions[,feature]),
               subModel =
                 as.factor(rep(1:(nEnsemble + 1), each = n)))
  tmpX <- plotData$x[1:n]
  xGrid <- seq(min(tmpX),
               max(tmpX),
               length.out = 101)
  xDens <- sapply(1:100,
                  function(idx)
                    sum(tmpX > xGrid[idx - 1] &
                          tmpX < xGrid[idx]))
  rectData <- data.frame(xStart = xGrid[1:100],
                         xEnd = xGrid[2:101],
                         alpha = xDens,
                         x = (xGrid[1:100] - xGrid[2:101]) / 2,
                         w = diff(xGrid))
  plot <- ggplot(plotData, aes(x = x, y = y, alpha = subModel)) +
    geom_line(color = "blue") +
    scale_alpha_manual(values = c(rep(0.125, nEnsemble), 1)) +
    theme_bw() + theme(legend.position = "none") +
    # geom_rect(data =rectData,
    #           mapping = aes(xmin = xStart,
    #                         xmax = xEnd,
    #                         ymin = -Inf,
    #                         ymax = Inf,
    #                         alpha = alpha),
    #           show.legend = FALSE) +
    # geom_rug(sides = "b", alpha = 0.05,
    #          linetype = 1,
    #          linewidth = 0.25) +
    xlab(feature) +
    ylab("Effect")
  return(plot)
}
#' @export
getVarDecomp <- function(modelEvalData)
{
  n <- length(modelEvalData$totalPredictions)
  effectNames <- unique(modelEvalData$predictionsData$Effect)
  nEnsemble <- modelEvalData$predictionsData %>%
    filter(Effect == effectNames[1]) %>%
    filter(PHO == "After") %>% nrow() / n
  varMatrix <- matrix(0,
                      nrow = length(effectNames),
                      ncol = length(effectNames))
  tmpData <- modelEvalData$predictionsData %>%
    filter(PHO == "After") %>%
    select(-PHO)
  for(modelIdx in 1:nEnsemble)
  {
    relData <- tmpData %>%
      filter(Model == modelIdx) %>%
      select(-Model) %>%
      tidyr::pivot_wider(id_cols = Observation,
                         names_from = Effect,
                         values_from = y) %>%
      select(-Observation)
    tmpVar <- var(relData)
    relVarMatrix <- abs(tmpVar) / sum(abs(tmpVar))
    varMatrix <- varMatrix + relVarMatrix
  }
  # effectModelVar <- modelEvalData$predictionsData %>%
  #   filter(PHO == "After") %>%
  #   mutate(model = rep(1:nEnsemble,
  #                      each = n * length(effectNames)),
  #                      ) %>%
  #   group_by(Effect, model) %>%
  #   summarise(variance = var(y))
  # totalVar <- effectModelVar %>%
  #   group_by(model) %>%
  #   summarise(totalVar = sum(abs(variance)))
  # varPercentage <- effectModelVar %>%
  #   left_join(totalVar, by = "model") %>%
  #   mutate(variancePercentage = abs(variance) / totalVar)
  # varDecomposition <- varPercentage %>%
  #   group_by(Effect) %>%
  #   summarise(test = sum(variancePercentage)/nEnsemble)
}
