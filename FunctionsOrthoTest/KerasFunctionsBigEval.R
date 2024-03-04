compareEstimations <- function(PHOModelList, trueEffectList,
                               FeatureNumber)
{
  feature = paste("X", FeatureNumber, sep = "")
  plot(originalData[,feature], nonLinF[[FeatureNumber]])
  points(originalData[,feature], predict_Feature(PHOModelList, feature), 
         col = "steelblue")
}
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
evaluateModel <- function(PHOModelList, nonLinF, simSetting,
                          modelRunTime)
{
  data <- PHOModelList$data
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(data)
  totalPredictions <- PHOModelList %>% predict_PHO()
  separatePredictions <- 
    lapply(PHOModelList$PHOEnsemble, evaluateSingleModel, 
           data = data, modelInfoList = modelInfoList)
  predictionsData <- 
    data.frame(y = unlist(separatePredictions),
               Effect = 
                 rep(rep(names(separatePredictions[[1]][[1]]), 
                         each = n),
                     length(separatePredictions)),
               PHO = rep(rep(c("After", "Before"), 
                             each = n), 
                         length(separatePredictions)))
  totalFeaturePredsPost <- 
    lapply(seq_along(separatePredictions[[1]][[1]]),
           function(modelIdx)
           {
             lapply(separatePredictions,
                    function(predictionList) 
                      predictionList[[1]][[modelIdx]]) %>%
               unlist() %>% matrix(nrow = n) %>% rowMeans()
           })
  names(totalFeaturePredsPost) <- names(separatePredictions[[1]][[1]])
  totalFeaturePredsPre <- 
    lapply(seq_along(separatePredictions[[1]][[1]]),
           function(modelIdx)
           {
             lapply(separatePredictions,
                    function(predictionList) 
                      predictionList[[2]][[modelIdx]]) %>%
               unlist() %>% matrix(nrow = n) %>% rowMeans()
           })
  names(totalFeaturePredsPre) <- names(separatePredictions[[1]][[1]])
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
  fitData <- prepareData(data, modelInfoList)
  modelIdxList <- getModelIdxList(modelInfoList)
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
evaluateModelGeneric <- function(PHOModelList)
{
  data <- PHOModelList$data
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(data)
  totalPredictions <- PHOModelList %>% predict_PHO()
  separatePredictions <- 
    lapply(PHOModelList$PHOEnsemble, evaluateSingleModel, 
           data = data, modelInfoList = modelInfoList)
  predictionsData <- 
    data.frame(y = unlist(separatePredictions),
               Effect = 
                 rep(rep(names(separatePredictions[[1]][[1]]), 
                         each = n),
                     length(separatePredictions)),
               PHO = rep(rep(c("After", "Before"), 
                             each = n), 
                         length(separatePredictions)))
  totalFeaturePredsPost <- 
    lapply(seq_along(separatePredictions[[1]][[1]]),
           function(modelIdx)
           {
             lapply(separatePredictions,
                    function(predictionList) 
                      predictionList[[1]][[modelIdx]]) %>%
               unlist() %>% matrix(nrow = n) %>% rowMeans()
           })
  names(totalFeaturePredsPost) <- names(separatePredictions[[1]][[1]])
  totalFeaturePredsPre <- 
    lapply(seq_along(separatePredictions[[1]][[1]]),
           function(modelIdx)
           {
             lapply(separatePredictions,
                    function(predictionList) 
                      predictionList[[2]][[modelIdx]]) %>%
               unlist() %>% matrix(nrow = n) %>% rowMeans()
           })
  names(totalFeaturePredsPre) <- names(separatePredictions[[1]][[1]])
  return(list(data = data,
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre))
}
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
plotEffectGenericPost <- function(modelEvalData, 
                                  feature)
{
  originalData <- modelEvalData$data
  plotData <- 
    data.frame(x = modelEvalData$data[,feature],
               y = modelEvalData$totalFeaturePredsPost[[feature]])
  ggplot(plotData, aes(x = x, y = y)) + geom_line()
}
plotEffectGenericPre <- function(modelEvalData, 
                                 feature)
{
  originalData <- modelEvalData$data
  plotData <- 
    data.frame(x = modelEvalData$data[,feature],
               y = modelEvalData$totalFeaturePredsPre[[feature]])
  ggplot(plotData, aes(x = x, y = y)) + geom_line()
}
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
                 modelEvalData$totalFeaturePredsPost[[modelName]])
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