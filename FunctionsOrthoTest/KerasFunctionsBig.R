#Get submodel until penultimate layer
getIntermediateModel <- function(model, layerIdx = length(model$layers) - 1)
{
  deep_part_in <- model %>% get_layer(index = as.integer(layerIdx))
  intermediate_mod <- keras_model(model$input,
                                  deep_part_in$output)
  return(intermediate_mod)
}
#Solve linear system after removing linear dependencies
solveSingularMatrix <- function(tmpU, Pivot)
{
  tmpUReduced <- tmpU[,Pivot]
  tryCatch(tmp <-solve(t(tmpUReduced) %*% tmpUReduced),
           error = function(error)
           {
             Pivot <<- solveSingularMatrix(tmpU,
                                           Pivot[1:(length(Pivot) - 1)])
           })
  return(Pivot)
}
getModelIdxList <- function(modelInfoList)
{
  modelIdxList <- lapply(seq_along(modelInfoList$theta),
                         function(interOrderIdx)
                           lapply(seq_along(modelInfoList$theta[[interOrderIdx]]),
                                  function(x) c(interOrderIdx, x))) %>%
    unlist(recursive = FALSE)
  return(modelIdxList)
}
getU <- function(modelList, modelIdxList, modelInfoList, data)
{
  dataDictionary <- getDataDictionary(modelInfoList)
  U_List <- 
    lapply(seq_along(modelIdxList),
           function(idx)
           {
             input <- modelList[[modelIdxList[[idx]]]] %>%
               getIntermediateModel() %>%
               predict(data[[dataDictionary[[modelIdxList[[idx]]]]]], 
                       verbose = 0)
             if(modelList[[modelIdxList[[1]]]]$output$node$layer$get_config()$use_bias)
               input <- cbind(input, 1)
             return(input)
           })
  U_Dims <-
    lapply(seq_along(U_List),
           function(modelIdx)
             ncol(U_List[[modelIdx]]))
  U <- unlist(U_List) %>%
    matrix(ncol =
             sum(U_Dims %>% unlist()))
  U <- cbind(U, 1)
  U_IndicesList <- list(1:U_Dims[[1]])
  for(idx in seq_along(U_Dims)[-1])
  {
    lastIdx <- U_IndicesList[[idx - 1]][U_Dims[[idx - 1]]]
    U_IndicesList[[idx]] <- (lastIdx + 1):(lastIdx + U_Dims[[idx]])
  }
  retList <- list(U = U,
                  U_Dims = U_Dims,
                  U_IndicesList = U_IndicesList)
  return(retList)
}
getW_List <- function(modelList, modelIdxList, modelInfoList, U_IndicesList)
{
  W_List_Sep <- lapply(seq_along(modelIdxList),
                       function(idx)
                       {
                         weights <- modelList[[modelIdxList[[idx]]]] %>%
                           get_layer(index = -1) %>% get_weights()
                         return(unlist(weights))
                       })
  W <- c(unlist(W_List_Sep), 0)
  W_List <- 
    lapply(seq_along(W_List_Sep),
           function(modelIdx) 
           {
             tmpW <- W
             tmpW[-U_IndicesList[[modelIdx]]] <- 0
             return(tmpW)
           }
    )
  return(W_List)
}
PHO <- function(modelList, modelInfoList, data)
{
  modelIdxList <- getModelIdxList(modelInfoList)
  #Initialize model intercept
  U_Object <- getU(modelList, modelIdxList, modelInfoList, data)
  W_List <- getW_List(modelList, modelIdxList, modelInfoList, U_Object$U_IndicesList)
  W_List_old <- W_List
  modelOrder <- 
    lapply(modelIdxList, 
           function(modelIdx)
             modelIdx[1]) %>% unlist()
  #Iterate over interaction depth
  for(orthoIdx in 2:(length(modelInfoList$theta) - is.null(modelInfoList$theta$Linear)))
  {
    lowerOrderIdxList <- which(modelOrder >= orthoIdx)
    relIndices <- U_Object$U_IndicesList[lowerOrderIdxList] %>% 
      unlist()
    tmpU <- U_Object$U
    tmpU[,-relIndices] <- 0
    tmpU[,ncol(tmpU)] <- 1
    H <- crossprod(tmpU)
    qrRes <- qr(H)
    H_order <- qrRes$pivot[1:qrRes$rank]
    Pivot <- solveSingularMatrix(tmpU, H_order)
    tmpUReduced <- tmpU[,Pivot]
    tmpInverse <- solve(t(tmpUReduced) %*% tmpUReduced)
    higherOrderIdxList <- which(modelOrder == (orthoIdx - 1))
    outputsList <- 
      lapply(higherOrderIdxList, 
             function(higherOrderIdx)
             {
               return(U_Object$U %*% 
                        W_List[[higherOrderIdx]])
             })
    ZList <- 
      lapply(outputsList, 
             function(outputs)
             {
               tmpZ <- tmpInverse %*% t(tmpUReduced) %*% outputs
               Z <- rep(0, ncol(tmpU))
               Z[Pivot] <- tmpZ
               return(Z)
             })
    for(higherOrderIdx in seq_along(higherOrderIdxList))
    {
      modelIdx <- higherOrderIdxList[[higherOrderIdx]]
      W_List[[modelIdx]] <- 
        W_List[[modelIdx]] - 
        ZList[[higherOrderIdx]]
    }
    for(lowerOrderIdx in lowerOrderIdxList)
    {
      tmpWeightUpdate <- rep(0, U_Object$U_Dims[[lowerOrderIdx]])
      for(higherOrderIdx in seq_along(higherOrderIdxList))
      {
        tmpWeightUpdate <- 
          tmpWeightUpdate + 
          ZList[[higherOrderIdx]][U_Object$U_IndicesList[[lowerOrderIdx]]]
      }
      weightUpdate <- rep(0, ncol(tmpU))
      weightUpdate[U_Object$U_IndicesList[[lowerOrderIdx]]] <- tmpWeightUpdate
      W_List[[lowerOrderIdx]] <-
        W_List[[lowerOrderIdx]] + weightUpdate
    }
  }
  allOutputMeans <- 
    lapply(seq_along(modelIdxList), 
           function(modelIdx)
           {
             return(U_Object$U %*% 
                      W_List[[modelIdx]] %>% mean())
           })
  W_List <- 
    lapply(seq_along(W_List),
           function(W_Idx) 
           {
             tmp_W <- W_List[[W_Idx]]
             tmp_W[length(tmp_W)] <- if(W_Idx == 1)
               sum(unlist(allOutputMeans)[-1]) else
                 -allOutputMeans[[W_Idx]]
             return(tmp_W)
           })
  retList <- list(modelList = modelList,
                  W_List = W_List,
                  U_Dims = U_Object$U_Dims,
                  W_List_old = W_List_old)
  return(retList)
}
# predict_Feature <- function(PHOModelList, feature)
# {
#   PHOEnsemble <- PHOModelList$PHOEnsemble
#   modelInfoList <- PHOModelList$modelInfoList
#   featureData <- PHOModelList$data[,feature]
#   modelNames <- 
#     lapply(modelInfoList$theta,
#            function(modelOrder)
#              lapply(modelOrder,
#                     function(subTheta)
#                       paste(unlist(subTheta), collapse = "_")
#              )) %>% unlist(recursive = FALSE)
#   LinearIndices <- 
#     which(sapply(names(modelNames), function(x) grepl("Linear", x)))
#   modelNames[LinearIndices] <- 
#     paste(modelNames[LinearIndices], "_Linear", sep = "")
#   modelIdxList <- getModelIdxList(modelInfoList)
#   relModelIdx <- which(modelNames == feature)
#   fitData <- prepareData(PHOModelList$data, modelInfoList)
#   U_Object <- 
#     getU(PHOModel$modelList, modelIdxList, modelInfoList, fitData)
#   U_Feature_List <- 
#     lapply(PHOEnsemble,
#            function(PHOModel) PHOModel$modelList[[modelIdxList[[relModelIdx]]]] %>%
#              getIntermediateModel() %>% predict(featureData,
#                                                 verbose = 0))
#   U_IndicesList <- list(1:PHOEnsemble[[1]]$U_Dims[[1]])
#   for(idx in seq_along(PHOEnsemble[[1]]$U_Dims)[-1])
#   {
#     lastIdx <- 
#       U_IndicesList[[idx - 1]][PHOEnsemble[[1]]$U_Dims[[idx - 1]]]
#     U_IndicesList[[idx]] <- 
#       (lastIdx + 1):(lastIdx + PHOEnsemble[[1]]$U_Dims[[idx]])
#   }
#   all_W <- 
#     lapply(PHOEnsemble, 
#            function(singleModel)
#            {
#              singleModel$W_List
#            })
#   U_global_List <- list()
#   for(iEnsemble in seq_along(PHOEnsemble))
#   {
#     U_global_List[[iEnsemble]] <-
#       matrix(0, nrow = nrow(U_Feature_List[[iEnsemble]]), 
#              ncol = sum(unlist(PHOEnsemble[[1]]$U_Dims)) + 1)
#     U_global_List[[iEnsemble]][,U_IndicesList[[relModelIdx]]] <- 
#       U_Feature_List[[iEnsemble]]
#   }
#   allOutputs <- list()
#   for(iEnsemble in seq_along(PHOEnsemble))
#   {
#     allOutputs[[iEnsemble]] <- 
#       U_global_List[[iEnsemble]] %*% 
#       all_W[[iEnsemble]][[relModelIdx]]
#   }
#   outputs <- unlist(allOutputs) %>% matrix(ncol = length(allOutputs)) %>%
#     rowMeans()
#   return(outputs)
# }
predict_Feature_PrePHO <- function(PHOModelList, feature)
{
  PHOEnsemble <- PHOModelList$PHOEnsemble
  modelInfoList <- PHOModelList$modelInfoList
  featureData <- PHOModelList$data[,feature]
  modelNames <- 
    lapply(modelInfoList$theta,
           function(modelOrder)
             lapply(modelOrder,
                    function(subTheta)
                      paste(unlist(subTheta), collapse = "_")
             )) %>% unlist(recursive = FALSE)
  LinearIndices <- 
    which(sapply(names(modelNames), function(x) grepl("Linear", x)))
  modelNames[LinearIndices] <- 
    paste(modelNames[LinearIndices], "_Linear", sep = "")
  modelIdxList <- getModelIdxList(modelInfoList)
  relModelIdx <- which(modelNames == feature)
  U_Feature_List <- 
    lapply(PHOEnsemble,
           function(PHOModel) PHOModel$modelList[[modelIdxList[[relModelIdx]]]] %>%
             getIntermediateModel() %>% predict(featureData,
                                                verbose = 0))
  U_IndicesList <- list(1:PHOEnsemble[[1]]$U_Dims[[1]])
  for(idx in seq_along(PHOEnsemble[[1]]$U_Dims)[-1])
  {
    lastIdx <- 
      U_IndicesList[[idx - 1]][PHOEnsemble[[1]]$U_Dims[[idx - 1]]]
    U_IndicesList[[idx]] <- 
      (lastIdx + 1):(lastIdx + PHOEnsemble[[1]]$U_Dims[[idx]])
  }
  all_W <- 
    lapply(PHOEnsemble, 
           function(singleModel)
           {
             singleModel$W_List_old
           })
  U_global_List <- list()
  for(iEnsemble in seq_along(PHOEnsemble))
  {
    U_global_List[[iEnsemble]] <-
      matrix(0, nrow = nrow(U_Feature_List[[iEnsemble]]), 
             ncol = sum(unlist(PHOEnsemble[[1]]$U_Dims)) + 1)
    U_global_List[[iEnsemble]][,U_IndicesList[[relModelIdx]]] <- 
      U_Feature_List[[iEnsemble]]
  }
  allOutputs <- list()
  for(iEnsemble in seq_along(PHOEnsemble))
  {
    allOutputs[[iEnsemble]] <- 
      U_global_List[[iEnsemble]] %*% 
      all_W[[iEnsemble]][[relModelIdx]]
  }
  outputs <- unlist(allOutputs) %>% matrix(ncol = length(allOutputs)) %>%
    rowMeans()
  return(outputs)
}
predict_PHO <- function(PHOModelList, data = NULL)
{
  if(is.null(data))
  {
    data <- prepareData(PHOModelList$data, 
                        PHOModelList$modelInfoList)
  }
  modelIdxList <- getModelIdxList(PHOModelList$modelInfoList)
  sepPredictions <- 
    lapply(PHOModelList$PHOEnsemble,
           function(PHOModel)
           {
             U_Object <- getU(PHOModel$modelList, modelIdxList, 
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
fitPHOModel <- function(modelFormula, list_of_deep_models, 
                        data, nEnsemble = 20, 
                        progresstext = FALSE)
{
  modelInfoList <- getThetaFromFormula(modelFormula, list_of_deep_models)
  fitData <- prepareData(data, modelInfoList)
  Y <- data[,which(colnames(data) == as.character(modelInfoList$outcome))]
  PHOEnsemble <- list()
  for(i in 1:nEnsemble)
  {
    if(progresstext)
    {
      cat('\r',paste0("Fitting model ", i, " of ", nEnsemble))
      flush.console()
    }
    modelObject <- createModel(modelInfoList, list_of_deep_models)
    wholeModel <- modelObject$model
    modelList <- modelObject$modelList
    #Fit model####
    callback <- keras$callbacks$EarlyStopping(monitor = "loss", patience = 10)
    history <- wholeModel %>% fit(fitData, Y, epochs = 500, callbacks = callback,
                                  verbose = 0)
    #Orthogonalize####
    PHOEnsemble[[i]] <- PHO(modelList, modelInfoList, fitData)
  }
  PHOModelList <- list(PHOEnsemble = PHOEnsemble, 
                       modelInfoList = modelInfoList,
                       data = data)
  return(PHOModelList)
}
getVarDecomp <- function(PHOObject, data)
{
  lastLayerModel <- PHOObject$model %>% getIntermediateModel()
  lastLayerPreds <- lastLayerModel %>% PHOObject$predFunc(data)
  modelVars <- apply(lastLayerPreds, 2, var)
  modelNames <- lapply(lastLayerModel$output$node$layer$inbound_nodes[[1]]$inbound_layers,
                       function(subModel) subModel$name) %>% unlist()
  names(modelVars) <- modelNames
  return(modelVars)
}
