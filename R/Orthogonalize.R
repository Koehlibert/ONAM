#' @importFrom dplyr %>%
#' Get submodel until penultimate layer
getIntermediateModel <- function(model, layerIdx = length(model$layers) - 1)
{
  deep_part_in <- model %>% keras::get_layer(index = as.integer(layerIdx))
  intermediate_mod <- keras::keras_model(model$input,
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
             Pivot <<-
               ONAM:::solveSingularMatrix(tmpU,
                                          Pivot[1:(length(Pivot) - 1)])
           })
  return(Pivot)
}
getModelIdxList <- function(modelInfoList)
{
  modelIdxList <-
    lapply(seq_along(modelInfoList$theta),
           function(interOrderIdx)
             lapply(seq_along(modelInfoList$theta[[interOrderIdx]]),
                    function(x) c(interOrderIdx, x))) %>%
    unlist(recursive = FALSE)
  return(modelIdxList)
}
getU <- function(modelList, modelIdxList, modelInfoList, data)
{
  dataDictionary <- ONAM:::getDataDictionary(modelInfoList)
  U_List <-
    lapply(seq_along(modelIdxList),
           function(idx)
           {
             input <- modelList[[modelIdxList[[idx]]]] %>%
               ONAM:::getIntermediateModel() %>%
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
                           keras::get_layer(index = -1) %>%
                           keras::get_weights()
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
  modelIdxList <- ONAM:::getModelIdxList(modelInfoList)
  #Initialize model intercept
  U_Object <-
    ONAM:::getU(modelList, modelIdxList, modelInfoList, data)
  W_List <-
    ONAM:::getW_List(modelList, modelIdxList, modelInfoList,
                     U_Object$U_IndicesList)
  W_List_old <- W_List
  modelOrder <-
    lapply(modelIdxList,
           function(modelIdx)
             modelIdx[1]) %>% unlist()
  #Iterate over interaction depth
  for(orthoIdx in 2:(length(modelInfoList$theta) -
                     is.null(modelInfoList$theta$Linear)))
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
    Pivot <- ONAM:::solveSingularMatrix(tmpU, H_order)
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
#' @export
fitPHOModel <- function(modelFormula, list_of_deep_models,
                        data, nEnsemble = 20,
                        progresstext = FALSE)
{
  modelInfoList <-
    ONAM:::getThetaFromFormula(modelFormula, list_of_deep_models)
  fitData <-
    ONAM:::prepareData(data, modelInfoList)
  Y <- data[,which(colnames(data) == as.character(modelInfoList$outcome))]
  PHOEnsemble <- list()
  for(i in 1:nEnsemble)
  {
    if(progresstext)
    {
      cat('\r',paste0("Fitting model ", i, " of ", nEnsemble))
      flush.console()
    }
    modelObject <-
      ONAM:::createModel(modelInfoList, list_of_deep_models)
    wholeModel <- modelObject$model
    modelList <- modelObject$modelList
    #Fit model####
    callback <-
      keras::keras$callbacks$EarlyStopping(monitor = "loss",
                                    patience = 10)
    history <- wholeModel %>%
      keras::fit(fitData, Y, epochs = 500, callbacks = callback,
                                         verbose = 0)
    #Orthogonalize####
    PHOEnsemble[[i]] <-
      ONAM:::PHO(modelList, modelInfoList, fitData)
  }
  PHOModelList <- list(PHOEnsemble = PHOEnsemble,
                       modelInfoList = modelInfoList,
                       data = data)
  return(PHOModelList)
}
