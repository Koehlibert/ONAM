#Define DNN architecture for simulation
getSubModel <- function(inputs, regularizer = NULL)
{
  outputs <- inputs %>%
    # layer_dense(units = 512, activation = "relu",
    #             use_bias = TRUE) %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 256, activation = "relu",
    #             use_bias = TRUE,
    #             kernel_regularizer = regularizer) %>%
    # # layer_dropout(rate = 0.2) %>%
    layer_dense(units = 128, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 64, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    # layer_dropout(rate = 0.2) %>%
    layer_dense(units = 32, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    # layer_dropout(rate = 0.2) %>%
    layer_dense(units = 16, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    layer_dense(units = 8, activation = "linear",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    layer_dense(units = 1, activation = "linear",
                use_bias = TRUE)
  subModel <- keras_model(inputs, outputs)
  return(subModel)
}
#Define Linear Model DNN structure
getLinearSubModel <- function(inputs)
{
  outputs <- inputs %>%
    layer_dense(units = 1, activation = "linear",
                use_bias = FALSE,
                dtype = "float64")
  subModel <- keras_model(inputs, outputs)
}
#Derive Theta from model Formula
getThetaFromFormula <- function(modelFormula, list_of_deep_models)
{
  #Separate Symbols
  thetaList <- lapply(modelFormula, findSymbol)
  outcomeVar <- thetaList[[2]][[1]]
  partsList <- list()
  while(length(thetaList) > 0)
  {
    tmpItem <- thetaList[[length(thetaList)]]
    idxToRemove <- c(length(thetaList))
    while(length(tmpItem) > 1 & any(unlist(lapply(tmpItem, is.list))))
    {
      idxToRemove <- c(idxToRemove, length(tmpItem))
      tmpItem <- tmpItem[[length(tmpItem)]]
    }
    partsList <- c(partsList, list(tmpItem))
    thetaList[[idxToRemove]] <- NULL
  }
  signIdx <- which(lapply(partsList, function(item)
    all(lapply(item, function(subItem) subItem == as.name("+") |
                 subItem == as.name("~") | subItem == as.name(outcomeVar)) %>%
          unlist())) %>% unlist())
  partsList[signIdx] <- NULL
  additiveCompsIdx <- which(lapply(partsList, function(item)
    any(lapply(item, function(subItem) subItem == as.name("+")) %>%
          unlist()) | length(item) == 1) %>% unlist())
  LongAdditiveComps <- partsList[additiveCompsIdx]
  partsList[additiveCompsIdx] <- NULL
  additiveComps <- lapply(LongAdditiveComps, function(item)
    if(length(item) == 1) return(item) else
      return(item[which(lapply(item,
                               function(subItem) subItem != as.name("+")) %>%
                          unlist())])) %>% unlist() %>% unique()
  interOrder <- lapply(partsList, function(item) length(item) - 1) %>% unlist()
  highestOrder <- max(interOrder)
  orderedPartsList <- partsList[(1:length(partsList))[order(interOrder,
                                                            decreasing = TRUE)]]
  orderCounts <- table(interOrder)
  orderCounts <- orderCounts[order(as.numeric(names(orderCounts)),
                                   decreasing = TRUE)]
  countIndex <- list()
  start_index <- 1
  for (i in seq_along(orderCounts)) {
    end_index <- start_index + orderCounts[i] - 1
    seq_list <- start_index:end_index
    countIndex[[i]] <- seq_list
    start_index <- end_index + 1
  }
  uniqueOrders <- as.numeric(names(orderCounts))
  thetaDeepWithName <- lapply(seq_along(uniqueOrders), function(countIdx)
    orderedPartsList[countIndex[[countIdx]]])
  namesIdx <- lapply(seq_along(thetaDeepWithName), function(itemIdx)
    lapply(seq_along(thetaDeepWithName[[itemIdx]]), function(innerItemIdx)
      lapply(seq_along(thetaDeepWithName[[itemIdx]][[innerItemIdx]]),
             function(innerInnerItemIdx)
               if(as.character(thetaDeepWithName[[itemIdx]][[innerItemIdx]][[innerInnerItemIdx]]) %in%
                  names(list_of_deep_models))
                 return(c(itemIdx, innerItemIdx, innerInnerItemIdx))))) %>%
    unlist() %>% split(rep(1:(length(.)/3), each = 3))
  thetaDeep <- thetaDeepWithName
  modelList <- list()
  for(item in namesIdx)
  {
    name = as.character(thetaDeep[[item]])
    thetaDeep[[item]] <- NULL
    if(length(modelList) < item[1])
      modelList[[item[1]]] <- list()
    if(length(modelList[[item[1]]]) < item[2])
      modelList[[item[1]]][[item[2]]] <- name else
        modelList[[item[1]]][[item[2]]] <- c(modelList[[item[2]]], name)
  }
  names(thetaDeep) <- uniqueOrders
  names(modelList) <- uniqueOrders
  theta <- c(thetaDeep, Linear = list(additiveComps))
  modelInfoList <- list(theta = theta,
                        modelNames = modelList,
                        outcome = outcomeVar)
  return(modelInfoList)
}
findSymbol <- function(currList)
{
  return(lapply(currList, function(item) if(is.symbol(item)) item else findSymbol(item)))
}
remove_element <- function(lst, indices)
  {
  if (length(indices) == 1) {
    lst[[indices]] <- NULL
  } else {
    lst[[indices[1]]] <- remove_element(lst[[indices[1]]], indices[-1])
  }
  return(lst)
}
remove_string_recursive <- function(lst, remove_str)
  {
  if (is.list(lst)) {
    lst <- lapply(lst, function(x) remove_string_recursive(x, remove_str))
    lst <- lapply(lst, function(sublist) Filter(function(item) {
      is.symbol(item) || !item %in% as.name(strings_to_remove)
    }, sublist))
    return(lst)
  } else {
    return(lst)
  }
}
createModel <- function(modelInfoList, list_of_deep_models)
{
  inputsList <- createInputs(modelInfoList)
  modelList <- createModels(modelInfoList, inputsList, list_of_deep_models)
  wholeModel <- compileModel(inputsList, modelList)
  return(list(model = wholeModel,
              modelList = modelList))
}
createInputs <- function(modelInfoList)
{
  if(length(modelInfoList$theta$Linear) > 0)
    linInputs <- lapply(1:length(modelInfoList$theta$Linear),
                        function(x) layer_input(shape = 1))
  else
    linInputs <- NULL
  deepInputs <-
    lapply(1:length(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                                "Linear")]),
           function(interCountIdx)
           {
             nInputs <- as.numeric(names(modelInfoList$theta)[interCountIdx])
             subInputList <-
               lapply(1:length(modelInfoList$theta[[interCountIdx]]),
                      function(tmp) layer_input(shape = nInputs))
           })
  names(deepInputs) <-
    names(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                      "Linear")])
  return(list(Deep = deepInputs,
              Additive = linInputs))
}
createModels <- function(modelInfoList, inputsList, list_of_deep_models)
{
  linModels <- lapply(inputsList$Additive, getLinearSubModel)
  deepModels <- list()
  for(p_idx in names(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                                 "Linear")]))
  {
    deepModels[[p_idx]] <-
      lapply(seq_along(modelInfoList$modelNames[[p_idx]]),
             function(modelIdx)
             {
               modelName <- modelInfoList$modelNames[[p_idx]][[modelIdx]]
               return(list_of_deep_models[[modelName]](inputsList$Deep[[p_idx]][[modelIdx]]))
             })
  }
  all_models <- c(deepModels,
                  Additive = list(linModels))
  return(all_models)
}
concatenate_ModelList <- function(modelList, bias = FALSE)
{
  tmpOutput <- layer_concatenate(lapply(modelList,
                                        function(model) model$output)) %>%
    layer_dense(1, use_bias = bias, trainable = FALSE)
  tmpWeights <- tmpOutput$node$layer$get_weights()
  tmpWeights[[1]] <- matrix(rep(1, length(modelList)),
                                 ncol = 1)
  if(bias) tmpWeights[[2]] <- tmpWeights[[2]] - tmpWeights[[2]]
  tmpOutput$node$layer %>% set_weights(tmpWeights)
  return(tmpOutput)
}
compileModel <- function(inputsList, modelList)
{
  subModels <- unlist(modelList, use.names = FALSE) #Why does this matter?
  all_inputs <- unlist(inputsList, use.names = FALSE)
  wholeModel <- subModels %>% concatenate_ModelList(bias = TRUE)
  wholeModel <- keras_model(all_inputs, wholeModel)
  wholeModel <- wholeModel %>% compile(loss = loss_mean_squared_error(),
                                       optimizer = optimizer_adam())
  return(wholeModel)
}
prepareData <- function(originalData, modelInfoList)
{
  additiveData <- lapply(modelInfoList$theta$Linear, function(featureName)
    originalData[,as.character(featureName)])
  deepData <-
    lapply(unlist(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                              "Linear")], recursive = FALSE),
           function(subTheta) originalData[,as.character(unlist(subTheta))])
  newData <- c(deepData, additiveData)
  newData <- unname(newData)
  return(newData)
}
getDataDictionary <- function(modelInfoList)
{
  dictionaryList <- list()
  tmpIdx <- 0
  for(i1 in seq_along(modelInfoList$theta))
  {
    dictionaryList[[i1]] <- list()
    for(i2 in seq_along(modelInfoList$theta[[i1]]))
    {
      tmpIdx <- tmpIdx + 1
      dictionaryList[[i1]][[i2]] <- tmpIdx
    }
  }
  return(dictionaryList)
}
getSubModel_big <- function(inputs, regularizer = NULL)
{
  outputs <- inputs %>%
    # layer_dense(units = 512, activation = "relu",
    #             use_bias = TRUE) %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 512, activation = "relu",
    #             use_bias = TRUE,
    #             kernel_regularizer = regularizer) %>%
    # layer_dropout(rate = 0.2) %>%
    layer_dense(units = 256, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 128, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 64, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    # layer_dropout(rate = 0.2) %>%
    layer_dense(units = 32, activation = "relu",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    layer_dense(units = 16, activation = "linear",
                use_bias = TRUE,
                kernel_regularizer = regularizer,
                dtype = "float64") %>%
    layer_dense(units = 1, activation = "linear",
                use_bias = TRUE)
  subModel <- keras_model(inputs, outputs)
  return(subModel)
}
