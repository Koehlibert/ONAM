clone_model_withWeights <- function(model)
{
  newModel <- clone_model(model)
  newModel %>% set_weights(model %>% get_weights())
}
clone_layer_newName <- function(original_layer)
{
  layerName <- original_layer$name
  if(str_detect(layerName, "input"))
  {
    return(inputDict[[which(layerName == names(inputDict))]])
  }
  else
  {
    inboundNodes <- original_layer$inbound_nodes[[1]]$inbound_layers
    if(str_detect(layerName, "dense"))
    {
      return(clone_layer_newName(inboundNodes) %>%
               layer_dense(units = original_layer$get_config()$units,
                           activation = original_layer$get_config()$activation,
                           use_bias = original_layer$get_config()$use_bias,
                           input_shape = original_layer$input_shape[[2]],
                           trainable = original_layer$trainable))
    }else if(str_detect(layerName, "dropout"))
    {
      return(clone_layer_newName(inboundNodes) %>%
               layer_dropout(rate = original_layer$get_config()$rate,
                             input_shape = original_layer$get_config()$batch_input_shape[[2]]))
    }else if(str_detect(layerName, "concatenate"))
    {
      return(layer_concatenate(lapply(inboundNodes,
                                      clone_layer_newName) %>% unlist()))
    }
  }
}
clone_model_newNames <- function(original_model)
{
  originalInput <- findInput(original_model$output)
  inputDict <- createInputDictionary(originalInput)
  environment(clone_layer_newName) <- environment()
  newOutput <-
    clone_layer_newName(original_model$output$node$layer)
  newModel <- keras_model(inputDict, newOutput)
  newModel %>% set_weights(original_model %>% get_weights())
  return(newModel)
}
findInput <- function(currentInput)
{
  if(str_detect(currentInput$node$layer$name, "input"))
  {
    return(currentInput)
  }else
  {
    if(is.list(currentInput$node$layer$input))
    {
      return(lapply(currentInput$node$layer$input, findInput))
    }else
    {
      return(findInput(currentInput$node$layer$input))
    }
  }
  lapply(currentInput$layer$node, findInput)
}
createInputDictionary <- function(original_InputsList)
{
  if(!is.list(original_InputsList))
  {
    original_InputsList <- list(original_InputsList)
  }
  inputDict <- list()
  for(input in original_InputsList)
  {
    if(!input$name %in% names(inputDict))
    {
      inputDict[[input$name]] <- 
        layer_input(shape = input$node$layer$input_shape[[1]][[2]])
    }
  }
  return(inputDict)
}
