clone_model_withWeights <- function(model)
{
  newModel <- ONAM:::clone_model(model)
  newModel %>% keras::set_weights(model %>% keras::get_weights())
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
      return(ONAM:::clone_layer_newName(inboundNodes) %>%
               keras::layer_dense(units = original_layer$get_config()$units,
                           activation = original_layer$get_config()$activation,
                           use_bias = original_layer$get_config()$use_bias,
                           input_shape = original_layer$input_shape[[2]],
                           trainable = original_layer$trainable))
    }else if(stringr::str_detect(layerName, "dropout"))
    {
      return(ONAM:::clone_layer_newName(inboundNodes) %>%
               keras::layer_dropout(rate = original_layer$get_config()$rate,
                             input_shape = original_layer$get_config()$batch_input_shape[[2]]))
    }else if(stringr::str_detect(layerName, "concatenate"))
    {
      return(layer_concatenate(lapply(inboundNodes,
                                      ONAM:::clone_layer_newName) %>% unlist()))
    }
  }
}
clone_model_newNames <- function(original_model)
{
  originalInput <- ONAM:::findInput(original_model$output)
  inputDict <- ONAM:::createInputDictionary(originalInput)
  environment(clone_layer_newName) <- environment()
  newOutput <-
    clone_layer_newName(original_model$output$node$layer)
  newModel <- keras::keras_model(inputDict, newOutput)
  newModel %>% keras::set_weights(original_model %>%
                                    keras::get_weights())
  return(newModel)
}
findInput <- function(currentInput)
{
  if(stringr::str_detect(currentInput$node$layer$name, "input"))
  {
    return(currentInput)
  }else
  {
    if(is.list(currentInput$node$layer$input))
    {
      return(lapply(currentInput$node$layer$input, ONAM:::findInput))
    }else
    {
      return(ONAM:::findInput(currentInput$node$layer$input))
    }
  }
  lapply(currentInput$layer$node, ONAM:::findInput)
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
        keras::layer_input(shape = input$node$layer$input_shape[[1]][[2]])
    }
  }
  return(inputDict)
}
