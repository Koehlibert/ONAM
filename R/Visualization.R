#' Plot Main Effect
#' @param evalData Model output as obtained from ONAM::evaluateModel
#' @param effect Effect to be plotted, must be present in the model formula. For interaction terms, use plotInteractionEffect
#' @returns Returns a ggplot2 object of the specified effect
#' @export
plotMainEffect <- function(evalData, effect)
{
  plotData <-
    data.frame(x = evalData$data[,effect],
               y = evalData$finalTotalPredictions[,effect])
  out_plot <- ggplot(plotData, aes(x = x, y = y)) +
    geom_point() + ylab("Effect") + xlab(effect)
  return(out_plot)
}
