#' Plot Main Effect
#' @param evalData Model output as obtained from ONAM::evaluateModel
#' @param effect Effect to be plotted, must be present in the model formula. For interaction terms, use plotInteractionEffect
#' @returns Returns a ggplot2 object of the specified effect
#' @export plotMainEffect
plotMainEffect <- function(evalData, effect)
{
  if(!effect %in% colnames(evalData$finalTotalPredictions))
  {
    stop(paste(effect,
               " is not present in the fitted model effects.",
               sep = ""))
  }
  plotData <-
    data.frame(x = evalData$data[,effect],
               y = evalData$finalTotalPredictions[,effect])
  out_plot <- ggplot(plotData, aes(x = x, y = y)) +
    geom_point() + ylab("Effect") + xlab(effect)
  return(out_plot)
}
#' Plot Main Effect
#' @param evalData Model output as obtained from ONAM::evaluateModel
#' @param effect1 First effect to be plotted
#' @param effect2 Second effect to be plotted
#' @param interpolate If TRUE, values will be interpolated for a smooth plot. If FALSE (default), only observations in the data will be plotted.
#' @param customColors color palette object for the interaction plot. Default is "spectral", returning a color palette based on the spectral theme.
#' @param n_interpolate number of values per coordinate axis to interpolate. Ignored if interpolate = FALSE.
#' @returns Returns a ggplot2 object of the specified effect interaction
#' @export plotInterEffect
plotInterEffect <- function(evalData, effect1, effect2,
                            interpolate = FALSE,
                            customColors = "spectral",
                            n_interpolate = 200)
{
  inter <- paste(effect1, effect2, sep = "_")
  if(!inter %in% colnames(evalData$finalTotalPredictions))
  {
    inter <- paste(effect2, effect1, sep = "_")
    tmp <- effect1
    effect1 <- effect2
    effect2 <- tmp
    if(!inter %in% colnames(evalData$finalTotalPredictions))
    {
      stop(paste("No interaction effect fitted for ",
                 effect1, " and ", effect2,".", sep = ""))
    }
  }
  if(typeof(customColors) != "closure")
  {
    if(customColors == "spectral")
    {
      customColors <- colorRampPalette(colors = (x = RColorBrewer::brewer.pal(n = 11, name = "Spectral")))
    }
  }
  if(interpolate)
  {
    tmpInterp <-
      akima::interp(x = BIBIEvalData$data[,effect1],
                    y = BIBIEvalData$data[,effect2],
                    z =
                      BIBIEvalData$finalTotalPredictions[,inter],
                    nx = n_interpolate, ny = n_interpolate,
                    duplicate = "mean")
    plotData <-
      data.frame(x = rep(tmpInterp$x, length(tmpInterp$y)),
                 y = rep(tmpInterp$y, each = length(tmpInterp$x)),
                 Prediction = tmpInterp$z %>% c())
    plotData <- plotData[which(!is.na(plotData$Prediction)),]
    aes_gradient <-
      scale_fill_gradientn(colors =
                             customColors(n = 100),
                           values =
                             scales::rescale(c(min(plotData$Prediction),
                                               mean(plotData$Prediction),
                                               max(plotData$Prediction))),
                           guide = "colorbar",
                           limits = c(min(plotData$Prediction),
                                      max(plotData$Prediction)))
    geom_param <- geom_tile()
    aes_param <- aes(x = x, y = y, fill = Prediction)
  }else
  {
    plotData <-
      data.frame(x = BIBIEvalData$data[,effect1],
                 y = BIBIEvalData$data[,effect2],
                 Prediction =
                   BIBIEvalData$finalTotalPredictions[,inter])
    aes_gradient <-
      scale_color_gradientn(colors =
                             customColors(n = 100),
                           values =
                             scales::rescale(c(min(plotData$Prediction),
                                               mean(plotData$Prediction),
                                               max(plotData$Prediction))),
                           guide = "colorbar",
                           limits = c(min(plotData$Prediction),
                                      max(plotData$Prediction)))
    geom_param <- geom_point()
    aes_param <- aes(x = x, y = y, color = Prediction)
  }
  inter_theme <- theme(plot.title.position = "plot",
                       plot.caption.position =  "plot",
                       # plot.margin = grid::unit(c(0,0,0,0), "mm"),
                       panel.grid = element_blank(),
                       panel.background = element_blank(),
                       legend.position = "right")
  out_plot <-
    ggplot(plotData, aes_param) +
    # geom_point(size = 0.75) +
    geom_param +
    aes_gradient +
    inter_theme +
    ylab(effect1) + xlab(effect2)
  return(out_plot)
}
