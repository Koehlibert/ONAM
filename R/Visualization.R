utils::globalVariables(c("x", "y", "prediction", "x1", "x2", "y1", "y2"))
#' Plot Main Effect
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
#' @param effect Effect to be plotted, must be present in the model formula.
#' For interaction terms, use plotInteractionEffect
#' @returns Returns a ggplot2 object of the specified effect
#' @examples
#' \donttest{
#' # Basic example for a simple ONAM-model
#' # Create training data
#' n <- 1000
#' x1 <- runif(n, -2, 2)
#' x2 <- runif(n, -2, 2)
#' y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
#'   pweibull(-x2, shape = 0.5)) +
#'   x1 * x2
#' data_train <- cbind(x1, x2, y)
#' # Define model
#' model_formula <- y ~ mod1(x1) + mod1(x2) +
#'   mod1(x1, x2)
#' list_of_deep_models <- list(mod1 = ONAM:::get_submodel)
#' # Fit model
#' mod <- onam(model_formula, list_of_deep_models,
#'             data_train, n_ensemble = 2, epochs = 10,
#'             progresstext = TRUE, verbose = 1)
#' plot_main_effect(mod, "x1")
#' }
#' @export plot_main_effect
plot_main_effect <- function(object, effect) {
  check_inputs_plot(object, effect)
  if (inherits(object, "onam_prediction")) {
    data_plot <-
      data.frame(x = object$data[, effect],
                 y = object$predictions_features[, effect])
  } else if (inherits(object, "onam")) {
    data_plot <-
      data.frame(x = object$data[, effect],
                 y = object$outputs_post_ensemble[, effect])
  }
  if (effect %in% object$categorical_features) {
    plt <- plot_main_categorical(data_plot)
  } else {
    plt <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point()
  }
  plt + ggplot2::ylab(eff_label_helper(object$model_info$target)) +
    ggplot2::xlab(effect)
}
#' Plot Interaction Effect
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
#' @param effect1 First effect to be plotted.
#' @param effect2 Second effect to be plotted.
#' @param interpolate If TRUE, values will be interpolated for a smooth plot.
#' If FALSE (default), only observations in the data will be plotted.
#' @param custom_colors color palette object for the interaction plot. Default
#' is "spectral", returning a color palette based on the spectral theme.
#' @param n_interpolate number of values per coordinate axis to interpolate.
#' Ignored if 'interpolate = FALSE'.
#' @returns Returns a 'ggplot2' object of the specified effect interaction
#' @examples
#' \donttest{
#' # Basic example for a simple ONAM-model
#' # Create training data
#' n <- 1000
#' x1 <- runif(n, -2, 2)
#' x2 <- runif(n, -2, 2)
#' y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
#'   pweibull(-x2, shape = 0.5)) +
#'   x1 * x2
#' data_train <- cbind(x1, x2, y)
#' # Define model
#' model_formula <- y ~ mod1(x1) + mod1(x2) +
#'   mod1(x1, x2)
#' list_of_deep_models <- list(mod1 = ONAM:::get_submodel)
#' # Fit model
#' mod <- onam(model_formula, list_of_deep_models,
#'             data_train, n_ensemble = 2, epochs = 10,
#'             progresstext = TRUE, verbose = 1)
#' plot_inter_effect(mod, "x1", "x2", interpolate = TRUE)
#' }
#' @export plot_inter_effect
plot_inter_effect <- function(object,
                              effect1,
                              effect2,
                              interpolate = FALSE,
                              custom_colors = "spectral",
                              n_interpolate = 200) {
  inter <- paste(effect1, effect2, sep = "_")
  if (!is.null(check_inputs_plot(object, inter, interaction = 1))) {
    inter <- paste(effect2, effect1, sep = "_")
    if (!is.null(check_inputs_plot(object, inter, interaction = 2))) {
      stop(paste(
        "No interaction effect fitted for ",
        effect1,
        " and ",
        effect2,
        ".",
        sep = ""
      ))
    }
    tmp <- effect1
    effect1 <- effect2
    effect2 <- tmp
  }
  if (typeof(custom_colors) != "closure") {
    if (custom_colors == "spectral") {
      custom_colors <-
        grDevices::colorRampPalette(colors = (x = RColorBrewer::brewer.pal(n = 11, name = "Spectral")))
    }
  }
  if (inherits(object, "onam_prediction")) {
    eff <- object$predictions_features[, inter]
  } else if (inherits(object, "onam")) {
    eff <- object$outputs_post_ensemble[, inter]
  }
  if (interpolate) {
    if (!requireNamespace("akima", quietly = TRUE)) {
      stop("Package \"akima\" must be installed for interpolation.",
           call. = FALSE)
    }
    tmp_interp <-
      akima::interp(
        x = object$data[, effect1],
        y = object$data[, effect2],
        z = eff,
        nx = n_interpolate,
        ny = n_interpolate,
        duplicate = "mean"
      )
    data_plot <-
      data.frame(
        x = rep(tmp_interp$x, length(tmp_interp$y)),
        y = rep(tmp_interp$y, each = length(tmp_interp$x)),
        prediction = tmp_interp$z %>% c()
      )
    data_plot <- data_plot[which(!is.na(data_plot$prediction)),]
    aes_gradient <-
      ggplot2::scale_fill_gradientn(
        colors =
          custom_colors(n = 100),
        values =
          scales::rescale(c(
            min(data_plot$prediction),
            mean(data_plot$prediction),
            max(data_plot$prediction)
          )),
        guide = "colorbar",
        limits = c(min(data_plot$prediction),
                   max(data_plot$prediction)),
        name = eff_label_helper(object$model_info$target)
      )
    geom_param <- ggplot2::geom_tile()
    aes_param <- ggplot2::aes(x = x, y = y, fill = prediction)
  } else {
    data_plot <-
      data.frame(x = object$data[, effect1],
                 y = object$data[, effect2],
                 prediction = eff)
    aes_gradient <-
      ggplot2::scale_color_gradientn(
        colors =
          custom_colors(n = 100),
        values =
          scales::rescale(c(
            min(data_plot$prediction),
            mean(data_plot$prediction),
            max(data_plot$prediction)
          )),
        guide = "colorbar",
        limits = c(min(data_plot$prediction),
                   max(data_plot$prediction)),
        name = eff_label_helper(object$model_info$target)
      )
    geom_param <- ggplot2::geom_point()
    prediction <- NULL #remove cmd check note
    aes_param <- ggplot2::aes(x = x, y = y, color = prediction)
  }
  inter_theme <- ggplot2::theme(
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    # plot.margin = grid::unit(c(0,0,0,0), "mm"),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.position = "right"
  )
  ggplot2::ggplot(data_plot, aes_param) +
    # geom_point(size = 0.75) +
    geom_param +
    aes_gradient +
    inter_theme +
    ggplot2::ylab(effect1) + ggplot2::xlab(effect2)
}
eff_label_helper <- function(target) {
  if (target == "continuous") {
    "Effect"
  } else if (target == "binary") {
    "Effect on logit scale"
  }
}
plot_main_categorical <- function(data_plot) {
  x_uq <- unique(data_plot$x)
  y_uq <- unique(data_plot$y)
  data_plot_categ <-
    data.frame(x = x_uq,
               y = y_uq)
  ggplot2::ggplot(data_plot_categ, ggplot2::aes(x = x, y = y)) +
    geom_bar(stat = "identity") +
    geom_text(ggplot2::aes(
      y = 0,
      label = x,
      vjust = 0.5 + 0.75 * sign(y)
    )) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}
