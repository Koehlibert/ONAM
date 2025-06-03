library(dplyr)
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(patchwork)
library(akima)
nSim <- 10
n_inter <- 3
p_inf <- 3
nVals <- c(2000, 5000)
Effects <- c(1, 2, 3)
simSetting <- expand.grid(nVals, Effects)
plot_list_main <- list(list(), list())
plot_list_inter <- list(list(), list())
plot_labels_main <-
  apply(expand.grid(LETTERS[1:3], 1:3) %>% arrange(Var1),
        1, function(x)
          paste(x, collapse = ""))
plot_labels_inter <-
  apply(expand.grid(LETTERS[1:9], 1:2) %>% arrange(Var1),
        1, function(x)
          paste(x, collapse = ""))
plot_theme_main <- theme(
  plot.caption = element_text(hjust = 0),
  plot.title.position = "plot",
  plot.caption.position =  "plot",
  plot.title = element_text(vjust = -6),
  plot.margin = grid::unit(c(-5.5, 0, 0, 0), "mm"),
  # panel.grid = element_blank(),
  panel.background = element_blank(),
  axis.line.x = element_line(color = "black",
                             linewidth = 0.1),
  axis.line.y = element_line(color = "black",
                             linewidth = 0.1)
) +
  grids(linetype = "dashed",)
plot_theme_inter <-
  theme(
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.title = element_text(vjust = -6,
                              hjust = -0.01),
    plot.margin = grid::unit(c(-5.5, 0, 0, 3), "mm"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "right"
  )
get_average_data_inter <- function(data)
{
  mean_plot_data <-
    data %>%
    group_by(x1, x2) %>% summarize(y = mean(y)) %>%
    mutate(ensembleIdx = "Mean")
  tmp_interp <-
    interp(
      x = mean_plot_data$x1,
      y = mean_plot_data$x2,
      z = mean_plot_data$y,
      nx = 200,
      ny = 200,
      duplicate = "mean"
    )
  plot_data_inter <-
    data.frame(
      x = rep(tmp_interp$x, length(tmp_interp$y)),
      y = rep(tmp_interp$y, each = length(tmp_interp$x)),
      Prediction = tmp_interp$z %>% c()
    )
  plot_data_inter <-
    plot_data_inter[which(!is.na(plot_data_inter$Prediction)), ]
  return(plot_data_inter)
}
customColors <-
  colorRampPalette(colors = (x = RColorBrewer::brewer.pal(n = 11, name = "Spectral")))
inter_color_scale <- function(yMin, yMax, yMean)
{
  return(
    scale_fill_gradientn(
      colors =
        customColors(n = 100),
      values =
        scales::rescale(c(yMin,
                          yMean,
                          yMax)),
      guide = "colorbar",
      limits = c(yMin,
                 yMax)
    )
  )
}
for (iOuter in 1:3)
  # for(iOuter in 1)
{
  settingIndices <- 2 * (iOuter - 1) + 1:2
  # plotData <- data.frame(x = NULL, y = NULL, run = NULL, Orthogonalization = NULL,
  #                        Feature = NULL, n = NULL, TrueEffect = NULL)
  # for(i_setting in settingIndices)
  for (i_setting in settingIndices)
  {
    n <- simSetting[i_setting, 1]
    nonLinFIdx <- 3 * (simSetting[i_setting, 2] - 1) + 1:3
    interFIdx <- 3 * (simSetting[i_setting, 2] - 1) + 1:3
    noisesd <- simSetting[i_setting, 3]
    SettingString <- paste("n_", n, "_Eff_",
                           paste(nonLinFIdx, collapse = "_"),
                           sep = "")
    tmp_plot_data_main1 <- data.frame(x = NULL,
                                      y = NULL,
                                      ensembleIdx = NULL)
    tmp_plot_data_main2 <- data.frame(x = NULL,
                                      y = NULL,
                                      ensembleIdx = NULL)
    tmp_plot_data_main3 <- data.frame(x = NULL,
                                      y = NULL,
                                      ensembleIdx = NULL)
    tmp_plot_data_inter1 <- data.frame(x = NULL,
                                       y = NULL,
                                       ensembleIdx = NULL)
    tmp_plot_data_inter2 <- data.frame(x = NULL,
                                       y = NULL,
                                       ensembleIdx = NULL)
    tmp_plot_data_inter3 <- data.frame(x = NULL,
                                       y = NULL,
                                       ensembleIdx = NULL)
    for (j in 1:nSim)
    {
      resFileName <-
        paste("./UniformFeatureResults/",
              SettingString,
              "_run_",
              j,
              ".RDS",
              sep = "")
      tmpRes <- readRDS(resFileName)
      X <- tmpRes$data
      tmp_plot_data_main1 <-
        rbind(
          tmp_plot_data_main1,
          data.frame(
            x = X[, 1],
            y = tmpRes$predictions_features[, "X1"],
            ensembleIdx = j
          )
        )
      tmp_plot_data_main2 <-
        rbind(
          tmp_plot_data_main2,
          data.frame(
            x = X[, 2],
            y = tmpRes$predictions_features[, "X2"],
            ensembleIdx = j
          )
        )
      tmp_plot_data_main3 <-
        rbind(
          tmp_plot_data_main3,
          data.frame(
            x = X[, 3],
            y = tmpRes$predictions_features[, "X3"],
            ensembleIdx = j
          )
        )
      tmp_plot_data_inter1 <-
        rbind(
          tmp_plot_data_inter1,
          data.frame(
            x1 = X[, 1],
            x2 = X[, 2],
            y = tmpRes$predictions_features[, "X1_X2"],
            ensembleIdx = j
          )
        )
      tmp_plot_data_inter2 <-
        rbind(
          tmp_plot_data_inter2,
          data.frame(
            x1 = X[, 1],
            x2 = X[, 3],
            y = tmpRes$predictions_features[, "X1_X3"],
            ensembleIdx = j
          )
        )
      tmp_plot_data_inter3 <-
        rbind(
          tmp_plot_data_inter3,
          data.frame(
            x1 = X[, 2],
            x2 = X[, 3],
            y = tmpRes$predictions_features[, "X2_X3"],
            ensembleIdx = j
          )
        )
    }
    trueFileName <-
      paste("./UniformFeatureResults/",
            (i_setting + 1) %/% 2,
            "_TRUE.RDS",
            sep = "")
    # SettingString <- paste("n_", n, "_Eff_",
    #                        paste(nonLinFIdx, collapse = "_"),
    #                        sep = "")
    # trueFileName <- paste("./ONAM/UniformFeatureResults_New/", SettingString,
    #                       "_TRUE.RDS", sep = "")
    trueData <- readRDS(trueFileName)
    # tmp_true_data_main1 <- data.frame(x = trueData[[nSim + 1]][,1],
    #                                   y = trueData[[nSim + 1]][,18],
    #                                   ensembleIdx = "True")
    tmpTrue <- trueData[[nSim + 1]]
    # tmp_true_data_main1 <- data.frame(x = trueData[,1],
    #                                   y = trueData[,4],
    #                                   ensembleIdx = "True")
    tmp_true_data_main1 <- data.frame(
      x = tmpTrue$data[, "X1"],
      y = tmpTrue$predictions_features[, "X1"],
      ensembleIdx = "True"
    )
    tmp_plot_data_main1 <- tmp_plot_data_main1 %>%
      mutate(ensembleIdx = factor(ensembleIdx))
    mean_plot_data_main1 <- tmp_plot_data_main1 %>%
      group_by(x) %>% summarize(y = mean(y)) %>%
      mutate(ensembleIdx = "Mean")
    tmp_plot_data_main1 <-
      rbind(tmp_plot_data_main1,
            mean_plot_data_main1,
            tmp_true_data_main1)
    # tmp_true_data_main2 <- data.frame(x = trueData[[nSim + 1]][,2],
    #                                   y = trueData[[nSim + 1]][,17],
    #                                   ensembleIdx = "True")
    # tmp_true_data_main2 <- data.frame(x = trueData[,2],
    #                                   y = trueData[,5],
    #                                   ensembleIdx = "True")
    tmp_true_data_main2 <- data.frame(
      x = tmpTrue$data[, "X2"],
      y = tmpTrue$predictions_features[, "X2"],
      ensembleIdx = "True"
    )
    tmp_plot_data_main2 <- tmp_plot_data_main2 %>%
      mutate(ensembleIdx = factor(ensembleIdx))
    mean_plot_data_main2 <- tmp_plot_data_main2 %>%
      group_by(x) %>% summarize(y = mean(y)) %>%
      mutate(ensembleIdx = "Mean")
    tmp_plot_data_main2 <-
      rbind(tmp_plot_data_main2,
            mean_plot_data_main2,
            tmp_true_data_main2)
    # tmp_true_data_main3 <- data.frame(x = trueData[[nSim + 1]][,3],
    #                                   y = trueData[[nSim + 1]][,16],
    #                                   ensembleIdx = "True")
    # tmp_true_data_main3 <- data.frame(x = trueData[,3],
    #                                   y = trueData[,6],
    #                                   ensembleIdx = "True")
    tmp_true_data_main3 <- data.frame(
      x = tmpTrue$data[, "X3"],
      y = tmpTrue$predictions_features[, "X3"],
      ensembleIdx = "True"
    )
    tmp_plot_data_main3 <- tmp_plot_data_main3 %>%
      mutate(ensembleIdx = factor(ensembleIdx))
    mean_plot_data_main3 <- tmp_plot_data_main3 %>%
      group_by(x) %>% summarize(y = mean(y)) %>%
      mutate(ensembleIdx = "Mean")
    tmp_plot_data_main3 <-
      rbind(tmp_plot_data_main3,
            mean_plot_data_main3,
            tmp_true_data_main3)
    tmp_idx <- (iOuter - 1) * 3 + 1
    tmp_idx_inter <- (iOuter - 1) * 6 + 1
    #Graphic parameters
    group_colors = c(rep("steelblue", nSim), "royalblue", "grey15")
    names(group_colors) <- c(as.character(1:nSim), "Mean", "True")
    group_alphas = c(rep(0.5, nSim), 1, 1)
    plot_list_main[[2 - i_setting %% 2]][[tmp_idx]] <-
      # plot_list_main[[1]][[tmp_idx]] <-
      ggplot(tmp_plot_data_main1,
             aes(
               x = x,
               y = y,
               color = ensembleIdx,
               alpha = ensembleIdx
             )) +
      labs(title = plot_labels_main[tmp_idx]) +
      plot_theme_main +
      geom_line() +
      scale_color_manual(
        values = group_colors,
        breaks = c("1", "Mean",
                   "True"),
        labels = c("Estimate", "Mean estimate",
                   "True function")
      ) +
      scale_alpha_manual(values = group_alphas,
                         guide = "none") +
      labs(color = "") +
      xlab(TeX("$X_{1}$")) +
      ylab(TeX("$f_{1}(X_{1})$"))
    plot_list_main[[2 - i_setting %% 2]][[tmp_idx + 1]] <-
      # plot_list_main[[1]][[tmp_idx + 1]] <-
      ggplot(tmp_plot_data_main2,
             aes(
               x = x,
               y = y,
               color = ensembleIdx,
               alpha = ensembleIdx
             )) +
      labs(title = plot_labels_main[tmp_idx + 1]) +
      plot_theme_main +
      geom_line() +
      scale_color_manual(
        values = group_colors,
        breaks = c("1", "Mean",
                   "True"),
        labels = c("Estimate", "Mean estimate",
                   "True function")
      ) +
      scale_alpha_manual(values = group_alphas,
                         guide = "none") +
      labs(color = "") +
      xlab(TeX("$X_{2}$")) +
      ylab(TeX("$f_{2}(X_{2})$"))
    plot_list_main[[2 - i_setting %% 2]][[tmp_idx + 2]] <-
      # plot_list_main[[1]][[tmp_idx + 2]] <-
      ggplot(tmp_plot_data_main3,
             aes(
               x = x,
               y = y,
               color = ensembleIdx,
               alpha = ensembleIdx
             )) +
      labs(title = plot_labels_main[tmp_idx + 2]) +
      plot_theme_main +
      geom_line() +
      scale_color_manual(
        values = group_colors,
        breaks = c("1", "Mean",
                   "True"),
        labels = c("Estimate", "Mean estimate",
                   "True function")
      ) +
      scale_alpha_manual(values = group_alphas,
                         guide = "none") +
      labs(color = "") +
      xlab(TeX("$X_{3}$")) +
      ylab(TeX("$f_{3}(X_{3})$"))
    plot_data_inter1_mean <-
      get_average_data_inter(tmp_plot_data_inter1)
    # tmp_true_data_inter1 <-
    #   data.frame(x1 = trueData[[nSim + 1]][,1],
    #              x2 = trueData[[nSim + 1]][,2],
    #              y = trueData[[nSim + 1]][,15])
    # tmp_true_data_inter1 <-
    #   data.frame(x1 = trueData[,1],
    #              x2 = trueData[,2],
    #              y = trueData[,7])
    tmp_true_data_inter1 <-
      data.frame(
        x1 = tmpTrue$data[, "X1"],
        x2 = tmpTrue$data[, "X2"],
        y = tmpTrue$predictions_features[, "X1_X2"]
      )
    plot_data_inter1_true <-
      get_average_data_inter(tmp_true_data_inter1)
    yMin1 <-
      min(plot_data_inter1_mean$Prediction,
          plot_data_inter1_true$Prediction)
    yMax1 <-
      max(plot_data_inter1_mean$Prediction,
          plot_data_inter1_true$Prediction)
    yMean1 <-
      mean(c(
        plot_data_inter1_mean$Prediction,
        plot_data_inter1_true$Prediction
      ))
    color_scale_1 <- inter_color_scale(yMin1, yMax1, yMean1)
    plot_list_inter[[2 - i_setting %% 2]][[tmp_idx_inter]] <-
      # plot_list_inter[[1]][[tmp_idx_inter]] <-
      ggplot(plot_data_inter1_mean, aes(x = x, y = y, fill = Prediction)) +
      # geom_point(size = 0.75) +
      geom_tile() +
      labs(title = plot_labels_inter[tmp_idx_inter]) +
      plot_theme_inter +
      # scale_fill_gradientn(colors = customColors(n = 100)) +
      color_scale_1 +
      xlab(TeX("$X_{1}$")) +
      ylab(TeX("$X_{2}$"))
    plot_list_inter[[2 - i_setting %% 2]][[tmp_idx_inter + 1]] <-
      # plot_list_inter[[1]][[tmp_idx_inter + 1]] <-
      ggplot(plot_data_inter1_true, aes(x = x, y = y, fill = Prediction)) +
      # geom_point(size = 0.75) +
      geom_tile() +
      labs(title = plot_labels_inter[tmp_idx_inter + 1]) +
      plot_theme_inter +
      # scale_fill_gradientn(colors = customColors(n = 100)) +
      color_scale_1 +
      xlab(TeX("$X_{1}$")) +
      ylab(TeX("$X_{2}$"))
    plot_data_inter2_mean <-
      get_average_data_inter(tmp_plot_data_inter2)
    # tmp_true_data_inter2 <-
    #   data.frame(x1 = trueData[[nSim + 1]][,1],
    #              x2 = trueData[[nSim + 1]][,3],
    #              y = trueData[[nSim + 1]][,14])
    # tmp_true_data_inter2 <-
    #   data.frame(x1 = trueData[,1],
    #              x2 = trueData[,3],
    #              y = trueData[,8])
    tmp_true_data_inter2 <-
      data.frame(
        x1 = tmpTrue$data[, "X1"],
        x2 = tmpTrue$data[, "X3"],
        y = tmpTrue$predictions_features[, "X1_X3"]
      )
    plot_data_inter2_true <-
      get_average_data_inter(tmp_true_data_inter2)
    yMin2 <-
      min(plot_data_inter2_mean$Prediction,
          plot_data_inter2_true$Prediction)
    yMax2 <-
      max(plot_data_inter2_mean$Prediction,
          plot_data_inter2_true$Prediction)
    yMean2 <-
      mean(c(
        plot_data_inter2_mean$Prediction,
        plot_data_inter2_true$Prediction
      ))
    color_scale_2 <- inter_color_scale(yMin2, yMax2, yMean2)
    plot_list_inter[[2 - i_setting %% 2]][[tmp_idx_inter + 2]] <-
      # plot_list_inter[[1]][[tmp_idx_inter + 2]] <-
      ggplot(plot_data_inter2_mean, aes(x = x, y = y, fill = Prediction)) +
      # geom_point(size = 0.75) +
      geom_tile() +
      labs(title = plot_labels_inter[tmp_idx_inter + 2]) +
      plot_theme_inter +
      # scale_fill_gradientn(colors = customColors(n = 100)) +
      color_scale_2 +
      xlab(TeX("$X_{1}$")) +
      ylab(TeX("$X_{3}$"))
    plot_list_inter[[2 - i_setting %% 2]][[tmp_idx_inter + 3]] <-
      # plot_list_inter[[1]][[tmp_idx_inter + 3]] <-
      ggplot(plot_data_inter2_true, aes(x = x, y = y, fill = Prediction)) +
      # geom_point(size = 0.75) +
      geom_tile() +
      labs(title = plot_labels_inter[tmp_idx_inter + 3]) +
      plot_theme_inter +
      # scale_fill_gradientn(colors = customColors(n = 100)) +
      color_scale_2 +
      xlab(TeX("$X_{1}$")) +
      ylab(TeX("$X_{3}$"))
    plot_data_inter3_mean <-
      get_average_data_inter(tmp_plot_data_inter3)
    # tmp_true_data_inter3 <-
    #   data.frame(x1 = trueData[[nSim + 1]][,2],
    #              x2 = trueData[[nSim + 1]][,3],
    #              y = trueData[[nSim + 1]][,13])
    # tmp_true_data_inter3 <-
    #   data.frame(x1 = trueData[,2],
    #              x2 = trueData[,3],
    #              y = trueData[,9])
    tmp_true_data_inter3 <-
      data.frame(
        x1 = tmpTrue$data[, "X2"],
        x2 = tmpTrue$data[, "X3"],
        y = tmpTrue$predictions_features[, "X2_X3"]
      )
    plot_data_inter3_true <-
      get_average_data_inter(tmp_true_data_inter3)
    yMin3 <-
      min(plot_data_inter3_mean$Prediction,
          plot_data_inter3_true$Prediction)
    yMax3 <-
      max(plot_data_inter3_mean$Prediction,
          plot_data_inter3_true$Prediction)
    yMean3 <-
      mean(c(
        plot_data_inter3_mean$Prediction,
        plot_data_inter3_true$Prediction
      ))
    color_scale_3 <- inter_color_scale(yMin3, yMax3, yMean3)
    plot_list_inter[[2 - i_setting %% 2]][[tmp_idx_inter + 4]] <-
      # plot_list_inter[[1]][[tmp_idx_inter + 4]] <-
      ggplot(plot_data_inter3_mean, aes(x = x, y = y, fill = Prediction)) +
      # geom_point(size = 0.75) +
      geom_tile() +
      labs(title = plot_labels_inter[tmp_idx_inter + 4]) +
      plot_theme_inter +
      # scale_fill_gradientn(colors = customColors(n = 100)) +
      color_scale_3 +
      xlab(TeX("$X_{2}$")) +
      ylab(TeX("$X_{3}$"))
    plot_list_inter[[2 - i_setting %% 2]][[tmp_idx_inter + 5]] <-
      # plot_list_inter[[1]][[tmp_idx_inter + 5]] <-
      ggplot(plot_data_inter3_true, aes(x = x, y = y, fill = Prediction)) +
      # geom_point(size = 0.75) +
      geom_tile() +
      labs(title = plot_labels_inter[tmp_idx_inter + 5]) +
      plot_theme_inter +
      # scale_fill_gradientn(colors = customColors(n = 100)) +
      color_scale_3 +
      xlab(TeX("$X_{2}$")) +
      ylab(TeX("$X_{3}$"))
  }
}
names(plot_list_main[[1]]) <- letters[1:length(plot_list_main[[1]])]
names(plot_list_main[[2]]) <- letters[1:length(plot_list_main[[2]])]
plot_layout_main <-
  c("aaabbbccc
     aaabbbccc
     dddeeefff
     dddeeefff
     ggghhhiii
     ggghhhiii")
big_plot_n_main_2000 <-
  wrap_plots(plot_list_main[[1]], guides = "collect",
             design = plot_layout_main) &
  theme(legend.position = "bottom")
ggsave(
  "sim_main_n_2000.png",
  big_plot_n_main_2000,
  device = "png",
  width = 9,
  height = 10,
  bg = "transparent"
)
big_plot_n_main_5000 <-
  wrap_plots(plot_list_main[[2]], guides = "collect",
             design = plot_layout_main) &
  theme(legend.position = "bottom")
ggsave(
  "sim_main_n_5000.png",
  big_plot_n_main_5000,
  device = "png",
  width = 9,
  height = 10,
  bg = "transparent"
)
plot_list_inter_extra <- list(
  A = ggplot(),
  B = ggplot() + annotate("text", label = "Setting 1"),
  C = ggplot() + annotate("text", label = "Setting 2"),
  D = ggplot() + annotate("text", label = "Setting 3")
)
names(plot_list_inter[[1]]) <-
  letters[1:length(plot_list_inter[[1]])]
plot_list_inter_2000_final <-
  c(plot_list_inter[[1]], plot_list_inter_extra)
names(plot_list_inter[[2]]) <-
  letters[1:length(plot_list_inter[[2]])]
# plot_layout_inter <-
#   c("aaabbb
#      aaabbb
#      cccddd
#      cccddd
#      eeefff
#      eeefff
#      ggghhh
#      ggghhh
#      iiijjj
#      iiijjj
#      kkklll
#      kkklll
#      mmmnnn
#      mmmnnn
#      oooppp
#      oooppp
#      qqqrrr
#      qqqrrr")

# plot_layout_inter <-
#   c("AABBAAAAAACCAA
#      aaabbbAAcccddd
#      aaabbbAAcccddd
#      eeefffAAggghhh
#      eeefffAAggghhh
#      iiijjjAAkkklll
#      iiijjjAAkkklll
#      AAAAAADDAAAAAA
#      AAAAmmmnnnAAAA
#      AAAAmmmnnnAAAA
#      AAAAooopppAAAA
#      AAAAooopppAAAA
#      AAAAqqqrrrAAAA
#      AAAAqqqrrrAAAA")
# big_plot_n_inter_2000 <-
#   wrap_plots(plot_list_inter_2000_final, guides = "collect",
#              design = plot_layout_inter) +
#   theme(legend.position = "none")

plots_inter_2000_l <- lapply(1:9, function(idx) {
  plot_list_inter[[1]][[2 * idx]] %>% get_legend() %>% as_ggplot()
})
plots_inter_2000 <-
  lapply(plot_list_inter[[1]],
         function(plt) plt + theme(legend.position = "none"))
plt_2000_1 <-
  plots_inter_2000[1:6] %>%
  append(plots_inter_2000_l[1], 1) %>%
  append(plots_inter_2000_l[2], 4) %>%
  append(plots_inter_2000_l[3], 7)
plt_2000_2 <-
  plots_inter_2000[7:12] %>%
  append(plots_inter_2000_l[4], 1) %>%
  append(plots_inter_2000_l[5], 4) %>%
  append(plots_inter_2000_l[6], 7)
plt_2000_3 <-
  plots_inter_2000[13:18] %>%
  append(plots_inter_2000_l[7], 1) %>%
  append(plots_inter_2000_l[8], 4) %>%
  append(plots_inter_2000_l[9], 7)
setting_2000_1 <- ggarrange(plotlist = plt_2000_1,
                       nrow = 3, ncol = 3,
                       widths = c(1, 0.25, 1)) %>%
  annotate_figure(top = text_grob("Setting 1",
                                  face = "bold",
                                  size = 24))
setting_2000_2 <- ggarrange(plotlist = plt_2000_2,
                       nrow = 3, ncol = 3,
                       widths = c(1, 0.25, 1)) %>%
  annotate_figure(top = text_grob("Setting 2",
                                  face = "bold",
                                  size = 24))
setting_2000_3 <- ggarrange(plotlist = plt_2000_3,
                       nrow = 3, ncol = 3,
                       widths = c(1, 0.25, 1)) %>%
  annotate_figure(top = text_grob("Setting 3",
                                  face = "bold",
                                  size = 24))

setting_2000_all <-
  ggarrange(ggarrange(setting_2000_1, setting_2000_2, ncol = 2),
            ggarrange(ggplot() + theme_void(),
                      setting_2000_3,
                      ggplot() + theme_void(),
                      widths = c(0.4, 1, 0.4),
                      ncol = 3),
            nrow = 2)
ggsave("sim_inter_n_2000.pdf",
       setting_2000_all,
       width = 15,
       height = 12,
       bg = "transparent")

plots_inter_5000_l <- lapply(1:9, function(idx) {
  plot_list_inter[[2]][[2 * idx]] %>% get_legend() %>% as_ggplot()
})
plots_inter_5000 <-
  lapply(plot_list_inter[[2]],
         function(plt) plt + theme(legend.position = "none"))
plt_5000_1 <-
  plots_inter_5000[1:6] %>%
  append(plots_inter_5000_l[1], 1) %>%
  append(plots_inter_5000_l[2], 4) %>%
  append(plots_inter_5000_l[3], 7)
plt_5000_2 <-
  plots_inter_5000[7:12] %>%
  append(plots_inter_5000_l[4], 1) %>%
  append(plots_inter_5000_l[5], 4) %>%
  append(plots_inter_5000_l[6], 7)
plt_5000_3 <-
  plots_inter_5000[13:18] %>%
  append(plots_inter_5000_l[7], 1) %>%
  append(plots_inter_5000_l[8], 4) %>%
  append(plots_inter_5000_l[9], 7)
setting_5000_1 <- ggarrange(plotlist = plt_5000_1,
                            nrow = 3, ncol = 3,
                            widths = c(1, 0.25, 1)) %>%
  annotate_figure(top = text_grob("Setting 1",
                                  face = "bold",
                                  size = 24))
setting_5000_2 <- ggarrange(plotlist = plt_5000_2,
                            nrow = 3, ncol = 3,
                            widths = c(1, 0.25, 1)) %>%
  annotate_figure(top = text_grob("Setting 2",
                                  face = "bold",
                                  size = 24))
setting_5000_3 <- ggarrange(plotlist = plt_5000_3,
                            nrow = 3, ncol = 3,
                            widths = c(1, 0.25, 1)) %>%
  annotate_figure(top = text_grob("Setting 3",
                                  face = "bold",
                                  size = 24))

setting_5000_all <-
  ggarrange(ggarrange(setting_5000_1, setting_5000_2, ncol = 2),
            ggarrange(ggplot() + theme_void(),
                      setting_5000_3,
                      ggplot() + theme_void(),
                      widths = c(0.4, 1, 0.4),
                      ncol = 3),
            nrow = 2)
ggsave("sim_inter_n_5000.pdf",
       setting_5000_all,
       width = 15,
       height = 12,
       bg = "transparent")
# big_plot_n_inter_2000 <-
#   ggarrange(plotlist = plot_list_inter[[1]],
#             ncol = 2,
#             nrow = 9)
# ggsave(
#   "sim_inter_n_2000.pdf",
#   big_plot_n_inter_2000,
#   device = "pdf",
#   width = 6,
#   height = 20,
#   useDingbats = TRUE
# )
# big_plot_n_inter_5000 <-
#   wrap_plots(plot_list_inter[[1]], guides = "collect",
#              design = plot_layout_inter) &
#   theme(legend.position = "none")
# big_plot_n_inter_5000 <-
#   ggarrange(plotlist = plot_list_inter[[2]],
#             ncol = 2,
#             nrow = 9)
# ggsave(
#   "sim_inter_n_5000.pdf",
#   big_plot_n_inter_5000,
#   device = "pdf",
#   width = 6,
#   height = 20,
#   useDingbats = TRUE
# )
#
# big_plot_n_inter_5000 <-
#   ggarrange(plotlist = plot_list_inter[[1]],
#             ncol = 2,
#             nrow = 9)

