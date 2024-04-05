# imagePath <- "//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/SimulationEval/Plots"
nVals <- c(500,1000,10000)
Effects <- c(1,2,3)
noiseVals <- c(0.1,1,2)
simSetting <- expand.grid(nVals, Effects, noiseVals)
nSim <- 10
#Setting of interest: sd = 1, n = 500, 1000, 10000, eff = 1,2,3
errorChangeData <- data.frame(Feature = NULL, Change = NULL,
                              n = NULL, run = NULL,
                              Setting = NULL, sd = NULL)
for(iOuter in 1)
{
  settingIndices <- 3 * (iOuter - 1) + 1:3
  # plotData <- data.frame(x = NULL, y = NULL, run = NULL, Orthogonalization = NULL,
  #                        Feature = NULL, n = NULL, TrueEffect = NULL)
  for(iSetting in settingIndices)
  {
    n <- simSetting[iSetting, 1]
    nonLinFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
    interFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
    noisesd <- simSetting[iSetting, 3]
    SettingString <- paste("n_", n, "_Eff_",
                           paste(nonLinFIdx, collapse = "_"),
                           "_sd_", noisesd, sep = "")
    for(j in 1:nSim)
    {
      resFileName <- paste(SettingString, "_run_", j, ".RDS", sep = "")
      tmpRes <- readRDS(resFileName)
      tmpData <- data.frame(Feature = c("X1", "X2", "X3"),
                            change =
                              unlist(lapply(tmpRes$effectErrorsPost, mean)) /
                              unlist(lapply(tmpRes$effectErrorsPre, mean)),
                            n = n,
                            run = j,
                            Setting = simSetting[iSetting, 2],
                            sd = noisesd)
      errorChangeData <- rbind(errorChangeData, tmpData)
    }
  }
}
errorChangeData$n <- as.factor(errorChangeData$n)
errorChangeData$logChange <- log(errorChangeData$change)
saveRDS(errorChangeData, "errorChangeData.RDS")
ErrorChangePlot <- ggplot(errorChangeData, aes(x = n, y = change,
                                               color = Feature)) +
  geom_boxplot() +
  facet_wrap(~Setting~sd, scales = "free_y") +
  ggtitle("Change in relative error between true and estimated function before and after orthogonalization")
ggsave(paste("ErrorChangePlot.png", sep = ""), ErrorChangePlot,
       # path = imagePath,
       device = "png", width = 8, height = 5)
LogErrorChangePlot <- ggplot(errorChangeData,
                             aes(x = n, y = logChange,
                                 color = Feature)) +
  geom_boxplot() +
  facet_wrap(~Setting~sd, scales = "free_y") +
  ylab("Log of change in error after orthogonalisation") +
  ggtitle("Change in relative error between true and estimated function before and after orthogonalization")
ggsave(paste("LogErrorChangePlot.png", sep = ""), LogErrorChangePlot,
       # path = imagePath,
       device = "png", width = 8, height = 5)
tmpData <- errorChangeData %>% filter(sd == 0.1)
tmpErrorChangePlot <- ggplot(tmpData,
                             aes(x = n, y = change,
                                 color = Feature)) +
  geom_boxplot() +
  facet_wrap(~Setting~sd, scales = "free_y") +
  ylab("Change in error after orthogonalisation") +
  ggtitle("Change in relative error between true and estimated function before and after orthogonalization")
ggsave(paste("tmpErrorChangePlot.png", sep = ""), tmpErrorChangePlot,
       # path = imagePath,
       device = "png", width = 8, height = 5)
