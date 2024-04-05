imagePath <- "//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/SimulationEval/Plots"
nVals <- c(500,1000,10000)
Effects <- c(1,2,3)
noiseVals <- c(0.1,1,2)
simSetting <- expand.grid(nVals, Effects, noiseVals)
nSim <- 10
#Setting of interest: sd = 1, n = 500, 1000, 10000, eff = 1,2,3
errorChangeData <- data.frame(Change = NULL,
                              n = NULL, run = NULL, 
                              Setting = NULL, sd = NULL)
for(iOuter in 1:9)
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
      resFileName <- paste("./SimulationResults/", SettingString, "_run_", j, ".RDS", sep = "")
      tmpRes <- readRDS(resFileName)
      tmpData <- data.frame(change = rowMeans(matrix(
                              unlist(lapply(tmpRes$effectErrorsPost, mean)) /
                              unlist(lapply(tmpRes$effectErrorsPre, mean)), 
                              nrow = 3)),
                            n = n,
                            run = j,
                            Setting = simSetting[iSetting, 2],
                            sd = noisesd)
      errorChangeData <- rbind(errorChangeData, tmpData)
    }
  }
}
errorChangeData$n <- as.factor(errorChangeData$n)
saveRDS(errorChangeData, "errorChangeData.RDS")
ErrorChangePlot <- ggplot(errorChangeData, aes(x = n, y = change)) + 
  geom_boxplot() +
  facet_wrap(~Setting~sd, scales = "free_y") +
  ggtitle("Change in relative error between true and estimated function before and after orthogonalization")
ggsave(paste("ErrorChangePlot.png", sep = ""), ErrorChangePlot,
       path = imagePath,
       device = "png", width = 8, height = 5)