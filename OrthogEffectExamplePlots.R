imagePath <- "//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/SimulationEval/Plots"
nVals <- c(500,1000,10000)
Effects <- c(1,2,3)
noiseVals <- c(0.1,1,2)
simSetting <- expand.grid(nVals, Effects, noiseVals)
nSim <- 10
#Setting of interest: sd = 1, n = 500, 1000, 10000, eff = 1,2,3
errorChangeData <- data.frame(Feature = NULL, Change = NULL,
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
      tmpData <- data.frame(Feature = c("X1", "X2", "X3"),
                            change = 
                              unlist(lapply(tmpRes$effectErrorsPre, mean)) /
                              unlist(lapply(tmpRes$effectErrorsPost, mean)),
                            n = n,
                            run = j,
                            Setting = simSetting[iSetting, 2],
                            sd = noisesd)
      errorChangeData <- rbind(errorChangeData, tmpData)
    }
  }
}
errorData$n <- as.factor(errorData$n)
# ErrorPlot <- ggplot(errorData, aes(x = n, y = Error, color = Feature)) + geom_boxplot() +
#   facet_wrap(~Orthogonalization) + 
#   ggtitle(paste("Squared area between true and estimated effect, noise sd = ", 
#                 noisesd, ", DGP setting = ", simSetting[iSetting, 2], 
#                 sep = ""))
# ggsave(paste("ErrorPlot", iOuter, ".png", sep = ""), ErrorPlot,
#        path = imagePath,
#        device = "png", width = 8, height = 5)