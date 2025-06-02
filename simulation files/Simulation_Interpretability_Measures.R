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
interp_measures <-
  data.frame(
    effects = rep(Effects, each = length(nVals)),
    n = rep(nVals, length(Effects)),
    I1 = NA,
    I2 = NA
  )
for (iOuter in 1:3)
  # for(iOuter in 1)
{
  settingIndices <- 2 * (iOuter - 1) + 1:2
  for (i_setting in settingIndices)
  {
    n <- simSetting[i_setting, 1]
    nonLinFIdx <- 3 * (simSetting[i_setting, 2] - 1) + 1:3
    interFIdx <- 3 * (simSetting[i_setting, 2] - 1) + 1:3
    noisesd <- simSetting[i_setting, 3]
    SettingString <- paste("n_", n, "_Eff_",
                           paste(nonLinFIdx, collapse = "_"),
                           sep = "")
    tmp_i2 <- tmp_i1 <- rep(0, nSim)
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
      tmp_decomp <- ONAM:::decompose(tmpRes)$var_decomp
      tmp_i1[j] <- tmp_decomp[1]
      tmp_i2[j] <- tmp_decomp[2]
    }
    interp_measures[i_setting, c(3,4)] <- c(mean(tmp_i1), mean(tmp_i2))
  }
}
