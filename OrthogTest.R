modelEvalData <-
  readRDS("UniformFeatureResults/n_10000_Eff_1_2_3_sd_0.1_run_1.RDS")

mainF1Pre <- modelEvalData$totalFeaturePredsPre$X1
mainF2Pre <- modelEvalData$totalFeaturePredsPre$X2
mainF3Pre <- modelEvalData$totalFeaturePredsPre$X3
interF12Pre <- modelEvalData$totalFeaturePredsPre$X1_X2
interF13Pre <- modelEvalData$totalFeaturePredsPre$X1_X3
interF23Pre <- modelEvalData$totalFeaturePredsPre$X2_X3
resFPre <-
  modelEvalData$totalFeaturePredsPre$X1_X2_X3_X4_X5_X6_X7_X8_X9_X10

sumMainPre <- mainF1Pre + mainF2Pre + mainF3Pre
sumInterPre <- interF12Pre + interF13Pre + interF23Pre
sumInterMainPre <- sumMainPre + sumInterPre

t(resFPre) %*% sumInterMainPre
t(sumInterPre) %*% sumMainPre

mainF1Post <- modelEvalData$totalFeaturePredsPost$X1
mainF2Post <- modelEvalData$totalFeaturePredsPost$X2
mainF3Post <- modelEvalData$totalFeaturePredsPost$X3
interF12Post <- modelEvalData$totalFeaturePredsPost$X1_X2
interF13Post <- modelEvalData$totalFeaturePredsPost$X1_X3
interF23Post <- modelEvalData$totalFeaturePredsPost$X2_X3
resFPost <-
  modelEvalData$totalFeaturePredsPost$X1_X2_X3_X4_X5_X6_X7_X8_X9_X10

sumMainPost <- mainF1Post + mainF2Post + mainF3Post
sumInterPost <- interF12Post + interF13Post + interF23Post
sumInterMainPost <- sumMainPost + sumInterPost

t(resFPost) %*% sumInterMainPost
t(sumInterPost) %*% sumMainPost
