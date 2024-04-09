runSimulation <- function(idx)
{
  #initial setup####
  nSim <- 100
  p <- 10
  p_inf <- 3
  n_inter <- factorial(p_inf)/(2*factorial(p_inf - 2))
  AllNet <- TRUE
  noisy <- TRUE
  regularize <- TRUE
  nVals <- c(2000,5000,10000)
  Effects <- c(1,2,3)
  noiseVals <- c(0.1,0.5,1)
  simSetting <- expand.grid(nVals, Effects, noiseVals)
  lotf <- list(function(x) cos(2*x),
               function(x) tanh(3*x),
               function(x) -x^3,
               function(x) cos(x*3-2)*(-x*3),
               function(x) ifelse(x > 0, pweibull(x, shape = 3),
                                  pweibull(-x, shape = 0.5)),
               function(x) x^2,
               function(x) sin(x)*cos(x),
               function(x) sqrt(abs(x)),
               function(x) dnorm(x)-0.125,
               function(x) -x * tanh(3*x) * sin(4*x))
  interf <- list(function(x1, x2) x1 * (-x2),
                 function(x1, x2) sin(0.1*x1) * exp(x2),
                 function(x1, x2) max(x1, x2),
                 function(x1, x2) x1 - x2,
                 function(x1, x2) ifelse(x1 > 0, sqrt(abs(x2)), x2),
                 function(x1, x2) sqrt(x1^2 + x2^2),
                 function(x1, x2) cos(x1) * x2^3,
                 function(x1, x2) exp(0.5*x1) + sqrt(abs(x2-1)),
                 function(x1, x2) sqrt((x1 - 2)^2 + (x2 + 1) ^2)
  )
  #Simulation setting####
  iSetting <- idx %/% nSim + 1
  n <- simSetting[iSetting, 1]
  nonLinFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
  interFIdx <- 3 * (simSetting[iSetting, 2] - 1) + 1:3
  noisesd <- simSetting[iSetting, 3]
  SettingString <- paste("n_", n, "_Eff_",
                         paste(nonLinFIdx, collapse = "_"),
                         "_sd_", noisesd, sep = "")
  #create data####
  X <- matrix(rnorm(p * n), ncol = p)
  originalData <- X
  #purify effects to get only nonlinear part#####
  interFPre <- lapply(1:n_inter, function(i)
  {
    ONAM:::purifyInterFunction(i, interf = interf,
                               interFIdx = interFIdx, X = X)
  })
  nonLinFPre <- lapply(1:p_inf, function(i)
  {
    ONAM:::purifyNonLinFunction(i, lotf = lotf, nonLinFIdx = nonLinFIdx,
                                X = X)
  })
  PHOList <-
    ONAM:::stackedOrthFunction(nonLinFPre, interFPre, 0.1,
                               originalData)
  nonLinF <- PHOList$nonLinF
  interF <- PHOList$interF
  # trueBetas <- betas + PHOList$b
  #Generate data####
  Y <- rep(0, n)
  # Y <- X %*% trueBetas
  for(idx in 1:p_inf)
  {
    Y <- Y + nonLinF[[idx]]
  }
  for(idx in 1:n_inter)
  {
    Y <- Y + interF[[idx]]
  }
  if(noisy)
  {
    noise <- rnorm(n, sd = noisesd)
    Y <- Y + noise
  }
  originalData <- cbind(originalData, Y)
  colnames(originalData) <- c(paste("X", 1:p, sep = ""), "Y")
  modelFormula <-
    formula(Y ~ deep_model1(X1) + deep_model1(X2) + deep_model1(X3) +
              deep_model1(X1, X2) + deep_model1(X1, X3) + deep_model1(X2, X3) +
              deep_model1(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10))
  list_of_deep_models = list(deep_model1 = getSubModel)
  modelRunTime <-
    system.time({modelRes <-
      ONAM:::fitPHOModel(modelFormula, list_of_deep_models,
                         originalData, 10)})
  modelEvalData <-
    ONAM:::evaluateModel(modelRes, nonLinF,
                         simSetting[iSetting,],
                         modelRunTime)
  #resFileName <- paste(SettingString, "_run_", j, ".RDS", sep = "")
  #saveRDS(modelEvalData, file = resFileName)
  return(modelEvalData)
}
