rm(list = ls())
gc()

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

# setwd("//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/Keras Stacked Orthogonalization LA//")
# setwd("Z:/Biostatistik/Projekte_Koehler/Deepregression/Keras Stacked Orthogonalization LA//")
source("FunctionsOrthoTest//KerasFunctionsSource.R")
set.seed(1)

# create registry
reg <- makeRegistry(file.dir = paste0("/home/dkoehler/IML/results"), seed = 1)
# create job ids and add them to the registry
#ids <- batchMap(fun = runSimulation, args = list(idx = 1:(nSim * nrow(simSetting))),
#                reg = reg)
ids <- batchMap(fun = runSimulation, args = list(idx = 1:(2)),
                reg = reg)

# submit jobs
submitJobs(ids, res = list(walltime = 6*60*60, memory = 2048,
                           partition = "short"), reg = reg) 

#results <- reduceResultsList(reg = r)
#save(results, file = "AllPScores.rds")
