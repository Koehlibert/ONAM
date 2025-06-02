rm(list = ls())
library(batchtools)

setwd("/home/koehler/IML")
cluster.functions <-
  makeClusterFunctionsSlurm(
    template = "batchtools.slurm.tmpl",
    array.jobs = TRUE,
    scheduler.latency = 1,
    fs.latency = 65
  )
set.seed(1)

library(MASS)
library(dplyr)
library(ONAM)
library(bench)
n_sim <- 50
p_vals <- c(10, 25, 40, 80)
p_inf_vals <- c(3, 10, 20, 40)
n_inter_vals <- p_inf_vals
n_vals <- c(2000, 5000)
sim_setting <- expand.grid(n_vals, 1:length(p_vals))
sim_setting <-
  sim_setting[rep(seq_len(nrow(sim_setting)), each = n_sim),]
sim <- function(i) {
  gc()
  library(MASS)
  library(dplyr)
  library(ONAM)
  library(bench)
  n_sim <- 50
  p_vals <- c(10, 25, 40, 80)
  p_inf_vals <- c(3, 10, 20, 40)
  n_inter_vals <- p_inf_vals
  n_vals <- c(2000, 5000)
  sim_setting <- expand.grid(n_vals, 1:length(p_vals))
  sim_setting <-
    sim_setting[rep(seq_len(nrow(sim_setting)), each = n_sim),]
  lotf <- list(function(x)
    cos(2 * x),
    function(x)
      tanh(0.5 * x),
    function(x)
      dnorm(x - 1.5) + dnorm(x + 1.5),
    function(x)
      cos(x * 3 - 2) * (-x * 3),
    function(x)
      ifelse(x > 0, pweibull(x, shape = 3),
             pweibull(-x, shape = 0.5)),
    function(x)
      x ^ 2,
    function(x)
      cos(2 * x),
    function(x)
      tanh(x),
    function(x)
      - x ^ 3)
  interf <-
    list(function(x1, x2)
      sin(1.5 * (x1 ^ 2 - x2 ^ 2)) + dnorm(0.5 * x1 * x2),
      function(x1, x2)
        cos(x1 + x2) + sin(x1 * x2),
      function(x1, x2)
        0.5 * sin((x1 - 1) ^ 2 + (x2 + 1) ^ 2),
      function(x1, x2)
        sin(0.75 * x2 * x1) + 0.25 * sqrt(x1 ^ 2 + x2 ^ 2) +
        0.25 * cos((x1 - pi) * (x2 + pi)),
      function(x1, x2)
        sin(x1 ^ 2 + x2 ^ 2),
      function(x1, x2)
        sin(x1 + x2),
      function(x1, x2)
        - cos(1.5 * x1 - 0.75 * x2),
      function(x1, x2)
        cos((x1 - pi) * (x2 + pi)),
      function(x1, x2)
        ((dnorm(x1) / dnorm(0) - 0.5) * 2) *
        ((pnorm(x2) - 0.5) * 2) +
        sin(x1 - x2))
  n <- sim_setting[i, 1]
  tmp_idx <- sim_setting[i, 2]
  p <- p_vals[tmp_idx]
  sigma <- matrix(0.5, ncol = p, nrow = p)
  diag(sigma) <- 1
  mu_vec <- rep(0, p)
  p_inf <- p_inf_vals[tmp_idx]
  n_inter <- n_inter_vals[tmp_idx]
  Z <- mvrnorm(n, mu = mu_vec, Sigma = sigma)
  X <- (pnorm(Z) - 0.5) * 6
  rm(Z)
  Y <- 0
  set.seed(p)
  for (i_p in 1:p_inf) {
    Y <- Y + lotf[[sample(seq_along(lotf), 1)]](X[, i_p])
  }
  pairs <- expand.grid(1:p, 1:p)
  pairs <- pairs[which(pairs[, 1] < pairs[, 2]),]
  inter_rows <- sample((1:nrow(pairs)), n_inter)
  chosen_pairs <- matrix(nrow = 0, ncol = 2)
  for (i_p_int in 1:n_inter) {
    x1 <- pairs[inter_rows[i_p_int], 1]
    x2 <- pairs[inter_rows[i_p_int], 2]
    chosen_pairs <- rbind(chosen_pairs, pairs[inter_rows[i_p_int], ])
    Y <-
      Y + interf[[sample(seq_along(interf), 1)]](X[, x1], X[, x2])
  }
  train_data <- cbind(X, Y)
  colnames(train_data) <- c(paste("x", 1:p, sep = ""), "y")
  formula <- formula(paste0(
    "y ~ ",
    paste0("mod(", paste0("x", 1:p_inf), ") + ", collapse = ""),
    paste0(apply(chosen_pairs, 1, function(x) {
      paste0("mod(x", x[1], ", x", x[2], ") + ")
    }), collapse = ""),
    "mod(.)"
  ))
  list_of_deep_models =
    list(mod = ONAM:::get_submodel)
  runtime <-
    system.time({
      onam(
        formula,
        list_of_deep_models,
        train_data,
        n_ensemble = 1,
        epochs = 500,
        progresstext = FALSE,
        verbose = 0
      )})
  c(n, p_inf,
    runtime)
}

reg <- makeRegistry(file.dir = paste0("/home/koehler/IML/results"))

ids <-
  batchMap(fun = sim,
           args = list(i = 1:nrow(simSetting)),
           reg = reg)

submitJobs(
  ids,
  res = list(
    memory = 100000,
    partition = "batch",
    walltime = 50000
  ),
  reg = reg
)

while (!waitForJobs(ids = ids)) {
  Sys.sleep(1)
}

results <- reduceResultsList(reg = reg)
save(results, file = "test.rds")
