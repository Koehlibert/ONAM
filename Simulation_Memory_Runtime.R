rm(list = ls())
gc()
library(MASS)
library(dplyr)
library(ONAM)
library(bench)
source("SimulationSettings.R")
n_big <- 50000
n_sim <- 10
p_vals <- c(10, 50, 100)
p_inf_vals <- c(3, 20, 50)
n_inter_vals <- p_inf_vals
n_vals <- c(2000, 5000, 10000)
sim_setting <- expand.grid(1:3, n_vals)
sim_setting <-
  sim_setting[rep(seq_len(nrow(sim_setting)), each = 10), ]
benchmark_results <- matrix(nrow = 0, ncol = 4)
sim_fct <- function(i) {
  n <- sim_setting[i, 2]
  tmp_idx <- sim_setting[i, 1]
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
  for (i_p in 1:p_inf) {
    Y <- Y + lotf[[sample(seq_along(lotf), 1)]](X[, i_p])
  }
  pairs <- expand.grid(1:p, 1:p)
  pairs <- pairs[which(pairs[, 1] < pairs[, 2]),]
  chosen_pairs <- matrix(nrow = 0, ncol = 2)
  for (i_p_int in 1:n_inter) {
    tmp <- sample(1:nrow(pairs), 1)
    x1 <- pairs[tmp, 1]
    x2 <- pairs[tmp, 2]
    pairs <- pairs[-tmp,]
    chosen_pairs <- rbind(chosen_pairs, pairs[tmp, ])
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
  callback <-
    keras::keras$callbacks$EarlyStopping(monitor = "loss",
                                         patience = 20)
  benchmark <-
    mark(
      onam(
        formula,
        list_of_deep_models,
        train_data,
        n_ensemble = 1,
        epochs = 500,
        callback = callback,
        progresstext = FALSE,
        verbose = 1
      ),
      iterations = 1,
      check = FALSE
    )
  c(n, p_inf,
    benchmark$median,
    benchmark$mem_alloc)
}
