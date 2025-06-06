% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/onam.R
\name{summary.onam}
\alias{summary.onam}
\alias{print.summary.onam}
\title{Get summary of an onam object}
\usage{
\method{summary}{onam}(object, ...)

\method{print}{summary.onam}(x, ...)
}
\arguments{
\item{object}{onam object of class \code{onam} as returned from
\code{\link{onam}} to be summarized}

\item{...}{further arguments passed to or from other methods.}

\item{x}{object of class \code{\link{summary.onam}}.}
}
\value{
Gives summary of the \code{onam} object, including model inputs, number
of ensembles, correlation of model output and original outcome variable, and
interpretability metrics i_1 and i_2
}
\description{
generates a summary of a fitted \code{onam} object including
information on ensembling strategy and performance metrics such as
correlation and degree of interpretabiltity
}
\examples{
\donttest{
# Basic example for a simple ONAM-model
# Create training data
n <- 1000
x1 <- runif(n, -2, 2)
x2 <- runif(n, -2, 2)
y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
  pweibull(-x2, shape = 0.5)) +
  x1 * x2
data_train <- cbind(x1, x2, y)
# Define model
model_formula <- y ~ mod1(x1) + mod1(x2) +
  mod1(x1, x2)
list_of_deep_models <- list(mod1 = ONAM:::get_submodel)
# Fit model
callback <-
keras::keras$callbacks$EarlyStopping(monitor = "loss",
                                     patience = 10)
mod <- onam(model_formula, list_of_deep_models,
                   data_train, n_ensemble = 2, epochs = 50,
                   callback = callback,
                   progresstext = TRUE, verbose = 1)
summary(mod)
}
}
