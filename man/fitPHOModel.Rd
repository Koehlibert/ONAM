% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Orthogonalize.R
\name{fitPHOModel}
\alias{fitPHOModel}
\title{Fit orthogonal neural additive model}
\usage{
fitPHOModel(
  modelFormula,
  list_of_deep_models,
  data,
  nEnsemble = 20,
  progresstext = FALSE,
  verbose = 0
)
}
\arguments{
\item{modelFormula}{Formula for model fitting. Specify deep parts with the same name as \code{list_of_deep_models}.}

\item{list_of_deep_models}{List of named models used in \code{model_formula}.}

\item{data}{Data to be fitted}

\item{nEnsemble}{Number of orthogonal neural additive model ensembles}

\item{progresstext}{Show model fitting progress. If \code{TRUE}, shows current number of ensemble being fitted}

\item{verbose}{Verbose argument for internal model fitting. Used for debugging.}
}
\value{
Returns a pho model object, containing all ensemble members, ensemble weights, and main and interaction effect outputs.
}
\description{
Fit orthogonal neural additive model
}
