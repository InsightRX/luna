% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_model.R
\name{fit_model}
\alias{fit_model}
\title{Fit model using NONMEM or nlmixr2}
\usage{
fit_model(model, data, tool = "nonmem", path, ...)
}
\arguments{
\item{model}{pharmpy model object}

\item{data}{data.frame with data to fit}

\item{tool}{either \code{nonmem} or \code{nlmixr}}

\item{path}{path to .rds file to save fit results to}

\item{...}{passed onto \code{run_nmfe()} function}
}
\description{
Takes a pharmpy-loaded NONMEM model as input, and returns a pharmpy model
results object. So essentially this function is a drop-in replacement for the
run_modelfit() function in pharmr/pharmpy.
}
