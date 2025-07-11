% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_modelfit_data.R
\name{clean_modelfit_data}
\alias{clean_modelfit_data}
\title{Clean / check the dataset before passing to model fitting tool}
\usage{
clean_modelfit_data(model, try_make_numeric = TRUE, data = NULL)
}
\arguments{
\item{try_make_numeric}{should function try to turn character columns
into numeric columns? If \code{FALSE} will just set all values to 0 (but
retain column to avoid issues).}
}
\description{
Clean / check the dataset before passing to model fitting tool
}
