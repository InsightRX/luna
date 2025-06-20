% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_nlme.R
\name{call_nmfe}
\alias{call_nmfe}
\title{Call nmfe}
\usage{
call_nmfe(
  model_file,
  output_file,
  path,
  nmfe = "/opt/NONMEM/nm_current/run/nmfe75",
  console = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{model_file}{model file, e.g. "run.mod"}

\item{output_file}{output file, e.g. "run.lst"}

\item{path}{run folder path, e.g. "run1"}

\item{nmfe}{path to nmfe batch file to run NONMEM}

\item{console}{show output from nmfe in console? Default \code{FALSE}}

\item{verbose}{verbose output?}
}
\description{
Call nmfe
}
