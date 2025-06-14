% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna_tag.R
\name{luna_tag}
\alias{luna_tag}
\title{Add a tag to a run
Either add a tag or clear tags from a run}
\usage{
luna_tag(id, tag = NULL, clear = FALSE, force = TRUE, verbose = FALSE)
}
\arguments{
\item{id}{run id, e.g. \code{run1}. This will be the folder in which the NONMEM
model is run.}

\item{tag}{tag to add (character vector)}

\item{clear}{should existing notes be cleared? Default is \code{FALSE}}
}
\description{
Add a tag to a run
Either add a tag or clear tags from a run
}
\examples{
\dontrun{
luna_tag("run1", "final") # adds a tag
luna_tag("run1", "intermediate", clear=TRUE) # first clears existing tags, then adds a tag
luna_tag("run1", clear=TRUE) # only clears existing tags
}

}
