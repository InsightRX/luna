% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna_new_project.R
\name{luna_new_project}
\alias{luna_new_project}
\title{Create a new project folder with template YAML project file}
\usage{
luna_new_project(
  name,
  folder = getwd(),
  description,
  import_models = TRUE,
  force = FALSE,
  template = NULL,
  verbose = TRUE
)
}
\arguments{
\item{name}{name of the project, will be used as as the filename for the
YAML file. Cannot contain spaces.}

\item{folder}{folder to create the project in. If the folder doesn't exist,
it will be created.}

\item{description}{project description}

\item{import_models}{when creating a new project file in a folder, should
any existing models be added to the project?}

\item{force}{if a project file already exists, overwrite it. \code{FALSE} by
default}

\item{template}{Create yaml file from a template. Use
\link{luna_project_templates} to show a list of available templates}

\item{verbose}{verbosity}
}
\description{
Create a new project folder with template YAML project file
}
