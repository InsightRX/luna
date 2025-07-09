#' Create a new project folder with template YAML project file
#'
#' @param name name of the project, will be used as as the filename for the
#' YAML file. Cannot contain spaces.
#' @param description project description
#' @param folder folder to create the project in. If the folder doesn't exist,
#'   it will be created.
#' @param import_models when creating a new project file in a folder, should
#' any existing models be added to the project?
#' @param template Create yaml file from a template. Use
#' \link{luna_project_templates} to show a list of available templates
#' @param force if a project file already exists, overwrite it. `FALSE` by
#' default
#' @param verbose verbosity
#'
#' @export
#'
luna_new_project <- function(
  name,
  folder = getwd(),
  description,
  import_models = TRUE,
  force = FALSE,
  template = NULL,
  verbose = TRUE
) {

  ## make sure we have a full folder name:
  folder <- normalizePath(folder, winslash = "/", mustWork = FALSE)

  ## If parent folder doesn't exist, throw an error
  parent_folder <- dirname(folder)
  if(parent_folder != "." && !dir.exists(paths = parent_folder)) {
    cli::cli_abort(
      glue::glue(
        "Parent folder {parent_folder} does not exist. Please create the folder first."
      )
    )
  }

  ## create folder if it doesn't exist
  if(!dir.exists(paths = folder)) {
    if(verbose)
      cli::cli_alert_info(paste0("Creating folder: ", folder))
    dir.create(folder)
  }
  project_yaml_file <- file.path(folder, paste0(name, ".yaml"))
  if(file.exists(project_yaml_file) && !force) {
    cli::cli_abort(
      paste0(
        "Project file exists in this folder. Please specify different project name ",
        "or folder, or use `force=TRUE` argument. If you just want to reload an ",
        "existing luna project, use `luna_load_project()`"
      )
    )
  }

  # create a project object from template
  if(!is.null(template)) {
    template <- stringr::str_replace(template, "\\.yaml$", "")
    project <- read_yaml_safe(
      file.path(system.file("templates", package = "luna"),
      paste0(template, ".yaml"))
    )
  } else {
    project <- list(
      yaml = list(
        project = list(
        )
      ),
      metadata = list(
        name = name,
        folder = folder
      )
    )
  }
  project$yaml$project$description <- description

  ## Import models, if present in folder and requested
  if(import_models) {
    models <- stringr::str_replace_all(
      dir(folder, pattern = "\\.mod$"),
      "\\.mod$",
      ""
    ) |>
      stringr::str_sort(numeric = TRUE)
    project$yaml$runs <- list()
  }
  for(i in 1:length(models)) {
    model_file <- file.path(folder, paste0(models[i], ".mod"))
    if(file.exists(model_file)) {
      project$yaml$runs[[i]] <- list(
        id = models[i],
        description = get_description_from_model(model_file)
      )
    }
  }
  class(project) <- c("luna.project", class(project))

  ## Save to file
  save_project(
    project = project,
    path = project_yaml_file,
    verbose = verbose,
    force = force
  )

  ## Create cache
  if(verbose) cli::cli_alert_info("Creating / updating luna cache")
  create_cache(
    project = project,
    verbose = verbose
  )

  cli::cli_alert_success("Done")

}

get_description_from_model <- function(model_file) {
  # Read the model file
  model_content <- readLines(model_file)

  # Find the $PROBLEM record
  problem_line_index <- which(grepl("^\\$PROB", model_content))

  if (length(problem_line_index) == 0) {
    return("No description available")
  }

  # Extract the description from the $PROBLEM record
  # The description is typically the text after $PROBLEM
  problem_line <- model_content[problem_line_index]
  description <- sub("^\\$PROB(LEM)?\\s*", "", problem_line)

  # If the description is empty, try to get the next line
  if (nchar(trimws(description)) == 0 && problem_line_index < length(model_content)) {
    description <- model_content[problem_line_index + 1]
  }

  # Clean up the description
  description <- trimws(description)

  # If still empty, return a default message
  if (nchar(description) == 0) {
    return("N/A")
  }

  return(description)
}

#' Save a project to a YAML file
#'
#' @param project uno project object
#' @param path path to file to save project
#' @param force force saving of the file, even if it already exists
#' @param verbose verbosity
#'
save_project <- function(
  project,
  path,
  verbose = FALSE,
  force = FALSE
) {
  if(file.exists(path) && !force) {
    cli::cli_abort(
      "Project file exists in this folder. Please specify different",
      "project name or folder, or use `force=TRUE` argument."
    )
  }
  if(verbose)
    cli::cli_alert_info("Saving project file")
  yaml::write_yaml(project$yaml, path)
}



#' Create a new project object based on a predefined template
#'
#' @param template template for new project. Either a reference to one of the
#'   built-in templates (e.g. `default`), or path to a file.
#'
new_project_from_template <- function(
  template = "default"
) {

}
