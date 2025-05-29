.onLoad <- function(libname, pkgname) {

  ## Make sure we have a setting file at ~/.config/luna/config.yaml
  ## Create if not
  check_default_config_file()

  ## Throw info warning if no OpenAI key present
  check_llm_key()

  invisible()
}

check_default_config_file <- function() {
  config_folder <- "~/.config/luna"
  if (!dir.exists(config_folder)) {
    dir.create(config_folder, recursive = TRUE)
  }

  config_file <- file.path(config_folder, "config.yaml")
  if(!file.exists(config_file)) {
    file.copy(
      system.file(package = "luna", "config/default.yaml"),
      config_file
    )
  }
}
