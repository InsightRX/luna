#' Get luna config, either from global config file,
#' or from project config
#'
#' @param verbose verbose output?
#'
get_luna_config <- function(
  verbose = TRUE
) {
  global_conf_file <- "~/.config/luna/config.yaml"
  if(!file.exists("~/.config/luna/config.yaml")) {
    cli::cli_abort("Luna config file not found. A config file should've been created upon first load of the `luna` package. Please check installation. Luna config file can also be created manually, at `~/.config/luna/config.yaml`.")
  }
  conf <- yaml::read_yaml(global_conf_file)
  if(is_luna_cache_available(abort = FALSE)) {
    proj_yaml <- .luna_cache$get("yaml")
    proj_conf <- proj_yaml$project$config
    ## use any specified project conf to overwrite env conf
    for(key in names(proj_conf)) {
      if(is.null(conf[[key]]) || proj_conf[[key]] != conf[[key]]) {
        if(verbose)
          cli::cli_alert_info("Overriding global luna setting for `{key}` with project-specific setting")
        conf[[key]] <- proj_conf[[key]]
      }
    }
  } else {
    if(verbose) cli::cli_alert_warning("Luna projecty cache not found, not using project-specific config")
  }
  conf
}
