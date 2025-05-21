.onLoad <- function(libname, pkgname) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if(api_key == "") {
    cli::cli_alert_info(
      paste0(
        "The environment variable OPENAI_API_KEY is not set.",
        "Please set this to take full advantage of agentic support."
      )
    )
  }
  invisible()
}
