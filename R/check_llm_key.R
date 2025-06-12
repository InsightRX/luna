#' Check presence of LLM API key
#'
check_llm_key <- function() {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if(api_key == "") {
    packageStartupMessage(
      paste(
        "The environment variable OPENAI_API_KEY is not set.",
        "Please set this to take full advantage of agentic support."
      )
    )
  }
}
