#' Ask AI agent to help
#'
#' By default it will pick the most recent error from NONMEM
#' as prompt, but can also ask questions.
#'
#' @param prompt question to ask LLM. By default NULL, which means it will
#' look at the log and extract the last event, e.g. an error from NONMEM
#' @param model LLM, e.g. "gpt-4.1-mini"
#' @param verbose verbose output?
#' @param ... optional parameters passed onto ellmer::chat()
#'
#' @examples
#' # example code
#' luna_help() # will get help from LLM on last NONMEM error
#' luna_help("How do I write a model with non-linear elimination?")
#'
#' @export
#'
luna_help <- function(
  prompt = NULL,
  model = "gpt-4.1-mini",
  verbose = TRUE,
  ...
) {
  if(is.null(prompt)) {
    if(verbose)
      cli::cli_alert_info("Looking up last event")
    event <- log_get_last_event()
    if(is.null(event)) {
      cli::cli_abort("No recent event found, please call `luna_help()` with a question.")
    }
  }
  cli::cli_alert_info("Asking for help using {model}")
  chat <- ellmer::chat_openai(
    system_prompt = paste0(
      "You are an expert user of NONMEM and nlmixr2.",
      "You are helpful but concise in answering questions. ",
      "Please answer only in text-form, not with markdown.",
      "Your answer should be no longer than 3 sentences.",
      "If possible, diagnose the problem and then provide a suggested solution."
    ),
    model = model
  )
  if(!is.null(prompt)) {
    user_prompt <- prompt
  } else {
    if(event$event == "error") {
      user_prompt <- paste0(
        "I'm getting this error from NONMEM when running a model: \n",
        "```\n",
        event$context$stdout, "\n",
        "```\n"
      )
    } else {
      cli::cli_abort("Sorry, I can currently only help with NONMEM errors.")
    }
  }
  response <- chat$chat(
    user_prompt,
    echo = FALSE,
    ...
  )
  cli::cli_text(response)
}
