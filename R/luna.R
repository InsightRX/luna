luna <- function() {
    cat("Available functions in luna:\n")
    knitr::kable(
      data.frame(
        Function = c(
          "luna_new_project",
          "luna_run",
          "luna_info"
        ),
        Description = c(
          "Create a new project",
          "List the models in the project",
          "Run a model",
          "Get information about a run (fit info, etc.)"
        )
      ),
      format = "pipe"
    )
}
