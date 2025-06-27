#' Convert runs in a project object to a data.frame
#'
#' @param x data.frame with model info
#' @param raw if `TRUE`, will not parse data but just return all
#' raw data
#'
runs_as_table <- function(
  x,
  raw = FALSE
) {

  ## check if cache is present
  is_cache_available <- is_luna_cache_available(abort = TRUE)

  ## Print a table of models and their descriptions
  models <- x$yaml$runs
  model_table <- data.frame()
  if(length(models) > 0) {
    timestamps <- .luna_cache$get("timestamps")
    model_table <- data.frame(
      "id" = sapply(models, function(y) {
        y$id
      }),
      "reference" = sapply(models, function(y) {
        ifelse0(y$reference, "")
      }),
      "description" = sapply(models, function(y) {
        ifelse0(y$description, "")
      }),
      "tag" = sapply(models, function(y) {
        ifelse0(paste(y$tag, collapse = ", "), "")
      }),
      "notes" = sapply(models, function(y) {
        ifelse0(paste(y$notes, collapse = ", "), "")
      }),
      "model_file" = sapply(models, function(y) {
        ifelse0(find_file_with_fallback(
          folder = x$metadata$folder,
          filename = file.path(y$id, paste0("run", ".mod")),
          fallback = paste0(y$id, ".mod"),
          verbose = FALSE,
          abort = FALSE
        ), "")
      }),
      "output_file" = sapply(models, function(y) {
        ifelse0(find_file_with_fallback(
          folder = x$metadata$folder,
          filename = file.path(y$id, paste0("run", ".lst")),
          fallback = paste0(y$id, ".lst"),
          verbose = FALSE,
          abort = FALSE
        ), "")
      }),
      "tools" = sapply(models, function(y) {
        ifelse0(get_tools(y), "")
      })
    )
    model_table$status <- sapply(model_table$id, function(y) {
      ifelse0(get_status(y, x$metadata$folder), "")
    })
    model_table$finished <- sapply(models, function(y) {
      ifelse0(get_time_ago(timestamps$results[[y$id]]), "")
    })
    fit_results <- get_all_results(model_table$model_file)
    if(!raw) {
      fit_results <- fit_results |>
        dplyr::select(model_file, ofv)
    }
    full_table <- dplyr::left_join(
      model_table,
      fit_results,
      by = "model_file"
    ) |>
      dplyr::select( # remove temporary columns
        -model_file,
        -output_file
      )
    ## Add dOFV
    full_table <- full_table |>
      dplyr::left_join(
        full_table |>
          dplyr::filter(!is.na(ofv)) |>
          dplyr::select(id, ofv_ref = ofv)
        ,
        by = dplyr::join_by(reference == id)
      ) |>
      dplyr::mutate(dofv = ofv - ofv_ref) |>
      dplyr::select(-ofv_ref)
    if(!raw) {
      column_widths <- list(
        id = NA,
        reference = 1,
        description = 2,
        tag = 1,
        notes = 2,
        status = NA,
        finished = NA,
        tools = 1.5,
        ofv = NA,
        dofv = NA
      )
      full_table <- truncate_columns(
        df = full_table,
        width_specs = column_widths
      )
    }
  }

  ## Return as tibble with custom luna class
  full_table <- dplyr::as_tibble(full_table)
  if(!raw) {
    class(full_table) <- c("luna.run_table", class(full_table))
  }
  full_table
}

get_tools <- function(run) {
  paste0(
    unique(
      unlist(lapply(run$tools, function(x) x$tool))
    ),
    collapse = ","
  )
}

#' helper function to extract all fit results based on
#' vector of model files
#'
get_all_results <- function(model_file) {
  cli::cli_alert_info("Parsing run results")
  res <- lapply(model_file, function(x) {
    tryCatch({
      res <- pharmr::read_modelfit_results(
        esttool = "nonmem",
        path = x
      )
      list(
        model_file = x,
        ofv = round(unlist(res$ofv), 2),
        minimization_successful = res$minimization_successful,
        covstep_successful = res$covstep_successful,
        runtime_total = res$runtime_total,
        warnings = unlist(res$warnings),
        significant_digits = res$significant_digits,
        function_evaluations = res$function_evaluations
      )
    },
    error = function(e) {})
  })
  tidyr::as_tibble(
    do.call(dplyr::bind_rows, res)
  )
}
