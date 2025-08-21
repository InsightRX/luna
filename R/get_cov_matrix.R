#' Create a covariance block matrix
#'
#' @param params parameters, a vector of standard deviations and correlations,
#' e.g. `list("CL" = 0.1, "V" = 0.2, "KA" = 0.3, "CL~V" = 0.3)`.
#' @param keep_all Should all parameters be kept in the covariance matrix,
#' even if they do not have a correlation with other parameters?
#' @param triangle return the lower triangle as a vector instead of a
#' matrix object?
#' @param limit lower limit, to avoid becoming zero, which is not allowed by
#' NONMEM (`A COVARIANCE IS ZERO, BUT THE BLOCK IS NOT A BAND MATRIX.`)
#'
#' @examples
#' \dontrun{
#' make_cov_matrix(list("CL" = 0.1, "V" = 0.2, "KA" = 0.3, "CL~V" = 0.3))
#' }
#'
get_cov_matrix <- function(
  params,
  keep_all = FALSE,
  triangle = FALSE,
  nonmem = TRUE,
  limit = 0.001
) {
  # Separate SDs and correlations
  sds <- params[!grepl("~", names(params))]
  cors <- params[grepl("~", names(params))]

  vars <- names(sds)
  n <- length(vars)
  cov_mat <- matrix(0, n, n, dimnames = list(vars, vars))

  # Fill diagonal with variances
  for (v in vars) {
    cov_mat[v, v] <- sds[[v]]^2
  }

  # Fill off-diagonal with covariances
  corr_params <- c()
  for (nm in names(cors)) {
    parts <- strsplit(nm, "~")[[1]]
    v1 <- parts[1]
    v2 <- parts[2]
    corr_params <- c(corr_params, v1, v2)
    rho <- cors[[nm]]
    cov_val <- rho * sds[[v1]] * sds[[v2]]
    cov_mat[v1, v2] <- cov_val
    cov_mat[v2, v1] <- cov_val
  }

  # Lower limit for covariance, also make sure there are no 0s in the block.
  if(!is.null(limit)) {
    for(i in 2:(nrow(cov_mat))) {
      for(j in 1:(i-1)) {
        cov_mat[i, j] <- max(c(limit, cov_mat[i, j]))
        cov_mat[j, i] <- max(c(limit, cov_mat[j, i]))
      }
    }
  }

  ## make sure the param names are unique, and in right order
  corr_params <- intersect(names(params), unique(corr_params))

  ## Keep only parameters that have corrs
  if(!keep_all) {
    idx <- match(corr_params, rownames(cov_mat))
    cov_mat <- cov_mat[idx, idx]
  }

  if(nonmem) {
    return(format_lower_triangle(cov_mat))
  }

  ## Lower triangle output?
  if(triangle) {
    cov_mat <- cov_mat[lower.tri(cov_mat, diag = TRUE)]
  }

  cov_mat
}

format_lower_triangle <- function(cov_mat, digits = 3, na_str = ".", ...) {
  vars <- colnames(cov_mat)
  n <- length(vars)

  # format numbers
  fmt <- function(x) ifelse(is.na(x), na_str, formatC(x, format = "f", digits = digits))

  lines <- character(n)
  for (i in seq_len(n)) {
    row_vals <- sapply(seq_len(i), function(j) fmt(cov_mat[i, j]))
    lines[i] <- paste(row_vals, collapse = "  ")
  }

  lines
}
