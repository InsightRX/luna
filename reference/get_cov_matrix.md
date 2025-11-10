# Create a covariance block matrix

Create a covariance block matrix

## Usage

``` r
get_cov_matrix(
  params,
  keep_all = FALSE,
  triangle = FALSE,
  nonmem = TRUE,
  limit = 0.001
)
```

## Arguments

- params:

  parameters, a vector of standard deviations and correlations, e.g.
  `list("CL" = 0.1, "V" = 0.2, "KA" = 0.3, "CL~V" = 0.3)`.

- keep_all:

  Should all parameters be kept in the covariance matrix, even if they
  do not have a correlation with other parameters?

- triangle:

  return the lower triangle as a vector instead of a matrix object?

- limit:

  lower limit, to avoid becoming zero, which is not allowed by NONMEM
  (`A COVARIANCE IS ZERO, BUT THE BLOCK IS NOT A BAND MATRIX.`)

## Examples

``` r
if (FALSE) { # \dontrun{
make_cov_matrix(list("CL" = 0.1, "V" = 0.2, "KA" = 0.3, "CL~V" = 0.3))
} # }
```
