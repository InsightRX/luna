# Clean / check the dataset before passing to model fitting tool

Clean / check the dataset before passing to model fitting tool

## Usage

``` r
clean_modelfit_data(
  model,
  try_make_numeric = TRUE,
  data = NULL,
  verbose = TRUE
)
```

## Arguments

- try_make_numeric:

  should function try to turn character columns into numeric columns? If
  `FALSE` will just set all values to 0 (but retain column to avoid
  issues).
