# For a Pharmpy grid search, fetch the fit info and attach to object

For a Pharmpy grid search, fetch the fit info and attach to object

## Usage

``` r
get_final_results_from_search(id, results, tool = NULL, verbose = TRUE)
```

## Arguments

- id:

  run id

- results:

  Pharmpy results object from grid search

- tool:

  Pharmpy search tool. If `NULL`, will try to infer from class of
  results object
