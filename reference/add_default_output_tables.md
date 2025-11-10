# Add one or more default output tables to a model, if they don't already exist in the model.

Add one or more default output tables to a model, if they don't already
exist in the model.

## Usage

``` r
add_default_output_tables(
  model,
  iiv = NULL,
  tables = c("fit", "parameters"),
  full_tables = FALSE,
  remove_existing = TRUE,
  verbose = TRUE
)
```

## Arguments

- model:

  Pharmpy model object

- iiv:

  vector of parameters with iiv. Optional, if not specified will use
  pharmpy function to retrieve it. Shortcut strings "basic" and "all"
  are also treated as NULL and will auto-detect parameters.

- tables:

  character vector of which default tables to add, options are `fit` and
  `parameters`.

- full_tables:

  For the default tables, should all input columns from be included in
  the output tables? Default `FALSE`.

- verbose:

  verbose output?
