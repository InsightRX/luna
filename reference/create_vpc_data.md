# Run a simulation based on supplied parameters estimates, and combine into proper format for VPC

Run a simulation based on supplied parameters estimates, and combine
into proper format for VPC

## Usage

``` r
create_vpc_data(
  fit = NULL,
  model = NULL,
  parameters = NULL,
  keep_columns = c(),
  n = 100,
  verbose = FALSE,
  id = NULL,
  use_pharmpy = TRUE
)
```

## Arguments

- fit:

  fit object from
  [`pharmr::run_modelfit()`](https://rdrr.io/pkg/pharmr/man/run_modelfit.html).
  Optional, can supply a `model` and `parameters` argument

- model:

  pharmpy model object. Optional, can also only supply just a `fit`
  object

- parameters:

  list of parameter estimates, e.g. `list(CL = 5, V = 50)`. Optional,
  can also supply a `fit` object.

- keep_columns:

  character vector of column names in original dataset to keep in the
  output dataset

- n:

  number of simulation iterations to generate

- verbose:

  verbose output?
