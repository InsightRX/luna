# Create covariate search space definition for pharmpy `covsearch`

See Pharmpy MFL documentation for more info:
https://pharmpy.github.io/latest/covsearch.html

## Usage

``` r
create_covariate_search_space(
  parameters,
  covariates,
  operation = c("LIN", "POW"),
  explore = TRUE,
  struct_parameters = NULL,
  struct_covariates = NULL,
  struct_operation = "POW"
)
```

## Arguments

- parameters:

  vector of parameter names

- covariates:

  vector of covariate names

- operation:

  parameter-covariate model type (operation)

- explore:

  should the specified `parameters` and `covariates` be used as
  structural model elements, or as exploration space?

- struct_parameters:

  vector of parameter names for structural model

- struct_covariates:

  vector of covariate names for structural model

- struct_operation:

  parameter-covariate model type (operation) for structural model
