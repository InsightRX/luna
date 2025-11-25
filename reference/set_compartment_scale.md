# Set scaling for certain compartments, e.g. dose and observation compartments.

Currently not available in Pharmpy, this is a workaround function.

## Usage

``` r
set_compartment_scale(
  model,
  compartment = NULL,
  expression = list(variable = "V", scale = 1000),
  update_inits = TRUE,
  verbose = TRUE
)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- compartment:

  compartment number. If `NULL` will be attempted to infer from ADVAN.
  If not a default ADVAN is used, will use 1 as default. So for safe
  use, please always specify the observation compartment to be scaled.

- expression:

  specification of new scaling, should always contain variable and scale
  arguments. E.g. `list(variable = "V", "scale" = 1000)`.

- update_inits:

  update initial estimates for basic PK parameters? This is likely
  needed when applying scale, or else it is very likely that the model
  starts too far off from the maximum likelihood and the fit will not
  converge properly. `TRUE` by default.

- verbose:

  verbose output?

## Value

Pharmpy model
