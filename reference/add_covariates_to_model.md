# Wrapper function to add covariates to a pharmpy model

Wrapper function to add covariates to a pharmpy model

## Usage

``` r
add_covariates_to_model(model, covariates, data = NULL)
```

## Arguments

- covariates:

  list of parameter-covariate effects, e.g.
  `list(CL = list(WT = "pow", CRCL = "lin"), V = list(WT = "pow")`
  Values in list need to match one of the effects allowed by pharmpy.

- data:

  data.frame as input to NONMEM / nlmixr.
