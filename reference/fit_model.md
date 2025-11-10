# Fit model using NONMEM or nlmixr2

Takes a pharmpy-loaded NONMEM model as input, and returns a pharmpy
model results object. So essentially this function is a drop-in
replacement for the run_modelfit() function in pharmr/pharmpy.

## Usage

``` r
fit_model(model, data, tool = "nonmem", path, ...)
```

## Arguments

- model:

  pharmpy model object

- data:

  data.frame with data to fit

- tool:

  either `nonmem` or `nlmixr`

- path:

  path to .rds file to save fit results to

- ...:

  passed onto `run_nmfe()` function
