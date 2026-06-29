# Syntax-check a NONMEM model

Uses pharmpy to parse the model. A successful parse indicates the model
is valid. Any pharmpy parse error is reported as a syntax issue.

## Usage

``` r
luna_check(id, folder = NULL, verbose = FALSE, ...)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- folder:

  path to folder containing the model file. Default is current
  directory.
