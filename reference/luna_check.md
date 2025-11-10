# Syntax-check a NONMEM model

Syntax-check a NONMEM model

## Usage

``` r
luna_check(id, folder = NULL, verbose = FALSE, ...)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- verbose:

  verbose output?
