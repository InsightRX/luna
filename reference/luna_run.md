# Run a NONMEM model

Run a NONMEM model

## Usage

``` r
luna_run(id, folder = NULL, nmfe = NULL, as_job = NULL, ...)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- folder:

  path to folder containing the model file. Default is current
  directory.

- as_job:

  run as an RStudio job (async), or in the console. If left `NULL` will
  use setting in luna config.
