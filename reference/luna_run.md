# Run a model

Dispatches to the configured execution backend. Supported methods:
`"pharmpy"` (default), `"psn"`, `"nmfe"` for NONMEM-based workflows, and
`"ferx"` for the ferx-nlme Rust engine.

## Usage

``` r
luna_run(id, folder = NULL, nmfe = NULL, as_job = NULL, ...)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the model is
  run.

- folder:

  path to folder containing the model file. Default is current
  directory.

- as_job:

  run as an RStudio job (async), or in the console. If left `NULL` will
  use setting in luna config. Not yet supported for `method = "ferx"`.
