# Show fit results for a ferx model run

Reads the `{id}.fitrx` file saved by
[`luna_run()`](https://insightrx.github.io/luna/reference/luna_run.md)
(when `method = "ferx"`) and prints a formatted parameter summary.

## Usage

``` r
luna_ferx_info(id, folder = NULL)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the model is
  run.

- folder:

  path to folder containing the model file. Default is current
  directory.

## Value

The `ferx_fit` result object, invisibly.
