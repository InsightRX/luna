# Syntax-check a model

For NONMEM models (`method = "pharmpy"`, `"psn"`, `"nmfe"`): uses
pharmpy to parse the model file. For ferx models (`method = "ferx"`):
verifies the `.ferx` file exists.

## Usage

``` r
luna_check(id, folder = NULL, verbose = FALSE, ...)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the model is
  run.

- folder:

  path to folder containing the model file. Default is current
  directory.
