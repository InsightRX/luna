# Creates a ggplot2 panel object with basic goodness of fit plots

Creates a ggplot2 panel object with basic goodness of fit plots

## Usage

``` r
luna_gof(
  id,
  folder = NULL,
  residual = c("CWRES", "NPDE"),
  theme = ggplot2::theme_classic,
  smooth_method = "loess",
  ltbs = FALSE,
  verbose = TRUE
)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- folder:

  path to folder containing the model file. Default is current
  directory.

- residual:

  either "CWRES" or "NPDE"

- smooth_method:

  ggplot2-supported smooth method, e.g. "loess"

- ltbs:

  log-transform-both-sides error model? If `TRUE`, will exponentiate DV,
  PRED, and IPRED.
