# Get table for basic GOF plots based on run id

Get table for basic GOF plots based on run id

## Usage

``` r
get_table_for_plots(
  id,
  folder = NULL,
  residual = c("CWRES", "NPDE"),
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

- ltbs:

  log-transform-both-sides error model? If `TRUE`, will exponentiate DV,
  PRED, and IPRED.
