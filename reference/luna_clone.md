# Clone model

Clone model

## Usage

``` r
luna_clone(
  id,
  new_id,
  folder = NULL,
  force = FALSE,
  update_inits = FALSE,
  verbose = TRUE
)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- new_id:

  new run id

- folder:

  path to folder containing the model file. Default is current
  directory.

- update_inits:

  use the final estimates from the source model as initial estimates for
  the cloned model. Default `FALSE`
