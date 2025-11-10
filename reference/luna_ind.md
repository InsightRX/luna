# Creates a ggplot2 plot with predictions and observations, split by subject

Creates a ggplot2 plot with predictions and observations, split by
subject

## Usage

``` r
luna_ind(
  id,
  folder = NULL,
  data = NULL,
  theme = ggplot2::theme_classic,
  smooth_method = "loess",
  exponentiate = FALSE,
  show = list(dv = TRUE, ipred = TRUE, pred = TRUE, doses = TRUE, se = TRUE),
  se_factor = 1,
  ltbs = FALSE,
  ncol = 3,
  nrow = 3,
  page = 1,
  scales = NULL,
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

- data:

  instead of getting an output table from a run, supply a data.frame
  with the relevant data columns.

- smooth_method:

  ggplot2-supported smooth method, e.g. "loess"

- exponentiate:

  exponentiate DV, PRED, and IPRED columns?

- se_factor:

  when plotting uncertainty for IPRED, factor to multiply the SE with.
  Default is 1, i.e. show the standard error. If set to 1.96, it will
  show the 95% CI. (Use [`qnorm()`](https://rdrr.io/r/stats/Normal.html)
  to find relevant factor for CI)

- ltbs:

  log-transform-both-sides error model? If `TRUE`, will exponentiate DV,
  PRED, and IPRED.

- ncol:

  number of faceting columns to show

- nrow:

  number of faceting rows to show

- page:

  page number of plots to show

- scales:

  passed to `facet_wrap()` function as the `scales` argument, can be
  "free", "free_x", or "free_y".

- subject_ids:

  list of subject IDs to be plotted.
