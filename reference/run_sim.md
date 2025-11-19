# Run simulations

Run simulations

## Usage

``` r
run_sim(
  fit = NULL,
  data = NULL,
  model = NULL,
  id = get_random_id("sim_"),
  force = FALSE,
  t_obs = NULL,
  dictionary = list(ID = "ID", DV = "DV", EVID = "EVID", AMT = "AMT", CMT = "CMT", MDV =
    "MDV"),
  regimen = NULL,
  covariates = NULL,
  tool = c("auto", "nonmem", "nlmixr2"),
  n_subjects = NULL,
  n_iterations = 1,
  variables = c("ID", "TIME", "DV", "EVID", "IPRED", "PRED"),
  add_pk_variables = TRUE,
  output_file = "simtab",
  update_table = TRUE,
  seed = 12345,
  verbose = TRUE
)
```

## Arguments

- data:

  dataset (data.frame). Optional, can also be included in `model` object
  (if specified as pharmpy model object).

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- force:

  if run folder (`id`) exists, should existing results be removed before
  rerunning NONMEM? Default `FALSE`.

- t_obs:

  a vector of observations times. If specified, will override the
  observations in each subject in the input dataset.

- regimen:

  if specified, will replace the regimens for each subject with a custom
  regimen. Can be specified in two ways. The simplest way is to just
  specify a list with elements `dose`, `interval`, `n`, and `route` (and
  `t_inf` / `rate` for infusions). E.g.
  `regimen = list(dose = 500, interval = 12, n = 5, route = "oral")`.
  Alternatively, regimens can be specified as a data.frame. The
  data.frame specified all dosing times (`dose`, `time` columns) and
  `route` and `t_inf` / `rate`. The data.frame may also optionally
  contain a `regimen` column that specifies a name for the regimen. This
  can be used to simulate multiple regimens.

- covariates:

  if specified, will replace subjects with subjects specified in a
  data.frame. In the data.frame, the column names should correspond
  exactly to any covariates included in the model. An `ID` column is
  required, and for time-varying covariates, a `TIME` column is also
  required (otherwise it will be assumed covariates are not changing
  over time).

- n_subjects:

  number of subjects to simulate, when using sampled data (i.e. requires
  `covariates` argument)

- n_iterations:

  number of iterations of the entire simulation to perform. The dataset
  for the simulation will stay the same between each iterations.

- add_pk_variables:

  calculate basic PK variables that can be extracted in post-processing,
  such as CMAX_OBS, TMAX_OBS, AUC_SS.

- update_table:

  should any existing \$TABLE records be removed, and a new `simtab` be
  created? This is default. If `FALSE`, it will leave \$TABLEs as
  specifed in the model. However, in the return object, only the first
  table is returned back. If `FALSE`, the `add_pk_variables` argument
  will be ignored.

- verbose:

  verbose output?

## Value

data.frame with simulation results
