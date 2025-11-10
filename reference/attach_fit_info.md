# Attach fit info and tables to a fit object, e.g. from model fit or Pharmpy grid search final results

Attach fit info and tables to a fit object, e.g. from model fit or
Pharmpy grid search final results

## Usage

``` r
attach_fit_info(
  fit,
  model,
  fit_folder,
  output_file = file.path(fit_folder, "model.lst"),
  is_sim_model = FALSE,
  verbose = TRUE
)
```

## Arguments

- fit:

  pharmpy fit object

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- output_file:

  NONMEM output file, default is `run.lst`

- verbose:

  verbose output?
