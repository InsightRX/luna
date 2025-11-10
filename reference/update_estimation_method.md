# Wrapper around pharmr's functions to set/add estimation methods

The current pharmpy functionality is not stable, hence the need for this
wrapper.

## Usage

``` r
update_estimation_method(model, estimation_method, verbose = TRUE)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- estimation_method:

  Optional. Character vector of estimation method(s) to apply to model.
  Will remove all existing estimation steps in the model and update with
  methods specified in argument.

- verbose:

  verbose output?
