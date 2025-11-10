# Show a diff between two models

Show a diff between two models

## Usage

``` r
luna_diff(id, reference = NULL, folder = NULL, verbose = TRUE)
```

## Arguments

- id:

  either a single `id` or a vector of exactly two model `id`s. If
  specified as a single value, the `id` needs to have a `reference`
  model specified to which it can be compared. If `id` is of length 2,
  then it will just diff the models for the two `id`s.

- reference:

  id for reference model to compare to (optional)

- folder:

  path to folder containing the model file. Default is current
  directory.
