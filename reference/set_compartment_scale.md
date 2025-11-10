# Set scaling for certain compartments, e.g. dose and observation compartments.

Currently not available in Pharmpy, this is a workaround function.

## Usage

``` r
set_compartment_scale(
  model,
  compartment = 1,
  expression = list(variable = "V", scale = 1000),
  verbose = TRUE
)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- compartment:

  compartment number

- expression:

  specification of new scaling, should always contain variable and scale
  arguments. E.g. `list(variable = "V", "scale" = 1000)`.

- verbose:

  verbose output?

## Value

Pharmpy model
