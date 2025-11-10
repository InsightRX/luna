# Get compartment scale definition

Assumes scale is always defined either as a single variable (e.g.
`S2 = V2`), or as a variable divided by a factor (e.g. `S2 = V2/1000`.
Other expressions will very likely not result in a correct extraction,
or errors.

## Usage

``` r
get_compartment_scale(model, compartment = 2)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- compartment:

  compartment number

## Value

a list with elements `variable` and `scale`, e.g.
