# Get observation compartment number from model

Get observation compartment number from model

## Usage

``` r
get_obs_compartment(model)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

## Value

single integer value

## Details

For ADVAN1-4/11-12 this is easy, for other ADVANs we have to make some
assumptions based on whether scaling parameters have already been
defined for the model. Logic is as follows:

- if S1 is defined and not S2, assume it's 1.

- if S2 is defined and not S1, assume it's 2

- if both are defined, assume it's 2 but show a warning

- if none are defined, assume it's 2 but show a warning
