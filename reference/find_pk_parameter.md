# Find / match PK parameter based on generic name.

E.g. a user may request `pharmr::remove_iiv("V")`, to remove IIV on the
central volume. But if in the model the central volume is actually
parametrized as `V1` or `V2`, then it will error. When wrapped in
`find_pk_parameter` this adds more safety. It will first look if the
parameter is used in the model as such. If not found directly, it will
attempt other common names for the parameter, depending on the ADVAN
number of the model.

## Usage

``` r
find_pk_parameter(parameter, model)
```

## Arguments

- parameter:

  name of the parameter to find

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.
