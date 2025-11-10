# Updates PK parameter tables (patab)

E.g. useful to call after pharmr::add_peripheral_compartment() to update
the \$TABLE with parameter estimates

## Usage

``` r
update_pk_tables(model, ...)
```

## Arguments

- model:

  pharmpy model object or NONMEM model code (character) or path to
  NONMEM model file.

- ...:

  passed to add_default_output_tables()
