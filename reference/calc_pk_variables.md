# Calculate some basic PK variables from simulated or observed data

Calculate some basic PK variables from simulated or observed data

## Usage

``` r
calc_pk_variables(data, regimen = NULL, dictionary = NULL)
```

## Arguments

- data:

  data.frame in NONMEM format

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

## Value

data.frame
