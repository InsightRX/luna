# Set inter-individual variability on parameters

Set inter-individual variability on parameters

## Usage

``` r
set_iiv(mod, iiv, iiv_type = "exp")
```

## Arguments

- mod:

  pharmpy model object

- iiv:

  what parameters to put IIV on. Can be one of three formats:

  - character: `all` or `basic`.

  - character: `c("CL", "V")`. Will assume SD of 0.5 for initial
    estimate.

  - list of numeric: e.g. `list(CL = 0.5, V = 0.5)` with SD for initial
    estimates.

- iiv_type:

  one of IIV types accepted by pharmr::add_iiv(), i.e. `add`, `prop`,
  `exp` (default), `log`, or `re_log`.
