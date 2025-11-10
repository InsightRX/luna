# Combine several regimens into a single data.frame, which can be passed into `luna::run_sim()` as `regimen` argument.

Combine several regimens into a single data.frame, which can be passed
into
[`luna::run_sim()`](https://insightrx.github.io/luna/reference/run_sim.md)
as `regimen` argument.

## Usage

``` r
combine_regimens(...)
```

## Arguments

- ...:

  each argument is a named regimen, that in itself is specified as a
  list containing multiple regimens, each created using
  [`create_regimen()`](https://insightrx.github.io/luna/reference/create_regimen.md).
  See examples.

## Details

This allows both for combination of two or more phases, e.g. loading
doses and maintenance phase in a single regimen. It also allows for
specification of multiple separate regimens to simulate, e.g. a
high-dose regimen and a low-dose regimen.

## Examples

``` r
if (FALSE) { # \dontrun{
regimens <- combine_regimens(
  "without_load" = list(
    create_regimen(
      dose = 500,
      interval = 12,
      n = 10,
      route = "oral"
    )
  ),
  "with_load" = list(
    create_regimen(
      dose = 2000,
      n = 1,
      interval = 12,
      route = "iv",
      t_inf = 1
    ),
    create_regimen(
      dose = 500,
      n = 5,
      interval = 24,
      route = "oral"
    )
 )
)
} # }
```
