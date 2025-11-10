# Create a single regimen

The resulting data.frame can be passed to
[`luna::run_sim()`](https://insightrx.github.io/luna/reference/run_sim.md)
as the `regimen` argument.

## Usage

``` r
create_regimen(
  dose,
  interval = 24,
  n,
  t_inf = NULL,
  route = c("oral", "iv", "sc", "im")
)
```

## Examples

``` r
if (FALSE) { # \dontrun{
reg1 <- create_regimen(
  dose = 500,
  interval = 12,
  n = 10,
  route = "oral"
)
luna::run_sim(..., regimen = reg1)
} # }
```
