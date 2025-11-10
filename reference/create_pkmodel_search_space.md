# Create PK mmodel search space definition for pharmpy `modelsearch`

See Pharmpy MFL documentation for more info:
https://pharmpy.github.io/latest/modelsearch.html

## Usage

``` r
create_pkmodel_search_space(
  absorption = c("FO", "ZO"),
  elimination = c("FO", "MM"),
  peripherals = c(0, 1),
  transits = c(0, 1, 3),
  lagtime = c("OFF", "ON")
)
```

## Arguments

- absorption:

  absorption model options

- elimination:

  elimination model options

- peripherals:

  peripheral compartment options

- transits:

  transit model options

- lagtime:

  lagtime options
