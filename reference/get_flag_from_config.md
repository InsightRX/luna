# Get a binary value from config, based on path

Get a binary value from config, based on path

## Usage

``` r
get_flag_from_config(
  flag = c("tools", "modelfit", "force"),
  default = FALSE,
  config = NULL
)
```

## Arguments

- flag:

  character vector indicating path to flag

- default:

  default value for flag

- config:

  list of config settings. If `NULL` will get it using
  [`get_luna_config()`](https://insightrx.github.io/luna/reference/get_luna_config.md)
