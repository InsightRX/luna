# Helper function to determine nmfe location from various sources The order is as follows:

1.  argument specified by user

2.  check project settings (not implemented for now, will add later)

3.  check pharmpy config

4.  throw error, force user to specify

## Usage

``` r
get_nmfe_location_for_run(nmfe = NULL, verbose = FALSE)
```
