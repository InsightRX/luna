# Combine columns with run info into a data.frame and make sure that rows match (e.g. parameters)

data.frames in list should have the same column names but can have
different row names (e.g. parameter names).

## Usage

``` r
combine_info_columns(fit_info, table = "info_tab", label = "Detail")
```
