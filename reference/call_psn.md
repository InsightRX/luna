# Call PsN

Call PsN

## Usage

``` r
call_psn(
  model_file,
  output_file,
  path,
  options = c(),
  tool = c("execute", "vpc", "bootstrap", "sir", "proseval", "update_inits", "cdd"),
  console = TRUE,
  verbose = TRUE
)
```

## Arguments

- model_file:

  model file, e.g. "run.mod"

- output_file:

  output file, e.g. "run.lst"

- path:

  run folder path, e.g. "run1"

- options:

  a vector of arguments to pass to the PsN tool, e.g.
  `c("--samples=100", "--dir="test")`

- console:

  show output from nmfe in console? Default `FALSE`

- verbose:

  verbose output?
