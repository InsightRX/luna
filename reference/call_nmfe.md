# Call nmfe

Call nmfe

## Usage

``` r
call_nmfe(
  model_file,
  output_file,
  path,
  nmfe = "/opt/NONMEM/nm_cxurrent/run/nmfe75",
  console = FALSE,
  check_only = FALSE,
  verbose = FALSE
)
```

## Arguments

- model_file:

  model file, e.g. "run.mod"

- output_file:

  output file, e.g. "run.lst"

- path:

  run folder path, e.g. "run1"

- nmfe:

  path to nmfe batch file to run NONMEM

- console:

  show output from nmfe in console? Default `FALSE`

- check_only:

  only run NM-TRAN, to check the model syntax

- verbose:

  verbose output?
