# Find a file(s) from a model run with a potential fallback

Find a file(s) from a model run with a potential fallback

## Usage

``` r
find_file_with_fallback(
  folder,
  filename,
  fallback,
  verbose = TRUE,
  abort = TRUE
)
```

## Arguments

- folder:

  main model folder

- filename:

  by default will look for NONMEM nmfe results file, but can also look
  e.g. for a table. In that case specify `filename`.

- fallback:

  fallback filename
