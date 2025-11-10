# Clean pharmpy run folders like modelfit1 etc

Clean pharmpy run folders like modelfit1 etc

## Usage

``` r
clean_pharmpy_runfolders(
  id = NULL,
  folder,
  tool,
  remove = TRUE,
  verbose = TRUE
)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- verbose:

  verbose output?

- clean:

  should folders really be removed (`TRUE`), or just show a warning
  (`FALSE`)
