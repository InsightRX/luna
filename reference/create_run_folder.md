# Create a folder for a run

Create a folder for a run

## Usage

``` r
create_run_folder(id, path, force = FALSE, verbose = TRUE)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run. If no folder is specified, it will create a folder `run1` in
  the current working directory, and will increment the run number for
  each subsequent run.

- path:

  path to nonmem model. If not specified, will assume current working
  directory.

- force:

  if run folder (`id`) exists, should existing results be removed before
  rerunning NONMEM? Default `FALSE`.

- verbose:

  verbose output?
