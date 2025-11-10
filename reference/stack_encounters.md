# Stack encounters when data from multiple encounters is available for the same ID, and TIME is starting at 0 for each encounter.

Stack encounters when data from multiple encounters is available for the
same ID, and TIME is starting at 0 for each encounter.

## Usage

``` r
stack_encounters(
  data,
  gap = 100,
  reset_encounters = TRUE,
  time = "TIME",
  verbose = FALSE
)
```

## Arguments

- data:

  NONMEM input dataset

- gap:

  rounding resolution for next . E.g. if set to `100` and if the maximum
  encounter length in the data is 168 hours, will start the encounters
  at t = 0, 200, 400 etc.

- reset_encounters:

  add an EVID=3 event to reset all compartments to 0 before starting the
  new encounter? Default is `TRUE`.

- time:

  time column, `"TIME"` by default

- verbose:

  verbose output
