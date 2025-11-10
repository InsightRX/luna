# Generic function for running a pharmpy tool, like bootstrap, or modelsearch. A separate function is available for `fit()`

Generic function for running a pharmpy tool, like bootstrap, or
modelsearch. A separate function is available for `fit()`

## Usage

``` r
call_pharmpy_tool(
  id,
  model = NULL,
  results = NULL,
  tool = NULL,
  folder = NULL,
  clean = TRUE,
  verbose = TRUE,
  force = FALSE,
  options = list()
)
```

## Arguments

- id:

  model id. Optional. If not specified, will generate random modelfit
  id. The `id` will be used to create the run folder.

- model:

  Pharmpy model object, preferably created using
  [`luna::create_model()`](https://insightrx.github.io/luna/reference/create_model.md).

- verbose:

  verbose output?

- options:

  list of arguments pass on to `tool` as argument. Documentation for
  available arguments for each Pharmpy tool can be found here:
  https://pharmpy.github.io/latest/mfl.html.

- clear:

  if one or more run folders exists for the tool, do we want to remove
  them first?

## Value

fit object
