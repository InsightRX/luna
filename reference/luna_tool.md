# Run external tools (e.g. diagnostics) on models or run outputs This can be used e.g. for bootstraps and VPCs. The function is implemented in a modular way so that it can be easily extended.

Run external tools (e.g. diagnostics) on models or run outputs This can
be used e.g. for bootstraps and VPCs. The function is implemented in a
modular way so that it can be easily extended.

## Usage

``` r
luna_tool(id, tool = NULL, force = FALSE, as_job = NULL, verbose = TRUE)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- tool:

  id for the tool, needs to be referenced in project YAML. See examples
  for further details.

- as_job:

  run as an RStudio job (async), or in the console. If left `NULL` will
  use setting in luna config.

- verbose:

  verbose output
