# For a given run, get info on what folders are available for selected Pharmpy or PsN tools

For a given run, get info on what folders are available for selected
Pharmpy or PsN tools

## Usage

``` r
get_tools_info(id, folder, tools = c())
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- folder:

  path to folder containing the model file. Default is current
  directory.

- tools:

  vector of tools for which to find info
