# Load a project from yaml and gather results. Project is then stored/updated in .luna_cache

Load a project from yaml and gather results. Project is then
stored/updated in .luna_cache

## Usage

``` r
luna_load_project(name = NULL, folder = ".", verbose = TRUE)
```

## Arguments

- name:

  name of the project, will be used as as the filename for the YAML
  file. Cannot contain spaces.

- folder:

  folder to create the project in. If the folder doesn't exist, it will
  be created.

- verbose:

  verbosity
