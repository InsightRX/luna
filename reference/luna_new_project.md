# Create a new project folder with template YAML project file

Create a new project folder with template YAML project file

## Usage

``` r
luna_new_project(
  name,
  folder = getwd(),
  description,
  import_models = TRUE,
  force = FALSE,
  template = NULL,
  verbose = TRUE
)
```

## Arguments

- name:

  name of the project, will be used as as the filename for the YAML
  file. Cannot contain spaces.

- folder:

  folder to create the project in. If the folder doesn't exist, it will
  be created.

- description:

  project description

- import_models:

  when creating a new project file in a folder, should any existing
  models be added to the project?

- force:

  if a project file already exists, overwrite it. `FALSE` by default

- template:

  Create yaml file from a template. Use
  [luna_project_templates](https://insightrx.github.io/luna/reference/luna_project_templates.md)
  to show a list of available templates

- verbose:

  verbosity
