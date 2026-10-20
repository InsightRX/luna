# Create a new project object based on a predefined template

Create a new project object based on a predefined template

## Usage

``` r
new_project_from_template(template = "clean")
```

## Arguments

- template:

  template for new project. Either a reference to one of the built-in
  templates (e.g. `clean`), or path to a file.

## Value

a list with elements `yaml` (the project YAML contents) and `metadata`
(empty, to be populated by the caller)
