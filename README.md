# Luna

Luna is a tool to organize your NONMEM and nlmixr2 modeling workflows. 
It is an acronym for Light-weight Universal NLME Admininstrator. 
The organization of projects is implemented through YAML files, which
makes it easy to work with from either RStudio or from the command line.

## Example usage

```
library(luna)

## Create a new project (only if starting from scratch, or if creating a new Luna project from existing model folder)
project <- luna_new_project(
  "uvm",
  description = "UVM vancomycin in obese patients",
  folder = proj_path,
  # force = TRUE,
  return_object = TRUE
)

## Load an existing project
project <- luna_load_project(
  folder = proj_path
)

## To list runs, just print the luna project object
project

## To run models:
luna_run("run4")

## If the model was already run, use `force` to rerun:
luna_run("run4", force=TRUE)

## Show model run info:
luna_info("run1")

## Show model run info side-by-side for multiple runs:
luna_compare("run1", "run2", "run3")

## Load the dataset from a model into R
dat <- luna_dataset("run2")
```

## Todo:
- add OFV and basic run results to project, and store in cache
- luna_vpc
- luna_simulation
- luna_bootstrap
- luna_psn(tool = 'sse', args = list())
- luna_plot("run1", "dv_vs_pred")
- luna_report()

