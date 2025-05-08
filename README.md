# Luna

Luna is a tool to organize your NONMEM and nlmixr2 modeling workflows. 
It is an acronym for Light-weight Universal NLME Admininstrator. 
The organization of projects is implemented through YAML files, which
makes it easy to work with from either RStudio or from the command line.

## Goals and rationale

The goal of Luna is to implement a structured model development:

- if model development and results are structured, it’s easier for anyone to 
interpret. (human and AI)
- future: AI agent could use (or write) specs like this, but it’s not an immediate goal.

## Justification

__But don't we already have workflow managers, like Pirana, Finch Studio, ShinyMixR, etc?__

Yes, and those tools are great and work for a lot of folks. However, we believe
that, especially for advanced modelers or modelers who come in from a 
(software-) engineering background, often prefer an interface that is accessible
either programmatically (from R) or from the command line (CLI). This is the
primary target audience of Luna. However, we do think that many novice modelers will
also like the flexibility of Luna, especially modelers with experience in R.

__But why not just keep a modeling workflow in RMarkdown or Quarto notebooks__

That is certainly an option, and may work for some projects. However,
we feel that for most model development projects, having an R notebook as the
primary source of your model workflow quickly becomes unwieldy: after a few 
model runs the notebook file quickly becomes too long to navigate easily. Luna
will have features to export to R notebooks in the future, so the ability to
use R notebooks for creating automated reports and slides will still be
available.

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
- create cache system
- add OFV and basic run results to project, and store in cache
- arrange models by run number, not alphabeticallyh
- functionality:
  - luna_vpc()
  - luna_simulation()
  - luna_bootstrap()
  - luna_psn(tool = 'sse', args = list())
  - luna_plot("run1", "dv_vs_pred")
  - luna_report()

