# Luna

**THIS R PACKAGE IS UNDER DEVELOPMENT, NOT READY FOR PRODUCTION USE!!**

Luna is a tool to organize your NONMEM and nlmixr2 modeling workflows in
a structured and traceable manner. Luna is an acronym for **Light-weight
Universal NLME Admininstrator**

The organization of projects is implemented through YAML files, which
forces the modeler to define each step during the modeling process prior
to execution of the step. This ensures that all steps are properly
logged and at any time during the model development a clear log of
modeling steps is available.

## Goals and rationale

The goal of Luna is to implement a structured model development: if
model development and results are structured, it’s easier for anyone to
interpret (human and AI agents).

## Justification

**But don’t we already have workflow managers, like Pirana, Finch
Studio, ShinyMixR, etc?**

Yes, and those tools are great and work for a lot of folks. However, we
believe that, especially for advanced modelers or modelers who come in
from a (software-) engineering background, often prefer an interface
that is accessible either programmatically (from R) or from the command
line (CLI). This is the primary target audience of Luna. However, we do
think that many novice modelers will also like the flexibility of Luna.

**But why not just keep a modeling workflow in RMarkdown or Quarto
notebooks**

That is certainly an option, and may work for some projects. However, we
feel that for most model development projects, having an R notebook as
the primary source of your model workflow quickly becomes unwieldy:
after a few model runs the notebook file quickly becomes too long to
navigate easily. Luna will have features to export to R notebooks in the
future, so the ability to use R notebooks for creating automated reports
and slides will still be available.

# Implementation

## Structure

All high-level luna functions that the user needs start with `luna_`.
For example, to run a model, use `luna_run(...)`, and to get a list of
models in the current project, use
[`luna_list()`](https://insightrx.github.io/luna/reference/luna_list.md).

The luna package also exposes various other functions to perform certain
tasks, but those do not make use of the `luna` workflow and in principle
should not be used within a luna workflow.

## Dependencies and extensions

Luna depends on Pharmpy and pharmr (the R wrapper around Pharmpy). The
dependency is primarily to use parsing of models and result files. It
can of course also leverage Pharmpy’s model execution tools, bootstrap
tool, and the automated model development tools. Luna is, however,
flexible in what tools are used to perform modeling analyses. It
provides an interface to PsN for running models, bootstraps, and any
other PsN tools.

# Example usage

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

## Available functions

    ## Projects
    luna_new_project()
    luna_load_project()
    luna_list()

    ## Model run workflow
    luna_run()
    luna_check()
    luna_info()
    luna_compare()
    luna_help()
    luna_note()
    luna_tag()
    luna_edit()

    ## External tools
    luna_tool()

    ## Diagnostic plots
    luna_gof()
    luna_ind()
    luna_xpose()

    ## Misc
    luna_dataset()
    luna_tables()
