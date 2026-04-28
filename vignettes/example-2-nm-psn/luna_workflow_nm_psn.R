# =============================================================================
# luna_workflow.R  (PSN backend)
# Example luna workflow: busulfan population PK model building
#
# This example is structurally identical to example-1-nm-pharmpy but uses
# PsN (Perl-speaks-NONMEM) as the execution engine instead of Pharmpy.
#
# Key differences vs pharmpy:
#   - Execution dispatched via PsN `execute` command
#   - Tools (bootstrap, vpc) called through PsN command-line interface
#   - Run folders follow PsN naming: run1/NM_run1/, run1/NM_run2/, ...
#   - SCM (stepwise covariate modelling) available as a PSN-specific tool
#   - Config must set  tools.modelfit.method: psn
#
# Prerequisites:
#   - NONMEM installed and nmfe path known to PsN
#   - PsN installed and on system PATH (`execute --version` works in terminal)
#   - luna config updated: run luna_config() and set method: psn
#
# Dataset : ../data/busulfan_adults.csv  (shared; 250 adults, 4 mg/kg Q24h x4 doses)
# Models  : project/run1–run5.mod     (standard NONMEM control files)
# =============================================================================

library(luna)

PROJECT_DIR  <- "vignettes/example-2-nm-psn/project"
PROJECT_NAME <- "busulfan"

# =============================================================================
# 1. SETUP & PROJECT INITIALISATION
# =============================================================================

# --- 1a. Check PsN is available ----------------------------------------------
#
# PsN must be installed and accessible from R's system PATH.
# Verify before attempting any runs.

psn_ok <- system("execute --version", intern = FALSE, ignore.stdout = TRUE,
                  ignore.stderr = TRUE) == 0
if (!psn_ok) stop("PsN not found on PATH. Install PsN and ensure `execute` is accessible.")

# --- 1b. Update luna config to use PSN ---------------------------------------
#
# Open ~/.config/luna/config.yaml and set:
#
#   tools:
#     modelfit:
#       method: psn
#     bootstrap:
#       method: psn
#       samples: 500
#     vpc:
#       method: psn
#       samples: 1000
#
# luna_config() opens the file directly:

# luna_config()   # uncomment to open config in editor

# --- 1c. Create or load project ----------------------------------------------

luna_new_project(
  name        = PROJECT_NAME,
  folder      = PROJECT_DIR,
  description = "Busulfan popPK model building via PsN",
  force       = TRUE
)

proj <- luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)

# =============================================================================
# 2. PRE-RUN INSPECTION & SYNTAX CHECKS
# =============================================================================

luna_list()

# Syntax-check all models before submitting (uses pharmpy parser)
for (id in paste0("run", 1:5)) luna_check(id)

# Review diffs along the model building chain
luna_diff("run2")           # 1-cmt → 2-cmt
luna_diff("run3")           # add WT allometry
luna_diff("run4", "run3")   # SEX on CL (not adopted branch)
luna_diff("run5", "run3")   # IOV on CL (adopted)

# Inspect the dataset
dat <- luna_dataset("run1")
dplyr::glimpse(dat)

# =============================================================================
# 3. RUNNING MODELS VIA PSN
# =============================================================================
#
# luna_run() passes method = "psn" to pharmr.extra::run_nlme(), which wraps
# PsN's `execute` command.
#
# PsN execute behaviour:
#   - Creates run1/ folder
#   - Copies run1.mod and dataset into run1/
#   - Runs NONMEM inside run1/NM_run1/
#   - Output .lst lands at run1/run1.lst (symlinked as run.lst)
#
# Pass extra PsN execute flags through `...` (forwarded to run_nlme):
#   threads  : parallel NONMEM instances for retries
#   retries  : number of re-estimation attempts with perturbed inits
#   picky    : only accept runs with successful covariance step

luna_run("run1")
luna_run("run2")

luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_compare("run1", "run2")   # confirm 2-cmt improvement before proceeding

luna_run("run3")

# run4 and run5 branch from run3; submit in parallel if resources allow
luna_run("run4")
luna_run("run5")

luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_list()

# =============================================================================
# 4. POST-RUN MODEL COMPARISON & SELECTION
# =============================================================================

luna_info("run2")
luna_info("run3")
luna_info("run5")

# Step-by-step OFV comparison
luna_compare("run1", "run2")          # structural: 1-cmt vs 2-cmt
luna_compare("run2", "run3")          # covariate: + WT allometry
luna_compare("run3", "run4", "run5")  # run3 extensions

cmp <- luna_compare("run3", "run4", "run5", return_object = TRUE)
cmp

luna_diff("run5", "run3")

# =============================================================================
# 5. ANNOTATIONS
# =============================================================================

luna_tag("run1", tag = "base_1cmt")
luna_tag("run2", tag = "base_2cmt")
luna_tag("run3", tag = "base_with_wt")
luna_tag("run4", tag = "not_adopted")
luna_tag("run5", tag = "final")

luna_note("run2", note = "dOFV vs run1: substantial; 2-cmt adopted")
luna_note("run3", note = "dOFV vs run2: significant WT effect; IIV reduced")
luna_note("run4", note = "dOFV vs run3 < 3.84 (1 df); SEX on CL not significant")
luna_note("run5", note = "dOFV vs run3: significant; IOV on CL adopted as final")

luna_list()
luna_log(n = 20)

# =============================================================================
# 6. DIAGNOSTIC PLOTS
# =============================================================================

luna_gof("run5")
luna_gof("run5", residual = "NPDE")
luna_gof("run5", ltbs = TRUE)

luna_ind("run5", nrow = 3, ncol = 3, page = 1)
luna_ind("run5", nrow = 3, ncol = 3, page = 2)

xpdb <- luna_xpose("run5")
# xpose::dv_vs_ipred(xpdb)
# xpose::res_vs_idv(xpdb, res = "CWRES")

# =============================================================================
# 7. OUTPUT TABLES
# =============================================================================

tabs   <- luna_tables("run5")
tab    <- tabs[[1]]
head(tab)

# =============================================================================
# 8. CLONE & ITERATE
# =============================================================================

luna_clone("run5", new_id = "run6", update_inits = TRUE)
# luna_edit("run6")
# luna_run("run6")

# =============================================================================
# 9. PSN TOOLS: BOOTSTRAP & VPC
# =============================================================================
#
# Tools are defined in the YAML under run5$tools with psn:: prefix.
# luna_tool() reads the tool entry, parses PsN CLI options, and calls execute.
#
# Bootstrap (500 samples, 4 threads, up to 3 retries per sample):
#   Creates: project/run5/bootstrap_dir/
#   Output : project/run5/bootstrap_dir/raw_results_run5.csv
#
# VPC (1000 simulations, auto-binning):
#   Creates: project/run5/vpc_dir/
#   Output : project/run5/vpc_dir/vpctab*

luna_tool("run5", tool = "psn::bootstrap")
luna_tool("run5", tool = "psn::vpc")

# Reload to attach tool results to the project cache
luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_info("run5")

# =============================================================================
# 9b. PSN-SPECIFIC: STEPWISE COVARIATE MODELLING (SCM)
# =============================================================================
#
# SCM automates forward-selection / backward-elimination of covariates.
# It is a PsN-native tool and must be added to the run YAML tools list
# before calling. Example entry to add to busulfan.yaml under run3:
#
#   tools:
#     - tool: psn::scm
#       options:
#         - config_file: scm_run3.cfg
#           threads: 4
#
# The SCM config file (scm_run3.cfg) specifies the covariate search:
#
#   model=run3.mod
#   search_direction=both
#   p_value=0.05
#   linearize=1
#   covariates=AGE,WT,HT,SEX,ALT,AST,SCR,ALB,CRP
#   parameters=CL,V1
#   categorical=SEX
#   continuous=AGE,WT,HT,ALT,AST,SCR,ALB,CRP
#
# Then run:

# luna_tool("run3", tool = "psn::scm")

# =============================================================================
# 10. MISCELLANEOUS
# =============================================================================

# luna_edit_project()
luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_list()
luna_log(n = 30)
