# =============================================================================
# luna_workflow.R
# Example luna workflow: busulfan population PK model building
#
# Dataset : ../data/busulfan_adults.csv  (shared; 250 adults, 4 mg/kg Q24h x4 doses)
# Models  : project/run1–run5.mod     (1-cmt -> 2-cmt -> covariates -> IOV)
# Sections:
#   1. Setup & project initialisation
#   2. Pre-run inspection & syntax checks
#   3. Running models           [requires local NONMEM + pharmpy]
#   4. Post-run model comparison & selection
#   5. Annotations (tags, notes, log)
#   6. Diagnostic plots (GOF, individual fits, xpose)
#   7. Output tables
#   8. Cloning & iterating on models
#   9. Post-hoc tools (bootstrap, VPC)
#  10. Miscellaneous utilities
#
# NOTE: Sections 3 onward require completed NONMEM runs (.lst files present).
#       All calls are drafted and annotated so the workflow can be executed
#       once NONMEM access is available.
# =============================================================================

library(luna)

PROJECT_DIR <- "vignettes/example-1-nm-pharmpy/project"
PROJECT_NAME <- "busulfan"

# =============================================================================
# 1. SETUP & PROJECT INITIALISATION
# =============================================================================

# --- 1a. First-time setup: create project and auto-import .mod files ---------
#
# luna_new_project() scans the folder for .mod files and populates the YAML.
# A busulfan.yaml already exists in project/; use force = TRUE to regenerate,
# or skip and go straight to luna_load_project() for subsequent sessions.

luna_new_project(
  name        = PROJECT_NAME,
  folder      = PROJECT_DIR,
  description = "Busulfan popPK model building - 250 adults, 4 mg/kg Q24h IV",
  force       = TRUE       # omit (or set FALSE) once project file is in place
)

# --- 1b. Load project in subsequent sessions ---------------------------------
#
# luna_load_project() reads the YAML + any existing .lst output files,
# rebuilds the in-memory cache, and returns the project object invisibly.

proj <- luna_load_project(
  name   = PROJECT_NAME,
  folder = PROJECT_DIR
)

# --- 1c. View global luna config (opens ~/.config/luna/config.yaml) ----------
#
# Edit here to change the execution method (pharmpy / psn / nmfe),
# default bootstrap/VPC sample sizes, and display preferences.

# luna_config()    # uncomment to open config in editor

# --- 1d. List available project templates ------------------------------------

luna_project_templates()

# =============================================================================
# 2. PRE-RUN INSPECTION & SYNTAX CHECKS
# =============================================================================

# --- 2a. Tabular overview of all runs ----------------------------------------
#
# luna_list() prints a formatted table with run ID, description, status,
# OFV, and any tags/notes. Useful at the start of every session.

luna_list()

# --- 2b. Syntax-check a model without running it ----------------------------
#
# luna_check() uses pharmpy to parse the .mod file and reports any errors
# (missing dataset, bad THETA bounds, unknown ADVANs, etc.).
# Run on all models before submitting to catch issues early.

luna_check("run1")
luna_check("run2")
luna_check("run3")
luna_check("run4")
luna_check("run5")

# --- 2c. Diff model code between two runs ------------------------------------
#
# luna_diff() prints a side-by-side diff of two .mod files.
# With reference = NULL, luna uses the 'reference' field from the YAML.
# Useful for reviewing what changed between a parent and child model.

luna_diff("run2")              # run2 vs its YAML reference (run1)
luna_diff("run3")              # run3 vs run2
luna_diff("run4", "run3")      # explicit: run4 vs run3 (non-significant branch)
luna_diff("run5", "run3")      # run5 vs run3 (adopted IOV extension)

# --- 2d. Inspect the dataset linked to a model file --------------------------
#
# luna_dataset() reads the dataset path from $DATA in the .mod file via
# pharmpy and returns it as a data frame.

dat <- luna_dataset("run1")
head(dat)
dplyr::glimpse(dat)

# Quick sanity checks
table(dat$EVID)                         # 1000 dose events, 4000 observations
range(dat$DV[dat$EVID == 0])            # DV range (ng/mL)
length(unique(dat$ID))                  # 250 patients

# --- 2e. Open a model in the editor ------------------------------------------
#
# luna_edit() opens the .mod file in RStudio's source editor for manual review
# or hand-editing before running.

# luna_edit("run1")   # uncomment to open

# =============================================================================
# 3. RUNNING MODELS
# [Requires local NONMEM installation + pharmpy dispatcher configured]
# =============================================================================

# luna_run() reads the .mod file via pharmpy, creates a run subfolder (run1/),
# copies the model, submits to NONMEM, and writes output to run1/run.lst.
# Set as_job = TRUE to run asynchronously in an RStudio background job.

luna_run("run1")
luna_run("run2")

# After reviewing run1/run2 results with luna_compare() below, proceed:
luna_run("run3")

# run4 (SEX on CL) and run5 (IOV on CL) can be submitted in parallel;
# both depend only on run3 output for comparison, not sequentially on each
# other's results.
luna_run("run4")
luna_run("run5")

# Reload project after runs complete to refresh OFV/status in cache
luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_list()

# =============================================================================
# 4. POST-RUN MODEL COMPARISON & SELECTION
# [Requires completed .lst files]
# =============================================================================

# --- 4a. Detailed parameter output for a single run --------------------------
#
# luna_info() parses the .lst file and returns a structured list containing:
#   - THETA estimates (with RSE%)
#   - OMEGA / SIGMA matrices
#   - OFV, condition number, covariance step status
#   - Run metadata

info_run2 <- luna_info("run2")
info_run2

info_run3 <- luna_info("run3")
info_run3

info_run5 <- luna_info("run5")
info_run5

# --- 4b. Side-by-side comparison of multiple runs ----------------------------
#
# luna_compare() calls luna_info() on each run and produces a formatted
# parameter table for visual model selection.
#
# Step 1: 1-cmt vs 2-cmt structural comparison
luna_compare("run1", "run2")

# Step 2: No covariates vs WT allometry
luna_compare("run2", "run3")

# Step 3: run3 vs competing extensions (SEX on CL vs IOV on CL)
luna_compare("run3", "run4", "run5")

# Return as a data frame for further processing
cmp <- luna_compare("run3", "run4", "run5", return_object = TRUE)
cmp

# --- 4c. Code diff for final model selection decisions -----------------------

luna_diff("run5", "run3")   # review IOV additions relative to selected base

# =============================================================================
# 5. ANNOTATIONS: TAGS, NOTES, LOG
# =============================================================================

# --- 5a. Tagging runs --------------------------------------------------------
#
# luna_tag() adds string tags to a run entry in the YAML (e.g. "base",
# "not_adopted", "final"). Tags are visible in luna_list().

luna_tag("run1", tag = "base_1cmt")
luna_tag("run2", tag = "base_2cmt")
luna_tag("run3", tag = "base_with_wt")
luna_tag("run4", tag = "not_adopted")
luna_tag("run5", tag = "final")

# --- 5b. Adding notes to runs ------------------------------------------------
#
# luna_note() appends free-text notes to a run's YAML entry.
# Useful for recording decision rationale.

luna_note("run2", note = "dOFV vs run1: substantial; 2-cmt adopted")
luna_note("run3", note = "dOFV vs run2: significant WT effect; IIV CL/V reduced")
luna_note("run4", note = "dOFV vs run3 < 3.84 (1 df); SEX on CL not significant")
luna_note("run5", note = "dOFV vs run3: significant; IOV on CL adopted")

# View updated project table reflecting tags and notes
luna_list()

# --- 5c. View session action log ---------------------------------------------
#
# luna_log() shows the last n events (model runs, tool calls, etc.)
# with timestamps expressed as relative time (e.g. "5 minutes ago").

luna_log(n = 20)

# =============================================================================
# 6. DIAGNOSTIC PLOTS
# [Requires completed .lst + tab_run* output table files]
# =============================================================================

# --- 6a. Goodness-of-fit plots -----------------------------------------------
#
# luna_gof() returns a patchwork of:
#   - DV vs PRED  (population predictions)
#   - DV vs IPRED (individual predictions)
#   - CWRES vs TIME
#   - CWRES vs PRED
#   - CWRES distribution (histogram + QQ)
#
# Supports CWRES (default) or NPDE residuals.

gof_run2 <- luna_gof("run2")
gof_run2

gof_run3 <- luna_gof("run3")
gof_run3

gof_final <- luna_gof("run5")
gof_final

# NPDE-based residuals (requires npde output in table file)
# luna_gof("run5", residual = "NPDE")

# log-transformed IPRED/DV scale (useful for wide concentration ranges)
# luna_gof("run5", ltbs = TRUE)

# --- 6b. Individual concentration-time profiles ------------------------------
#
# luna_ind() returns a faceted grid of individual observed vs
# predicted concentration-time profiles.
#   - show$dv    : observed data points
#   - show$ipred : individual predictions (solid line)
#   - show$pred  : population predictions (dashed line)
#   - show$doses : vertical dose lines
#   - page / nrow / ncol : control pagination

# First page (9 subjects, 3x3 grid)
luna_ind("run5", nrow = 3, ncol = 3, page = 1)

# Page through remaining subjects
luna_ind("run5", nrow = 3, ncol = 3, page = 2)
luna_ind("run5", nrow = 3, ncol = 3, page = 3)

# Suppress SE ribbon and population prediction line for cleaner plots
luna_ind(
  "run5",
  show = list(dv = TRUE, ipred = TRUE, pred = FALSE, doses = TRUE, se = FALSE),
  nrow = 4, ncol = 4, page = 1
)

# --- 6c. xpose integration ---------------------------------------------------
#
# luna_xpose() imports the run's output table into an xpose_data object,
# enabling the full xpose4/xpose diagnostic plot suite.

xpdb_final <- luna_xpose("run5")

# Example xpose plots (requires xpose package)
# xpose::dv_vs_ipred(xpdb_final)
# xpose::res_vs_idv(xpdb_final, res = "CWRES")
# xpose::prm_vs_iteration(xpdb_final)

# =============================================================================
# 7. OUTPUT TABLES
# =============================================================================

# --- 7a. Load tab_run* output tables -----------------------------------------
#
# luna_tables() finds and reads the $TABLE output file(s) for a run,
# returning a named list of data frames (one per FILE= statement).

tabs_run5 <- luna_tables("run5")
str(tabs_run5)

tab <- tabs_run5[[1]]    # tab_run5 (main output table)
head(tab)

# Compute individual ETA distributions post-hoc
eta_summary <- tab |>
  dplyr::group_by(ID) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(ID, AGE, WT, SEX, ETA1, ETA2, ETA3) |>
  tidyr::pivot_longer(cols = starts_with("ETA"), names_to = "eta") |>
  dplyr::group_by(eta) |>
  dplyr::summarise(mean = mean(value), sd = sd(value), .groups = "drop")

eta_summary

# =============================================================================
# 8. CLONING & ITERATING ON MODELS
# =============================================================================

# --- 8a. Clone an existing model as the basis for a new run ------------------
#
# luna_clone() copies the .mod file to a new ID and registers it in the YAML.
# Use update_inits = TRUE to pull final estimates from the .lst as new initials.

# Example: clone run5 to explore a maturation function on CL (hypothetical run6)
luna_clone(
  id         = "run5",
  new_id     = "run6",
  update_inits = TRUE     # seed run6 with run5 final estimates
)

# Inspect the new file (opens in editor)
# luna_edit("run6")

# After editing run6.mod to add the maturation function, run it:
# luna_run("run6")

# =============================================================================
# 9. POST-HOC TOOLS: BOOTSTRAP & VPC
# =============================================================================

# --- 9a. Bootstrap ------------------------------------------------------------
#
# luna_tool() dispatches the configured tool (pharmpy or psn) for a given run.
# Bootstrap resamples the dataset n times and re-estimates parameters,
# providing empirical confidence intervals.
# Sample size is set in config (default: 500); override via ... if supported.

luna_tool("run5", tool = "bootstrap")

# --- 9b. Visual Predictive Check (VPC) ---------------------------------------
#
# VPC simulates from the final model and overlays observed vs predicted
# percentile bands to evaluate model predictive performance.

luna_tool("run5", tool = "vpc")

# Reload project to pick up bootstrap/VPC results attached to run5
luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_list()

# View updated run5 info including tool results
luna_info("run5")

# =============================================================================
# 10. MISCELLANEOUS UTILITIES
# =============================================================================

# --- 10a. Edit the project YAML directly --------------------------------------
#
# luna_edit_project() opens busulfan.yaml in the source editor.
# Use this to manually adjust descriptions, references, or add custom fields.

# luna_edit_project()   # uncomment to open

# --- 10b. Refresh project state at any time ----------------------------------
#
# Always safe to call after external changes (e.g. manual .lst edits,
# file copies from a remote cluster).

luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)

# --- 10c. Final project overview ---------------------------------------------

luna_list()
luna_log(n = 30)
