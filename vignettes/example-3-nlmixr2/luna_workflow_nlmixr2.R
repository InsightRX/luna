# =============================================================================
# luna_workflow_nlmixr2.R
# Example workflow: busulfan population PK model building with nlmixr2
#
# nlmixr2 is not a supported luna execution backend yet (only pharmpy / PSN).
# Luna is used here for project organisation (run list, tags, notes, log).
# All model fitting, comparison, and diagnostics use nlmixr2 directly.
#
# Dataset  : ../data/busulfan_adults.csv  (shared; 250 adults, 4 mg/kg Q24h x4 doses)
# Models   : project/run1.R – run5.R    (nlmixr2 model functions)
# Sections:
#   1. Setup
#   2. Data preparation
#   3. Model fitting
#   4. Model comparison
#   5. Luna annotations (tags, notes, log)
#   6. Diagnostic plots (xpose.nlmixr2, ggPMX)
#   7. Individual fits
#   8. Bootstrap & uncertainty
#   9. VPC
# =============================================================================

library(luna)
library(nlmixr2)
library(xpose)
library(xpose.nlmixr2)
library(ggplot2)
library(dplyr)

PROJECT_DIR  <- system.file("extdata", "example-3-nlmixr2", "project", package = "luna")
PROJECT_NAME <- "busulfan"

# Source all model definitions
source(file.path(PROJECT_DIR, "run1.R"))
source(file.path(PROJECT_DIR, "run2.R"))
source(file.path(PROJECT_DIR, "run3.R"))
source(file.path(PROJECT_DIR, "run4.R"))
source(file.path(PROJECT_DIR, "run5.R"))

# =============================================================================
# 1. LUNA PROJECT SETUP
# =============================================================================
#
# luna_new_project() requires .mod files for auto-import; since this example
# uses R model functions, we create the project with import_models = FALSE
# and rely on the pre-built busulfan.yaml.

luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_list()

# =============================================================================
# 2. DATA PREPARATION
# =============================================================================

dat_raw <- read.csv(system.file("extdata", "data", "busulfan_adults.csv", package = "luna"))

# nlmixr2 accepts NONMEM-style datasets directly.
# Observations with MDV = 1 are excluded from the likelihood automatically.
# Dosing rows (EVID = 1) drive the regimen.
#
# nlmixr2 covariate access: column names are case-insensitive in the model block.
# WT, SEX, etc. are referenced directly as-is from the data frame.

dat <- dat_raw |>
  mutate(
    # nlmixr2 uses lowercase 'id' and 'time' as primary keys;
    # uppercase variants are also recognised but rename for safety
    id   = ID,
    time = TIME
  )

dplyr::glimpse(dat)
table(dat$EVID)               # 1000 dose events, 4000 observations
range(dat$DV[dat$EVID == 0])  # DV range (ng/mL)

# =============================================================================
# 3. MODEL FITTING
# =============================================================================
#
# nlmixr2(model, data, est, control) fits the model.
#
# Estimation methods:
#   "focei"  – FOCE with interaction (closest to NONMEM FOCEI; default choice)
#   "saem"   – Stochastic approximation EM (robust for complex models/sparse data)
#   "laplace"– Laplacian approximation
#
# FOCEI is used here for direct OFV comparability across runs.
#
# foceiControl() key options:
#   print      : print interval (iterations)
#   covMethod  : covariance method ("r,s" = robust sandwich; "r" = Hessian)
#   maxOuterIterations : max FOCE outer iterations

foce_ctrl <- foceiControl(print = 20, covMethod = "r,s")

# --- Run 1: 1-cmt base -------------------------------------------------------
fit1 <- nlmixr2(run1_model, dat, est = "focei", control = foce_ctrl)
print(fit1)

# --- Run 2: 2-cmt ------------------------------------------------------------
fit2 <- nlmixr2(run2_model, dat, est = "focei", control = foce_ctrl)
print(fit2)

# --- Run 3: 2-cmt + WT allometry --------------------------------------------
fit3 <- nlmixr2(run3_model, dat, est = "focei", control = foce_ctrl)
print(fit3)

# --- Run 4: 2-cmt + WT + SEX on CL (not adopted) ---------------------------
fit4 <- nlmixr2(run4_model, dat, est = "focei", control = foce_ctrl)
print(fit4)

# --- Run 5: 2-cmt + WT + IOV on CL ----------------------------------
#
# IOV adds 4 random effects per subject; SAEM handles this more robustly.
saem_ctrl <- saemControl(nBurn = 200, nEm = 300, print = 50)
fit5_saem <- nlmixr2(run5_model, dat, est = "saem", control = saem_ctrl)

# Re-estimate with FOCEI using SAEM estimates as initial values for comparability
fit5 <- nlmixr2(run5_model, dat, est = "focei",
                control = foce_ctrl,
                params  = fit5_saem$parFixed)
print(fit5)

# =============================================================================
# 4. MODEL COMPARISON
# =============================================================================

# --- 4a. Objective function values -------------------------------------------
#
# nlmixr2 stores the objective function (−2 × log-likelihood) in $objf.
# For nested models (run1 ⊂ run2 ⊂ run3 ⊂ run5), dOFV follows chi-squared.

ofv_table <- data.frame(
  run  = paste0("run", 1:5),
  npar = c(
    length(fit1$parFixed),
    length(fit2$parFixed),
    length(fit3$parFixed),
    length(fit4$parFixed),
    length(fit5$parFixed)
  ),
  OFV  = c(fit1$objf, fit2$objf, fit3$objf, fit4$objf, fit5$objf),
  AIC  = c(AIC(fit1), AIC(fit2), AIC(fit3), AIC(fit4), AIC(fit5)),
  BIC  = c(BIC(fit1), BIC(fit2), BIC(fit3), BIC(fit4), BIC(fit5))
) |>
  mutate(
    dOFV = OFV - lag(OFV),
    dOFV = ifelse(run %in% c("run1", "run4"), NA_real_, dOFV)
  )

print(ofv_table)

# --- 4b. Likelihood ratio tests ----------------------------------------------
#
# Nested model pairs (df = difference in estimated parameters):
#   run2 vs run1  (1-cmt → 2-cmt):   expect very significant
#   run3 vs run2  (+ WT allometry):   expect significant
#   run4 vs run3  (+ SEX on CL):      expect non-significant (p > 0.05)
#   run5 vs run3  (+ IOV on CL):      expect significant

lrt <- function(fit_full, fit_reduced) {
  d_ofv <- fit_reduced$objf - fit_full$objf
  d_par <- length(fit_full$parFixed) - length(fit_reduced$parFixed)
  p_val <- pchisq(d_ofv, df = d_par, lower.tail = FALSE)
  data.frame(dOFV = round(d_ofv, 2), df = d_par, p = round(p_val, 4))
}

cat("\n--- LRT: run2 vs run1 (1-cmt vs 2-cmt) ---\n")
print(lrt(fit2, fit1))

cat("\n--- LRT: run3 vs run2 (+ WT allometry) ---\n")
print(lrt(fit3, fit2))

cat("\n--- LRT: run4 vs run3 (+ SEX on CL) ---\n")
print(lrt(fit4, fit3))

cat("\n--- LRT: run5 vs run3 (+ IOV on CL) ---\n")
print(lrt(fit5, fit3))

# --- 4c. Parameter summaries ------------------------------------------------

# Fixed effects (population parameters)
fixef(fit5)

# Random effects variance-covariance
VarCorr(fit5)

# Full parameter table (estimates + RSE)
fit5$parFixed
fit5$parFixedDf    # with confidence intervals if covariance step succeeded

# =============================================================================
# 5. LUNA ANNOTATIONS
# =============================================================================
#
# Even without luna execution support, luna_tag() and luna_note() keep the
# project YAML in sync with modelling decisions for traceability.

luna_tag("run1", tag = "base_1cmt")
luna_tag("run2", tag = "base_2cmt")
luna_tag("run3", tag = "base_with_wt")
luna_tag("run4", tag = "not_adopted")
luna_tag("run5", tag = "final")

luna_note("run2", note = paste0("dOFV vs run1 = ", round(fit1$objf - fit2$objf, 1), "; 2-cmt adopted"))
luna_note("run3", note = paste0("dOFV vs run2 = ", round(fit2$objf - fit3$objf, 1), "; WT allometry adopted"))
luna_note("run4", note = paste0("dOFV vs run3 = ", round(fit3$objf - fit4$objf, 1), "; SEX on CL not significant"))
luna_note("run5", note = paste0("dOFV vs run3 = ", round(fit3$objf - fit5$objf, 1), "; IOV on CL adopted"))

luna_list()
luna_log(n = 20)

# =============================================================================
# 6. DIAGNOSTIC PLOTS — xpose.nlmixr2
# =============================================================================
#
# xpose.nlmixr2::xpose_data_nlmixr2() converts a nlmixr2 fit into an xpose_data
# object, enabling the full xpose diagnostic plot suite.

xpdb5 <- xpose.nlmixr2::xpose_data_nlmixr2(fit5)

# DV vs IPRED and DV vs PRED
xpose::dv_vs_ipred(xpdb5)
xpose::dv_vs_pred(xpdb5)

# Residuals vs time and predictions
xpose::res_vs_idv(xpdb5,  res = "CWRES")
xpose::res_vs_pred(xpdb5, res = "CWRES")

# CWRES distribution
xpose::res_distrib(xpdb5, res = "CWRES")

# ETA distributions and correlations
xpose::eta_distrib(xpdb5)
xpose::eta_cont(xpdb5, var = c("WT", "AGE", "SCR", "ALB"))
xpose::eta_cat(xpdb5,  var = "SEX")

# =============================================================================
# 6b. DIAGNOSTIC PLOTS — ggPMX
# =============================================================================
#
# ggPMX provides an alternative diagnostic framework with a report-ready
# plot controller object.

ctr <- ggPMX::pmx_nlmixr(fit5, conts = c("WT", "AGE", "SCR", "ALB", "CRP"),
                           cats  = "SEX",
                           res   = "CWRES")

# Individual pages of the diagnostic report
ggPMX::pmx_plot_dv_ipred(ctr)
ggPMX::pmx_plot_dv_pred(ctr)
ggPMX::pmx_plot_res_time(ctr)
ggPMX::pmx_plot_eta_box(ctr)
ggPMX::pmx_plot_eta_matrix(ctr)

# Export full PDF diagnostic report
# ggPMX::pmx_report(ctr, name = "busulfan_gof_run5", format = "report")

# =============================================================================
# 7. INDIVIDUAL CONCENTRATION–TIME PROFILES
# =============================================================================
#
# augPred() generates augmented (densified) predictions for each subject,
# suitable for individual-fit spaghetti plots.

aug5 <- nlme::augPred(fit5, minimum = 0, maximum = 96, length.out = 200)

# Plot first 9 subjects
ids_page1 <- unique(aug5$id)[1:9]
aug5 |>
  filter(id %in% ids_page1) |>
  ggplot(aes(x = time)) +
  geom_line(aes(y = .fitted, colour = .type)) +
  geom_point(data = filter(dat, id %in% ids_page1, EVID == 0),
             aes(y = DV), size = 1) +
  facet_wrap(~id, ncol = 3) +
  labs(x = "Time (h)", y = "Busulfan (ng/mL)", colour = NULL) +
  theme_classic()

# =============================================================================
# 8. BOOTSTRAP — parameter uncertainty
# =============================================================================
#
# nlmixr2 does not have a built-in bootstrap tool. Use
# nlmixr2extra::bootPlot() / nlmixr2extra::nlmixrBoot() if available,
# or implement a manual bootstrap loop.

if (requireNamespace("nlmixr2extra", quietly = TRUE)) {
  boot5 <- nlmixr2extra::nlmixrBoot(
    fit5,
    nboot   = 200,
    restart = FALSE
  )
  nlmixr2extra::bootPlot(boot5)
} else {
  message("nlmixr2extra not installed; skipping bootstrap.")
  message("Install with: install.packages('nlmixr2extra')")
}

# =============================================================================
# 9. VISUAL PREDICTIVE CHECK (VPC)
# =============================================================================
#
# vpc package (Rongen) works with nlmixr2 fits directly.

if (requireNamespace("vpc", quietly = TRUE)) {
  # Simulate from the final model
  vpc_dat <- nlmixr2::nlmixr2(
    run5_model, dat, est = "simulate",
    control = nlmixr2::nlmixr2Control(nsim = 500)
  )

  vpc::vpc(
    obs     = dat[dat$EVID == 0, ],
    sim     = vpc_dat,
    obs_cols = list(dv = "DV", idv = "TIME", id = "id"),
    sim_cols = list(dv = "DV", idv = "TIME", id = "id"),
    pi      = c(0.05, 0.95),
    ci      = c(0.05, 0.95),
    bins    = "jenks"
  )
} else {
  message("vpc package not installed; skipping VPC.")
  message("Install with: install.packages('vpc')")
}
