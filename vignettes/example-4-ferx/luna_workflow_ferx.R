# =============================================================================
# luna_workflow_ferx.R
# Example workflow: busulfan population PK model building with ferx-nlme
#
# ferx-nlme is a Rust-based NLME estimation engine with an R wrapper (ferx).
# luna_run() dispatches to ferx when method = "ferx" is set in the project
# config. Model files use the .ferx DSL; results are saved as {id}.fitrx.
#
# Dataset  : busulfan_adults_mgl.csv (DV in mg/L; converted from ng/mL)
# Models   : project/run1.ferx – run3.ferx
# Sections:
#   1. Setup
#   2. Project structure
#   3. Model checking & running
#   4. Inspecting fit results
#   5. Model comparison
#   6. Luna annotations (tags, notes, log)
#   7. Diagnostic plots (GOF)
#   8. VPC
# =============================================================================

library(luna)
library(ferx)
library(ggplot2)
library(dplyr)

PROJECT_DIR  <- system.file("extdata", "example-4-ferx", "project", package = "luna")
PROJECT_NAME <- "busulfan"

# =============================================================================
# 1. LUNA PROJECT SETUP
# =============================================================================
#
# The project YAML (busulfan.yaml) contains:
#   project.config.tools.modelfit.method: ferx
#   project.config.tools.modelfit.data:   ../../data/busulfan_adults_mgl.csv
#
# luna_load_project() reads the YAML, initialises the disk cache, and
# makes config available to all luna_* functions in the session.

luna_load_project(name = PROJECT_NAME, folder = PROJECT_DIR)
luna_list()

# =============================================================================
# 2. PROJECT STRUCTURE
# =============================================================================
#
# .ferx model files live alongside the project YAML:
#
#   run1.ferx   1-cmt IV infusion, proportional RUV
#   run2.ferx   2-cmt IV infusion, proportional RUV
#   run3.ferx   2-cmt + allometric WT scaling on CL and V1
#
# Units throughout: CL/Q (L/h), V (L), DV (mg/L)
# Data:  busulfan_adults_mgl.csv — 250 adults, AMT/RATE in mg/mg/h, DV in mg/L

# =============================================================================
# 3. MODEL CHECKING AND RUNNING
# =============================================================================
#
# luna_check() verifies that the .ferx file exists (file-presence check).
# luna_run() calls ferx::ferx_fit() and saves the result as {id}.fitrx.
# Additional ferx_fit() arguments (method, covariance, etc.) can be passed
# directly through luna_run() via ...

luna_check("run1")

# --- Run 1: 1-cmt base -------------------------------------------------------
luna_run("run1")

# --- Run 2: 2-cmt ------------------------------------------------------------
luna_run("run2")

# --- Run 3: 2-cmt + WT allometry ---------------------------------------------
luna_run("run3")

# Refresh the run list (status and timestamps update automatically)
luna_list()

# =============================================================================
# 4. INSPECTING FIT RESULTS
# =============================================================================
#
# luna_ferx_info() reads {id}.fitrx and prints key convergence statistics.
# The full ferx_fit object can be read back directly for deeper access.

luna_ferx_info("run1")
luna_ferx_info("run3")

# Read the raw fit objects for detailed access
fit1 <- ferx::ferx_load_fit(file.path(PROJECT_DIR, "run1.fitrx"))
fit2 <- ferx::ferx_load_fit(file.path(PROJECT_DIR, "run2.fitrx"))
fit3 <- ferx::ferx_load_fit(file.path(PROJECT_DIR, "run3.fitrx"))

# Tidy parameter table (theta, omega, sigma with SE and %RSE)
ferx_estimates(fit3)

# ETA shrinkage
fit3$shrinkage_eta

# Individual estimates (one row per subject)
head(fit3$individual_estimates)

# =============================================================================
# 5. MODEL COMPARISON
# =============================================================================
#
# OFV comparisons: dOFV follows chi-squared for nested models (1 df ~ 3.84).

ofv_table <- data.frame(
  run  = c("run1", "run2", "run3"),
  OFV  = c(fit1$ofv, fit2$ofv, fit3$ofv),
  AIC  = c(fit1$aic, fit2$aic, fit3$aic),
  BIC  = c(fit1$bic, fit2$bic, fit3$bic)
) |>
  mutate(dOFV = OFV - lag(OFV))

print(ofv_table)

# Convergence and covariance status
data.frame(
  run       = c("run1", "run2", "run3"),
  converged = c(fit1$converged, fit2$converged, fit3$converged),
  cov_ok    = c(!is.null(fit1$cov_matrix),
                !is.null(fit2$cov_matrix),
                !is.null(fit3$cov_matrix))
)

# =============================================================================
# 6. LUNA ANNOTATIONS
# =============================================================================

luna_tag("run1", tag = "base_1cmt")
luna_tag("run2", tag = "base_2cmt")
luna_tag("run3", tag = c("final", "wt_allometry"))

luna_note("run2", note = paste0(
  "dOFV vs run1 = ", round(fit1$ofv - fit2$ofv, 1),
  "; 2-cmt significantly better"
))
luna_note("run3", note = paste0(
  "dOFV vs run2 = ", round(fit2$ofv - fit3$ofv, 1),
  "; WT allometry adopted"
))

luna_list()
luna_log(n = 10)

# =============================================================================
# 7. DIAGNOSTIC PLOTS (GOF)
# =============================================================================
#
# ferx saves individual/population predictions and weighted residuals in
# result$sdtab (ID, TIME, DV, PRED, IPRED, CWRES, IWRES, ETA1, ...).

sdtab <- fit3$sdtab
if ("MDV" %in% names(sdtab)) {
  sdtab <- sdtab |> filter(MDV == 0 | is.na(MDV))
}

# DV vs PRED
ggplot(sdtab, aes(x = PRED, y = DV)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linewidth = 0.7) +
  labs(title = "run3: DV vs PRED", x = "Population prediction (mg/L)",
       y = "Observed DV (mg/L)") +
  theme_bw()

# DV vs IPRED
ggplot(sdtab, aes(x = IPRED, y = DV)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "steelblue", linewidth = 0.7) +
  labs(title = "run3: DV vs IPRED", x = "Individual prediction (mg/L)",
       y = "Observed DV (mg/L)") +
  theme_bw()

# CWRES vs TIME
ggplot(sdtab, aes(x = TIME, y = CWRES)) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_hline(yintercept = c(-2, 0, 2), linetype = c("dashed", "solid", "dashed"),
             colour = c("grey50", "red", "grey50")) +
  labs(title = "run3: CWRES vs TIME") +
  theme_bw()

# ETA distributions
etas <- fit3$ebe_etas
ggplot(tidyr::pivot_longer(etas, -ID, names_to = "eta", values_to = "value"),
       aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  facet_wrap(~eta, scales = "free") +
  labs(title = "run3: ETA distributions") +
  theme_bw()

# =============================================================================
# 8. VPC (Visual Predictive Check)
# =============================================================================
#
# ferx_simulate() re-uses the fitted parameter estimates to generate replicate
# datasets. Pass fit = result to simulate at estimated (not initial) values.

sim3 <- ferx_simulate(
  model = file.path(PROJECT_DIR, "run3.ferx"),
  data  = system.file("extdata", "data", "busulfan_adults_mgl.csv",
                      package = "luna"),
  n_sim = 200,
  seed  = 42,
  fit   = fit3
)

# Build a simple percentile VPC
obs <- sdtab |>
  select(ID, TIME, DV)

sim_pct <- sim3 |>
  group_by(TIME) |>
  summarise(
    p05 = quantile(DV_SIM, 0.05),
    p50 = quantile(DV_SIM, 0.50),
    p95 = quantile(DV_SIM, 0.95),
    .groups = "drop"
  )

obs_pct <- obs |>
  group_by(TIME) |>
  summarise(
    p05 = quantile(DV, 0.05),
    p50 = quantile(DV, 0.50),
    p95 = quantile(DV, 0.95),
    .groups = "drop"
  )

ggplot() +
  geom_ribbon(data = sim_pct, aes(x = TIME, ymin = p05, ymax = p95),
              fill = "steelblue", alpha = 0.3) +
  geom_line(data = sim_pct, aes(x = TIME, y = p50),
            colour = "steelblue", linewidth = 0.8) +
  geom_line(data = obs_pct, aes(x = TIME, y = p05),
            linetype = "dashed", colour = "black") +
  geom_line(data = obs_pct, aes(x = TIME, y = p50), colour = "black") +
  geom_line(data = obs_pct, aes(x = TIME, y = p95),
            linetype = "dashed", colour = "black") +
  geom_point(data = obs, aes(x = TIME, y = DV), alpha = 0.3, size = 0.6) +
  labs(title = "run3: VPC (n_sim = 200)",
       x = "Time (h)", y = "Busulfan concentration (mg/L)") +
  theme_bw()
