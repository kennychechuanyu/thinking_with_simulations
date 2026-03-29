# Thinking With Simulations — Run All Analyses
# This script reproduces all simulation results and figures.
# Expected total runtime: ~5-8 minutes
# Requirements: R >= 4.0, ggplot2, viridis

cat("=== Thinking With Simulations: Full Reproduction ===\n\n")

cat("01_blocking.R\n")
source("analysis/01_blocking.R")

cat("\n02_critical_test.R\n")
source("analysis/02_critical_test.R")

cat("\n03_parameter_sweep.R\n")
source("analysis/03_parameter_sweep.R")

cat("\n04_virtual_experiment.R\n")
source("analysis/04_virtual_experiment.R")

cat("\n05_novel_figures.R\n")
source("analysis/05_novel_figures.R")

cat("\n07_robustness.R\n")
source("analysis/07_robustness.R")

cat("\n08_model_recovery.R\n")
source("analysis/08_model_recovery.R")

cat("\nfigures.R\n")
source("figures.R")

cat("\n=== All analyses and figures complete. ===\n")
print(sessionInfo())
