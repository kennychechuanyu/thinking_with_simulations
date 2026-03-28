# Thinking With Simulations — Run All Analyses
# This script reproduces all simulation results and figures.
# Expected total runtime: ~5-8 minutes
# Requirements: R >= 4.0, ggplot2, viridis

cat("=== Thinking With Simulations: Full Reproduction ===\n\n")

cat("Section 2: Blocking and Overexpectation...\n")
source("analysis/01_blocking.R")

cat("\nSection 3: Model Comparison and Critical Test...\n")
source("analysis/02_critical_test.R")

cat("\nSection 4: Parameter and Design Sweeps...\n")
source("analysis/03_parameter_sweep.R")

cat("\nSection 5: Virtual Experiment and Power Analysis...\n")
source("analysis/04_virtual_experiment.R")

cat("\nSections 3-4: Novel Figures (overexpectation sweep, obs function, mechanism isolation)...\n")
source("analysis/05_novel_figures.R")

cat("\nSection 4.7: Robustness of the Critical Test...\n")
source("analysis/07_robustness.R")

cat("\nSection 5 Extension: Model Recovery Analysis...\n")
source("analysis/08_model_recovery.R")

cat("\nGenerating all figures...\n")
source("figures.R")

cat("\n=== All analyses and figures complete. ===\n")
cat("Session info:\n")
print(sessionInfo())
