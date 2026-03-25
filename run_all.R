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

cat("\nNovel Figures (Sections 3-4 additions)...\n")
source("analysis/05_novel_figures.R")

cat("\nGenerating all figures...\n")
source("figures.R")

cat("\n=== All analyses and figures complete. ===\n")
cat("Session info:\n")
print(sessionInfo())
