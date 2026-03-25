# Thinking With Models Through Simulation

Companion code and materials for the tutorial paper *Thinking With Models Through Simulation* by Kenny Yu.

## Overview

This tutorial develops the Simulation Thinking Cycle, a six-step framework for using computational simulation as a discipline of theoretical reasoning. Through worked examples using associative learning models (Rescorla-Wagner and Mackintosh), it demonstrates how simulation reveals hidden predictions, how to design critical experiments through model comparison, and how to separate structural theoretical claims from artifacts of parameterization.

## Requirements

- **R >= 4.0** (base R only for all analyses)
- **ggplot2** and **viridis** (for figure generation only)

## File Organization

```
R/
  models.R                  Core model functions (RW, Mackintosh, observation functions)
  exercises.R               Hands-on exercises for each tutorial section

analysis/
  01_blocking.R             Section 2: Blocking and overexpectation
  02_critical_test.R        Section 3: Model comparison and critical test design
  03_parameter_sweep.R      Section 4: Parameter and design sensitivity
  04_virtual_experiment.R   Section 5: Virtual experiment and power analysis
  05_novel_figures.R        Figures 9-11: Overexpectation sweep, observation
                              function impact, mechanism isolation

figures.R                   Generate all manuscript figures (2-11)
run_all.R                   Reproduce everything in one command

Figures/                    All 11 figures (PDF and PNG)

main.tex                   Manuscript source
references.bib             Bibliography
```

## How to Run

Set your working directory to the project root, then:

```r
# Reproduce all analyses and figures
source("run_all.R")

# Or run individual sections
source("R/models.R")
source("analysis/01_blocking.R")
```

Each analysis script sources `R/models.R` automatically and can be run independently.

## Expected Runtimes

| Script | Approximate Runtime |
|--------|---------------------|
| `analysis/01_blocking.R` | < 1 second |
| `analysis/02_critical_test.R` | < 1 second |
| `analysis/03_parameter_sweep.R` | ~10 seconds |
| `analysis/04_virtual_experiment.R` | ~3-5 minutes |
| `analysis/05_novel_figures.R` | ~10 seconds |
| `figures.R` | ~5 minutes |
| **Total (`run_all.R`)** | **~5-8 minutes** |

## Reproducibility

All scripts use `set.seed(2026)` for reproducible results.

## License

Code is released under the MIT License. The manuscript text is copyright the author.
