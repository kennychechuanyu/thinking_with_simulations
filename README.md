# Thinking With Models Through Simulation: A Methodological Tutorial

Companion code and materials for the tutorial paper:

> Yu, K. (2026). Thinking With Models Through Simulation: A Methodological Tutorial. *Manuscript in preparation.*

## Overview

This methodological tutorial articulates the Simulation Thinking Cycle, a six-step framework (Question, Formalize, Predict, Simulate, Surprise, Reflect) for using computational simulation as a discipline of theoretical reasoning. Through worked examples using associative learning models (Rescorla-Wagner and Mackintosh), it develops six thinking skills and three methodological distinctions:

- **Commitment anatomy**: decomposing models into core claims, auxiliary assumptions, and implementation choices
- **Observation functions**: how the mapping from internal states to behavior can qualitatively alter which model appears to fit data
- **Structural vs. parametric predictions**: separating theoretical claims from artifacts of parameterization

## Requirements

- **R >= 4.0** (base R only for core analyses)
- **ggplot2** and **viridis** (for figure generation)

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
  06_robustness.R           Section 4.7: Robustness checks for the critical test

figures.R                   Generate all manuscript figures
run_all.R                   Reproduce everything in one command

Figures/                    All 14 figures (PNG)
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
| `analysis/06_robustness.R` | ~10 seconds |
| `figures.R` | ~5 minutes |
| **Total (`run_all.R`)** | **~5-8 minutes** |

## Reproducibility

All scripts use `set.seed(2026)` for reproducible results.

## License

Code is released under the MIT License.
