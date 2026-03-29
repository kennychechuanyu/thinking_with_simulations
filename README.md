# Thinking With Models Through Simulation

Companion code and materials for the paper:

> Yu, K. (2026). Thinking With Models Through Simulation. *Manuscript in preparation.*

## Overview

This methodological tutorial articulates the Simulation Thinking Cycle, a six-step framework (Question, Formalize, Anticipate, Simulate, Surprise, Reflect) for using computational simulation as a discipline of theoretical reasoning. Through worked examples using associative learning models (Rescorla-Wagner and Mackintosh), it develops six thinking skills and three methodological distinctions:

- **Commitment anatomy**: decomposing models into core claims, auxiliary assumptions, and implementation choices
- **Observation functions**: how the mapping from internal states to behavior can qualitatively alter which model appears to fit data
- **Structural vs. parametric predictions**: separating theoretical claims from artifacts of parameterization

## Requirements

- **R >= 4.0** (base R only for core analyses)
- **ggplot2** and **viridis** (for figure generation)
- **patchwork** or **gridExtra** (for multi-panel robustness figures)
- **shiny** and **bslib** (for the interactive companion app)

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
  07_robustness.R           Section 4.7: Robustness checks for the critical test
  08_model_recovery.R       Section 5 extension: Simplified model recovery analysis

figures.R                   Generate all manuscript figures
run_all.R                   Reproduce everything in one command

shiny_app/
  app.R                     Interactive companion app (4 modules)

Figures/                    All figures (PNG)
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
| `analysis/07_robustness.R` | ~10 seconds |
| `analysis/08_model_recovery.R` | ~2-5 minutes |
| `figures.R` | ~5 minutes |
| **Total (`run_all.R`)** | **~5-8 minutes** |

## Reproducibility

All scripts use `set.seed(2026)` for reproducible results.

## License

Code is released under the MIT License.
