# Thinking with Computational Models to Guide Scientific Inquiry

Code for the paper

> Yu, K. (2026). *Thinking with computational models to guide scientific inquiry* [Manuscript submitted for publication]. KU Leuven.

The repository contains the code for every example in the paper, the scripts that
produce each figure, table and reported number, the saved results of the simulation
study in Sections 6 and 7, and a companion app. The examples in Sections 2 to 7 use
two models of associative learning, the Rescorla–Wagner model and a model with
adaptive associability. Section 8 uses a drift diffusion model.

All commands are run from the repository root. On Windows, `Rscript.exe` is in the
`bin` folder of the R installation if it is not on the PATH.

## A first example

```sh
Rscript analysis/section2_blocking.R
```

This takes about a second. It checks the values reported in Section 2 and draws
Figure 2. To follow the updates trial by trial up to the first compound trial, as
Section 2.2 does by hand, start R in the repository root and run

```r
source("R/models.R")
design <- c(make_trials("A", 1, 10, "A+"), make_trials(c("A", "B"), 1, 10, "AB+"))
rw_hand_trace(design, beta = c(A = .12, B = .12), n_trace = 11)
```

The scripts for Sections 3 to 5 work in the same way and take a second or two each.

## Reproducing the paper

```sh
Rscript run_all.R
```

This runs the checks of the model code, then the analysis scripts in the order of
the paper, each in a separate R process, and last the checks of the app. The worked
example of Sections 6.2 to 6.4 is simulated and fitted again. The other results of
Sections 6 and 7 are computed from the saved study in `results/section6/`. Figure 1
is compiled separately from its TikZ source.

To run the 3,600 simulated experiments of Sections 6 and 7 again (67,200 fits, about
5 CPU hours, or 45 minutes with eight workers):

```sh
Rscript run_all.R --full-study --workers=8
```

This also repeats the check of the starting vectors, the refits with wider bounds
and the simulated experiments used by the app. Single experiments can be rerun from
their seeds and compared with the saved rows:

```sh
Rscript analysis/section6_study.R --jobs=10,17,2808
```

The output goes to `results/section6/subset/`. With `--pilot`, the script runs the
pilot of Supplement S2 (four experiments per generating process, 80 participants,
the base parameter set) with 12 starting vectors per fit and writes to
`results/section6/pilot/`.

Two checks can also be run on their own. The first, which `run_all.R` also runs,
repeats the diffusion simulations of Section 8 with two time steps (Supplement S1).
The second recomputes the standard errors of the two predictive comparisons from the
saved fits of the study, without fitting again, and stops if they differ from the
saved ones.

```sh
Rscript analysis/section8_decision_model.R --timestep-check
Rscript analysis/section6_predictive_se.R
```

Each analysis script checks the values that the paper reports. A script whose
settings have been changed therefore stops at these checks, in some scripts after
rewriting its figures. `git checkout -- Figures results` restores the saved files.
To try other settings, use the app's Explore tab.

## Scripts and outputs

Each figure is saved in `Figures/` as a PNG file, which is in the repository, and as
a PDF file, which is not.

| Paper item | Script | Output |
|---|---|---|
| Figure 1 | `Figures/fig01_thinking_cycle.tex` (TikZ; compiled in `Figures/` with `pdflatex`, then converted with `pdftoppm`) | `Figures/fig01_thinking_cycle.png` |
| Sections 2.3 and 3.3, checks of the implementation; Supplement S4.1, the conditions in all 100 settings of Section 4.3 | `tests/verify_models.R` | console |
| Section 2, Figure 2 | `analysis/section2_blocking.R` | `Figures/fig02_blocking` |
| Section 3, Table 3, Figure 3; Supplement S4.2, Table S4 | `analysis/section3_new_outcome.R` | `Figures/fig03_new_outcome`, `results/supplementS4_adaptive_trials.csv`, `results/supplement_table_s4.tex` |
| Section 4.1, Figure 4 | `analysis/section4_1_interventions.R` | `Figures/fig04_interventions` |
| Section 4.2, Figure 5 | `analysis/section4_2_carryover.R` | `Figures/fig05_carryover` |
| Section 4.3 | `analysis/section4_3_training_and_rate.R` | console |
| Section 4.4, Table 5 | `analysis/section4_4_update_rules.R` | console |
| Section 5, Table 6, Figure 6 | `analysis/section5_responses.R` | `Figures/fig06_responses` |
| Sections 6.5 and 7, the 3,600 simulated experiments | `analysis/section6_study.R` (run by `run_all.R --full-study`) | `results/section6/`: `jobs.csv`, `generating_parameters.csv`, `results.csv`, `diagnostics.csv`, `parameters.csv`, `manifest.txt` |
| Sections 6.2–6.4, the worked example, the profile of associability carryover and the fits to the generating probabilities; Supplement S2.8, the fitted parameters | `analysis/section6_worked_example.R` | `results/section6/`: `worked_example.rds`, `worked_example_responses.csv`, `carryover_profile.csv`, `landscape.csv`, `mimicry_summary.csv`, `supplement_fitted_parameters.tex` |
| Supplement S2.7, the number of starting vectors | `analysis/section6_starting_values_check.R` (run by `run_all.R --full-study`) | `results/section6/starting_values_check.csv` |
| Section 7.2, the first-response check; Supplement S2.10, the refits with wider bounds | `analysis/section7_first_response_and_bounds.R` | `results/section6/`: `initial_response_checks.csv`, `initial_response_summary.csv`, `wider_bounds_refits.csv` |
| Numbers of Sections 6 and 7 and Supplement S2 taken from the study; Tables 9 and S2 | `analysis/section6_report.R` | `results/section6/`: `section6_numbers.tex`, `summary_rates.tex`, `supplement_table_s2.tex`, `summary.csv`, `summary_by_configuration.csv` |
| Figure 7 | `analysis/section6_fig07_one_experiment.R` | `Figures/fig07_one_experiment`, `results/section6/worked_example_contrasts.csv` |
| Figure 8 | `analysis/section6_fig08_detection.R` | `Figures/fig08_detection` |
| Figure 9 | `analysis/section7_fig09_shared_prediction.R` | `Figures/fig09_shared_prediction` |
| Table 7 | the candidates are defined by `inference_spec()` in `R/inference.R` | |
| Section 8, Figure 10; Supplement S1, Table S1 | `analysis/section8_decision_model.R` | `Figures/fig10_decision_times`, `results/section8_ddm_checks.csv`, `results/section8_ddm_timestep_checks.csv` |
| Supplement S2, checks of the implementation | `tests/verify_inference.R` | console |
| Figure S1 | `analysis/section6_figS1_mimicry_and_profile.R` | `Figures/figS1_mimicry_and_profile` |
| Figure S2 | `analysis/section6_figS2_fit_by_light.R` | `Figures/figS2_fit_by_light` |
| Supplement S3, Table S3, Figure S3; Table 5 again, with a separate implementation | `analysis/supplementS3_update_rule.R` | `Figures/figS3_update_rule`, `results/supplementS3_update_rule.csv` |
| The simulated experiments in the app | `shiny_app/make_inference_cases.R` | `shiny_app/data/inference_cases.rds` |

`results/section6/README.md` describes the files of the simulation study.

## Repository layout

```
R/
  models.R              Rescorla–Wagner and adaptive models, response functions (Sections 2–5)
  inference.R           Sections 6 and 7: generator, likelihood, fitting and held-out comparison
  inference.cpp         the learning recursions and likelihood of inference.R, compiled with Rcpp
  figure_style.R        colors, number format, ggplot2 theme and figure saving
analysis/               analysis scripts, named by section of the paper
tests/
  verify_models.R       checks of R/models.R (Sections 2.3 and 3.3)
  verify_inference.R    checks of R/inference.R and R/inference.cpp (Supplement S2)
  verify_app.R          checks of the app's numbers against the paper
  verify_app_flow.R     a walk through the app with shiny::testServer
  verify_app_feedback.R checks of the feedback on predictions and conclusions
results/                saved outputs; results/session_info.txt records the versions used
Figures/                figures as PNG files, and the TikZ source of Figure 1
shiny_app/              the companion app
run_all.R               runs the checks and every analysis in the order of the paper
```

## Companion app

The app follows the investigation of the paper in seven steps, one for each of
Sections 2 to 8, from one learning rule to a simulated experiment and a model of
simple decisions, and can be used without the paper. Each step asks for an
expectation and a reason before it shows the result. Its Explore tab lets you change
the settings. To run it in R:

```r
shiny::runApp("shiny_app", launch.browser = TRUE)
```

The app also runs in a web browser without R, at
<https://kennychechuanyu.github.io/thinking_with_simulations/>. The first visit takes
a while, because R and the packages are loaded into the browser. This version is
made with shinylive (0.5.0 or later):

```r
shinylive::export("shiny_app", "site", template_params = list(title = "Thinking with Computational Models"))
```

The app computes the model results as it runs, with `shiny_app/R/models.R`, a copy of
`R/models.R`, and fits no models. The simulated experiments of Steps 5 and 6
are taken from the simulation study of Sections 6 and 7 by
`shiny_app/make_inference_cases.R`, which regenerates their responses from the saved
seeds, analyzes them with the study's code and writes
`shiny_app/data/inference_cases.rds`. The app uses no file outside `shiny_app/`.

`tests/verify_app.R` checks that the app's copy of `R/models.R` is identical to the
original and that the app's calculations give the values in the paper.
`tests/verify_app_flow.R` walks through the seven steps with `shiny::testServer`, and
`tests/verify_app_feedback.R` checks the feedback the app gives on every combination
of answers. `run_all.R` runs these checks after the analyses.

## Requirements and tested setup

- R 4.1 or later (4.2 or later on Windows), in a UTF-8 locale, which the Greek
  letters in the figures and the app need.
- ggplot2 (3.5.0 or later), patchwork (1.2.0 or later) and ragg for the figures. The
  figure PDFs are written with the Quartz device on macOS and with `cairo_pdf()`
  elsewhere; if neither is available, only the PNG files are written. `R/models.R`
  uses base R only.
- Rcpp and a C++ compiler for Sections 6 and 7, and therefore for `run_all.R`: the Xcode
  command line tools on macOS (`xcode-select --install`), Rtools on Windows, g++ on
  Linux.
- shiny, bslib (0.6.0 or later) and ggplot2 for the app and its checks.
- For Figure 1 only: LaTeX with TikZ, the standalone class and the tgheros font, and
  pdftoppm (Poppler).

The saved outputs were made on macOS 26 (Apple silicon) with R 4.5.2, ggplot2 4.0.2,
patchwork 1.3.2, Rcpp 1.1.1, ragg 1.5.0, systemfonts 1.3.1, textshaping 1.0.4, shiny
1.13.0 and bslib 0.10.0. `run_all.R` writes the versions in use to
`results/session_info.txt`. On this setup the scripts reproduce the saved files
exactly. On other systems the fits can end at slightly different points. The log
likelihoods then differ little, but poorly determined estimates can differ more, and
with `--full-study` a few rates in Table S2 can change by tenths of a percentage
point, which stops the run at its checks. The figures use Helvetica; where it is not
installed, R substitutes another font.

Every simulation sets its seed, and each experiment of the simulation study has its
own (`results/section6/jobs.csv`), so the results do not depend on the number of
workers. On Windows, which cannot fork R processes, `--workers` is ignored and the
study runs on one core.

## License

MIT License; see `LICENSE`.
