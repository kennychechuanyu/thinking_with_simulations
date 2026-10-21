# Section 5 results

These files hold the seeds of the simulated experiments of Section 5 and
Supplement S2, and everything computed from them. All records are simulated; there
are no human data. Each script named below is run from the repository root.
`Rscript run_all.R` runs all of them except `analysis/section5_predictive_se.R`.
Only with `--full-study` does it rerun `section5_study.R`, the check of the
starting vectors and the refits with wider bounds.

To check the standard errors of the two predictive comparisons and the decisions
against the saved fits, without fitting again, run

```sh
Rscript analysis/section5_predictive_se.R
```

It regenerates the answers from their seeds and scores them at the saved
parameters, which takes about ten seconds, and stops if anything differs from
`results.csv`.

| File | Written by | Contents | Used for |
|---|---|---|---|
| `jobs.csv` | `analysis/section5_study.R` | One row per simulated experiment (3,600): generating process (`scenario`), number of participants `n`, repetition, `id`, configuration and seed (610000 + `id`) | Supplement S2; regenerating any experiment from its seed |
| `generating_parameters.csv` | `analysis/section5_study.R` | The generating parameters of each process in each of the four configurations | Supplement S2 |
| `results.csv` | `analysis/section5_study.R` | One row per analysis of an experiment (`view` by `policy`): the response contrast (`estimate`) with its standard error and 95% interval, whether it was detected, the true contrast (`true_effect`), the three candidates' held-out scores, the carryover candidate's differences from the other two and their standard errors (`carry_vs_fixed_se`, `carry_vs_reset_se`), `winner`, `decision` and `model_decision` | Sections 5.5 and 5.6, Figures 10 and 11, Table S2 |
| `diagnostics.csv` | `analysis/section5_study.R` | One row per fit (three candidates by two folds in each analysis): negative log likelihood, convergence, optimizer code and message, the number of solutions compared (`n_starts`) and how many searches converged (`starts_ok`), the range of their objectives, whether the Nelder–Mead refinement was needed (`refined`), and the parameters whose estimates lie within 0.1% of their allowed range from a bound (`boundary`) | Supplement S2 |
| `parameters.csv` | `analysis/section5_study.R` | The estimated parameters of every fit, in the order of `diagnostics.csv` | Section 5.6 (how the candidates account for a difference produced by the response function); `analysis/section5_predictive_se.R` |
| `manifest.txt` | `analysis/section5_study.R` | The study design, the number of workers, the versions of R and Rcpp, the platform, and the md5 sums of the four source files of the run | Provenance of the five files above |
| `worked_example.rds` | `analysis/section5_worked_example.R` | The worked example: its 80 participants' records, the generating parameters (seed 8123), the three candidates' fits, the held-out comparison and the response contrast | Section 5.2, Figures 9 and S2 |
| `worked_example_responses.csv` | `analysis/section5_worked_example.R` | The worked example's answers, one row per participant: the participant's number, cue allocation (a_s of Supplement S2), then `B1`–`B10` for the answers to B's ten presentations and `D1`–`D10` for D's | Section 5.2 |
| `carryover_profile.csv` | `analysis/section5_worked_example.R` | The profile of associability carryover: `g_alpha` fixed at 0, .1, ..., 1, with the negative log likelihood, convergence and the re-estimated `eta`, `g_value`, `mu` and `slope` (gamma) | Section 5.3, Figure S1b |
| `mimicry_summary.csv` | `analysis/section5_worked_example.R` | For the fits to the generating probabilities of the associability process: the share of the reset candidate's full-record KL divergence that lies in the first two answers to each cue, and the smallest and largest D-minus-B difference in generating probability on presentations 8–10 | Section 5.4 |
| `landscape.csv` | `analysis/section5_worked_example.R` | Each candidate fitted directly to the generating probabilities of the associability and value processes, with no rate types (tau = 0), by view: the KL divergence at the generating values, with the parameters the candidate lacks set to 0 (`reference_kl`); the KL divergence after fitting (`fitted_kl`); whether the optimizer converged (`optimizer_converged`) and whether the divergence fell below 1e-8 (`kl_lower_bound_reached`); and the fitted parameters | Section 5.4, Supplement S2, Figure S1a |
| `initial_response_checks.csv` | `analysis/section5_first_response_and_bounds.R` | For every experiment, the paired test of equal first answers to B and D: the mean D-minus-B difference in first answers (`initial_difference`), the numbers of participants whose first answers differ with a yes to D only (`discordant_control`) or to B only (`discordant_blocked`), the one-sided p value and rejection at .05 | Section 5.6, Figure 11b |
| `initial_response_summary.csv` | `analysis/section5_first_response_and_bounds.R` | For the analyses with old-value carryover fixed at 0, by process: the number of experiments (`repetitions`), how often carryover is preferred, how often the first-answer test rejects equality, how often both happen, and how often the test rejects equality among the experiments in which carryover is preferred | Section 5.6 |
| `wider_bounds_refits.csv` | `analysis/section5_first_response_and_bounds.R --refit` | 24 experiments with 160 participants, refitted on their full records with wider parameter bounds: the decision and the winner before and after, and the held-out scores with the wider bounds | Supplement S2 |
| `starting_values_check.csv` | `analysis/section5_starting_values_check.R` | Eight simulated experiments for each main generating process and N (seeds 991001–991064), each candidate fitted to the participants outside fold 1: the negative log likelihood, convergence and refinement of the study's fitting procedure (`study_`), the negative log likelihood and convergence with only the four fixed starting vectors (`fixed_`), and the better of the study's fit and the best of 200 searches from random starts (`reference_nll`) | Supplement S2 |
| `summary.csv` | `analysis/section5_report.R` | Rates for each process, `n`, view and policy, with Monte Carlo standard errors (`_mcse`), coverage of the 95% interval, and bias and RMSE of the contrast | Section 5.5, Figure 10, Table S2 |
| `summary_by_configuration.csv` | `analysis/section5_report.R` | The detection, carryover, unresolved and fit-problem rates for each configuration | Section 5.5 |
| `section5_numbers.tex` | `analysis/section5_report.R` | The numbers of Section 5 and Supplement S2 that the paper takes from these results, as LaTeX macros | The paper's text |
| `section5_table.tex` | `analysis/section5_report.R` | Table S2 | The supplement |
| `worked_example_contrasts.csv` | `analysis/section5_fig09_one_experiment.R` | The worked example's observed D-minus-B difference in yes proportions at each presentation, the numbers of participants who said yes only to D and only to B, and the 95% interval of Agresti and Min (2005) for a difference between paired proportions | Figure 9b |

Not in the repository: `jobs/` holds one checkpoint per experiment
(`job_0001.rds` to `job_3600.rds`), from which `section5_study.R` builds
`results.csv`, `diagnostics.csv` and `parameters.csv`; `subset/` holds the
experiments rerun with `--jobs`, and `pilot/` the run at the pilot's settings
(`--pilot`).

## Codes used in the tables

- `scenario`, the generating process: `associability` (carried associability),
  `reset` (transfer-only adaptation), `value` (value generalization), `null`
  (physical-cue differences alone) and `response` (a history-specific response
  intercept).
- `config`, the parameter configuration of Supplement S2: 1 (the base
  configuration), 2 (slower and more heterogeneous learning), 3 (a less steep
  response function) or 4 (a reversed physical-cue contrast and, except in
  `null`, a weaker version of the generating process).
- `view`, the presentations analyzed for each cue: `late` (8–10), `early` (1–6)
  or `full` (1–10).
- `policy`: `value_allowed` estimates old-value carryover g_V in every
  candidate, `value_zero` fixes it at 0, and `response_allowed` also estimates
  the history-specific intercept.
- `fit`, in `diagnostics.csv` and `parameters.csv`: the candidate (`fixed`,
  `reset` or `carry`) and the held-out fold; `carry_2` is the carryover
  candidate fitted to the participants outside fold 2 and scored on fold 2.
- The parameter columns of `generating_parameters.csv`, `parameters.csv` and
  `landscape.csv` are those of Supplement S2, listed with their symbols and
  bounds at the top of `R/inference.R`: `mu` (mu), `spread` (tau), `cue` (d),
  `intercept` (b), `log_slope` (log gamma), `g_value` (g_V), `eta`, `g_alpha`
  and `history_intercept` (b_hist). In `generating_parameters.csv`, `adaptive`
  is TRUE for the adaptive model and FALSE for Rescorla–Wagner.
- `winner` is the candidate with the highest held-out score. `decision` applies
  the margin of Supplement S2: `carry` when the carryover candidate's score
  exceeds both alternatives' by more than two participant-level standard
  errors, `no_carry` when either alternative's score exceeds the carryover
  candidate's by that margin (the alternatives have no associability carryover
  but may carry old value), `unresolved` otherwise, and `fit_problem` when one
  of the analysis's six fits did not converge. Analyses with a fit problem stay
  in the denominators of the rates. `model_decision` is the winner when its
  score exceeds both others' by the margin, `unresolved` otherwise, and
  `fit_problem` as in `decision`.
- `carry_vs_fixed_se` and `carry_vs_reset_se` are `sqrt(N)` times the standard
  deviation of the participants' differences in held-out log score; `se` is the
  standard error of the response contrast. Participants in the same fold share
  fitted parameters, so these standard errors are descriptive (Section 5.2,
  Supplement S2). With `carry_vs_fixed` and `carry_vs_reset` they give
  `decision`; `model_decision` can also need the standard error of the
  fixed-rate versus reset difference, which is not saved.
- `forced_correct` in `summary.csv` is the rate at which the candidate matching
  the generating process has the highest held-out score, without the margin. It
  is missing where the policy leaves out the generating process. In `reset`, the
  carryover candidate also reproduces the generating process, with g_alpha = 0;
  in `null` and `response`, where g_V = 0, both adaptive candidates reproduce
  it with eta = 0. In these scenarios the highest score does not single out one
  process.
