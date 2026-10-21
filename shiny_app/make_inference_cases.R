# Simulated experiments for Step 5 of the companion app. In the app, the reader
# designs the experiment: 40 or 160 participants, and the last three, the first
# six, or all ten presentations recorded. For each of three generating
# processes, this script takes one experiment of each size from the Section 5
# study, regenerates its records from the saved seed, and analyzes them under
# each design. The response contrast and the held-out comparison of the three
# candidates, with old-value carryover estimated and with it fixed at 0, use the
# study's functions in R/inference.R; the first-answer check repeats the test of
# analysis/section5_first_response_and_bounds.R. Where the study saved the same
# analysis, the script stops unless the two agree. The study ran the comparison
# with old-value carryover fixed at 0 only for carried associability and value
# generalization, with 160 participants and all ten presentations; the other
# fixed-at-0 analyses are made here. Writes shiny_app/data/inference_cases.rds.
#   Rscript shiny_app/make_inference_cases.R   (about half a minute)
#
# Selection: for each process, the first experiment of each size in the
# study's first configuration. The value-generalization experiment with 160
# participants is also the first in that configuration in which the fixed-at-0
# comparison favored associability carryover; the app says so when it reveals
# the process, and reports how often that happened across all 400 experiments.

if (!file.exists("R/inference.R")) stop("Run this script from the repository root.")
source("R/inference.R")

study_dir <- "results/section5"
jobs <- read.csv(file.path(study_dir, "jobs.csv"))
generating <- read.csv(file.path(study_dir, "generating_parameters.csv"))
saved <- read.csv(file.path(study_dir, "results.csv"))
study <- read.csv(file.path(study_dir, "summary.csv"))
first_checks <- read.csv(file.path(study_dir, "initial_response_checks.csv"))
first_summary <- read.csv(file.path(study_dir, "initial_response_summary.csv"))

sources <- c("value", "associability", "reset")
sizes <- c(40L, 160L)
views <- c("late", "early", "full")
view_cols <- list(late = 8:10, early = 1:6, full = 1:10)
# The app's names for the two choices of g_V, and the study's.
policies <- c(estimated = "value_allowed", zero = "value_zero")

first_in_config1 <- function(scenario, n) {
  min(saved$id[saved$scenario == scenario & saved$n == n & saved$config == 1 &
                 saved$view == "full" & saved$policy == "value_allowed"])
}
stopifnot(first_in_config1("value", 160) ==
  min(saved$id[saved$scenario == "value" & saved$n == 160 & saved$config == 1 &
                 saved$policy == "value_zero" & saved$decision == "carry"]))

# Section 5.6: the exact one-sided paired test of equal first answers to B
# (column 1) and D (column n_pres + 1). Among the participants whose two first
# answers differ, the number who answer yes to D only is binomial with
# probability .5 under equality.
first_response_check <- function(y, n_pres = 10L) {
  d_only <- sum(y[, n_pres + 1L] == 1 & y[, 1L] == 0)
  b_only <- sum(y[, n_pres + 1L] == 0 & y[, 1L] == 1)
  discordant <- d_only + b_only
  p <- if (discordant == 0) 1 else pbinom(d_only - 1, discordant, .5, lower.tail = FALSE)
  list(d_only = d_only, b_only = b_only, p = p, n = nrow(y))
}

# The D-minus-B difference in yes proportions at each recorded presentation q,
# with the interval of Figure 9b (Agresti and Min, 2005): add 1/2 to each cell
# of the paired 2 x 2 table, then use the Wald interval for the difference of
# the two discordant proportions. These are the points and bars of the app's
# contrast plot.
contrast_by_presentation <- function(y, cols, n_pres = 10L) {
  n <- nrow(y)
  t(vapply(cols, function(q) {
    n_d_only <- sum(y[, n_pres + q] == 1 & y[, q] == 0)
    n_b_only <- sum(y[, q] == 1 & y[, n_pres + q] == 0)
    p_d <- (n_d_only + .5) / (n + 2)
    p_b <- (n_b_only + .5) / (n + 2)
    se <- sqrt((p_d + p_b - (p_d - p_b)^2) / (n + 2))
    c(presentation = q, estimate = (n_d_only - n_b_only) / n,
      lower = p_d - p_b - qnorm(.975) * se, upper = p_d - p_b + qnorm(.975) * se)
  }, numeric(4)))
}

# The carryover candidate's held-out score minus that of each alternative, with
# its standard error.
score_table <- function(comparison) {
  scores <- as.data.frame(comparison$carry_contrasts)
  scores$against <- rownames(scores)
  rownames(scores) <- NULL
  scores[, c("against", "difference", "se")]
}

# The analyses of one design, which use only the recorded presentations. The
# comparisons are returned for the checks against the study and dropped before
# saving.
analyze <- function(observed, view) {
  recorded <- inference_view(observed, view)
  comparisons <- list(estimated = inference_compare(recorded, value = TRUE),
                      zero = inference_compare(recorded, value = FALSE))
  list(cols = view_cols[[view]],
       effect = inference_effect(recorded),
       by_presentation = as.data.frame(contrast_by_presentation(observed$y, view_cols[[view]])),
       scores = lapply(comparisons, score_table),
       decision = lapply(comparisons, `[[`, "decision"),
       first = if (1L %in% view_cols[[view]]) first_response_check(observed$y) else NULL,
       comparisons = comparisons)
}

cases <- list()
for (src in sources) for (n in sizes) {
  id <- first_in_config1(src, n)
  job <- jobs[jobs$id == id, ]
  setting <- generating[generating$scenario == src & generating$config == job$config, ]
  par <- unlist(setting[inference_names])
  generated <- inference_generate(job$n, par, setting$adaptive, job$seed)
  observed <- generated$observed
  truth_p <- generated$truth$probability

  designs <- lapply(views, function(view) analyze(observed, view))
  names(designs) <- views

  # Agreement with the study, wherever it saved the same analysis.
  for (view in views) for (policy in names(policies)) {
    row <- saved[saved$id == id & saved$view == view & saved$policy == policies[[policy]], ]
    if (!nrow(row)) next
    comparison <- designs[[view]]$comparisons[[policy]]
    effect <- designs[[view]]$effect
    stopifnot(nrow(row) == 1,
              abs(effect[["estimate"]] - row$estimate) < 1e-9,
              abs(effect[["lower"]] - row$lower) < 1e-9, abs(effect[["upper"]] - row$upper) < 1e-9,
              effect[["detected"]] == row$detected,
              abs(comparison$carry_contrasts["fixed", "difference"] - row$carry_vs_fixed) < 5e-5,
              abs(comparison$carry_contrasts["reset", "difference"] - row$carry_vs_reset) < 5e-5,
              identical(comparison$decision, row$decision))
  }
  # The study made the first-answer check for every experiment.
  first <- designs$full$first
  saved_first <- first_checks[first_checks$id == id, ]
  stopifnot(first$d_only == saved_first$discordant_control,
            first$b_only == saved_first$discordant_blocked,
            abs(first$p - saved_first$initial_p) < 1e-12)
  for (view in views) designs[[view]]$comparisons <- NULL

  # true_contrast is the generating D-minus-B difference in the probability of a
  # yes at each presentation, averaged over the participants.
  cases[[src]][[as.character(n)]] <- list(
    id = id, seed = job$seed, n = job$n, source = src, config = job$config,
    generating = as.list(par),
    y = observed$y, allocation = observed$allocation,
    true_contrast = colMeans(truth_p[, 11:20] - truth_p[, 1:10]),
    designs = designs)
  cat(sprintf("%s, %d participants: experiment %d analyzed under %d designs\n",
              src, n, id, length(views)))
}

# Rates across the study's 400 experiments per design with old-value carryover
# estimated; for 160 participants and all ten presentations, also the rates with
# it fixed at 0 and those of the first-answer check.
rates <- study[study$scenario %in% sources & study$policy == "value_allowed",
               c("scenario", "n", "view", "detected", "carry", "no_carry", "unresolved",
                 "repetitions", "fit_problem")]
rownames(rates) <- NULL
zero_rates <- do.call(rbind, lapply(c("associability", "value"), function(src) {
  restricted <- saved[saved$scenario == src & saved$n == 160 & saved$view == "full" &
                        saved$policy == "value_zero", ]
  data.frame(scenario = src, n = 160L, view = "full", repetitions = nrow(restricted),
             carry = mean(restricted$decision == "carry"))
}))

saveRDS(list(cases = cases, rates = rates, zero_rates = zero_rates, first_rates = first_summary),
        "shiny_app/data/inference_cases.rds")
cat("Saved shiny_app/data/inference_cases.rds\n")
