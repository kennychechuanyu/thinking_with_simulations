# Sections 5.2-5.4, Supplement S2: the worked example, one experiment of 80
# participants simulated with associability carryover (seed 8123, chosen before
# the result was seen). Computes the candidates' fits and held-out comparison
# (Figures 9 and S2), the profile likelihood of g_alpha (Figure S1b) and the
# mimicry fits (Figure S1a).
#   Rscript analysis/section5_worked_example.R

if (!file.exists("R/inference.R")) stop("Run this script from the repository root.")
source("R/inference.R")
source("R/figure_style.R")  # paper_number()

n_participants <- 80
example_seed <- 8123L
starts <- 48L
output <- "results/section5"
scenarios <- inference_scenarios()

# Section 5.2: the experiment, the three candidates fitted to all 80
# participants (Figures 9b and S2), their held-out comparison (Figure 9c) and
# the response contrast.
example <- inference_generate(n_participants, scenarios$associability$par, TRUE,
                              seed = example_seed)
example$fits <- list()
for (model in c("fixed", "reset", "carry")) {
  # The carryover fit also starts from the reset solution (Supplement S2).
  nested <- if (model == "carry") example$fits$reset$par else NULL
  example$fits[[model]] <- inference_fit(example$observed, inference_spec(model),
                                         starts = starts, extra_start = nested)
}
example$comparison <- inference_compare(example$observed, starts = starts)
example$effect <- inference_effect(example$observed)
saveRDS(example, file.path(output, "worked_example.rds"))
# Columns B1-B10 hold the answers to B's ten presentations, D1-D10 those to D's.
responses <- example$observed$y
colnames(responses) <- c(paste0("B", 1:10), paste0("D", 1:10))
write.csv(data.frame(participant = seq_len(n_participants),
                     allocation = example$observed$allocation, responses),
          file.path(output, "worked_example_responses.csv"), row.names = FALSE)

# Section 5.3: g_alpha fixed at 0, .1, ..., 1, with the other parameters of the
# carryover candidate re-estimated at each value.
profile <- lapply(seq(0, 1, by = .1), function(g_alpha) {
  spec <- inference_spec("carry")
  spec$free <- setdiff(spec$free, "g_alpha")
  criterion <- function(par) {
    par["g_alpha"] <- g_alpha
    -sum(inference_loglik_cpp(par, TRUE, example$observed$y, example$observed$allocation))
  }
  fit <- inference_fit(example$observed, spec, starts = starts, criterion = criterion)
  data.frame(g_alpha = g_alpha, nll = fit$nll, converged = fit$converged,
             eta = fit$par["eta"], g_value = fit$par["g_value"],
             mu = fit$par["mu"], slope = exp(fit$par["log_slope"]))
})
profile <- do.call(rbind, profile)
write.csv(profile, file.path(output, "carryover_profile.csv"), row.names = FALSE)

# Section 5.4: each candidate fitted directly to the generating probabilities
# of the associability and value scenarios, without rate types (tau = 0) and
# with both cue allocations. The criterion is the expected KL divergence of one
# participant's kept answers, averaged over the two allocations, which are
# rows 1 and 3 of the probability matrix when tau = 0 (Supplement S2).
divergence <- function(p, p_fit) {
  p_fit <- pmax(1e-12, pmin(1 - 1e-12, p_fit))
  p * log(p / p_fit) + (1 - p) * log((1 - p) / (1 - p_fit))
}
# inference_fit() takes a record, but a criterion replaces its likelihood; this
# four-participant template only marks the answers each view keeps.
template <- list(y = matrix(0L, 4L, 20L), allocation = c(-1L, -1L, 1L, 1L), n_train = 10L)
landscape <- list()
for (scenario in c("associability", "value")) {
  generator <- scenarios[[scenario]]
  generator$par["spread"] <- 0
  generating <- inference_prob_cpp(generator$par, generator$adaptive)[c(1L, 3L), ]
  for (view in c("late", "early", "full")) {
    kept <- which(!is.na(inference_view(template, view)$y[1, ]))
    for (model in c("fixed", "reset", "carry")) {
      spec <- inference_spec(model)
      spec$free <- setdiff(spec$free, "spread")
      criterion <- function(par) {
        fitted <- inference_prob_cpp(par, spec$adaptive)[c(1L, 3L), kept, drop = FALSE]
        sum(divergence(generating[, kept, drop = FALSE], fitted)) / 2
      }
      fit <- inference_fit(template, spec, starts = starts, criterion = criterion)
      # The divergence before refitting: the generating values, with eta and
      # g_alpha set to 0 for the fixed-rate candidate and g_alpha set to 0 for
      # the reset candidate.
      reference <- generator$par
      if (model == "fixed") reference[c("eta", "g_alpha")] <- 0
      if (model == "reset") reference["g_alpha"] <- 0
      # fit$nll is the divergence of the selected search, the best converged
      # one within 1e-5 of the smallest found (Supplement S2), so it can lie
      # slightly above the smallest. Below 1e-8 it counts as reaching the lower
      # bound of zero.
      landscape[[length(landscape) + 1L]] <- data.frame(
        scenario = scenario, view = view, model = model,
        reference_kl = criterion(reference), fitted_kl = fit$nll,
        optimizer_converged = fit$converged, kl_lower_bound_reached = fit$nll < 1e-8,
        as.list(fit$par))
    }
  }
}
landscape <- do.call(rbind, landscape)
write.csv(landscape, file.path(output, "landscape.csv"), row.names = FALSE)

mimicry <- function(view, model) {
  landscape[landscape$scenario == "associability" & landscape$view == view &
              landscape$model == model, ]
}
# Section 5.4: the reset candidate's divergence over the full record, answer by
# answer, and the share of it in the first two answers to each cue.
generator <- scenarios$associability
generator$par["spread"] <- 0
reset_par <- unlist(mimicry("full", "reset")[inference_names])
reset_by_answer <- colSums(divergence(inference_prob_cpp(generator$par, TRUE)[c(1L, 3L), ],
                                      inference_prob_cpp(reset_par, TRUE)[c(1L, 3L), ])) / 2
first_two_share <- sum(reset_by_answer[c(1, 2, 11, 12)]) / sum(reset_by_answer)
# Section 5.4: the generating D-B difference in response probability on
# presentations 8-10, averaged over the two allocations.
generating <- inference_prob_cpp(generator$par, TRUE)[c(1L, 3L), ]
late_gap <- colMeans(generating[, 18:20] - generating[, 8:10])
write.csv(data.frame(reset_first_two_share = first_two_share,
                     late_gap_min = min(late_gap), late_gap_max = max(late_gap)),
          file.path(output, "mimicry_summary.csv"), row.names = FALSE)

contrasts <- example$comparison$carry_contrasts
cat(sprintf("Response contrast %s, 95%% interval %s to %s\n",
            paper_number(example$effect[["estimate"]], 3),
            paper_number(example$effect[["lower"]], 3),
            paper_number(example$effect[["upper"]], 3)))
cat(sprintf("Old-value carryover g_V: fixed-rate %s, reset %s\n",
            paper_number(example$fits$fixed$par[["g_value"]], 2),
            paper_number(example$fits$reset$par[["g_value"]], 2)))
cat(sprintf("Held-out log score, carryover minus fixed-rate %.1f (SE %.1f),",
            contrasts["fixed", "difference"], contrasts["fixed", "se"]),
    sprintf("minus reset %.1f (SE %.1f)\n", contrasts["reset", "difference"],
            contrasts["reset", "se"]))
cat(sprintf("Profile likelihood of g_alpha highest at %g\n",
            profile$g_alpha[which.min(profile$nll)]))
cat(sprintf("KL divergence per participant, fixed-rate %.3f (%.3f before refitting), reset %.3f;",
            mimicry("full", "fixed")$fitted_kl, mimicry("full", "fixed")$reference_kl,
            mimicry("full", "reset")$fitted_kl),
    sprintf("reset on presentations 8-10 %.4f\n", mimicry("late", "reset")$fitted_kl))
cat(sprintf("Reset, full record: %s of its divergence lies in the first two answers to each cue\n",
            paper_number(first_two_share, 2)))
cat(sprintf("Generating D-B difference on presentations 8-10: %s to %s\n",
            paper_number(min(late_gap), 3), paper_number(max(late_gap), 3)))
cat("Saved results/section5/worked_example.rds, worked_example_responses.csv,",
    "carryover_profile.csv, landscape.csv and mimicry_summary.csv\n")

# Values reported in the paper
stopifnot(
  sprintf("%.3f", example$effect[c("estimate", "lower", "upper")]) ==
    c("0.212", "0.174", "0.251"),                                      # Section 5.2
  sprintf("%.2f", example$fits$fixed$par[["g_value"]]) == "0.70",      # Section 5.2
  sprintf("%.2f", example$fits$reset$par[["g_value"]]) == "0.50",
  sprintf("%.1f", contrasts[, "difference"]) == c("20.5", "21.1"),     # Section 5.2, Figure 9c
  sprintf("%.1f", contrasts[, "se"]) == c("5.9", "5.1"),
  example$comparison$decision == "carry",
  profile$g_alpha[which.min(profile$nll)] == 1,                        # Section 5.3
  max(profile$nll[profile$g_alpha >= .5]) - min(profile$nll) < .5,     # Section 5.3
  all(diff(profile$eta) < 1e-6),       # Section 5.3: less carryover, a faster estimated eta
  sprintf("%.3f", mimicry("full", "fixed")$reference_kl) == "1.417",   # Supplement S2
  sprintf("%.3f", mimicry("full", "fixed")$fitted_kl) == "0.119",      # Section 5.4
  sprintf("%.3f", mimicry("full", "reset")$fitted_kl) == "0.143",
  sprintf("%.4f", mimicry("late", "fixed")$fitted_kl) == "0.0019",
  sprintf("%.4f", mimicry("late", "reset")$fitted_kl) == "0.0015",
  # Section 5.4: on presentations 8-10 both alternatives carry over all old value
  c(mimicry("late", "fixed")$g_value, mimicry("late", "reset")$g_value) > .999,
  round(100 * first_two_share) == 41,                                  # Section 5.4
  sprintf("%.2f", range(late_gap)) == c("0.23", "0.33")                # Section 5.4
)
