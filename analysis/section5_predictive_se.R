# Checks the standard errors of the two predictive comparisons that the
# decision rule of Section 5.2 uses (carry_vs_fixed_se and carry_vs_reset_se in
# results.csv) without fitting again. The answers are regenerated from their
# seeds and scored at the saved fitted parameters; the script stops if the
# held-out scores, the standard errors or the decisions differ from those in
# results.csv. About ten seconds.
#   Rscript analysis/section5_predictive_se.R

if (!file.exists("R/inference.R")) stop("Run this script from the repository root.")
source("R/inference.R")
path <- "results/section5"
results <- read.csv(file.path(path, "results.csv"))
jobs <- read.csv(file.path(path, "jobs.csv"))
generating <- read.csv(file.path(path, "generating_parameters.csv"))
parameters <- read.csv(file.path(path, "parameters.csv"))
diagnostics <- read.csv(file.path(path, "diagnostics.csv"))
se_columns <- c("carry_vs_fixed_se", "carry_vs_reset_se")
key <- function(x) do.call(paste, c(x[c("id", "view", "policy")], sep = "|"))
stopifnot(nrow(jobs) == 3600L, nrow(results) == 11200L, nrow(parameters) == 67200L,
          nrow(diagnostics) == 67200L, !anyDuplicated(jobs$id), !anyDuplicated(key(results)))
parameters_by_analysis <- split(parameters, key(parameters))
fit_problem <- tapply(!diagnostics$converged, key(diagnostics), any)
stopifnot(setequal(names(parameters_by_analysis), key(results)),
          setequal(names(fit_problem), key(results)), !anyNA(fit_problem))
rows_by_id <- split(seq_len(nrow(results)), results$id)
stopifnot(setequal(names(rows_by_id), as.character(jobs$id)))

# This is the primary carry/no_carry/unresolved decision, not model_decision,
# which can also require the fixed-rate versus reset score difference's SE.
decide <- function(difference, se, failed) {
  if (failed) "fit_problem"
  else if (all(difference > 2 * se)) "carry"
  else if (any(difference < -2 * se)) "no_carry"
  else "unresolved"
}
recomputed <- matrix(NA_real_, nrow(results), 2L, dimnames = list(NULL, se_columns))
max_score_error <- max_difference_error <- 0
decision_mismatches <- 0L
for (j in seq_len(nrow(jobs))) {
  job <- jobs[j, ]
  g <- generating[generating$scenario == job$scenario & generating$config == job$config, ]
  stopifnot(nrow(g) == 1L)
  observed <- inference_generate(job$n, unlist(g[inference_names]), g$adaptive, job$seed)$observed
  fold <- inference_folds(observed)
  for (i in rows_by_id[[as.character(job$id)]]) {
    row <- results[i, ]
    row_key <- key(row)
    retained <- inference_view(observed, row$view)
    saved <- parameters_by_analysis[[row_key]]
    stopifnot(nrow(saved) == 6L,
              setequal(saved$fit, outer(c("fixed", "reset", "carry"), 1:2, paste, sep = "_")))
    ll <- matrix(NA_real_, job$n, 3L, dimnames = list(NULL, c("fixed", "reset", "carry")))
    for (model in colnames(ll)) for (f in 1:2) {
      held_out <- fold == f
      par <- unlist(saved[saved$fit == paste0(model, "_", f), inference_names])
      ll[held_out, model] <- inference_loglik_cpp(par, model != "fixed",
        retained$y[held_out, , drop = FALSE], retained$allocation[held_out], retained$n_train)
    }
    max_score_error <- max(max_score_error, abs(colSums(ll) -
      unlist(row[c("fixed_score", "reset_score", "carry_score")])))
    differences <- ll[, "carry"] - ll[, c("fixed", "reset")]
    stored_differences <- unlist(row[c("carry_vs_fixed", "carry_vs_reset")])
    max_difference_error <- max(max_difference_error, abs(colSums(differences) - stored_differences))
    recomputed[i, ] <- sqrt(job$n) * apply(differences, 2L, sd)
    decision_mismatches <- decision_mismatches +
      (decide(stored_differences, recomputed[i, ], fit_problem[[row_key]]) != row$decision)
  }
}
stopifnot(all(is.finite(recomputed)),
          max_score_error < 1e-7, max_difference_error < 1e-7, decision_mismatches == 0L)
cat(sprintf("Recomputed the standard errors for %d analyses without fitting.\n", nrow(results)))
cat(sprintf(paste("Largest difference from the saved scores %.3g and from the saved",
                  "score differences %.3g; decisions that differ: %d.\n"),
            max_score_error, max_difference_error, decision_mismatches))

stored_se <- as.matrix(results[se_columns])
max_se_error <- max(abs(stored_se - recomputed))
direct_decisions <- vapply(seq_len(nrow(results)), function(i) {
  decide(unlist(results[i, c("carry_vs_fixed", "carry_vs_reset")]), stored_se[i, ],
         fit_problem[[key(results[i, ])]])
}, "")
stopifnot(all(is.finite(stored_se)), max_se_error < 1e-7,
          identical(unname(direct_decisions), results$decision))
cat(sprintf("The saved standard errors agree within %.3g, and so do the decisions.\n", max_se_error))
