# Section 5.6 and Supplement S2: two checks on the saved study. The exact paired
# test of equal first answers to B and D, for every experiment (Figure 11b), and
# refits of 24 experiments with wider parameter bounds. The experiments are
# regenerated from their seeds. The refits are run again with --refit, as
# run_all.R --full-study does, and are otherwise read from
# wider_bounds_refits.csv: like the study itself, a refit whose comparison lies
# near the decision margin can end differently on another system. About 2 s;
# with --refit about 40 s, or 15 s with --workers=4.
#   Rscript analysis/section5_first_response_and_bounds.R --refit --workers=4

if (!file.exists("R/inference.R")) stop("Run this script from the repository root.")
source("R/inference.R")

arguments <- commandArgs(trailingOnly = TRUE)
valid <- grepl("^--workers=[1-9][0-9]*$|^--refit$", arguments)
if (!all(valid)) {
  stop("Unrecognized argument ", arguments[!valid][1], "; the options are --refit and --workers=N.")
}
refit_bounds <- "--refit" %in% arguments
workers <- 1L
workers_option <- grep("^--workers=", arguments, value = TRUE)
if (length(workers_option)) workers <- as.integer(sub("^--workers=", "", workers_option[1]))
# parallel::mclapply() forks processes, which Windows does not support.
if (.Platform$OS.type == "windows") workers <- 1L

path <- "results/section5"
jobs <- read.csv(file.path(path, "jobs.csv"))
generating <- read.csv(file.path(path, "generating_parameters.csv"))
results <- read.csv(file.path(path, "results.csv"))

regenerate <- function(job) {
  setting <- generating[generating$scenario == job$scenario & generating$config == job$config, ]
  par <- setNames(as.numeric(setting[1, inference_names]), inference_names)
  inference_generate(job$n, par, setting$adaptive, job$seed)$observed
}

# Section 5.6: with g_V = 0 and one response function for B and D, every
# candidate gives B (column 1) and D (column 11) the same probability of a yes
# on their first presentations. Under that equality, the number of participants
# who answer yes to D only, among those whose two first answers differ, is
# binomial with probability .5. The exact one-sided test rejects equality at .05.
first_response <- do.call(rbind, lapply(seq_len(nrow(jobs)), function(i) {
  job <- jobs[i, ]
  y <- regenerate(job)$y
  d_only <- sum(y[, 11] > y[, 1])
  b_only <- sum(y[, 11] < y[, 1])
  discordant <- d_only + b_only
  p_value <- if (discordant == 0) 1 else pbinom(d_only - 1, discordant, .5, lower.tail = FALSE)
  cbind(job, initial_difference = mean(y[, 11] - y[, 1]), discordant_control = d_only,
        discordant_blocked = b_only, initial_p = p_value, initial_rejection = p_value < .05)
}))
write.csv(first_response, file.path(path, "initial_response_checks.csv"), row.names = FALSE)

# Figure 11: the comparisons with g_V fixed at 0 (N = 160, full records) and the
# test's rejection rate in the same experiments. Under carried associability
# the restriction holds, so its rejections are false alarms.
restricted <- merge(results[results$policy == "value_zero", ],
                    first_response[c("id", "initial_rejection")], by = "id")
by_scenario <- split(restricted, restricted$scenario)
first_response_summary <- do.call(rbind, lapply(by_scenario, function(group) {
  carry <- group$decision == "carry"
  data.frame(scenario = group$scenario[1], repetitions = nrow(group), carry_preferred = mean(carry),
             initial_rejection = mean(group$initial_rejection),
             carry_and_rejection = mean(carry & group$initial_rejection),
             rejection_given_carry = mean(group$initial_rejection[carry]))
}))
write.csv(first_response_summary, file.path(path, "initial_response_summary.csv"),
          row.names = FALSE)

# Supplement S2: 24 experiments at N = 160, repetitions 1-8 (each configuration
# twice) of the associability, value and null scenarios, refitted on their full
# records with wider bounds for mu, tau, d, b, gamma and eta.
wider_bounds <- inference_bounds
wider_bounds["lower", c("mu", "cue", "intercept", "log_slope")] <- c(-4.5, -1, -12, log(.75))
wider_bounds["upper", c("mu", "spread", "cue", "intercept", "log_slope", "eta")] <-
  c(1.5, 1.5, 1, 4, log(32), .3)
refit_jobs <- jobs[jobs$n == 160 & jobs$rep <= 8 &
                     jobs$scenario %in% c("associability", "value", "null"), ]

# inference_fit() reads the global inference_bounds, which is widened only
# while a refit runs.
refit <- function(i) {
  job <- refit_jobs[i, ]
  default_bounds <- inference_bounds
  on.exit(inference_bounds <<- default_bounds)
  inference_bounds <<- wider_bounds
  comparison <- inference_compare(regenerate(job))
  saved <- results[results$id == job$id & results$view == "full" &
                     results$policy == "value_allowed", ]
  cbind(job, original_decision = saved$decision, wider_decision = comparison$decision,
        original_winner = saved$winner, wider_winner = comparison$winner,
        wider_fixed_score = comparison$scores["fixed"],
        wider_reset_score = comparison$scores["reset"],
        wider_carry_score = comparison$scores["carry"])
}
refit_file <- file.path(path, "wider_bounds_refits.csv")
if (!file.exists(refit_file)) refit_bounds <- TRUE
if (refit_bounds) {
  refits <- parallel::mclapply(seq_len(nrow(refit_jobs)), refit, mc.cores = workers,
                               mc.set.seed = FALSE)
  # A worker returns an error object, or NULL if it died, instead of the row.
  if (!all(vapply(refits, is.data.frame, logical(1)))) stop("A refit failed in a worker.")
  refits <- do.call(rbind, refits)
  write.csv(refits, refit_file, row.names = FALSE)
} else {
  refits <- read.csv(refit_file)
}

percent <- function(x) sprintf("%.1f", 100 * x)
associability_row <- first_response_summary[first_response_summary$scenario == "associability", ]
value_row <- first_response_summary[first_response_summary$scenario == "value", ]
changed <- refits[refits$original_decision != refits$wider_decision, ]
cat(sprintf("g_V fixed at 0, carryover favored: carried associability %s%%,",
            percent(associability_row$carry_preferred)),
    sprintf("value generalization %s%%\n", percent(value_row$carry_preferred)))
cat(sprintf("First answers, equality rejected: carried associability %s%%,",
            percent(associability_row$initial_rejection)),
    sprintf("value generalization %s%% (%s%% where carryover wrongly wins)\n",
            percent(value_row$initial_rejection), percent(value_row$rejection_given_carry)))
cat(sprintf("Wider bounds: the decision changes in %d of %d experiments\n",
            nrow(changed), nrow(refits)))
cat("Saved results/section5/initial_response_checks.csv and initial_response_summary.csv\n")
cat(if (refit_bounds) "Saved" else "Read", "results/section5/wider_bounds_refits.csv\n")

# Values reported in the paper
stopifnot(
  percent(associability_row$carry_preferred) == "96.8",       # Section 5.6, Figure 11a
  percent(value_row$carry_preferred) == "95.0",
  percent(associability_row$initial_rejection) == "1.5",      # Section 5.6, Figure 11b
  percent(value_row$initial_rejection) == "94.8",
  percent(value_row$rejection_given_carry) == "95.3",
  nrow(refits) == 24,                                         # Supplement S2
  nrow(changed) == 0
)
