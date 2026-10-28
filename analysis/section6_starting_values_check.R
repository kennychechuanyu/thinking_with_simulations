# Supplement S2.7: how many starting vectors the fits need. Simulates eight
# experiments for each main generating process and N, and fits each candidate
# to the participants outside fold 1, as the study does before scoring fold 1.
# The study's procedure (48 starting vectors, with the Nelder-Mead refinement
# when needed) and the same procedure with only the four fixed vectors are
# compared with a reference: the better of the study's fit and the best of 200
# further searches from random starts within the bounds. Writes
# results/section6/starting_values_check.csv. About 1 minute on 8 workers.
#   Rscript analysis/section6_starting_values_check.R --workers=8

if (!file.exists("R/inference.R")) stop("Run this script from the repository root.")
source("R/inference.R")

arguments <- commandArgs(trailingOnly = TRUE)
valid <- grepl("^--workers=[1-9][0-9]*$", arguments)
if (!all(valid)) stop("Unrecognized argument ", arguments[!valid][1], "; the option is --workers=N.")
workers <- 1L
if (length(arguments)) workers <- as.integer(sub("^--workers=", "", arguments[1]))
# parallel::mclapply() forks processes, which Windows does not support.
if (.Platform$OS.type == "windows") workers <- 1L

# The smallest negative log likelihood that nlminb reaches from 200 random
# starts within the bounds.
random_search <- function(observed, spec) {
  free <- spec$free
  lower <- inference_bounds["lower", free]
  upper <- inference_bounds["upper", free]
  objective <- function(x) {
    par <- setNames(numeric(9), inference_names)
    par[free] <- x
    value <- -sum(inference_loglik_cpp(par, spec$adaptive, observed$y, observed$allocation,
                                       observed$n_train))
    if (is.finite(value)) value else 1e100
  }
  control <- list(iter.max = 600L, eval.max = 1200L, rel.tol = 1e-8, x.tol = 1e-7)
  set.seed(4800L)
  starts <- lapply(1:200, function(i) lower + runif(length(free)) * (upper - lower))
  min(vapply(starts, function(x) tryCatch(
    nlminb(x, objective, lower = lower, upper = upper, control = control)$objective,
    error = function(e) Inf), numeric(1)))
}

cases <- expand.grid(scenario = c("associability", "reset", "value", "null"), n = c(40L, 160L),
                     rep = 1:8, stringsAsFactors = FALSE)
cases$seed <- 991000L + seq_len(nrow(cases))
run_case <- function(i) {
  case <- cases[i, ]
  generator <- inference_scenarios()[[case$scenario]]
  observed <- inference_generate(case$n, generator$par, generator$adaptive, case$seed)$observed
  half <- inference_subset(observed, which(inference_folds(observed) != 1L))
  rows <- list()
  reset <- list()
  for (model in c("fixed", "reset", "carry")) {
    spec <- inference_spec(model)
    fits <- lapply(c(study = 48L, fixed = 4L), function(starts) {
      inference_fit(half, spec, starts = starts,
                    extra_start = if (model == "carry") reset[[as.character(starts)]]$par)
    })
    if (model == "reset") reset <- list(`48` = fits$study, `4` = fits$fixed)
    rows[[model]] <- data.frame(case = i, case[c("scenario", "n", "seed")], candidate = model,
      study_nll = fits$study$nll, study_converged = fits$study$converged,
      study_refined = fits$study$refined, fixed_nll = fits$fixed$nll,
      fixed_converged = fits$fixed$converged,
      reference_nll = min(fits$study$nll, random_search(half, spec)))
  }
  do.call(rbind, rows)
}
started <- proc.time()[["elapsed"]]
finished <- parallel::mclapply(seq_len(nrow(cases)), run_case, mc.cores = workers,
                               mc.set.seed = FALSE)
if (!all(vapply(finished, is.data.frame, logical(1)))) stop("A case failed in a worker.")
check <- do.call(rbind, finished)
rownames(check) <- NULL
write.csv(check, "results/section6/starting_values_check.csv", row.names = FALSE)

shortfall_study <- check$study_nll - check$reference_nll
shortfall_fixed <- check$fixed_nll - check$reference_nll
cat(sprintf("%d fits to half-samples, %.0f s\n", nrow(check), proc.time()[["elapsed"]] - started))
cat(sprintf("48 starting vectors: largest shortfall from the reference %.2g; %d fits converged,",
            max(shortfall_study), sum(check$study_converged)),
    sprintf("%d of them after the Nelder-Mead refinement\n", sum(check$study_refined)))
cat(sprintf("Four fixed vectors alone: short by more than .001 in %d fits, by more than .1 in %d\n",
            sum(shortfall_fixed > .001), sum(shortfall_fixed > .1)))
cat("Saved results/section6/starting_values_check.csv\n")

# Values reported in Supplement S2.7
stopifnot(
  nrow(check) == 192,
  all(check$study_converged),
  max(shortfall_study) < 1e-5,
  sum(shortfall_fixed > .001) == 8
)
