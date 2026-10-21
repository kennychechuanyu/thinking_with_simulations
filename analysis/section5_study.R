# Sections 5.5 and 5.6, Supplement S2: the simulation study. 3,600 simulated
# experiments, each analyzed with two-fold held-out comparisons of the three
# candidates (67,200 fits). About 5 CPU-hours: 45 minutes on 8 workers.
#   Rscript analysis/section5_study.R --workers=8
# --jobs=10,17,2808 (or a range such as 1-16) reruns only those experiments, in
# results/section5/subset/, and compares them with the saved rows.
# --pilot runs the study at the pilot's settings (Supplement S2) in results/section5/pilot/.

if (!file.exists("R/inference.R")) stop("Run this script from the repository root.")
source("R/inference.R")

arguments <- commandArgs(trailingOnly = TRUE)
valid <- grepl("^--workers=[1-9][0-9]*$|^--pilot$|^--jobs=[0-9]+(-[0-9]+)?(,[0-9]+(-[0-9]+)?)*$",
               arguments)
if (!all(valid)) {
  stop("Unrecognized argument ", arguments[!valid][1],
       "; the options are --workers=N, --jobs=10,17,2808 or --jobs=1-16, and --pilot.")
}
pilot <- "--pilot" %in% arguments
jobs_option <- grep("^--jobs=", arguments, value = TRUE)
rerun <- length(jobs_option) > 0
if (pilot && rerun) stop("--jobs reruns experiments of the main study; use it without --pilot.")
workers <- 1L
workers_option <- grep("^--workers=", arguments, value = TRUE)
if (length(workers_option)) workers <- as.integer(sub("^--workers=", "", workers_option[1]))
# parallel::mclapply() forks processes, which Windows does not support.
if (.Platform$OS.type == "windows") workers <- 1L

# The design of Supplement S2: 400 repetitions, with 48 starting vectors for
# each fit. --pilot runs four repetitions at N = 80, base configuration only,
# with 12 starting vectors and the pilot's range of seeds.
repetitions <- if (pilot) 4L else 400L
sizes <- if (pilot) 80L else c(40L, 160L)
starts <- if (pilot) 12L else 48L
first_seed <- if (pilot) 510000L else 610000L

# One row per experiment. The order fixes each experiment's id and its seed,
# first_seed + id: the associability, value and null scenarios at each N, the
# response scenario at the largest N, then transfer-only adaptation (reset) at
# each N. Configurations 1-4 take turns over the repetitions, so each
# scenario-by-N cell has the same number of experiments in each configuration.
jobs <- rbind(
  expand.grid(scenario = c("associability", "value", "null"), n = sizes,
              rep = seq_len(repetitions), stringsAsFactors = FALSE),
  data.frame(scenario = "response", n = max(sizes), rep = seq_len(repetitions)),
  expand.grid(scenario = "reset", n = sizes, rep = seq_len(repetitions),
              stringsAsFactors = FALSE)
)
jobs$id <- seq_len(nrow(jobs))
jobs$config <- if (pilot) 1L else (jobs$rep - 1L) %% 4L + 1L
jobs$seed <- first_seed + jobs$id

selected <- jobs$id
if (rerun) {
  ranges <- strsplit(strsplit(sub("^--jobs=", "", jobs_option[1]), ",")[[1]], "-")
  selected <- sort(unique(unlist(lapply(ranges, function(ends) {
    seq(as.integer(ends[1]), as.integer(ends[length(ends)]))
  }))))
  if (!all(selected %in% jobs$id)) stop("Experiment ids run from 1 to ", nrow(jobs), ".")
}

# The four configurations of Supplement S2. Configuration 1, the base
# configuration, is each scenario as inference_scenarios() defines it.
generating_parameters <- function(scenario, config) {
  generator <- inference_scenarios()[[scenario]]
  # Slower and more heterogeneous learning
  if (config == 2L) generator$par[c("mu", "spread")] <- c(qlogis(.24), .5)
  # A less steep response function: b = -3, gamma = 6
  if (config == 3L) generator$par[c("intercept", "log_slope")] <- c(-3, log(6))
  # Reversed physical-cue contrast (d = -0.30) and a weaker version of the
  # scenario's own process; the null scenario changes only d
  if (config == 4L) {
    generator$par["cue"] <- -.3
    if (scenario == "associability") generator$par[c("eta", "g_alpha")] <- c(.035, .6)
    if (scenario == "reset") generator$par["eta"] <- .035
    if (scenario == "value") generator$par["g_value"] <- .45
    if (scenario == "response") generator$par["history_intercept"] <- -.6
  }
  generator
}

generating <- list()
for (scenario in c("associability", "reset", "value", "null", "response")) {
  for (config in if (pilot) 1L else 1:4) {
    generator <- generating_parameters(scenario, config)
    generating[[length(generating) + 1L]] <- data.frame(
      scenario = scenario, config = config, adaptive = generator$adaptive,
      as.list(generator$par))
  }
}
generating <- do.call(rbind, generating)

output <- "results/section5"
if (pilot) output <- "results/section5/pilot"
if (rerun) output <- "results/section5/subset"
checkpoints <- file.path(output, "jobs")
dir.create(checkpoints, recursive = TRUE, showWarnings = FALSE)

# Generates one experiment and analyzes it. The analyses see only the answers,
# the cue allocation and the training schedule. Each analysis adds one row to
# results, and each of its six fits (three candidates, two folds) one row to
# diagnostics and parameters. The tables are also saved as a checkpoint.
run_experiment <- function(id) {
  job <- jobs[id, ]
  generator <- generating_parameters(job$scenario, job$config)
  observed <- inference_generate(job$n, generator$par, generator$adaptive, job$seed)$observed
  # Response probabilities of the two rate types in each cue allocation;
  # columns 1-10 are B's presentations and columns 11-20 D's.
  probability <- inference_prob_cpp(generator$par, generator$adaptive)

  # The late, early and full views with g_V estimated. The response scenario is
  # analyzed in the full view only, with b_hist fixed at 0 and with b_hist
  # estimated. At the largest N, the associability and value records are also
  # analyzed with g_V fixed at 0 (Section 5.6).
  analyses <- if (job$scenario == "response") {
    data.frame(view = "full", policy = c("value_allowed", "response_allowed"))
  } else {
    data.frame(view = c("late", "early", "full"), policy = "value_allowed")
  }
  if (job$n == max(sizes) && job$scenario %in% c("associability", "value")) {
    analyses <- rbind(analyses, data.frame(view = "full", policy = "value_zero"))
  }

  results <- diagnostics <- parameters <- list()
  for (a in seq_len(nrow(analyses))) {
    analysis <- analyses[a, ]
    records <- inference_view(observed, analysis$view)
    effect <- inference_effect(records)
    comparison <- inference_compare(records, value = analysis$policy != "value_zero",
                                    response = analysis$policy == "response_allowed",
                                    starts = starts)
    # The quantity the response contrast estimates: the expected D-minus-B
    # difference in the kept presentations, over both rate types and both
    # cue allocations.
    kept <- which(!is.na(records$y[1, 1:10]))
    true_effect <- mean(probability[, 10L + kept] - probability[, kept])
    results[[a]] <- cbind(job, analysis, as.data.frame(as.list(effect)),
                          true_effect = true_effect, winner = comparison$winner,
                          decision = comparison$decision,
                          model_decision = comparison$model_decision,
                          fixed_score = comparison$scores["fixed"],
                          reset_score = comparison$scores["reset"],
                          carry_score = comparison$scores["carry"],
                          carry_vs_fixed = comparison$carry_contrasts["fixed", "difference"],
                          carry_vs_reset = comparison$carry_contrasts["reset", "difference"],
                          carry_vs_fixed_se = comparison$carry_contrasts["fixed", "se"],
                          carry_vs_reset_se = comparison$carry_contrasts["reset", "se"])
    for (name in names(comparison$fits)) {
      fit <- comparison$fits[[name]]
      diagnostics[[length(diagnostics) + 1L]] <- data.frame(
        id = job$id, view = analysis$view, policy = analysis$policy, fit = name,
        nll = fit$nll, converged = fit$converged, code = fit$code, starts_ok = fit$starts_ok,
        n_starts = length(fit$start_nll), start_range = diff(range(fit$start_nll)),
        refined = fit$refined, boundary = fit$boundary, message = fit$message)
      parameters[[length(parameters) + 1L]] <- data.frame(
        id = job$id, view = analysis$view, policy = analysis$policy, fit = name,
        as.list(fit$par))
    }
  }
  tables <- list(results = do.call(rbind, results), diagnostics = do.call(rbind, diagnostics),
                 parameters = do.call(rbind, parameters))
  saveRDS(tables, file.path(checkpoints, sprintf("job_%04d.rds", job$id)))
  tables
}

# Each experiment sets its own seed, so the results do not depend on the number
# of workers or on the batches, which only pace the progress report.
started <- proc.time()[["elapsed"]]
done <- 0L
for (batch in split(selected, ceiling(seq_along(selected) / (8L * workers)))) {
  finished <- parallel::mclapply(batch, run_experiment, mc.cores = workers,
                                 mc.set.seed = FALSE)
  # A worker returns an error object, or NULL if it died, instead of the tables.
  if (!all(vapply(finished, is.list, logical(1)))) stop("An experiment failed in a worker.")
  done <- done + length(batch)
  cat(sprintf("%d of %d experiments, %.0f s\n", done, length(selected),
              proc.time()[["elapsed"]] - started))
}

# The tables are put together from the checkpoints of exactly these experiments.
stored <- lapply(file.path(checkpoints, sprintf("job_%04d.rds", selected)), readRDS)
tables <- list()
for (name in c("results", "diagnostics", "parameters")) {
  tables[[name]] <- do.call(rbind, lapply(stored, `[[`, name))
  write.csv(tables[[name]], file.path(output, paste0(name, ".csv")), row.names = FALSE)
}

if (rerun) {
  # Fields are compared as written in the CSV files. SEs reconstructed from the
  # saved parameter CSV can differ in their last digits from a fresh fit's SEs.
  # Fitted numbers can also differ across systems, and a comparison near the
  # decision margin can end differently.
  for (name in names(tables)) {
    saved <- read.csv(file.path("results/section5", paste0(name, ".csv")),
                      colClasses = "character")
    saved <- saved[as.integer(saved$id) %in% selected, ]
    rownames(saved) <- NULL
    rerun_rows <- read.csv(file.path(output, paste0(name, ".csv")), colClasses = "character")
    numbers <- vapply(saved, function(x) !anyNA(suppressWarnings(as.numeric(x))), logical(1))
    largest <- max(0, abs(as.numeric(as.matrix(saved[numbers])) -
                            as.numeric(as.matrix(rerun_rows[numbers]))))
    saved_labels <- as.matrix(saved[!numbers])
    rerun_labels <- as.matrix(rerun_rows[!numbers])
    labels_differ <- sum(saved_labels != rerun_labels, na.rm = TRUE) +
      sum(is.na(saved_labels) != is.na(rerun_labels))
    cat(sprintf("%s.csv: %d rows, %s\n", name, nrow(rerun_rows),
                if (identical(saved, rerun_rows)) "equal to the saved rows"
                else if (labels_differ == 0)
                  sprintf("numbers within %.2g of the saved rows", largest)
                else sprintf("%d %s from the saved rows", labels_differ,
                             if (labels_differ == 1) "label or decision differs"
                             else "labels or decisions differ")))
  }
} else {
  write.csv(jobs, file.path(output, "jobs.csv"), row.names = FALSE)
  write.csv(generating, file.path(output, "generating_parameters.csv"), row.names = FALSE)
  sources <- c("R/models.R", "R/inference.R", "R/inference.cpp", "analysis/section5_study.R")
  writeLines(c(paste("study:", if (pilot) "pilot" else "main"),
               paste("experiments:", nrow(jobs)),
               paste("repetitions:", repetitions),
               paste("starts per fit:", starts),
               paste("workers:", workers),
               paste("R:", R.version.string),
               paste("platform:", R.version$platform),
               paste("Rcpp:", packageVersion("Rcpp")),
               paste0("md5 ", sources, ": ", tools::md5sum(sources))),
             file.path(output, "manifest.txt"))
  cat(sprintf("Saved %s: results.csv (%d analyses); diagnostics.csv and parameters.csv",
              output, nrow(tables$results)),
      sprintf("(%d fits); jobs.csv, generating_parameters.csv and manifest.txt\n",
              nrow(tables$diagnostics)))
}

# Values reported in the paper. A --jobs run reports its comparison with the
# saved rows above instead of counting the fits.
if (!pilot) {
  per_configuration <- table(jobs$scenario, jobs$n, jobs$config)
  main <- c("associability", "reset", "value", "null")
  stopifnot(
    nrow(jobs) == 3600,                                      # Supplement S2
    per_configuration[main, , ] == 100,                      # Section 5.5, Supplement S2
    per_configuration["response", "160", ] == 100,           # Supplement S2
    per_configuration["response", "40", ] == 0,
    rerun || nrow(tables$diagnostics) == 67200               # Supplement S2
  )
}
