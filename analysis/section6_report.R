# Sections 6 and 7, Supplement S2: the numbers that the paper reports from the
# saved results of the simulation study and the worked example. Writes the rates
# behind Figure 8 and Tables 9 and S2 with Monte Carlo standard errors
# (summary.csv) and for each parameter set (summary_by_configuration.csv), the
# numbers quoted in the text (section6_numbers.tex), Table 9 (summary_rates.tex)
# and Table S2 (supplement_table_s2.tex), and checks them against the paper.
#   Rscript analysis/section6_report.R

if (!file.exists("R/inference.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")  # paper_number()
source("R/inference.R")     # inference_bounds and inference_prob_cpp; sources R/models.R
path <- "results/section6"
checks <- c("initial_response_checks.csv", "initial_response_summary.csv",
            "wider_bounds_refits.csv")
if (!all(file.exists(file.path(path, checks)))) {
  stop("Run analysis/section7_first_response_and_bounds.R first; it writes ",
       "initial_response_checks.csv, initial_response_summary.csv and wider_bounds_refits.csv.")
}

results <- read.csv(file.path(path, "results.csv"))
diagnostics <- read.csv(file.path(path, "diagnostics.csv"))
parameters <- read.csv(file.path(path, "parameters.csv"))
jobs <- read.csv(file.path(path, "jobs.csv"))
generating <- read.csv(file.path(path, "generating_parameters.csv"))
example <- readRDS(file.path(path, "worked_example.rds"))
landscape <- read.csv(file.path(path, "landscape.csv"))
profile <- read.csv(file.path(path, "carryover_profile.csv"))
mimicry_summary <- read.csv(file.path(path, "mimicry_summary.csv"))
first_response <- read.csv(file.path(path, "initial_response_checks.csv"))
first_response_summary <- read.csv(file.path(path, "initial_response_summary.csv"))
bounds_refits <- read.csv(file.path(path, "wider_bounds_refits.csv"))
starting_values <- read.csv(file.path(path, "starting_values_check.csv"))

# Rates for each generating process (scenario), N, set of pear trials analyzed
# (view) and analysis (policy). The four parameter sets (config) have fixed
# weights of .25, so the Monte Carlo variance of a rate is computed within each
# parameter set and combined with these weights (Supplement S2.9).
cells <- split(results, interaction(results$scenario, results$n, results$view,
                                     results$policy, drop = TRUE))
summary_table <- do.call(rbind, lapply(cells, function(cell) {
  # forced_correct: the candidate matching the generating process has the
  # highest held-out score, without the margin. It is undefined when the
  # analysis leaves out the generating process.
  truth_included <- !(cell$scenario[1] == "value" && cell$policy[1] == "value_zero") &&
    !(cell$scenario[1] == "response" && cell$policy[1] != "response_allowed")
  family <- switch(cell$scenario[1], associability = "carry", reset = "reset", "fixed")
  indicators <- cbind(detected = cell$detected, carry = cell$decision == "carry",
                      no_carry = cell$decision == "no_carry",
                      unresolved = cell$decision == "unresolved",
                      fit_problem = cell$decision == "fit_problem",
                      forced_correct = if (truth_included) cell$winner == family else NA,
                      coverage = cell$lower <= cell$true_effect & cell$upper >= cell$true_effect)
  rates <- colMeans(indicators)
  variance <- lapply(split(seq_len(nrow(cell)), cell$config), function(rows) {
    rate <- colMeans(indicators[rows, , drop = FALSE])
    (length(rows) / nrow(cell))^2 * rate * (1 - rate) / length(rows)
  })
  mcse <- sqrt(Reduce("+", variance))
  names(mcse) <- paste0(names(rates), "_mcse")
  cbind(cell[1, c("scenario", "n", "view", "policy")], repetitions = nrow(cell),
        as.data.frame(as.list(c(rates, mcse))), true_effect = mean(cell$true_effect),
        effect_bias = mean(cell$estimate - cell$true_effect),
        effect_rmse = sqrt(mean((cell$estimate - cell$true_effect)^2)))
}))
rownames(summary_table) <- NULL
write.csv(summary_table, file.path(path, "summary.csv"), row.names = FALSE)

# Detection, carryover, unresolved and fit-problem rates within each parameter set
set_cells <- split(results, interaction(results$scenario, results$n, results$view,
                                        results$policy, results$config, drop = TRUE))
summary_by_set <- do.call(rbind, lapply(set_cells, function(cell) {
  cbind(cell[1, c("scenario", "n", "view", "policy", "config")], repetitions = nrow(cell),
        detected = mean(cell$detected), carry = mean(cell$decision == "carry"),
        unresolved = mean(cell$decision == "unresolved"),
        fit_problem = mean(cell$decision == "fit_problem"))
}))
write.csv(summary_by_set, file.path(path, "summary_by_configuration.csv"), row.names = FALSE)

# A rate of summary_table in percent, as the paper prints it; by default with
# N = 160 and g_V estimated.
percent_of <- function(scenario, view, metric, n = 160L, policy = "value_allowed") {
  row <- summary_table[summary_table$scenario == scenario & summary_table$view == view &
                         summary_table$n == n & summary_table$policy == policy, ]
  sprintf("%.1f", 100 * row[[metric]])
}

macros <- c(
  StudyReplications = max(summary_table$repetitions),
  StudyExperiments = format(nrow(jobs), big.mark = ","),
  StudyFits = format(nrow(diagnostics), big.mark = ","),
  StudyFailedFits = sum(!diagnostics$converged),
  StudyRefinedFits = format(sum(diagnostics$refined), big.mark = ","),
  StudyProblemAnalyses = sum(results$decision == "fit_problem"),
  StudyCoverageMin = sprintf("%.1f", 100 * min(summary_table$coverage)),
  StudyCoverageMax = sprintf("%.1f", 100 * max(summary_table$coverage)),
  # The largest Monte Carlo standard error a rate can have, .5 / sqrt(400)
  # (Supplement S2.9)
  StudyMaxMCSE = sprintf("%.1f", 100 * .5 / sqrt(max(summary_table$repetitions))),
  # The B-D difference is a difference of proportions, so it is written without
  # a leading zero.
  ExampleContrast = paper_number(example$effect[["estimate"]], 3),
  ExampleLower = paper_number(example$effect[["lower"]], 3),
  ExampleUpper = paper_number(example$effect[["upper"]], 3)
)
# The worked example of Sections 6.2 and 6.3
contrasts <- example$comparison$carry_contrasts
macros["ExampleFixedValueCarry"] <- paper_number(example$fits$fixed$par["g_value"], 2)
macros["ExampleResetValueCarry"] <- paper_number(example$fits$reset$par["g_value"], 2)
macros["ExampleCarryVsFixed"] <- sprintf("%.1f", contrasts["fixed", "difference"])
macros["ExampleCarryVsFixedSE"] <- sprintf("%.1f", contrasts["fixed", "se"])
macros["ExampleCarryVsReset"] <- sprintf("%.1f", contrasts["reset", "difference"])
macros["ExampleCarryVsResetSE"] <- sprintf("%.1f", contrasts["reset", "se"])
# Section 6.5: the range of the carryover rate over the four parameter sets
associability_rates <- summary_by_set[summary_by_set$scenario == "associability" &
                                        summary_by_set$n == 160 &
                                        summary_by_set$view == "full" &
                                        summary_by_set$policy == "value_allowed", ]
macros["AssocConfigurationMin"] <- sprintf("%.1f", 100 * min(associability_rates$carry))
macros["AssocConfigurationMax"] <- sprintf("%.1f", 100 * max(associability_rates$carry))
# Sections 6.5 and 7.3: ranking first without the margin against favoring
# associability carryover, with N = 160, all ten pear trials and g_V estimated
# in every candidate.
full_large_results <- results[results$n == 160 & results$view == "full" &
                                results$policy == "value_allowed", ]
large_full <- split(full_large_results, full_large_results$scenario)
stopifnot(all(vapply(large_full, nrow, 0L) == as.integer(macros["StudyReplications"])))
ranked_carry <- vapply(large_full, function(cell) sum(cell$winner == "carry"), 0L)
resolved_carry <- vapply(large_full, function(cell) sum(cell$decision == "carry"), 0L)
macros["AssocFullLargeRankedCarry"] <- sprintf("%.1f", 100 * ranked_carry[["associability"]] /
                                               nrow(large_full$associability))
macros["ValueFullLargeRankedCarryCount"] <- ranked_carry[["value"]]
macros["ValueFullLargeCarryCount"] <- resolved_carry[["value"]]
macros["ResponseSharedRankedCarry"] <- sprintf("%.1f", 100 * ranked_carry[["response"]] /
                                              nrow(large_full$response))
# Every generating process, set of pear trials and N with g_V estimated. For
# example, AssocFullLargeCarry is the rate at which associability carryover is
# favored for responses generated by carried associability, all ten pear
# trials, N = 160.
prefix <- c(associability = "Assoc", reset = "Reset", value = "Value", null = "Null")
suffix <- c(detected = "Detected", carry = "Carry", unresolved = "Unresolved",
            no_carry = "NoCarry")
for (scenario in names(prefix)) {
  for (view in c("late", "early", "full")) {
    for (n in c(40L, 160L)) {
      for (metric in names(suffix)) {
        name <- paste0(prefix[[scenario]], tools::toTitleCase(view),
                       if (n == 40L) "Small" else "Large", suffix[[metric]])
        macros[name] <- percent_of(scenario, view, metric, n)
      }
    }
  }
}
# Section 7: the restricted comparison with g_V fixed at 0 (Section 7.1), and
# the lower response intercept for B (Section 7.3)
macros["ValueOmittedCarry"] <- percent_of("value", "full", "carry", policy = "value_zero")
macros["AssocOmittedCarry"] <- percent_of("associability", "full", "carry", policy = "value_zero")
macros["ResponseSharedCarry"] <- percent_of("response", "full", "carry")
macros["ResponseFlexibleCarry"] <- percent_of("response", "full", "carry",
                                           policy = "response_allowed")
macros["ResponseDetected"] <- percent_of("response", "full", "detected")
# Section 6.4 and Supplement S2.8: the profile of g_alpha in the worked example
# at half carryover: the loss of fit as Figure S1b plots it (twice the
# log-likelihood loss), the corresponding likelihood ratio, and the estimated
# eta, which is also given at full carryover
profile_at <- function(g_alpha) profile[abs(profile$g_alpha - g_alpha) < 1e-9, ]
macros["ProfileHalfLoss"] <- sprintf("%.2f", 2 * (profile_at(.5)$nll - min(profile$nll)))
macros["ProfileHalfRatio"] <- sprintf("%.1f", exp(profile_at(.5)$nll - min(profile$nll)))
macros["ProfileEtaFull"] <- paper_number(profile_at(1)$eta, 3)
macros["ProfileEtaHalf"] <- paper_number(profile_at(.5)$eta, 3)
# Section 6.4 and Supplement S2.8: KL divergences of the mimicry fits to the
# generating probabilities of carried associability
mimicry <- function(view, model, column) {
  landscape[landscape$scenario == "associability" & landscape$view == view &
              landscape$model == model, column]
}
macros["MimicRWBefore"] <- sprintf("%.3f", mimicry("full", "fixed", "reference_kl"))
macros["MimicRWAfter"] <- sprintf("%.3f", mimicry("full", "fixed", "fitted_kl"))
macros["MimicResetLate"] <- sprintf("%.4f", mimicry("late", "reset", "fitted_kl"))
macros["MimicRWLate"] <- sprintf("%.4f", mimicry("late", "fixed", "fitted_kl"))
macros["MimicResetFull"] <- sprintf("%.3f", mimicry("full", "reset", "fitted_kl"))
# Section 6.4: the share of the reset candidate's divergence in the first two
# pear trials with each light, and the generating D-B difference on pear trials
# 8-10
macros["MimicResetFirstTwo"] <- sprintf("%.0f", 100 * mimicry_summary$reset_first_two_share)
macros["MimicLateGapMin"] <- paper_number(mimicry_summary$late_gap_min, 2)
macros["MimicLateGapMax"] <- paper_number(mimicry_summary$late_gap_max, 2)
# Section 7.2: the first-response check. Under carried associability the
# first-response probabilities are equal, so AssocInitialRejected is the rate of
# false alarms.
value_row <- first_response_summary[first_response_summary$scenario == "value", ]
associability_row <- first_response_summary[first_response_summary$scenario == "associability", ]
macros["ValueInitialRejected"] <- sprintf("%.1f", 100 * value_row$initial_rejection)
macros["ValueWrongFlagged"] <- sprintf("%.1f", 100 * value_row$rejection_given_carry)
macros["AssocInitialRejected"] <- sprintf("%.1f", 100 * associability_row$initial_rejection)
# Supplement S2.10: the refits with wider bounds
macros["BoundsRefitN"] <- nrow(bounds_refits)
macros["BoundsRefitChanged"] <- sum(bounds_refits$original_decision != bounds_refits$wider_decision)
writeLines(c("% Generated by analysis/section6_report.R.",
             sprintf("\\newcommand{\\%s}{%s}", names(macros), macros)),
           file.path(path, "section6_numbers.tex"))

# Tables 9 and S2 give, in percent of the experiments of each row, how often a
# positive B-D difference is detected and how often the comparison favors
# associability carryover, remains unresolved or favors another candidate
# (no_carry). These rates, and that of fit problems, are computed here from
# results.csv.
rates <- function(scenario, view, n, policy = "value_allowed", config = NULL) {
  d <- results[results$scenario == scenario & results$view == view & results$n == n &
                 results$policy == policy, ]
  if (!is.null(config)) d <- d[d$config == config, ]
  c(experiments = nrow(d), detected = 100 * mean(d$detected == 1),
    carry = 100 * mean(d$decision == "carry"),
    unresolved = 100 * mean(d$decision == "unresolved"),
    other = 100 * mean(d$decision == "no_carry"),
    problem = 100 * mean(d$decision == "fit_problem"))
}
# The four columns of rates, with the decisions in the order of the legend of
# Figure 8
rate_columns <- function(r) {
  paste(sprintf("%.1f", r[c("detected", "carry", "unresolved", "other")]), collapse = " & ")
}
pear_trials <- c(full = "1--10", early = "1--6", late = "8--10")
process_labels <- c(associability = "Carried associability", value = "Carried learned value",
                    reset = "Reset associability", null = "Physical-light differences alone",
                    response = "Lower response intercept for B")

# Table 9. The first block has the first eight rows of Figure 8, in the same
# order, and key names the macros of each of its rows, such as AssocFullLarge
# for carried associability, all ten pear trials and 160 participants.
block <- function(heading, section) {
  list(group = paste0(heading, " (Section~\\ref{", section, "})"))
}
table9_row <- function(scenario, view, n, policy = "value_allowed", key = NULL) {
  list(scenario = scenario, view = view, n = n, r = rates(scenario, view, n, policy), key = key)
}
rows <- list(
  block("Every candidate estimates the carryover of learned value", "sec:repeated_experiments"),
  table9_row("associability", "full", 160, key = "AssocFullLarge"),
  table9_row("associability", "early", 160, key = "AssocEarlyLarge"),
  table9_row("associability", "late", 160, key = "AssocLateLarge"),
  table9_row("associability", "full", 40, key = "AssocFullSmall"),
  table9_row("associability", "early", 40, key = "AssocEarlySmall"),
  table9_row("associability", "late", 40, key = "AssocLateSmall"),
  table9_row("value", "full", 160, key = "ValueFullLarge"),
  table9_row("reset", "full", 160, key = "ResetFullLarge"),
  block("Carryover of learned value fixed at zero in every candidate",
        "sec:restricted_comparison"),
  table9_row("associability", "full", 160, "value_zero"),
  table9_row("value", "full", 160, "value_zero"),
  block("Every candidate uses one response function for both lights", "sec:response_intercept"),
  table9_row("response", "full", 160),
  block("Every candidate may give B its own response intercept", "sec:response_intercept"),
  table9_row("response", "full", 160, "response_allowed")
)
r <- function(i) rows[[i]]$r
table9_rows <- character(0)
body <- character(0)
for (x in rows) {
  if (!is.null(x$group)) {
    body <- c(body, "\\addlinespace", sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", x$group))
  } else {
    row <- paste(process_labels[[x$scenario]], pear_trials[[x$view]], x$n, rate_columns(x$r),
                 sep = " & ")
    table9_rows <- c(table9_rows, row)
    body <- c(body, paste(row, "\\\\"))
  }
}
caption9 <- paste0(
  "\\caption{Percentages of simulated experiments, ", macros[["StudyReplications"]],
  " for each row, in which a positive B--D difference is detected and in which the ",
  "comparison of Section~\\ref{sec:comparing_explanations} favors associability carryover, ",
  "remains unresolved, or favors another candidate. The last three columns add to 100 apart ",
  "from rounding. Favoring associability carryover is correct only for responses generated ",
  "by carried associability. The first three columns give the generating process, the pear ",
  "trials analyzed for each light, and the number of simulated participants in each ",
  "experiment, $N$. The first block has the first eight rows of ",
  "Figure~\\ref{fig:inference_information}, in the same order. Supplementary Table~S2 ",
  "reports every combination.}"
)
header9 <- paste("\\textbf{Responses generated by} & \\textbf{Pear trials} & $\\boldsymbol{N}$ &",
                 "\\textbf{Detected} & \\textbf{Carryover} & \\textbf{Unresolved} &",
                 "\\textbf{Another} \\\\")
writeLines(c("\\begin{table}[htbp]", "\\centering", "\\small", caption9,
             "\\label{tab:summary_rates}",
             "\\begin{tabular}{>{\\raggedright\\arraybackslash}p{0.27\\linewidth}lcrrrr}",
             "\\toprule",
             " & & & & \\multicolumn{3}{c}{\\textbf{Decision of the comparison}} \\\\",
             "\\cmidrule(lr){5-7}", header9, "\\midrule",
             body[-1], "\\bottomrule", "\\end{tabular}", "\\end{table}"),
           file.path(path, "summary_rates.tex"))

# Table S2: the four main generating processes, each with both values of N and
# the three sets of pear trials, and the lower response intercept for B, which
# is analyzed only with all ten pear trials and N = 160. Every candidate
# estimates g_V and uses one response function for both lights. A heading that
# ends in \\* stays on the page of the row below it.
table_s2 <- list()
table_s2_rows <- character(0)
body <- character(0)
for (scenario in c("associability", "value", "reset", "null", "response")) {
  body <- c(body, if (scenario != "associability") "\\addlinespace",
            sprintf("\\multicolumn{6}{@{}l}{\\textit{%s}} \\\\*", process_labels[[scenario]]))
  sizes <- if (scenario == "response") 160 else c(160, 40)
  kept <- if (scenario == "response") "full" else c("full", "early", "late")
  for (n in sizes) {
    for (view in kept) {
      x <- list(scenario = scenario, view = view, n = n, r = rates(scenario, view, n))
      table_s2[[length(table_s2) + 1L]] <- x
      row <- paste(pear_trials[[view]], n, rate_columns(x$r), sep = " & ")
      table_s2_rows <- c(table_s2_rows, row)
      body <- c(body, paste(row, "\\\\"))
    }
  }
}
caption_s2 <- paste0(
  "\\caption{Percentages of simulated experiments, ", macros[["StudyReplications"]],
  " for each row, in which a positive B--D difference is detected and in which the ",
  "held-out comparison favors associability carryover, remains unresolved, or favors ",
  "another candidate. Every candidate estimates the carryover of learned value and uses one ",
  "response function for both lights. Favoring associability carryover is correct only for ",
  "responses generated by carried associability. Every fit converged, so the last three ",
  "columns add to 100 apart from rounding. The first two columns give the pear trials ",
  "analyzed for each light and the number of simulated participants in each experiment, ",
  "$N$. The first block repeats the first six rows of main-text Table~9. Responses ",
  "generated with a lower response intercept for B were analyzed only with all ten ",
  "responses and $N=160$.}\\label{tab:s2_rates}\\\\"
)
header_s2 <- paste("\\textbf{Pear trials} & $\\boldsymbol{N}$ & \\textbf{Detected} &",
                   "\\textbf{Carryover} & \\textbf{Unresolved} & \\textbf{Another} \\\\")
writeLines(c("\\begin{longtable}{@{}lrrrrr@{}}", caption_s2, "\\toprule",
             " & & & \\multicolumn{3}{c@{}}{\\textbf{Decision of the comparison}} \\\\",
             "\\cmidrule(l){4-6}", header_s2, "\\midrule", "\\endfirsthead",
             "\\toprule", header_s2, "\\midrule", "\\endhead",
             "\\bottomrule", "\\endfoot", body, "\\end{longtable}"),
           file.path(path, "supplement_table_s2.tex"))

cat(sprintf("%s experiments, %s fits (%s not converged)\n", macros[["StudyExperiments"]],
            macros[["StudyFits"]], macros[["StudyFailedFits"]]))
cat(sprintf("N = 160, all ten pear trials: associability carryover favored in %s%%",
            macros[["AssocFullLargeCarry"]]),
    sprintf("(carried associability), %s%% (carried learned value), %s%% (reset associability)\n",
            macros[["ValueFullLargeCarry"]], macros[["ResetFullLargeCarry"]]))
# Section 6.5: carried associability, all ten pear trials and 160 participants,
# for each of the four parameter sets
carry_by_set <- vapply(1:4, function(k) {
  rates("associability", "full", 160, config = k)[["carry"]]
}, 0)
cat("Carried associability, N = 160, all ten pear trials: associability carryover favored in",
    paste0(sprintf("%.1f", carry_by_set), "%", collapse = ", "), "with parameter sets 1-4\n")
cat("Saved results/section6/summary.csv, summary_by_configuration.csv,",
    "section6_numbers.tex, summary_rates.tex and supplement_table_s2.tex\n")

# Values reported in the paper, as printed
printed <- c(
  # Sections 6.2 and 6.3, Figure 7
  ExampleContrast = ".212", ExampleLower = ".174", ExampleUpper = ".251",
  ExampleFixedValueCarry = ".70", ExampleResetValueCarry = ".50",
  ExampleCarryVsFixed = "20.5", ExampleCarryVsFixedSE = "5.9",
  ExampleCarryVsReset = "21.1", ExampleCarryVsResetSE = "5.1",
  # Section 6.4 and Supplement S2.8
  ProfileHalfLoss = "0.78", ProfileHalfRatio = "1.5", ProfileEtaFull = ".045",
  ProfileEtaHalf = ".082",
  MimicRWBefore = "1.417", MimicRWAfter = "0.119", MimicResetFull = "0.143",
  MimicRWLate = "0.0019", MimicResetLate = "0.0015", MimicResetFirstTwo = "41",
  MimicLateGapMin = ".23", MimicLateGapMax = ".33",
  # Section 6.5, Figure 8, Tables 9 and S2
  StudyReplications = "400", StudyMaxMCSE = "2.5",
  AssocFullLargeDetected = "100.0", AssocFullLargeCarry = "59.5",
  AssocFullLargeUnresolved = "40.2", AssocConfigurationMin = "15.0",
  AssocFullLargeRankedCarry = "94.5", ValueFullLargeRankedCarryCount = "21",
  ValueFullLargeCarryCount = "0",
  AssocConfigurationMax = "91.0", AssocLateLargeCarry = "0.5", AssocLateLargeNoCarry = "21.2",
  AssocEarlyLargeCarry = "14.8", AssocLateSmallCarry = "0.5", AssocFullSmallCarry = "10.5",
  ValueFullLargeDetected = "100.0", ValueFullLargeCarry = "0.0", ValueFullLargeNoCarry = "41.8",
  ResetFullLargeDetected = "2.0", ResetFullLargeCarry = "0.2",
  # Section 7, Figure 9, Table 9
  AssocOmittedCarry = "96.8", ValueOmittedCarry = "95.0", AssocInitialRejected = "1.5",
  ValueInitialRejected = "94.8", ValueWrongFlagged = "95.3",
  ResponseDetected = "100.0", ResponseSharedCarry = "7.2", ResponseSharedRankedCarry = "68.0",
  ResponseFlexibleCarry = "0.0",
  # Supplement S2
  StudyExperiments = "3,600", StudyFits = "67,200", StudyFailedFits = "0",
  StudyRefinedFits = "1,432", StudyProblemAnalyses = "0", StudyCoverageMin = "93.8",
  StudyCoverageMax = "97.5", NullFullLargeDetected = "3.2", NullFullLargeCarry = "0.0",
  BoundsRefitN = "24", BoundsRefitChanged = "0"
)
# Table 9, as the main text prints it
printed_table9 <- c(
  "Carried associability & 1--10 & 160 & 100.0 & 59.5 & 40.2 & 0.2",
  "Carried associability & 1--6 & 160 & 99.8 & 14.8 & 83.5 & 1.8",
  "Carried associability & 8--10 & 160 & 99.2 & 0.5 & 78.2 & 21.2",
  "Carried associability & 1--10 & 40 & 96.5 & 10.5 & 86.2 & 3.2",
  "Carried associability & 1--6 & 40 & 87.2 & 5.2 & 87.2 & 7.5",
  "Carried associability & 8--10 & 40 & 85.8 & 0.5 & 74.5 & 25.0",
  "Carried learned value & 1--10 & 160 & 100.0 & 0.0 & 58.2 & 41.8",
  "Reset associability & 1--10 & 160 & 2.0 & 0.2 & 88.8 & 11.0",
  "Carried associability & 1--10 & 160 & 100.0 & 96.8 & 3.2 & 0.0",
  "Carried learned value & 1--10 & 160 & 100.0 & 95.0 & 5.0 & 0.0",
  "Lower response intercept for B & 1--10 & 160 & 100.0 & 7.2 & 90.0 & 2.8",
  "Lower response intercept for B & 1--10 & 160 & 100.0 & 0.0 & 82.0 & 18.0"
)
# Table S2, as the supplement prints it
printed_table_s2 <- c(
  # Carried associability
  "1--10 & 160 & 100.0 & 59.5 & 40.2 & 0.2", "1--6 & 160 & 99.8 & 14.8 & 83.5 & 1.8",
  "8--10 & 160 & 99.2 & 0.5 & 78.2 & 21.2", "1--10 & 40 & 96.5 & 10.5 & 86.2 & 3.2",
  "1--6 & 40 & 87.2 & 5.2 & 87.2 & 7.5", "8--10 & 40 & 85.8 & 0.5 & 74.5 & 25.0",
  # Carried learned value
  "1--10 & 160 & 100.0 & 0.0 & 58.2 & 41.8", "1--6 & 160 & 100.0 & 0.0 & 76.0 & 24.0",
  "8--10 & 160 & 74.0 & 0.5 & 80.0 & 19.5", "1--10 & 40 & 97.8 & 0.2 & 73.8 & 26.0",
  "1--6 & 40 & 98.2 & 0.5 & 80.5 & 19.0", "8--10 & 40 & 27.8 & 0.5 & 82.5 & 17.0",
  # Reset associability
  "1--10 & 160 & 2.0 & 0.2 & 88.8 & 11.0", "1--6 & 160 & 3.2 & 0.0 & 89.5 & 10.5",
  "8--10 & 160 & 2.5 & 0.0 & 85.2 & 14.8", "1--10 & 40 & 2.5 & 0.2 & 83.0 & 16.8",
  "1--6 & 40 & 2.2 & 0.2 & 85.0 & 14.8", "8--10 & 40 & 1.2 & 0.0 & 79.5 & 20.5",
  # Physical-light differences alone
  "1--10 & 160 & 3.2 & 0.0 & 84.5 & 15.5", "1--6 & 160 & 3.8 & 0.5 & 86.5 & 13.0",
  "8--10 & 160 & 2.2 & 0.5 & 78.5 & 21.0", "1--10 & 40 & 1.2 & 0.0 & 88.5 & 11.5",
  "1--6 & 40 & 1.2 & 0.0 & 84.5 & 15.5", "8--10 & 40 & 2.0 & 0.2 & 77.8 & 22.0",
  # Lower response intercept for B
  "1--10 & 160 & 100.0 & 7.2 & 90.0 & 2.8"
)
macro <- function(name) as.numeric(macros[[name]])
# A rate computed from results.csv agrees with a macro, which is rounded to .1.
near <- function(x, y) abs(x - y) < .051
# The four rates of a table row agree with the macros named by key, followed by
# Detected, Carry, Unresolved and NoCarry.
agrees <- function(r, key) {
  all(near(r[c("detected", "carry", "unresolved", "other")],
           vapply(paste0(key, c("Detected", "Carry", "Unresolved", "NoCarry")), macro, 0)))
}
carried <- results[results$scenario == "associability" & results$n == 160 &
                     results$view == "full" & results$policy == "value_allowed", ]
carried_effect <- tapply(carried$true_effect, carried$config, mean)
# D's associability at the end of the compound trials under the adaptive
# generating processes, for each parameter set, counterbalancing group and
# learning-rate type: D and its companion tone C learn at different rates when
# d is not 0.
control_training <- c(make_trials("X", 1, 10, "Phase 1"),
                      make_trials(c("C", "D"), 1, 10, "Phase 2"))
alpha_D <- unlist(lapply(which(generating$adaptive), function(i) {
  g <- generating[i, ]
  unlist(lapply(c(-1, 1), function(allocation) lapply(c(-1, 1), function(type) {
    rates <- .5 * plogis(g$mu + type * g$spread + c(X = 0, C = 0, D = -allocation) * g$cue)
    adaptive_final_state(control_training, S = 2 * rates, alpha_init = c(X = .5, C = .5, D = .5),
                         eta = g$eta)$alpha[["D"]]
  })))
}))
# The fits to the responses generated with a lower response intercept for B
# (Section 7.3), with g_V estimated.
response_fits <- merge(parameters, jobs[c("id", "scenario")], by = "id")
response_fits <- response_fits[response_fits$scenario == "response" &
                                 response_fits$policy == "value_allowed", ]
response_candidate <- sub("_.*", "", response_fits$fit)
median_g_value <- tapply(response_fits$g_value, response_candidate, median)
stopifnot(
  macros[names(printed)] == printed,
  identical(table9_rows, printed_table9),
  identical(table_s2_rows, printed_table_s2),
  # Supplement S2.10: with wider bounds, the candidate ranked first without the
  # margin changes for one dataset
  sum(bounds_refits$original_winner != bounds_refits$wider_winner) == 1,
  # Supplement S2.8: in the profile, eta is at its upper bound of .20 when
  # g_alpha is 0 or .1, and below it from g_alpha = .2 on
  all(abs(profile$eta[profile$g_alpha < .15] - .20) < 1e-8),
  all(profile$eta[profile$g_alpha > .15] < .20 - 1e-3),
  # Section 6.5: the rate is lowest in parameter set 4, whose version of
  # carried associability has the weakest effect, and highest in the base set
  # (1), whose version has the strongest
  associability_rates$config[which.min(associability_rates$carry)] ==
    names(which.min(carried_effect)),
  associability_rates$config[which.max(associability_rates$carry)] ==
    names(which.max(carried_effect)),
  associability_rates$config[which.min(associability_rates$carry)] == 4,
  associability_rates$config[which.max(associability_rates$carry)] == 1,
  # Section 6.5: with pear trials 8-10, a candidate without associability
  # carryover is favored more often than carryover
  as.numeric(percent_of("associability", "late", "no_carry")) >
    as.numeric(percent_of("associability", "late", "carry")),
  # Supplement S2.9: the one-sided 95% upper bound when no event occurs in 400
  # experiments
  abs(1 - .05^(1 / 400) - .0075) < 5e-5,
  # Section 6.5 and Supplement S2.9: a rate from a single parameter set, 100
  # experiments, has a Monte Carlo standard error of at most 5 percentage points
  summary_by_set$repetitions == 100,
  sprintf("%.0f", 100 * .5 / sqrt(100)) == "5",
  # Section 6.5: under reset associability, as under physical-light differences
  # alone, the expected B-D difference is zero (to rounding error in its sum
  # over the counterbalancing groups)
  all(abs(results$true_effect[results$scenario %in% c("reset", "null")]) < 1e-15),
  # Section 7.3: the fixed-rate and reset candidates account for the B-D
  # difference through carried learned value, the carryover candidate mainly
  # through carried associability
  all(median_g_value[c("fixed", "reset")] > .3), median_g_value[["carry"]] < .1,
  median(response_fits$g_alpha[response_candidate == "carry"]) > .3,
  # Supplement S2.2: D's associability after the compound trials lies between
  # .477 and .524
  sprintf("%.3f", range(alpha_D)) == c("0.477", "0.524"),
  # Supplement S2.7: the check of the starting vectors (192 fits, every one
  # within 1e-5 of the reference; the four fixed vectors alone short by more
  # than .001 in 8)
  nrow(starting_values) == 192, all(starting_values$study_converged),
  max(starting_values$study_nll - starting_values$reference_nll) < 1e-5,
  sum(starting_values$fixed_nll - starting_values$reference_nll > .001) == 8
)

# Table 9: the rates of every row and its 400 experiments; the rates of the
# first block, which Figure 8 also draws, agree with their macros, and so do the
# rates of the other blocks that the text quotes.
first_block <- Filter(function(x) !is.null(x$key), rows)
stopifnot(
  vapply(Filter(function(x) !is.null(x$r), rows), function(x) x$r[["experiments"]], 0) == 400,
  length(first_block) == 8,
  vapply(first_block, function(x) agrees(x$r, x$key), NA),
  near(r(11)[["carry"]], macro("AssocOmittedCarry")),
  near(r(12)[["carry"]], macro("ValueOmittedCarry")),
  near(r(14)[["detected"]], macro("ResponseDetected")),
  near(r(14)[["carry"]], macro("ResponseSharedCarry")),
  near(r(16)[["carry"]], macro("ResponseFlexibleCarry"))
)

# Words the text uses instead of some of these rates
stopifnot(
  # Section 6.5: under carried learned value, a difference is "detected in every
  # experiment", associability carryover is "never favored", and "Most
  # comparisons nevertheless remain unresolved"
  r(8)[["detected"]] == 100, r(8)[["carry"]] == 0, r(8)[["unresolved"]] > 50,
  # Section 6.5: under reset associability, a difference is "rarely detected" and
  # associability carryover is "almost never favored"
  r(9)[["detected"]] < 5, r(9)[["carry"]] < 1,
  # Section 7.3: with one response function, "most comparisons remain
  # unresolved"; with B's own response intercept, associability carryover is
  # "never favored" and "most comparisons again remain unresolved"
  r(14)[["unresolved"]] > 50, r(16)[["carry"]] == 0, r(16)[["unresolved"]] > 50,
  # Section 9.2: carried associability with 160 participants, detected "in
  # nearly every experiment" but favored "far less often", and "almost never"
  # with pear trials 8-10
  r(2)[["detected"]] >= 99, r(4)[["detected"]] >= 99,
  r(2)[["detected"]] - r(2)[["carry"]] >= 25, r(4)[["carry"]] < 1,
  # Section 9.2: the restricted comparison favors associability carryover in
  # "nearly all" of the experiments generated by carried learned value, and the
  # first-response check "would have flagged most of these wrong conclusions"
  r(12)[["carry"]] >= 90, macro("ValueWrongFlagged") > 50,
  # Supplement S2.9: under physical-light differences alone, "most comparisons
  # remained unresolved"
  macro("NullFullLargeUnresolved") > 50
)

# Table S2: 400 experiments in each row and no fit problems, and rates that
# agree with their macros. Responses generated with a lower response intercept
# for B are analyzed only with all ten pear trials and N = 160.
macro_key <- function(x) {
  paste0(prefix[[x$scenario]], tools::toTitleCase(x$view), if (x$n == 160) "Large" else "Small")
}
main_rows <- Filter(function(x) x$scenario != "response", table_s2)
response_rate <- table_s2[[length(table_s2)]]$r
stopifnot(
  length(main_rows) == 24,
  vapply(table_s2, function(x) x$r[["experiments"]], 0) == 400,
  vapply(table_s2, function(x) x$r[["problem"]], 0) == 0,
  vapply(main_rows, function(x) agrees(x$r, macro_key(x)), NA),
  all(results$view[results$scenario == "response"] == "full"),
  all(results$n[results$scenario == "response"] == 160),
  near(response_rate[["detected"]], macro("ResponseDetected")),
  near(response_rate[["carry"]], macro("ResponseSharedCarry"))
)

# Section 6.5: the rates for the four parameter sets
stopifnot(near(carry_by_set, c(91.0, 56.0, 76.0, 15.0)),
          near(min(carry_by_set), macro("AssocConfigurationMin")),
          near(max(carry_by_set), macro("AssocConfigurationMax")))

# Section 6.4 and Table 8: the KL divergences per response, from the unrounded
# fits: twenty responses per participant over all ten pear trials, six over
# pear trials 8-10.
kl <- c(fixed_full = mimicry("full", "fixed", "fitted_kl") / 20,
        reset_full = mimicry("full", "reset", "fitted_kl") / 20,
        fixed_late = mimicry("late", "fixed", "fitted_kl") / 6,
        reset_late = mimicry("late", "reset", "fitted_kl") / 6)
ratio <- c(kl[["fixed_full"]] / kl[["fixed_late"]], kl[["reset_full"]] / kl[["reset_late"]])
stopifnot(
  # Section 6.4 and Table 8: about 0.006 and 0.007 per response over all ten
  # pear trials
  sprintf("%.3f", kl[c("fixed_full", "reset_full")]) == c("0.006", "0.007"),
  # Section 6.4: about 0.0003 and 0.0002 over pear trials 8-10, about 5% and 3%
  # of their values over all ten pear trials, from the unrounded values and from
  # the rounded ones alike
  sprintf("%.4f", kl[c("fixed_late", "reset_late")]) == c("0.0003", "0.0002"),
  sprintf("%.0f", 100 / ratio) == c("5", "3"),
  sprintf("%.0f", 100 * c(.0003 / .006, .0002 / .007)) == c("5", "3"),
  # The values per participant that the macros print
  sprintf("%.3f", 20 * kl[c("fixed_full", "reset_full")]) ==
    macros[c("MimicRWAfter", "MimicResetFull")],
  sprintf("%.4f", 6 * kl[c("fixed_late", "reset_late")]) ==
    macros[c("MimicRWLate", "MimicResetLate")]
)

# Supplement S2.8: the six mimicry fits of the alternatives to carried
# associability converged and stayed above 1e-8, and the bounds they reached
alternatives <- landscape[landscape$scenario == "associability" & landscape$model != "carry", ]
at_upper <- function(x, p) (inference_bounds["upper", p] - x) / diff(inference_bounds[, p]) < .001
stopifnot(
  nrow(alternatives) == 6, alternatives$optimizer_converged,
  !alternatives$kl_lower_bound_reached, alternatives$fitted_kl > 1e-8,
  # On pear trials 8-10, both alternatives carry over all of the learned value
  # for the apple (g_V = 1)
  at_upper(mimicry("late", "fixed", "g_value"), "g_value"),
  at_upper(mimicry("late", "reset", "g_value"), "g_value"),
  # With all ten pear trials, the fixed-rate candidate's slope is at its upper
  # bound of 16, and the reset candidate's is not
  at_upper(mimicry("full", "fixed", "log_slope"), "log_slope"),
  !at_upper(mimicry("full", "reset", "log_slope"), "log_slope"),
  # With pear trials 1-6, both slopes are at that bound, and the reset
  # candidate's eta is at its upper bound of .20
  at_upper(mimicry("early", "fixed", "log_slope"), "log_slope"),
  at_upper(mimicry("early", "reset", "log_slope"), "log_slope"),
  at_upper(mimicry("early", "reset", "eta"), "eta")
)

# Supplement S2.4: the generating parameters as printed. Each process and
# parameter set is the base set with the changes listed for them; eta and
# g_alpha enter only the adaptive model, which carried and reset associability
# use.
printed_parameters <- function(scenario, config) {
  par <- c(mu = qlogis(.30), spread = .35, cue = .20, intercept = -4, log_slope = log(8),
           g_value = 0, eta = .05, g_alpha = .80, history_intercept = 0)
  if (scenario == "value") par["g_value"] <- .65
  if (scenario == "reset") par["g_alpha"] <- 0
  if (scenario == "response") par["history_intercept"] <- -.90
  if (config == 2) par[c("mu", "spread")] <- c(qlogis(.24), .50)
  if (config == 3) par[c("intercept", "log_slope")] <- c(-3, log(6))
  if (config == 4) {
    par["cue"] <- -.30
    if (scenario == "associability") par[c("eta", "g_alpha")] <- c(.035, .60)
    if (scenario == "reset") par["eta"] <- .035
    if (scenario == "value") par["g_value"] <- .45
    if (scenario == "response") par["history_intercept"] <- -.60
  }
  par
}
matches_printed <- vapply(seq_len(nrow(generating)), function(i) {
  g <- generating[i, ]
  shown <- if (g$adaptive) inference_names else setdiff(inference_names, c("eta", "g_alpha"))
  isTRUE(all.equal(unlist(g[shown]), printed_parameters(g$scenario, g$config)[shown],
                   tolerance = 1e-12))
}, NA)
# Supplement S2.5: the estimation bounds as the table prints them, with gamma on
# its own scale
printed_bounds <- rbind(lower = c(-3.5, 0, -.65, -8, 1.5, 0, 0, 0, -2.5),
                        upper = c(1, 1, .65, 2, 16, 1, .20, 1, 2.5))
shown_bounds <- inference_bounds
shown_bounds[, "log_slope"] <- exp(shown_bounds[, "log_slope"])
# Section 6.1: in the base parameter set, a tone or the buzzer starts with the
# learning rate .5 logistic(mu + tau z) of Equation S2, about .12 for the slower
# type (z = -1) and .19 for the faster type (z = +1).
base_sets <- generating[generating$config == 1, ]
base_rates <- .5 * plogis(cbind(base_sets$mu - base_sets$spread, base_sets$mu + base_sets$spread))
stopifnot(
  nrow(generating) == 20, matches_printed,
  generating$adaptive == (generating$scenario %in% c("associability", "reset")),
  abs(shown_bounds - printed_bounds) < 1e-12,
  paper_number(base_rates[, 1], 2) == ".12", paper_number(base_rates[, 2], 2) == ".19"
)

# Supplement S2.5 and S2.10: in most fits at least one estimate lies on a bound
# (column boundary of diagnostics.csv), and the four most frequent are eta at
# .20, gamma at 16, b at -8 and g_V at 0. g_V = 0 is the generating value of
# four of the five generating processes.
estimates <- merge(diagnostics[, c("id", "view", "policy", "fit", "boundary")], parameters,
                   by = c("id", "view", "policy", "fit"))
bound_counts <- unlist(lapply(colnames(inference_bounds), function(p) {
  on <- grepl(paste0("(^|;)", p, "(;|$)"), estimates$boundary)
  span <- inference_bounds["upper", p] - inference_bounds["lower", p]
  lower <- (estimates[[p]][on] - inference_bounds["lower", p]) / span < .001
  c(setNames(sum(lower), paste(p, "lower")), setNames(sum(!lower), paste(p, "upper")))
}))
stopifnot(
  nrow(estimates) == nrow(diagnostics), nrow(diagnostics) == 67200,
  mean(estimates$boundary != "") > .5,
  setequal(names(sort(bound_counts, decreasing = TRUE))[1:4],
           c("eta upper", "log_slope upper", "intercept lower", "g_value lower")),
  setequal(unique(generating$scenario[generating$g_value == 0]),
           c("associability", "reset", "null", "response")),
  all(generating$g_value[generating$scenario == "value"] > 0)
)

# Further statements of the text that rest on the saved results
first <- function(scenario, n) {
  first_response[first_response$scenario == scenario & first_response$n == n, ]
}
discordant <- with(first("associability", 160), mean(discordant_control + discordant_blocked))
rejected <- vapply(c("associability", "value", "reset", "null", "response"),
                   function(s) 100 * mean(first(s, 160)$initial_rejection), 0)
early_kl <- c(fixed = mimicry("early", "fixed", "fitted_kl"),
              reset = mimicry("early", "reset", "fitted_kl"))
full_kl <- c(fixed = mimicry("full", "fixed", "fitted_kl"),
             reset = mimicry("full", "reset", "fitted_kl"))
# The profile of g_alpha in the worked example: the negative log likelihood of
# the best fit with no carryover minus that with full carryover
profile_loss_none <- profile_at(0)$nll - profile_at(1)$nll
stopifnot(
  # Section 7.2: an experiment has on average "only about eight participants
  # whose two first responses differ"
  nrow(first("associability", 160)) == 400, round(discordant) == 8,
  # Supplement S2.10: with N = 160, the first-response check rejects equality in
  # 2.2% of the experiments generated by reset associability, 2.0% of those
  # generated by physical-light differences alone and 10.5% of those generated
  # with a lower response intercept for B (also Section 7.3), printed like the
  # tables: exact halves such as 9/400 = 2.25% round to even
  vapply(c("reset", "null", "response"), function(s) nrow(first(s, 160)), 0L) == 400,
  sprintf("%.1f", rejected[c("reset", "null", "response")]) == c("2.2", "2.0", "10.5"),
  near(rejected[["associability"]], macro("AssocInitialRejected")),
  near(rejected[["value"]], macro("ValueInitialRejected")),
  # Section 6.4: "a difference of about 0.4 in log likelihood, against a
  # difference of about 22 between full carryover and none", in the profile of
  # g_alpha
  sprintf("%.1f", macro("ProfileHalfLoss") / 2) == "0.4",
  round(profile_loss_none) == 22,
  # Section 6.5: with pear trials 1-6 the alternatives "also come closer to the
  # generating probabilities than over all ten" (Supplementary Figure S1a), both
  # per participant and per response
  early_kl < full_kl, early_kl / 12 < full_kl / 20,
  # Section 9.2: with pear trials 8-10, a candidate without associability
  # carryover was "wrongly favored in about one experiment in five"
  macro("AssocLateLargeNoCarry") > 15, macro("AssocLateLargeNoCarry") < 25
)

# Section 6.3: the fitted fixed-rate and reset candidates predict differences of
# about .05 and .06 on the first pear trial (columns 1 and 11 are the first
# responses to B and to D, averaged over the counterbalancing groups and the two
# learning-rate types), and the carryover candidate none.
first_difference <- vapply(c("fixed", "reset", "carry"), function(m) {
  p <- inference_prob_cpp(example$fits[[m]]$par, example$fits[[m]]$spec$adaptive)
  mean(p[, 11]) - mean(p[, 1])
}, 0)
# Section 7.3: under the lower response intercept for B, the generating D-B
# difference in the probability of a yes response, averaged over the
# counterbalancing groups and learning-rate types, is small on the first pear
# trial, largest around the sixth and smaller again on the tenth. One row for
# each parameter set; columns are the pear trials.
response_gap <- t(vapply(1:4, function(k) {
  g <- generating[generating$scenario == "response" & generating$config == k, ]
  p <- inference_prob_cpp(unlist(g[inference_names]), g$adaptive)
  colMeans(p[, 11:20] - p[, 1:10])
}, numeric(10)))
largest <- apply(response_gap, 1, max)
stopifnot(
  sprintf("%.2f", first_difference[c("fixed", "reset")]) == c("0.05", "0.06"),
  abs(first_difference[["carry"]]) < 1e-12,
  apply(response_gap, 1, which.max) %in% 6:7, which.max(response_gap[1, ]) == 6,
  response_gap[, 1] < largest / 5, response_gap[, 10] < largest
)
