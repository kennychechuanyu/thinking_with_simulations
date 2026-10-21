# Sections 5.2-5.6 and 7.2, Supplement S2: the reported numbers, from the saved
# results.
# Writes the rates behind Figure 10 and Table S2 with Monte Carlo standard errors
# (summary.csv) and by configuration (summary_by_configuration.csv), the numbers
# quoted in the text (section5_numbers.tex) and Table S2 (section5_table.tex).
#   Rscript analysis/section5_report.R

if (!dir.exists("results/section5")) stop("Run this script from the repository root.")
source("R/figure_style.R")  # paper_number()
source("R/models.R")
path <- "results/section5"
checks <- c("initial_response_summary.csv", "wider_bounds_refits.csv")
if (!all(file.exists(file.path(path, checks)))) {
  stop("Run analysis/section5_first_response_and_bounds.R first; it writes ",
       paste(checks, collapse = " and "), ".")
}

results <- read.csv(file.path(path, "results.csv"))
diagnostics <- read.csv(file.path(path, "diagnostics.csv"))
jobs <- read.csv(file.path(path, "jobs.csv"))
example <- readRDS(file.path(path, "worked_example.rds"))
landscape <- read.csv(file.path(path, "landscape.csv"))
profile <- read.csv(file.path(path, "carryover_profile.csv"))
mimicry_summary <- read.csv(file.path(path, "mimicry_summary.csv"))
first_response_summary <- read.csv(file.path(path, "initial_response_summary.csv"))
bounds_refits <- read.csv(file.path(path, "wider_bounds_refits.csv"))
starting_values <- read.csv(file.path(path, "starting_values_check.csv"))
generating <- read.csv(file.path(path, "generating_parameters.csv"))

# Rates for each scenario, N, view and policy. The four configurations have
# fixed weights of .25, so the Monte Carlo variance of a rate is computed within
# each configuration and combined with these weights (Supplement S2).
cells <- split(results, interaction(results$scenario, results$n, results$view,
                                     results$policy, drop = TRUE))
summary_table <- do.call(rbind, lapply(cells, function(cell) {
  # forced_correct: the candidate matching the generating process has the
  # highest held-out score, without the margin. It is undefined when the policy
  # leaves out the generating process.
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

# Detection, carryover, unresolved and fit-problem rates within each configuration
configuration_cells <- split(results, interaction(results$scenario, results$n, results$view,
                                                   results$policy, results$config, drop = TRUE))
by_configuration <- do.call(rbind, lapply(configuration_cells, function(cell) {
  cbind(cell[1, c("scenario", "n", "view", "policy", "config")], repetitions = nrow(cell),
        detected = mean(cell$detected), carry = mean(cell$decision == "carry"),
        unresolved = mean(cell$decision == "unresolved"),
        fit_problem = mean(cell$decision == "fit_problem"))
}))
write.csv(by_configuration, file.path(path, "summary_by_configuration.csv"), row.names = FALSE)

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
  # (Supplement S2)
  StudyMaxMCSE = sprintf("%.1f", 100 * .5 / sqrt(max(summary_table$repetitions))),
  # The contrast is a difference of proportions, so it is written without a
  # leading zero.
  ExampleContrast = paper_number(example$effect[["estimate"]], 3),
  ExampleLower = paper_number(example$effect[["lower"]], 3),
  ExampleUpper = paper_number(example$effect[["upper"]], 3)
)
# The worked example of Section 5.2
contrasts <- example$comparison$carry_contrasts
macros["ExampleFixedValueCarry"] <- paper_number(example$fits$fixed$par["g_value"], 2)
macros["ExampleResetValueCarry"] <- paper_number(example$fits$reset$par["g_value"], 2)
macros["ExampleCarryVsFixed"] <- sprintf("%.1f", contrasts["fixed", "difference"])
macros["ExampleCarryVsFixedSE"] <- sprintf("%.1f", contrasts["fixed", "se"])
macros["ExampleCarryVsReset"] <- sprintf("%.1f", contrasts["reset", "difference"])
macros["ExampleCarryVsResetSE"] <- sprintf("%.1f", contrasts["reset", "se"])
# Section 5.5: the range of the carryover rate over the four configurations
associability_rates <- by_configuration[by_configuration$scenario == "associability" &
                                          by_configuration$n == 160 &
                                          by_configuration$view == "full" &
                                          by_configuration$policy == "value_allowed", ]
macros["AssocConfigurationMin"] <- sprintf("%.1f", 100 * min(associability_rates$carry))
macros["AssocConfigurationMax"] <- sprintf("%.1f", 100 * max(associability_rates$carry))
# Ranking first without a margin versus a resolved carryover preference,
# with N = 160, full records and old-value carryover estimated in every candidate.
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
# Every scenario, view and N with g_V estimated. For example, AssocFullLargeCarry
# is the rate at which carryover is favored for associability, full view, N = 160.
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
# Section 5.6: g_V fixed at 0, and the response scenario
macros["ValueOmittedCarry"] <- percent_of("value", "full", "carry", policy = "value_zero")
macros["AssocOmittedCarry"] <- percent_of("associability", "full", "carry", policy = "value_zero")
macros["ResponseSharedCarry"] <- percent_of("response", "full", "carry")
macros["ResponseFlexibleCarry"] <- percent_of("response", "full", "carry",
                                           policy = "response_allowed")
macros["ResponseDetected"] <- percent_of("response", "full", "detected")
# Section 5.3: the profile of g_alpha in the worked example at half carryover:
# the loss of fit as Figure S1b plots it (twice the log-likelihood loss), the
# corresponding likelihood ratio, and the estimated eta, which is also given at
# full carryover
profile_at <- function(g_alpha) profile[abs(profile$g_alpha - g_alpha) < 1e-9, ]
macros["ProfileHalfLoss"] <- sprintf("%.2f", 2 * (profile_at(.5)$nll - min(profile$nll)))
macros["ProfileHalfRatio"] <- sprintf("%.1f", exp(profile_at(.5)$nll - min(profile$nll)))
macros["ProfileEtaFull"] <- paper_number(profile_at(1)$eta, 3)
macros["ProfileEtaHalf"] <- paper_number(profile_at(.5)$eta, 3)
# Section 5.4: KL divergences of the mimicry fits, associability scenario
mimicry <- function(view, model, column) {
  landscape[landscape$scenario == "associability" & landscape$view == view &
              landscape$model == model, column]
}
macros["MimicRWBefore"] <- sprintf("%.3f", mimicry("full", "fixed", "reference_kl"))
macros["MimicRWAfter"] <- sprintf("%.3f", mimicry("full", "fixed", "fitted_kl"))
macros["MimicResetLate"] <- sprintf("%.4f", mimicry("late", "reset", "fitted_kl"))
macros["MimicRWLate"] <- sprintf("%.4f", mimicry("late", "fixed", "fitted_kl"))
macros["MimicResetFull"] <- sprintf("%.3f", mimicry("full", "reset", "fitted_kl"))
# Section 5.4: the share of the reset candidate's divergence in the first two
# answers to each cue, and the generating D-B difference on presentations 8-10
macros["MimicResetFirstTwo"] <- sprintf("%.0f", 100 * mimicry_summary$reset_first_two_share)
macros["MimicLateGapMin"] <- paper_number(mimicry_summary$late_gap_min, 2)
macros["MimicLateGapMax"] <- paper_number(mimicry_summary$late_gap_max, 2)
# Section 5.6: the first-answer test. Under carried associability the zero-value
# restriction holds, so AssocInitialRejected is the rate of false alarms.
value_row <- first_response_summary[first_response_summary$scenario == "value", ]
associability_row <- first_response_summary[first_response_summary$scenario == "associability", ]
macros["ValueInitialRejected"] <- sprintf("%.1f", 100 * value_row$initial_rejection)
macros["ValueWrongFlagged"] <- sprintf("%.1f", 100 * value_row$rejection_given_carry)
macros["AssocInitialRejected"] <- sprintf("%.1f", 100 * associability_row$initial_rejection)
# Supplement S2: the refits with wider bounds
macros["BoundsRefitN"] <- nrow(bounds_refits)
macros["BoundsRefitChanged"] <- sum(bounds_refits$original_decision != bounds_refits$wider_decision)
writeLines(c("% Generated by analysis/section5_report.R.",
             sprintf("\\newcommand{\\%s}{%s}", names(macros), macros)),
           file.path(path, "section5_numbers.tex"))

# Table S2. A row ending in \\* stays on the page of the next row, so each
# generating process is printed on one page.
header <- "\\toprule $N$ & View & Detect & Carry & No assoc. & Unresolved & Problem \\\\ \\midrule"
caption <- paste0(
  "\\caption{Inference from retained responses, by generating process, number of ",
  "participants $N$, and view. Entries are percentages of ",
  max(summary_table$repetitions), " experiments per row. Detect: the lower limit of the 95\\% ",
  "interval for the response contrast is positive. Carry: the carryover candidate is ",
  "preferred. No assoc.: a candidate without associability carryover is preferred. ",
  "Unresolved: neither is preferred. Problem: a selected fit did not converge; these ",
  "experiments remain in the denominator. Because of rounding, the last four entries in ",
  "a row may not sum to 100. Monte Carlo standard errors are at most ",
  macros["StudyMaxMCSE"], " percentage points. Individual standard errors, and how often ",
  "the candidate matching the generating process had the highest held-out score without ",
  "the margin, are in \\texttt{results/section5/summary.csv}.}\\label{tab:s2_inference}\\\\"
)
labels <- c(associability = "Carried associability", reset = "Transfer-only adaptation",
            value = "Value generalization", null = "Physical-cue differences alone")
table_lines <- c("\\begin{longtable}{llrrrrr}", caption, header, "\\endfirsthead",
                 "\\caption[]{(continued)}\\\\", header, "\\endhead")
table_rows <- character(0)
for (scenario in names(labels)) {
  table_lines <- c(table_lines,
                   paste0("\\multicolumn{7}{l}{\\textit{", labels[[scenario]], "}}\\\\*"))
  for (n in c(40L, 160L)) {
    for (view in c("late", "early", "full")) {
      values <- vapply(c("detected", "carry", "no_carry", "unresolved", "fit_problem"),
                       function(metric) percent_of(scenario, view, metric, n), "")
      last <- n == 160L && view == "full"
      row <- paste0(n, " & ", tools::toTitleCase(view), " & ", paste(values, collapse = " & "))
      table_rows <- c(table_rows, row)
      table_lines <- c(table_lines, paste0(row, " \\\\", if (last) "" else "*"))
    }
  }
}
writeLines(c(table_lines, "\\bottomrule", "\\end{longtable}"),
           file.path(path, "section5_table.tex"))

cat(sprintf("%s experiments, %s fits (%s not converged)\n", macros[["StudyExperiments"]],
            macros[["StudyFits"]], macros[["StudyFailedFits"]]))
cat(sprintf("N = 160, full records: carryover favored in %s%% (carried associability),",
            macros[["AssocFullLargeCarry"]]),
    sprintf("%s%% (value generalization), %s%% (transfer-only adaptation)\n",
            macros[["ValueFullLargeCarry"]], macros[["ResetFullLargeCarry"]]))
cat("Saved results/section5/summary.csv, summary_by_configuration.csv,",
    "section5_numbers.tex and section5_table.tex\n")

# Values reported in the paper, as printed
printed <- c(
  # Section 5.2, Figure 9
  ExampleContrast = ".212", ExampleLower = ".174", ExampleUpper = ".251",
  ExampleFixedValueCarry = ".70", ExampleResetValueCarry = ".50",
  ExampleCarryVsFixed = "20.5", ExampleCarryVsFixedSE = "5.9",
  ExampleCarryVsReset = "21.1", ExampleCarryVsResetSE = "5.1",
  # Section 5.3
  ProfileHalfLoss = "0.78", ProfileHalfRatio = "1.5", ProfileEtaFull = ".045",
  ProfileEtaHalf = ".082",
  # Section 5.4 and Supplement S2
  MimicRWBefore = "1.417", MimicRWAfter = "0.119", MimicResetFull = "0.143",
  MimicRWLate = "0.0019", MimicResetLate = "0.0015", MimicResetFirstTwo = "41",
  MimicLateGapMin = ".23", MimicLateGapMax = ".33",
  # Sections 5.5 and 7.2, Figure 10, Table S2
  StudyReplications = "400", StudyMaxMCSE = "2.5",
  AssocFullLargeDetected = "100.0", AssocFullLargeCarry = "59.5",
  AssocFullLargeUnresolved = "40.2", AssocConfigurationMin = "15.0",
  AssocFullLargeRankedCarry = "94.5", ValueFullLargeRankedCarryCount = "21",
  ValueFullLargeCarryCount = "0",
  AssocConfigurationMax = "91.0", AssocLateLargeCarry = "0.5", AssocLateLargeNoCarry = "21.2",
  AssocEarlyLargeCarry = "14.8", AssocLateSmallCarry = "0.5", AssocFullSmallCarry = "10.5",
  ValueFullLargeDetected = "100.0", ValueFullLargeCarry = "0.0", ValueFullLargeNoCarry = "41.8",
  ResetFullLargeDetected = "2.0", ResetFullLargeCarry = "0.2",
  # Section 5.6, Figure 11
  AssocOmittedCarry = "96.8", ValueOmittedCarry = "95.0", AssocInitialRejected = "1.5",
  ValueInitialRejected = "94.8", ValueWrongFlagged = "95.3",
  ResponseDetected = "100.0", ResponseSharedCarry = "7.2", ResponseSharedRankedCarry = "68.0",
  # Supplement S2
  StudyExperiments = "3,600", StudyFits = "67,200", StudyFailedFits = "0",
  StudyRefinedFits = "1,432", StudyProblemAnalyses = "0", StudyCoverageMin = "93.8",
  StudyCoverageMax = "97.5", NullFullLargeDetected = "3.2", NullFullLargeCarry = "0.0",
  ResponseFlexibleCarry = "0.0", BoundsRefitN = "24", BoundsRefitChanged = "0"
)
# Table S2, as the supplement prints it
printed_table <- c(
  # Carried associability
  "40 & Late & 85.8 & 0.5 & 25.0 & 74.5 & 0.0", "40 & Early & 87.2 & 5.2 & 7.5 & 87.2 & 0.0",
  "40 & Full & 96.5 & 10.5 & 3.2 & 86.2 & 0.0", "160 & Late & 99.2 & 0.5 & 21.2 & 78.2 & 0.0",
  "160 & Early & 99.8 & 14.8 & 1.8 & 83.5 & 0.0", "160 & Full & 100.0 & 59.5 & 0.2 & 40.2 & 0.0",
  # Transfer-only adaptation
  "40 & Late & 1.2 & 0.0 & 20.5 & 79.5 & 0.0", "40 & Early & 2.2 & 0.2 & 14.8 & 85.0 & 0.0",
  "40 & Full & 2.5 & 0.2 & 16.8 & 83.0 & 0.0", "160 & Late & 2.5 & 0.0 & 14.8 & 85.2 & 0.0",
  "160 & Early & 3.2 & 0.0 & 10.5 & 89.5 & 0.0", "160 & Full & 2.0 & 0.2 & 11.0 & 88.8 & 0.0",
  # Value generalization
  "40 & Late & 27.8 & 0.5 & 17.0 & 82.5 & 0.0", "40 & Early & 98.2 & 0.5 & 19.0 & 80.5 & 0.0",
  "40 & Full & 97.8 & 0.2 & 26.0 & 73.8 & 0.0", "160 & Late & 74.0 & 0.5 & 19.5 & 80.0 & 0.0",
  "160 & Early & 100.0 & 0.0 & 24.0 & 76.0 & 0.0", "160 & Full & 100.0 & 0.0 & 41.8 & 58.2 & 0.0",
  # Physical-cue differences alone
  "40 & Late & 2.0 & 0.2 & 22.0 & 77.8 & 0.0", "40 & Early & 1.2 & 0.0 & 15.5 & 84.5 & 0.0",
  "40 & Full & 1.2 & 0.0 & 11.5 & 88.5 & 0.0", "160 & Late & 2.2 & 0.5 & 21.0 & 78.5 & 0.0",
  "160 & Early & 3.8 & 0.5 & 13.0 & 86.5 & 0.0", "160 & Full & 3.2 & 0.0 & 15.5 & 84.5 & 0.0"
)
carried <- results[results$scenario == "associability" & results$n == 160 &
                     results$view == "full" & results$policy == "value_allowed", ]
carried_effect <- tapply(carried$true_effect, carried$config, mean)
# D's associability at the end of compound training under the adaptive
# generating processes, for each configuration, cue allocation and rate type:
# D and E learn at different rates when d is not 0.
control_training <- c(make_trials("C", 1, 10, "Phase 1"), make_trials(c("D", "E"), 1, 10, "Phase 2"))
alpha_D <- unlist(lapply(which(generating$adaptive), function(i) {
  g <- generating[i, ]
  unlist(lapply(c(-1, 1), function(allocation) lapply(c(-1, 1), function(type) {
    rates <- .5 * plogis(g$mu + type * g$spread + c(C = 0, D = -allocation, E = 0) * g$cue)
    adaptive_final_state(control_training, S = 2 * rates, alpha_init = c(C = .5, D = .5, E = .5),
                         eta = g$eta)$alpha[["D"]]
  })))
}))
# The fits to the records with a response-function difference (Section 5.6),
# with old-value carryover estimated.
response_fits <- merge(read.csv(file.path(path, "parameters.csv")), jobs[c("id", "scenario")],
                       by = "id")
response_fits <- response_fits[response_fits$scenario == "response" &
                                 response_fits$policy == "value_allowed", ]
response_candidate <- sub("_.*", "", response_fits$fit)
median_g_value <- tapply(response_fits$g_value, response_candidate, median)
stopifnot(
  macros[names(printed)] == printed,
  identical(table_rows, printed_table),
  # Supplement S2: with wider bounds, the candidate ranked first without the
  # margin changes for one dataset
  sum(bounds_refits$original_winner != bounds_refits$wider_winner) == 1,
  # Supplement S2: in the profile, eta is at its upper bound of .20 when g_alpha
  # is 0 or .1, and below it from g_alpha = .2 on
  all(abs(profile$eta[profile$g_alpha < .15] - .20) < 1e-8),
  all(profile$eta[profile$g_alpha > .15] < .20 - 1e-3),
  # Section 5.5: the lowest rate is in the configuration with the weakest carried
  # effect and the highest in the one with the strongest
  associability_rates$config[which.min(associability_rates$carry)] ==
    names(which.min(carried_effect)),
  associability_rates$config[which.max(associability_rates$carry)] ==
    names(which.max(carried_effect)),
  # Section 5.5: with late responses, a candidate without associability
  # carryover is favored more often than carryover
  as.numeric(percent_of("associability", "late", "no_carry")) >
    as.numeric(percent_of("associability", "late", "carry")),
  # Supplement S2: the one-sided 95% upper bound when no event occurs in 400
  # experiments
  abs(1 - .05^(1 / 400) - .0075) < 5e-5,
  # Section 5.5: under transfer-only adaptation, as under physical-cue
  # differences alone, the expected response contrast is zero (to rounding
  # error in its sum over the cue allocations)
  all(abs(results$true_effect[results$scenario %in% c("reset", "null")]) < 1e-15),
  # Section 5.6: the fixed-rate and reset candidates account for the response
  # difference through old learned value, the carryover candidate mainly
  # through retained associability
  all(median_g_value[c("fixed", "reset")] > .3), median_g_value[["carry"]] < .1,
  median(response_fits$g_alpha[response_candidate == "carry"]) > .3,
  # Supplement S2: D's associability after compound training lies between .477
  # and .524
  sprintf("%.3f", range(alpha_D)) == c("0.477", "0.524"),
  # Supplement S2: the check of the starting vectors (192 fits, every one
  # within 1e-5 of the reference; the four fixed vectors alone short by more
  # than .001 in 8)
  nrow(starting_values) == 192, all(starting_values$study_converged),
  max(starting_values$study_nll - starting_values$reference_nll) < 1e-5,
  sum(starting_values$fixed_nll - starting_values$reference_nll > .001) == 8
)
