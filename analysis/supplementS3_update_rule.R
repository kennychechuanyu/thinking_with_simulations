# Supplement S3: the comparison of main-text Sections 3.4 and 4 under the rule
# for associability of main-text Equation 3 and under rank-based rules, which
# move associability a fixed fraction of the way toward 1 or 0 according to
# which cue makes the smaller error (Equation S6). Computes Table S3, the values
# in the text of Supplement S3 and those of main-text Section 4.4 and Table 5,
# and draws Figure S3; the data of Figure S3 and of the comparison with
# Rescorla-Wagner when learned values carry over go to
# results/supplementS3_update_rule.csv.
#   Rscript analysis/supplementS3_update_rule.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of main-text Sections 3 and 4; the salience of C varies in Figure
# S3b and Table 5. The labels of the two rules in the CSV file are
# "Error-difference (main text)" for Equation 3 and "Rank-based" for Equation
# S6.
lambda <- 1
salience <- .3
alpha_init <- .5
eta <- .05           # rate of Equation 3
alpha_min <- .01
alpha_max <- 1
rw_beta <- .12       # learning rate of every cue under Rescorla-Wagner (Equation 1)
n_phase1 <- 10       # A+ or X+
n_compound <- 10     # AB+ or CD+
n_pear <- 10         # B and D alone with the pear
k_compared <- 8      # pear trials after which the gaps are compared
rule_labels <- c("Error-difference (main text)", "Rank-based")

# The rules for associability. e_own is the cue's error e_i = |lambda - V_i|
# and e_other is e_-i, the error of the other cues' summed prediction (|lambda|
# for a cue presented alone), both from the start of the trial; rate is eta
# under Equation 3 and rho under the rank-based rules. Under the rank-based
# rules, errors closer than sqrt(machine epsilon), about 1.5e-8, count as equal.
# Equal errors leave associability unchanged under Equation S6 and lower it
# under the third rule of main-text Table 5, as Mackintosh (1975b) proposed.
tie_tolerance <- sqrt(.Machine$double.eps)
update_error_difference <- function(alpha, e_own, e_other, rate) {    # Equation 3
  alpha + rate * (e_other - e_own)
}
update_rank_based <- function(alpha, e_own, e_other, rate) {          # Equation S6
  if (e_own + tie_tolerance < e_other) alpha + rate * (1 - alpha)
  else if (e_other + tie_tolerance < e_own) alpha * (1 - rate)
  else alpha
}
update_rank_ties_lower <- function(alpha, e_own, e_other, rate) {
  if (e_own + tie_tolerance < e_other) alpha + rate * (1 - alpha)
  else alpha * (1 - rate)
}

# Trials of the adaptive model with any of these rules, each followed by the
# outcome; each element of trials holds the cues present. Each cue learns from
# its own error (main-text Equation 2), and associability stays within
# [alpha_min, alpha_max]. With Equation 3, the results are checked against
# adaptive_simulate() below.
run_trials <- function(trials, V, alpha, S, update, rate) {
  for (cues in trials) {
    V_start <- V
    for (i in cues) V[i] <- V[i] + S[i] * alpha[i] * (lambda - V_start[i])
    for (i in cues) {
      others <- setdiff(cues, i)
      e_own <- abs(lambda - V_start[i])
      e_other <- abs(lambda - if (length(others) > 0) sum(V_start[others]) else 0)
      alpha[i] <- max(alpha_min, min(alpha_max, update(alpha[i], e_own, e_other, rate)))
    }
  }
  list(V = V, alpha = alpha)
}

# The training of main-text Section 3, A+ then AB+ for B (the light that
# appeared with the trained tone) and X+ then CD+ for D (the comparison light),
# followed by B and D each trained alone with the pear. Learned values for the
# pear start at g_V times those for the apple (main-text Equation 5), and
# associability keeps a fraction g_alpha of its change from alpha_init
# (Equation 4). Returns the states after the compound trials, B's and D's
# learned values for the pear after k = 1, ..., n pear trials, the B-D gap
# V_D(k) - V_B(k), and the associabilities after k = 0, ..., n pear trials.
run_comparison <- function(update, rate, S_C = salience, g_V = 0, g_alpha = 1,
                           n = n_pear) {
  S <- c(A = salience, B = salience, X = salience, C = S_C, D = salience)
  blocking <- run_trials(rep(list("A"), n_phase1), c(A = 0, B = 0),
                         c(A = alpha_init, B = alpha_init), S, update, rate)
  blocking <- run_trials(rep(list(c("A", "B")), n_compound), blocking$V, blocking$alpha,
                         S, update, rate)
  control <- run_trials(rep(list("X"), n_phase1), c(X = 0, C = 0, D = 0),
                        c(X = alpha_init, C = alpha_init, D = alpha_init), S, update, rate)
  control <- run_trials(rep(list(c("C", "D")), n_compound), control$V, control$alpha,
                        S, update, rate)
  B <- list(V = c(B = g_V * blocking$V[["B"]]),
            alpha = c(B = alpha_init + g_alpha * (blocking$alpha[["B"]] - alpha_init)))
  D <- list(V = c(D = g_V * control$V[["D"]]),
            alpha = c(D = alpha_init + g_alpha * (control$alpha[["D"]] - alpha_init)))
  V_B_pear <- V_D_pear <- numeric(n)
  alpha_B_pear <- c(B$alpha[["B"]], numeric(n))
  alpha_D_pear <- c(D$alpha[["D"]], numeric(n))
  for (k in seq_len(n)) {
    B <- run_trials(list("B"), B$V, B$alpha, S, update, rate)
    D <- run_trials(list("D"), D$V, D$alpha, S, update, rate)
    V_B_pear[k] <- B$V
    V_D_pear[k] <- D$V
    alpha_B_pear[k + 1] <- B$alpha
    alpha_D_pear[k + 1] <- D$alpha
  }
  list(V_B = blocking$V[["B"]], V_D = control$V[["D"]],
       alpha_B = blocking$alpha[["B"]], alpha_D = control$alpha[["D"]],
       V_B_pear = V_B_pear, V_D_pear = V_D_pear, H = V_D_pear - V_B_pear,
       alpha_B_pear = alpha_B_pear, alpha_D_pear = alpha_D_pear)
}

# Number of updates that the bounds [alpha_min, alpha_max] clip in the run of
# Table S3.
count_clipped <- function(update, rate) {
  clipped <- 0L
  counted_update <- function(alpha, e_own, e_other, rate) {
    new_alpha <- update(alpha, e_own, e_other, rate)
    if (new_alpha < alpha_min || new_alpha > alpha_max) clipped <<- clipped + 1L
    new_alpha
  }
  run_comparison(counted_update, rate)
  clipped
}


# Under Equation 3, the comparison reproduces adaptive_simulate() of R/models.R:
# the states after the compound trials and the learned values for the pear
# after each pear trial. Its largest B-D gap is the .284 of main-text Section
# 3.4.
error_difference <- run_comparison(update_error_difference, eta)
reference <- function(first, pair, cue) {
  cues <- unique(c(first, pair))
  training <- adaptive_simulate(c(make_trials(first, lambda, n_phase1, "Phase 1"),
                                  make_trials(pair, lambda, n_compound, "Phase 2")),
                                S = setNames(rep(salience, length(cues)), cues),
                                alpha_init = setNames(rep(alpha_init, length(cues)), cues),
                                eta = eta, alpha_min = alpha_min, alpha_max = alpha_max)
  pear <- adaptive_simulate(make_trials(cue, lambda, n_pear, "Pear"),
                            S = setNames(salience, cue),
                            alpha_init = extract_final_alpha(training)[cue], eta = eta,
                            V_init = setNames(0, cue), cue_names = cue,
                            alpha_min = alpha_min, alpha_max = alpha_max)
  list(V = extract_final_V(training)[[cue]], alpha = extract_final_alpha(training)[[cue]],
       V_pear = pear$V[-1])
}
reference_B <- reference("A", c("A", "B"), "B")
reference_D <- reference("X", c("C", "D"), "D")
stopifnot(abs(error_difference$V_B - reference_B$V) < 1e-12,
          abs(error_difference$V_D - reference_D$V) < 1e-12,
          abs(error_difference$alpha_B - reference_B$alpha) < 1e-12,
          abs(error_difference$alpha_D - reference_D$alpha) < 1e-12,
          abs(error_difference$V_B_pear - reference_B$V_pear) < 1e-12,
          abs(error_difference$V_D_pear - reference_D$V_pear) < 1e-12,
          abs(error_difference$H[k_compared] - .284317) < 5e-6)

# rho of the rank-based rule is set so that B ends the compound trials with the
# same associability as under Equation 3; D keeps alpha_init under both. The
# gap after the first pear trial depends only on alpha_D - alpha_B, so what
# differs afterward comes from the rules themselves.
rho <- uniroot(function(rate) {
  run_comparison(update_rank_based, rate)$alpha_B - error_difference$alpha_B
}, c(1e-4, .9), tol = 1e-10)$root
rank_based <- run_comparison(update_rank_based, rho)
peak <- c(max(error_difference$H), max(rank_based$H))
peak_at <- c(which.max(error_difference$H), which.max(rank_based$H))
H_10 <- c(error_difference$H[10], rank_based$H[10])
percent_below_peak <- round(100 * (1 - H_10 / peak))
clipped <- c(count_clipped(update_error_difference, eta),
             count_clipped(update_rank_based, rho))

# D's lead in associability, alpha_D - alpha_B, after k = 0, ..., 10 pear
# trials, and the rise of each light's associability under Equation 3.
lead_error_difference <- error_difference$alpha_D_pear - error_difference$alpha_B_pear
lead_rank_based <- rank_based$alpha_D_pear - rank_based$alpha_B_pear
rise_B <- diff(error_difference$alpha_B_pear)
rise_D <- diff(error_difference$alpha_D_pear)

# Associability reset before the pear trials (g_alpha = 0).
reset_H <- cbind(
  run_comparison(update_error_difference, eta, g_alpha = 0)$H,
  run_comparison(update_rank_based, rho, g_alpha = 0)$H)

# Carried learned value, with associability carried over in full: the adaptive
# model's gap after eight pear trials minus that of Rescorla-Wagner. Under
# Rescorla-Wagner, B and D start the pear trials g_V times their learned values
# for the apple apart, and the gap shrinks by 12% with each pear trial
# (main-text Section 4.2).
rw_blocking <- rw_final(c(make_trials("A", lambda, n_phase1, "Phase 1"),
                          make_trials(c("A", "B"), lambda, n_compound, "Phase 2")),
                        beta = c(A = rw_beta, B = rw_beta))
rw_control <- rw_final(c(make_trials("X", lambda, n_phase1, "Phase 1"),
                         make_trials(c("C", "D"), lambda, n_compound, "Phase 2")),
                       beta = c(X = rw_beta, C = rw_beta, D = rw_beta))
H_rw <- function(g_V, k) {
  g_V * (rw_control[["D"]] - rw_blocking[["B"]]) * (1 - rw_beta)^k
}
H_adaptive_compared <- function(g_V) {
  c(run_comparison(update_error_difference, eta, g_V = g_V)$H[k_compared],
    run_comparison(update_rank_based, rho, g_V = g_V)$H[k_compared])
}
g_V_grid <- seq(0, 1, by = .05)
carryover <- do.call(rbind, lapply(g_V_grid, function(g_V) {
  data.frame(g_V = g_V, rule = rule_labels,
             delta_H8 = H_adaptive_compared(g_V) - H_rw(g_V, k = k_compared))
}))
delta_H8_full_carryover <- carryover$delta_H8[carryover$g_V == 1]
# How much the adaptive model's gap after eight pear trials changes between
# g_V = 0 and g_V = 1e-6. Under the rank-based rule, any g_V above about 2e-8
# (the tie tolerance divided by the light's learned value for the apple) makes
# its error on the first pear trial smaller than the error of predicting zero
# by more than the tolerance.
change_at_zero <- H_adaptive_compared(1e-6) - H_adaptive_compared(0)

# D's associability after the compound trials when its companion C is more
# salient than D by 1e-8 to 1e-1 (Figure S3b), and at the differences S_C - S_D
# quoted in the text, where a negative difference makes C less salient than D.
alpha_D_given_S_C <- function(S_C) {
  c(run_comparison(update_error_difference, eta, S_C = S_C)$alpha_D,
    run_comparison(update_rank_based, rho, S_C = S_C)$alpha_D)
}
salience_difference <- 10^seq(-8, -1, by = .25)
comparison_light <- do.call(rbind, lapply(salience_difference, function(d) {
  data.frame(salience_difference = d, rule = rule_labels,
             alpha_D = alpha_D_given_S_C(salience + d))
}))
rank_based_curve <- comparison_light$alpha_D[comparison_light$rule == rule_labels[2]]
quoted_differences <- c("1e-6" = 1e-6, ".01" = .01, ".10" = .10, "-1e-6" = -1e-6, "-.01" = -.01)
alpha_D_quoted <- sapply(quoted_differences, function(d) alpha_D_given_S_C(salience + d))
rownames(alpha_D_quoted) <- c("error_difference", "rank_based")

# Main-text Table 5 and Section 4.4: D's associability after the compound trials
# and the largest B-D gap during fifteen pear trials under Equation 3, Equation
# S6 and the rank-based rule under which equal errors lower associability, with
# C as salient as D, more salient by one part in a million, or more salient by
# one part in 100. Relative to S_D = .30, these are absolute differences of
# 3e-7 and .003.
relative_differences <- c(equal = 0, part_in_1e6 = 1e-6, part_in_100 = .01)
rules <- list(error_difference = list(update_error_difference, eta),
              rank_based = list(update_rank_based, rho),
              ties_lower = list(update_rank_ties_lower, rho))
table_5 <- do.call(rbind, lapply(names(rules), function(rule) {
  do.call(rbind, lapply(names(relative_differences), function(difference) {
    run <- run_comparison(rules[[rule]][[1]], rules[[rule]][[2]],
                          S_C = salience * (1 + relative_differences[[difference]]),
                          n = 15)
    data.frame(rule = rule, difference = difference, alpha_B = run$alpha_B,
               alpha_D = run$alpha_D, V_B = run$V_B, V_D = run$V_D,
               largest_gap = max(run$H), after = which.max(run$H),
               largest_absolute_gap = max(abs(run$H)), H8 = run$H[k_compared])
  }))
}))
table_5_rule <- function(rule) table_5[table_5$rule == rule, ]

# The lowest associability reached in any run with Equation 3 or Equation S6:
# B's after the compound trials, so neither lower bound (.01 here, .1 in Le
# Pelley et al., 2016) is reached.
lowest_associability <- function(update, rate, ...) {
  lowest <- 1
  tracked_update <- function(alpha, e_own, e_other, rate) {
    new_alpha <- update(alpha, e_own, e_other, rate)
    lowest <<- min(lowest, new_alpha)
    new_alpha
  }
  run_comparison(tracked_update, rate, ...)
  lowest
}
run_settings <- c(list(list(), list(g_alpha = 0)), lapply(g_V_grid, function(g_V) list(g_V = g_V)),
                  lapply(c(salience_difference, quoted_differences),
                         function(d) list(S_C = salience + d)),
                  lapply(relative_differences,
                         function(d) list(S_C = salience * (1 + d), n = 15)))
lowest <- min(sapply(run_settings, function(setting) c(
  do.call(lowest_associability, c(list(update_error_difference, eta), setting)),
  do.call(lowest_associability, c(list(update_rank_based, rho), setting)))))


# In the CSV, x is k for the B-D gap after k pear trials (H_by_k, Figure S3a),
# g_V for the adaptive minus the Rescorla-Wagner gap after eight pear trials
# (delta_H8_by_g_V), and S_C - S_D for D's associability after the compound
# trials (alpha_D_by_S_C_minus_S_D, Figure S3b); value holds these quantities.
gap_by_pear_trial <- rbind(
  data.frame(k = 1:n_pear, H = error_difference$H, rule = rule_labels[1]),
  data.frame(k = 1:n_pear, H = rank_based$H, rule = rule_labels[2]))
update_rule_data <- rbind(
  data.frame(what = "H_by_k", rule = gap_by_pear_trial$rule, x = gap_by_pear_trial$k,
             value = gap_by_pear_trial$H),
  data.frame(what = "delta_H8_by_g_V", rule = carryover$rule, x = carryover$g_V,
             value = carryover$delta_H8),
  data.frame(what = "alpha_D_by_S_C_minus_S_D", rule = comparison_light$rule,
             x = comparison_light$salience_difference, value = comparison_light$alpha_D))


# Figure S3: the B-D gap during the pear trials, and D's associability after
# the compound trials when its companion C is more salient, under Equation 3
# and Equation S6, drawn from the values saved in the CSV file. The figure
# names the rule of Equation 3 by its equation.
ink <- paper_colours[["ink"]]
light <- paper_colours[["light"]]
theme_figure <- theme_paper() + theme(axis.title.x = element_text(margin = margin(t = 6)))
rule_names <- c("Error-difference (main text)" = "Equation 3", "Rank-based" = "Rank-based rule")
figure_data <- update_rule_data
figure_data$rule_name <- factor(rule_names[figure_data$rule], levels = rule_names)
gap <- subset(figure_data, what == "H_by_k")
control <- subset(figure_data, what == "alpha_D_by_S_C_minus_S_D")
line_types <- c("Equation 3" = "solid", "Rank-based rule" = "22")

panel_gap <- ggplot(gap, aes(x, value, group = rule_name)) +
  geom_line(aes(linetype = rule_name), colour = ink, linewidth = .5) +
  geom_point(colour = ink, size = 1.3) +
  annotate("text", x = 7, y = .298, label = "Equation 3", hjust = .5, vjust = 0,
           size = text_size, colour = ink) +
  annotate("text", x = 10, y = .128, label = "Rank-based rule", hjust = 1, vjust = 1,
           size = text_size, colour = ink) +
  scale_linetype_manual(values = line_types) +
  scale_x_continuous(breaks = c(1, 5, 10)) +
  scale_y_continuous(labels = axis_number) +
  labs(x = expression("Completed pear trials, " * italic(k)), y = "B–D gap",
       title = "(a)  The B–D gap") +
  theme_figure

panel_control <- ggplot(control, aes(x, value, group = rule_name)) +
  geom_hline(yintercept = .5, colour = light, linewidth = .3) +
  geom_line(aes(linetype = rule_name), colour = ink, linewidth = .5) +
  annotate("text", x = 1e-5, y = .515, label = "Equation 3", hjust = .5, vjust = 0,
           size = text_size, colour = ink) +
  annotate("text", x = 1e-5, y = .27, label = "Rank-based rule", hjust = .5, vjust = 0,
           size = text_size, colour = ink) +
  scale_linetype_manual(values = line_types) +
  scale_x_log10(breaks = 10^seq(-8, -2, by = 2),
                labels = function(x) parse(text = sprintf("10^'−%d'", -round(log10(x))))) +
  scale_y_continuous(limits = c(.2, .56), breaks = c(.2, .3, .4, .5), labels = axis_number) +
  labs(x = expression(italic(S)[C] * " − " * italic(S)[D] * ", with " * italic(S)[D] * " = .30"),
       y = "D's associability",
       title = "(b)  C more salient than D") +
  theme_figure
equation3_curve <- control[control$rule_name == "Equation 3", c("x", "value")]


# Each line gives the values under Equation 3 first and under Equation S6
# second.
listed <- function(x) paste(paper_number(x, 3), collapse = ", ")
cat("Table S3, Equation 3 and the rank-based rule (Equation S6)\n")
cat(sprintf("  eta and rho: %s and %s\n", paper_number(eta, 2), listed(rho)))
cat(sprintf("  V_B, V_D after the compound trials: %s and %s\n",
            listed(c(error_difference$V_B, error_difference$V_D)),
            listed(c(rank_based$V_B, rank_based$V_D))))
cat(sprintf("  alpha_B, alpha_D after the compound trials: %s and %s\n",
            listed(c(error_difference$alpha_B, error_difference$alpha_D)),
            listed(c(rank_based$alpha_B, rank_based$alpha_D))))
cat(sprintf("  Largest B-D gap: %s after %d and %s after %d pear trials\n",
            listed(peak[1]), peak_at[1], listed(peak[2]), peak_at[2]))
cat(sprintf("  B-D gap after ten pear trials: %s and %s, %g%% and %g%% below the largest\n",
            listed(H_10[1]), listed(H_10[2]), percent_below_peak[1], percent_below_peak[2]))
cat(sprintf("  B-D gap with associability reset before the pear trials: %g and %g\n",
            max(abs(reset_H[, 1])), max(abs(reset_H[, 2]))))
cat("Text of Supplement S3\n")
cat(sprintf("  rho: %s\n", paper_number(rho, 4)))
cat(sprintf("  B-D gap after 1-10 pear trials: %s\n                             and %s\n",
            listed(error_difference$H), listed(rank_based$H)))
cat(sprintf("  Updates clipped by the bounds [.01, 1]: %d and %d\n", clipped[1], clipped[2]))
cat(sprintf("  Adaptive minus Rescorla-Wagner gap after eight pear trials, g_V = 1: %s and %s\n",
            listed(delta_H8_full_carryover[1]), listed(delta_H8_full_carryover[2])))
cat(sprintf(paste("  Change in the adaptive gap after eight pear trials from g_V = 0 to 1e-6:",
                  "%.2g and %s\n"),
            change_at_zero[1], paper_number(change_at_zero[2], 3)))
cat(sprintf("  alpha_D with S_C - S_D = 1e-6, .01, .10: %s and %s\n",
            listed(alpha_D_quoted[1, 1:3]), listed(alpha_D_quoted[2, 1:3])))
cat(sprintf("  alpha_D with S_C - S_D = -1e-6, -.01: %s and %s\n",
            listed(alpha_D_quoted[1, 4:5]), listed(alpha_D_quoted[2, 4:5])))
cat(sprintf("  Lowest associability in any run: %s\n", paper_number(lowest, 3)))
cat("Table 5 and Section 4.4, with C as salient as D, more salient by one part in a million\n")
cat("and by one part in 100: alpha_D, and the largest B-D gap during fifteen pear trials\n")
rule_rows <- c(error_difference = "Equation 3", rank_based = "Rank-based, ties unchanged",
               ties_lower = "Rank-based, ties lower associability")
for (rule in names(rules)) {
  row <- table_5_rule(rule)
  cat(sprintf("  %s: %s; %s\n", rule_rows[[rule]], listed(row$alpha_D),
              paste(sprintf("%s (%d)", paper_number(row$largest_gap, 3), row$after),
                    collapse = ", ")))
}

save_figure(panel_gap | panel_control, "figS3_update_rule", 6.27, 3.2)
write.csv(update_rule_data, "results/supplementS3_update_rule.csv", row.names = FALSE)
cat("Saved results/supplementS3_update_rule.csv\n")

# The one update that the bounds clip under Equation 3 is A's associability
# after the last compound trial, which no later update uses: without the upper
# bound, the states and gaps are the same.
alpha_max <- Inf
unbounded <- run_comparison(update_error_difference, eta)
unbounded_A <- adaptive_simulate(c(make_trials("A", lambda, n_phase1, "Phase 1"),
                                   make_trials(c("A", "B"), lambda, n_compound, "Phase 2")),
                                 S = c(A = salience, B = salience),
                                 alpha_init = c(A = alpha_init, B = alpha_init), eta = eta,
                                 alpha_max = alpha_max)
alpha_max <- 1
above_bound <- unbounded_A[unbounded_A$alpha > alpha_max, c("cue", "trial")]

# Values reported in the paper
error_difference_rule <- table_5_rule("error_difference")
rank_based_rule <- table_5_rule("rank_based")
ties_lower_rule <- table_5_rule("ties_lower")
stopifnot(
  identical(unbounded, error_difference),                                  # Supplement S3
  nrow(above_bound) == 1, above_bound$cue == "A",                          # Supplement S3
  above_bound$trial == n_phase1 + n_compound + 1,                          # Supplement S3
  abs(rho - .073) < 5e-4,                                                  # Table S3
  abs(rho - .0726) < 5e-5,                                                 # Supplement S3
  abs(rank_based$alpha_B - alpha_init * (1 - rho)^n_compound) < 1e-12,     # Supplement S3
  abs(c(error_difference$V_B, rank_based$V_B) - c(.681, .687)) < 5e-4,     # Table S3
  abs(c(error_difference$V_D, rank_based$V_D) - .803) < 5e-4,              # Table S3
  abs(c(error_difference$alpha_B, rank_based$alpha_B) - .235) < 5e-4,      # Table S3
  abs(c(error_difference$alpha_D, rank_based$alpha_D) - .500) < 5e-4,      # Table S3
  peak_at == c(8, 5),                                                      # Table S3
  abs(peak - c(.284, .213)) < 5e-4,                                        # Table S3
  abs(H_10 - c(.272, .140)) < 5e-4,                                        # Table S3
  reset_H == 0,                                                            # Table S3
  abs(c(error_difference$H[1], rank_based$H[1]) - .079) < 5e-4,            # Supplement S3
  clipped == c(1, 0),                                                      # Supplement S3
  abs(lowest - error_difference$alpha_B) < 1e-12,                          # Supplement S3
  sprintf("%.1e", tie_tolerance) == "1.5e-08",                             # Supplement S3
  # Supplement S3: B ends with lower associability than D by the same amount
  # under both rules
  abs(rank_based$alpha_B - error_difference$alpha_B) < 1e-8,
  rank_based$alpha_D == error_difference$alpha_D,
  # Supplement S3: under the rank-based rule, D's lead in associability stays
  # the same on the first pear trial and shrinks by the factor 1 - rho on each
  # later one; under Equation 3, a light's associability rises by eta times its
  # learned value for the pear, so D's lead grows during the ten pear trials
  # and the gap narrows only after the eighth.
  lead_rank_based[2] == lead_rank_based[1],
  abs(lead_rank_based[3:11] - (1 - rho) * lead_rank_based[2:10]) < 1e-12,
  abs(rise_B - eta * c(0, error_difference$V_B_pear[-n_pear])) < 1e-12,
  abs(rise_D - eta * c(0, error_difference$V_D_pear[-n_pear])) < 1e-12,
  diff(lead_error_difference)[1] == 0, diff(lead_error_difference)[-1] > 0,
  diff(error_difference$H[1:8]) > 0, diff(error_difference$H[8:10]) < 0,
  # Supplement S3: with learned values carried over, Rescorla-Wagner also
  # produces a gap, and with full carryover its gap after eight pear trials
  # exceeds the adaptive model's by .029 and .050.
  H_rw(g_V_grid[-1], k_compared) > 0,
  delta_H8_full_carryover < 0,                                             # Supplement S3
  abs(delta_H8_full_carryover - c(-.029, -.050)) < 5e-4,                   # Supplement S3
  abs(change_at_zero[1]) < 1e-6, abs(change_at_zero[2] + .024) < 5e-4,     # Supplement S3
  percent_below_peak == c(4, 34),                     # Supplement S3: 4% and about a third
  abs(rank_based_curve[1] - alpha_init) < 1e-12,                           # Supplement S3
  abs(rank_based_curve[salience_difference > 3e-8] -
        .5 * (1 - rho)^9) < 1e-12,                                         # Supplement S3
  # Supplement S3: in between, alpha_D falls part of the way
  rank_based_curve[2] > .5 * (1 - rho)^9, rank_based_curve[2] < alpha_init,
  # Supplement S3: the rank-based tie at g_V = 0 breaks above about 2e-8
  abs(tie_tolerance / c(rank_based$V_B, rank_based$V_D) - 2e-8) < .5e-8,
  abs(alpha_D_quoted["rank_based", c("1e-6", "-1e-6")] -
        c(.254, .746)) < 5e-4,                                             # Supplement S3
  abs(alpha_D_quoted["error_difference", c(".01", ".10", "-.01")] -
        c(.495, .452, .506)) < 5e-4,                                       # Supplement S3
  abs(alpha_D_quoted["error_difference", c("1e-6", "-1e-6")] - .5) < 1e-6, # Supplement S3
  # Supplement S3: when C is less salient than D, D's associability rises under
  # both rules
  alpha_D_quoted[, c("-1e-6", "-.01")] > alpha_init,
  # Figure S3b: under Equation 3, D stays within .0005 of .500 up to a
  # difference of 1e-6, and both curves lie within the panel
  abs(equation3_curve$value[equation3_curve$x <= 1e-6] - .5) < 5e-4,
  control$value > .2 & control$value < .56,
  # Supplement S3: one part in a million and one part in 100 of S_D are 3e-7
  # and .003
  abs(salience * relative_differences[-1] - c(3e-7, .003)) < 1e-15,
  # Table 5: under every rule, B ends the compound trials at about .235
  paper_number(table_5$alpha_B, 3) == ".235",
  # Table 5: under Equation 3, D keeps .500 and the largest gap is .284 after
  # eight pear trials, whether C is as salient as D or more salient by one part
  # in a million; Section 4.4: with C 1% more salient, D ends at about .498, and
  # the gap after eight pear trials changes by about .001
  paper_number(error_difference_rule$alpha_D, 3) == c(".500", ".500", ".498"),
  paper_number(error_difference_rule$largest_gap[1:2], 3) == ".284",
  error_difference_rule$after[1:2] == 8,
  paper_number(abs(error_difference_rule$H8[3] - error_difference_rule$H8[1]), 3) == ".001",
  # Table 5 and Section 4.4: under the rank-based rule, D keeps .500 only when
  # C is as salient as D and otherwise falls to .254, whether C is more salient
  # by one part in a million or by 1%; the largest gap is .213 after five pear
  # trials and otherwise only .017, after six (Supplement S3)
  paper_number(rank_based_rule$alpha_D, 3) == c(".500", ".254", ".254"),
  paper_number(rank_based_rule$largest_gap, 3) == c(".213", ".017", ".017"),
  rank_based_rule$after == c(5, 6, 6),
  # Table 5 and Supplement S3: when equal errors lower associability, D ends
  # the compound trials with B's associability of .235 and B's learned value
  # for the apple, so the B-D gap is zero after every pear trial
  paper_number(ties_lower_rule$alpha_D, 3) == ".235",
  ties_lower_rule$alpha_D == ties_lower_rule$alpha_B,
  ties_lower_rule$V_D == ties_lower_rule$V_B,
  ties_lower_rule$largest_absolute_gap == 0
)
