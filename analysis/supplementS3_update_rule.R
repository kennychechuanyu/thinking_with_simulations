# Supplement S3: the transfer comparison of Sections 3 and 4 under the
# error-difference update of Equation 8 and under a rank-based update
# (Equation S4), which moves associability a fixed fraction of the way toward 1
# or 0 according to which cue makes the smaller error. Computes Table S3 and the
# values in the text and draws Figure S3; the data of Figure S3 and of the
# value-carryover comparison go to results/supplementS3_update_rule.csv.
#   Rscript analysis/supplementS3_update_rule.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of Sections 3 and 4; the salience of E varies in Figure S3b.
lambda <- 1
salience <- .3
alpha_init <- .5
eta_error_difference <- .05
alpha_min <- .01
alpha_max <- 1
rw_alpha <- .4       # Rescorla-Wagner
rw_beta <- .3
n_phase1 <- 10       # A+ or C+
n_compound <- 10     # AB+ or DE+
n_transfer <- 10     # B and D alone with the new outcome
k_compared <- 8      # updates after which the gaps are compared (Equation 18)
rule_labels <- c("Error-difference (main text)", "Rank-based")

# The two associability updates. e_own is the cue's error e_i = |lambda - V_i|
# and e_other is e_-i, the error of the other cues' summed prediction (|lambda|
# for a cue presented alone), both from the start of the trial. Under the
# rank-based update, errors closer than sqrt(machine epsilon), about 1.5e-8,
# count as equal.
tie_tolerance <- sqrt(.Machine$double.eps)
update_error_difference <- function(alpha, e_own, e_other, eta) {     # Equation 8
  alpha + eta * (e_other - e_own)
}
update_rank_based <- function(alpha, e_own, e_other, eta) {           # Equation S4
  if (e_own + tie_tolerance < e_other) alpha + eta * (1 - alpha)
  else if (e_other + tie_tolerance < e_own) alpha * (1 - eta)
  else alpha
}

# Reinforced trials of the adaptive model with either update; each element of
# trials holds the cues present. Each cue learns from its own error (Equation
# 7), and associability stays within [alpha_min, alpha_max]. The
# error-difference branch is checked against adaptive_simulate() below.
run_trials <- function(trials, V, alpha, S, update, eta) {
  for (cues in trials) {
    V_start <- V
    for (i in cues) V[i] <- V[i] + S[i] * alpha[i] * (lambda - V_start[i])
    for (i in cues) {
      others <- setdiff(cues, i)
      e_own <- abs(lambda - V_start[i])
      e_other <- abs(lambda - if (length(others) > 0) sum(V_start[others]) else 0)
      alpha[i] <- max(alpha_min, min(alpha_max, update(alpha[i], e_own, e_other, eta)))
    }
  }
  list(V = V, alpha = alpha)
}

# The blocking and control conditions and the transfer phase, in which B and D
# are each trained alone with the new outcome. Values for the new outcome start
# at g_V times the old values (Equation 16), and associability keeps a fraction
# g_alpha of its change from alpha_init (Equation 14). Returns the states after
# compound training and the B-D gap H(k) after k = 1-10 transfer updates.
transfer_experiment <- function(update, eta, S_E = salience, g_V = 0, g_alpha = 1) {
  S <- c(A = salience, B = salience, C = salience, D = salience, E = S_E)
  blocking <- run_trials(rep(list("A"), n_phase1), c(A = 0, B = 0),
                         c(A = alpha_init, B = alpha_init), S, update, eta)
  blocking <- run_trials(rep(list(c("A", "B")), n_compound), blocking$V, blocking$alpha,
                         S, update, eta)
  control <- run_trials(rep(list("C"), n_phase1), c(C = 0, D = 0, E = 0),
                        c(C = alpha_init, D = alpha_init, E = alpha_init), S, update, eta)
  control <- run_trials(rep(list(c("D", "E")), n_compound), control$V, control$alpha,
                        S, update, eta)
  B <- list(V = c(B = g_V * blocking$V[["B"]]),
            alpha = c(B = alpha_init + g_alpha * (blocking$alpha[["B"]] - alpha_init)))
  D <- list(V = c(D = g_V * control$V[["D"]]),
            alpha = c(D = alpha_init + g_alpha * (control$alpha[["D"]] - alpha_init)))
  V_B_O2 <- V_D_O2 <- numeric(n_transfer)
  for (k in seq_len(n_transfer)) {
    B <- run_trials(list("B"), B$V, B$alpha, S, update, eta)
    D <- run_trials(list("D"), D$V, D$alpha, S, update, eta)
    V_B_O2[k] <- B$V
    V_D_O2[k] <- D$V
  }
  list(V_B = blocking$V[["B"]], V_D = control$V[["D"]],
       alpha_B = blocking$alpha[["B"]], alpha_D = control$alpha[["D"]],
       H = V_D_O2 - V_B_O2)
}

# Number of updates that the bounds [alpha_min, alpha_max] clip in the run of
# Table S3.
count_clipped <- function(update, eta) {
  clipped <- 0L
  counted_update <- function(alpha, e_own, e_other, eta) {
    new_alpha <- update(alpha, e_own, e_other, eta)
    if (new_alpha < alpha_min || new_alpha > alpha_max) clipped <<- clipped + 1L
    new_alpha
  }
  transfer_experiment(counted_update, eta)
  clipped
}


# The error-difference branch reproduces adaptive_simulate() after compound
# training, and its gap after eight transfer updates is the peak of Section 3.4.
error_difference <- transfer_experiment(update_error_difference, eta_error_difference)
reference <- adaptive_simulate(c(make_trials("A", lambda, n_phase1, "Phase 1"),
                                 make_trials(c("A", "B"), lambda, n_compound, "Phase 2")),
                               S = c(A = salience, B = salience),
                               alpha_init = c(A = alpha_init, B = alpha_init),
                               eta = eta_error_difference,
                               alpha_min = alpha_min, alpha_max = alpha_max)
stopifnot(abs(error_difference$V_B - extract_final_V(reference)[["B"]]) < 1e-12,
          abs(error_difference$alpha_B - extract_final_alpha(reference)[["B"]]) < 1e-12,
          abs(error_difference$H[k_compared] - .284317) < 5e-6)

# eta of the rank-based update is set so that B ends compound training with the
# same associability as under the error-difference update; D keeps alpha_init
# under both. H(1) depends only on alpha_D - alpha_B (Equation 12), so what
# differs afterward comes from the numerical form of the update.
eta_rank_based <- uniroot(function(eta) {
  transfer_experiment(update_rank_based, eta)$alpha_B - error_difference$alpha_B
}, c(1e-4, .9), tol = 1e-10)$root
rank_based <- transfer_experiment(update_rank_based, eta_rank_based)
peak <- c(max(error_difference$H), max(rank_based$H))
peak_at <- c(which.max(error_difference$H), which.max(rank_based$H))
H_10 <- c(error_difference$H[10], rank_based$H[10])
percent_below_peak <- round(100 * (1 - H_10 / peak))
clipped <- c(count_clipped(update_error_difference, eta_error_difference),
             count_clipped(update_rank_based, eta_rank_based))

# Associability reset at the transfer phase (g_alpha = 0).
reset_H <- cbind(
  transfer_experiment(update_error_difference, eta_error_difference, g_alpha = 0)$H,
  transfer_experiment(update_rank_based, eta_rank_based, g_alpha = 0)$H)

# Value carryover g_V with full associability carryover: Delta H(8) =
# H_adaptive(8) - H_RW(8) (Equation 18), where the Rescorla-Wagner gap shrinks by
# the factor 1 - alpha beta with each transfer update (Equation 17).
rw_blocking <- rw_final(c(make_trials("A", lambda, n_phase1, "Phase 1"),
                          make_trials(c("A", "B"), lambda, n_compound, "Phase 2")),
                        alpha = c(A = rw_alpha, B = rw_alpha), beta = rw_beta)
rw_control <- rw_final(c(make_trials("C", lambda, n_phase1, "Phase 1"),
                         make_trials(c("D", "E"), lambda, n_compound, "Phase 2")),
                       alpha = c(C = rw_alpha, D = rw_alpha, E = rw_alpha), beta = rw_beta)
H_rw <- function(g_V, k) {
  g_V * (rw_control[["D"]] - rw_blocking[["B"]]) * (1 - rw_alpha * rw_beta)^k
}
H_adaptive_compared <- function(g_V) {
  c(transfer_experiment(update_error_difference, eta_error_difference, g_V = g_V)$H[k_compared],
    transfer_experiment(update_rank_based, eta_rank_based, g_V = g_V)$H[k_compared])
}
g_V_grid <- seq(0, 1, by = .05)
carryover <- do.call(rbind, lapply(g_V_grid, function(g_V) {
  data.frame(g_V = g_V, rule = rule_labels,
             delta_H8 = H_adaptive_compared(g_V) - H_rw(g_V, k = k_compared))
}))
delta_H8_full_carryover <- carryover$delta_H8[carryover$g_V == 1]
# How much the adaptive model's gap after eight updates changes between g_V = 0
# and g_V = 1e-6. Under the rank-based update, any g_V above about 2e-8 (the tie
# tolerance divided by the cue's old value) makes the first transfer error
# smaller than the zero-prediction reference.
change_at_zero <- H_adaptive_compared(1e-6) - H_adaptive_compared(0)

# The control cue when its companion E is more salient than D by 1e-8 to 1e-1
# (Figure S3b), and at the differences S_E - S_D quoted in the text, where a
# negative difference makes E less salient than D.
alpha_D_given_S_E <- function(S_E) {
  c(transfer_experiment(update_error_difference, eta_error_difference, S_E = S_E)$alpha_D,
    transfer_experiment(update_rank_based, eta_rank_based, S_E = S_E)$alpha_D)
}
salience_difference <- 10^seq(-8, -1, by = .25)
control_cue <- do.call(rbind, lapply(salience_difference, function(d) {
  data.frame(salience_difference = d, rule = rule_labels,
             alpha_D = alpha_D_given_S_E(salience + d))
}))
rank_based_curve <- control_cue$alpha_D[control_cue$rule == rule_labels[2]]
quoted_differences <- c("1e-6" = 1e-6, ".01" = .01, ".10" = .10, "-1e-6" = -1e-6, "-.01" = -.01)
alpha_D_quoted <- sapply(quoted_differences, function(d) alpha_D_given_S_E(salience + d))
rownames(alpha_D_quoted) <- c("error_difference", "rank_based")

# The lowest associability reached in any of these runs, with either update:
# B's after compound training, so neither lower bound (.01 here, .1 in Le Pelley
# et al., 2016) is reached.
lowest_associability <- function(update, eta, ...) {
  lowest <- 1
  tracked_update <- function(alpha, e_own, e_other, eta) {
    new_alpha <- update(alpha, e_own, e_other, eta)
    lowest <<- min(lowest, new_alpha)
    new_alpha
  }
  transfer_experiment(tracked_update, eta, ...)
  lowest
}
run_settings <- c(list(list(), list(g_alpha = 0)), lapply(g_V_grid, function(g_V) list(g_V = g_V)),
                  lapply(c(salience_difference, quoted_differences),
                         function(d) list(S_E = salience + d)))
lowest <- min(sapply(run_settings, function(setting) c(
  do.call(lowest_associability, c(list(update_error_difference, eta_error_difference), setting)),
  do.call(lowest_associability, c(list(update_rank_based, eta_rank_based), setting)))))


# Figure S3
rule_colours <- setNames(c("#4477AA", "#B4436C"), rule_labels)

gap_by_update <- rbind(data.frame(k = 1:n_transfer, H = error_difference$H, rule = rule_labels[1]),
                       data.frame(k = 1:n_transfer, H = rank_based$H, rule = rule_labels[2]))
panel_gap <- ggplot(gap_by_update, aes(k, H, colour = rule)) +
  geom_line(linewidth = .9) +
  geom_point(size = 1.6, show.legend = FALSE) +
  scale_colour_manual(values = rule_colours, breaks = rule_labels) +
  scale_x_continuous(breaks = c(1, 5, 10)) +
  scale_y_continuous(labels = axis_number) +
  labs(x = expression("Completed updates with the new outcome, " * italic(k)),
       y = expression(italic(H) * "(" * italic(k) * ")"), title = "(a)  The B–D gap") +
  theme_supplement()

panel_control <- ggplot(control_cue, aes(salience_difference, alpha_D, colour = rule)) +
  geom_hline(yintercept = alpha_init, colour = "grey60", linewidth = .3, linetype = "22") +
  geom_line(linewidth = .9) +
  scale_colour_manual(values = rule_colours, breaks = rule_labels) +
  scale_x_log10(breaks = 10^seq(-8, -2, by = 2),
                labels = function(x) parse(text = sprintf("10^'−%d'", -round(log10(x))))) +
  scale_y_continuous(limits = c(.2, .55), labels = axis_number) +
  labs(x = expression(italic(S)[E] * " − " * italic(S)[D] * ", with " * italic(S)[D] * " = .30"),
       y = expression(italic("α")[D] * " after compound training"),
       title = "(b)  Control cue, E more salient than D") +
  theme_supplement()

figure <- (panel_gap | panel_control) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


# Each line gives the error-difference values first and the rank-based values
# second.
listed <- function(x) paste(paper_number(x, 3), collapse = ", ")
cat("Table S3, error-difference and rank-based updates\n")
cat(sprintf("  eta: %s and %s\n", paper_number(eta_error_difference, 2),
            listed(eta_rank_based)))
cat(sprintf("  V_B, V_D after compound training: %s and %s\n",
            listed(c(error_difference$V_B, error_difference$V_D)),
            listed(c(rank_based$V_B, rank_based$V_D))))
cat(sprintf("  alpha_B, alpha_D after compound training: %s and %s\n",
            listed(c(error_difference$alpha_B, error_difference$alpha_D)),
            listed(c(rank_based$alpha_B, rank_based$alpha_D))))
cat(sprintf("  Peak of H(k): %s at k = %d and %s at k = %d\n",
            listed(peak[1]), peak_at[1], listed(peak[2]), peak_at[2]))
cat(sprintf("  H(10): %s and %s, %g%% and %g%% below the peak\n", listed(H_10[1]),
            listed(H_10[2]), percent_below_peak[1], percent_below_peak[2]))
cat(sprintf("  H(k) with associability reset at transfer: %g and %g\n",
            max(abs(reset_H[, 1])), max(abs(reset_H[, 2]))))
cat("Text of Supplement S3\n")
cat(sprintf("  eta of the rank-based update: %s\n", paper_number(eta_rank_based, 4)))
cat(sprintf("  H(k), k = 1-10: %s\n              and %s\n", listed(error_difference$H),
            listed(rank_based$H)))
cat(sprintf("  Updates clipped by the bounds [.01, 1]: %d and %d\n", clipped[1], clipped[2]))
cat(sprintf("  H_adaptive(8) - H_RW(8) with g_V = 1 and g_alpha = 1: %s and %s\n",
            listed(delta_H8_full_carryover[1]), listed(delta_H8_full_carryover[2])))
cat(sprintf("  change in H_adaptive(8) from g_V = 0 to g_V = 1e-6: %.2g and %s\n",
            change_at_zero[1], paper_number(change_at_zero[2], 3)))
cat(sprintf("  alpha_D with S_E - S_D = 1e-6, .01, .10: %s and %s\n",
            listed(alpha_D_quoted[1, 1:3]), listed(alpha_D_quoted[2, 1:3])))
cat(sprintf("  alpha_D with S_E - S_D = -1e-6, -.01: %s and %s\n",
            listed(alpha_D_quoted[1, 4:5]), listed(alpha_D_quoted[2, 4:5])))
cat(sprintf("  lowest associability in any run: %s\n", paper_number(lowest, 3)))

# In the CSV, x is k for H(k) in Figure S3a, g_V for Delta H(8), and S_E - S_D
# for alpha_D in Figure S3b; value holds H(k), Delta H(8) or alpha_D.
update_rule_data <- rbind(
  data.frame(what = "H_by_k", rule = gap_by_update$rule, x = gap_by_update$k,
             value = gap_by_update$H),
  data.frame(what = "delta_H8_by_g_V", rule = carryover$rule, x = carryover$g_V,
             value = carryover$delta_H8),
  data.frame(what = "alpha_D_by_S_E_minus_S_D", rule = control_cue$rule,
             x = control_cue$salience_difference, value = control_cue$alpha_D))
save_figure(figure, "figS3_update_rule", width = 6.27, height = 3.6, dpi = 300)
write.csv(update_rule_data, "results/supplementS3_update_rule.csv", row.names = FALSE)
cat("Saved results/supplementS3_update_rule.csv\n")

# The one update that the bounds clip under the error-difference rule is A's
# associability after the last compound trial, which no later update uses:
# without the upper bound, the states and gaps are the same.
alpha_max <- Inf
unbounded <- transfer_experiment(update_error_difference, eta_error_difference)
alpha_max <- 1

# Values reported in the paper
stopifnot(
  identical(unbounded, error_difference),                                  # Supplement S3
  abs(eta_rank_based - .073) < 5e-4,                                       # Table S3
  abs(eta_rank_based - .0726) < 5e-5,                                      # Supplement S3
  abs(rank_based$alpha_B - alpha_init * (1 - eta_rank_based)^n_compound) < 1e-12,  # Supplement S3
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
  delta_H8_full_carryover < 0,                                             # Supplement S3
  abs(delta_H8_full_carryover - c(-.029, -.050)) < 5e-4,                   # Supplement S3
  abs(change_at_zero[1]) < 1e-6, abs(change_at_zero[2] + .024) < 5e-4,     # Supplement S3
  percent_below_peak == c(4, 34),                                          # Supplement S3
  abs(rank_based_curve[1] - alpha_init) < 1e-12,                           # Supplement S3
  abs(rank_based_curve[salience_difference > 3e-8] -
        .5 * (1 - eta_rank_based)^9) < 1e-12,                              # Supplement S3
  # Supplement S3: in between, alpha_D falls part of the way
  rank_based_curve[2] > .5 * (1 - eta_rank_based)^9, rank_based_curve[2] < alpha_init,
  # Supplement S3: the rank-based tie at g_V = 0 breaks above about 2e-8
  abs(tie_tolerance / c(rank_based$V_B, rank_based$V_D) - 2e-8) < .5e-8,
  abs(alpha_D_quoted["rank_based", c("1e-6", "-1e-6")] -
        c(.254, .746)) < 5e-4,                                             # Supplement S3
  abs(alpha_D_quoted["error_difference", c(".01", ".10", "-.01")] -
        c(.495, .452, .506)) < 5e-4,                                       # Supplement S3
  abs(alpha_D_quoted["error_difference", c("1e-6", "-1e-6")] - .5) < 1e-6  # Supplement S3
)
