# Section 2: blocking and overexpectation under the Rescorla-Wagner model
# (Equation 1). Computes the values reported in Sections 2.2-2.6, Table 1 and
# Supplement S4.2, and draws Figure 2 (blocking) and Figure 3 (response
# functions).
#   Rscript analysis/section2_blocking.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of Section 2.3 (lambda = 1 on every trial) and the slope of the
# sigmoid response function in Table 1.
alpha <- c(A = .4, B = .4)
beta <- .3
gamma <- 5


# Blocking: 10 A+ trials, then 10 AB+ trials. rw_simulate() numbers trials from
# 1, whereas Equation 1 counts from 0: V_A[t] and V_B[t] are the values at the
# start of trial t, after t - 1 trials (V_{A,t-1} and V_{B,t-1} in Equation 1),
# and V_A[21] and V_B[21] the values after the last trial. Figure 2a plots the
# values against the number of completed trials, trial - 1.
blocking_design <- c(make_trials("A", 1, 10, "Phase 1"),
                     make_trials(c("A", "B"), 1, 10, "Phase 2"))
blocking <- rw_simulate(blocking_design, alpha = alpha, beta = beta)
blocking$completed <- blocking$trial - 1L
V_A <- blocking$V[blocking$cue == "A"]
V_B <- blocking$V[blocking$cue == "B"]

# The shared prediction error at the start of each trial (Equation 1).
shared_error <- data.frame(trial = seq_along(blocking_design))
shared_error$delta <- sapply(shared_error$trial, function(t) {
  start <- blocking[blocking$trial == t, ]
  1 - sum(start$V[start$cue %in% blocking_design[[t]]$cues])
})

# Trial 11 is the first AB+ trial. B starts it at 0, so its value after that
# trial is its first gain.
V_A_alone <- V_A[11]
first_error <- shared_error$delta[11]
first_gain <- V_B[12]

# The fraction of the error left by each A+ trial (.88) and each AB+ trial
# (.76), and the value that B approaches (Sections 2.2-2.4).
kept_alone <- 1 - alpha[["A"]] * beta
kept_compound <- 1 - sum(alpha) * beta
limit_B <- (1 - V_A_alone) / 2

# The values after the last trial from rw_final(), the loop printed in Section
# 2.2, which tests/verify_models.R checks against rw_simulate(). Table 1 and
# Figure 3 use these values.
V_end <- rw_final(blocking_design, alpha = alpha, beta = beta)

cat("Sections 2.2-2.4, blocking: 10 A+ then 10 AB+ trials\n")
cat(sprintf("  V_A after the A+ trials: %s\n", paper_number(V_A_alone, 4)))
cat(sprintf("  first AB+ trial: delta = %s, gain of each cue = %s\n",
            paper_number(first_error, 4), paper_number(first_gain, 4)))
cat(sprintf("  ten gains as large as the first would total %s\n",
            paper_number(10 * first_gain, 3)))
cat(sprintf("  after the AB+ trials: V_A = %s, V_B = %s; V_B approaches %s\n",
            paper_number(V_end[["A"]], 4), paper_number(V_end[["B"]], 4),
            paper_number(limit_B, 4)))


# Table 1: the sigmoid (Equation 5) and threshold (Equation 6) response
# functions, each applied to both cues. "moved" is the same function with its
# midpoint or threshold moved to .1.
table_1 <- rbind(
  sigmoid = obs_sigmoid(V_end, gamma = gamma, theta = .5),
  sigmoid_moved = obs_sigmoid(V_end, gamma = gamma, theta = .1),
  threshold = obs_threshold(V_end, theta = .3),
  threshold_moved = obs_threshold(V_end, theta = .1)
)
rule_labels <- c("Sigmoid (gamma = 5, theta = .5)", "Sigmoid (gamma = 5, theta = .1)",
                 "Threshold (theta = .3)", "Threshold (theta = .1)")
cat("Table 1: p_A, p_B\n")
cat(sprintf("  %-32s %4s  %4s\n", rule_labels, paper_number(table_1[, "A"], 2),
            paper_number(table_1[, "B"], 2)), sep = "")


# Overexpectation (Section 2.6): A+ and B+ alternate for 10 trials each, then
# 10 AB+ trials.
overexpectation_design <- c(
  rep(c(make_trials("A", 1, 1, "Phase 1"), make_trials("B", 1, 1, "Phase 1")), 10),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)
V_separate <- rw_final(overexpectation_design[1:20], alpha = alpha, beta = beta)
V_together <- rw_final(overexpectation_design, alpha = alpha, beta = beta)
error_together <- 1 - sum(V_separate)
change_together <- alpha[["A"]] * beta * error_together
sigmoid_before_after <- obs_sigmoid(c(V_separate[["A"]], V_together[["A"]]),
                                    gamma = gamma, theta = .5)
threshold_before_after <- obs_threshold(c(V_separate[["A"]], V_together[["A"]]), theta = .3)
# With 200 AB+ trials instead of 10, both values approach .5.
V_long <- rw_final(c(overexpectation_design[1:20], make_trials(c("A", "B"), 1, 200, "Phase 2")),
                   alpha = alpha, beta = beta)
# The second case of Section 2.6: 20 A+ and 5 B+ trials. The values sum to more
# than 1, so the first AB+ trial lowers both; compound training keeps their
# difference, so they do not both approach .5.
unequal_training <- c(make_trials("A", 1, 20, "Phase 1"), make_trials("B", 1, 5, "Phase 1"))
unequal_before <- rw_final(unequal_training, alpha = alpha, beta = beta)
unequal_long <- rw_final(c(unequal_training, make_trials(c("A", "B"), 1, 200, "Phase 2")),
                         alpha = alpha, beta = beta)

# Supplement S4.2: after n reinforced trials of each cue alone, the first
# compound trial lowers both values when alpha beta exceeds 1 - 2^(-1/n).
loss_boundary <- 1 - 2^(-1 / 10)

cat("Section 2.6, overexpectation: 10 A+ and 10 B+ alternating, then 10 AB+ trials\n")
cat(sprintf("  V_A = V_B before the AB+ trials: %s\n", paper_number(V_separate[["A"]], 4)))
cat(sprintf("  first AB+ trial: delta = %s, change of each cue = %s\n",
            paper_number(error_together, 3), paper_number(change_together, 4)))
cat(sprintf("  V_A = V_B after the AB+ trials: %s; after 200 AB+ trials, %s\n",
            paper_number(V_together[["A"]], 4), paper_number(V_long[["A"]], 4)))
cat(sprintf("  sigmoid (gamma = 5, theta = .5): %s before, %s after\n",
            paper_number(sigmoid_before_after[1], 2), paper_number(sigmoid_before_after[2], 2)))
cat(sprintf("  threshold (theta = .3): %d before, %d after\n",
            threshold_before_after[1], threshold_before_after[2]))
cat(sprintf("  20 A+ and 5 B+ trials: V_A = %s and V_B = %s, sum %s\n",
            paper_number(unequal_before[["A"]], 4), paper_number(unequal_before[["B"]], 4),
            paper_number(sum(unequal_before), 4)))
cat(sprintf("  then 200 AB+ trials: V_A = %s and V_B = %s\n",
            paper_number(unequal_long[["A"]], 4), paper_number(unequal_long[["B"]], 4)))
cat(sprintf("  the first AB+ trial lowers both values when alpha beta > %s (Supplement S4.2)\n",
            paper_number(loss_boundary, 3)))


# Figure 2: (a) the values after each completed trial, with the single-cue
# reference of Section 2.3 and B's limit; (b) the shared error on each trial.
ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
red <- paper_colours[["red"]]
blue <- paper_colours[["blue"]]

# The single-cue reference, a new cue trained alone from zero (Equation 3),
# drawn alongside B over the AB+ trials.
single_cue <- data.frame(completed = 10:20, V = 1 - kept_alone^(0:10))

# Phase labels sit above the data, with one light boundary in each panel.
phase_marks <- function(boundary, centers) {
  list(
    geom_vline(xintercept = boundary, colour = "#C5C5C5",
               linewidth = .3, linetype = "longdash"),
    annotate("text", x = centers, y = 1.075,
             label = c("A+", "AB+"), size = text_size, colour = ink)
  )
}

values_panel <- ggplot() +
  phase_marks(10, c(5, 15)) +
  geom_line(data = single_cue, aes(completed, V),
            colour = grey, linetype = "22", linewidth = .55) +
  annotate("segment", x = 10, xend = 19.3, y = limit_B, yend = limit_B,
           colour = red, linewidth = .4, linetype = "dotted") +
  geom_line(data = blocking, aes(completed, V, colour = cue), linewidth = .7) +
  geom_point(data = subset(blocking, completed %in% c(0, 10, 20)),
             aes(completed, V, colour = cue, shape = cue), size = 1.9, stroke = .4) +
  annotate("text", x = 20.65, y = V_A[21], label = "A",
           colour = blue, size = text_size, hjust = 0) +
  annotate("text", x = 20.65, y = tail(single_cue$V, 1), label = "single cue",
           colour = grey, size = text_size, hjust = 0) +
  annotate("text", x = 20.65, y = V_B[21], label = "B",
           colour = red, size = text_size, hjust = 0) +
  annotate("segment", x = 17, xend = 18, y = .285, yend = limit_B + .018,
           colour = red, linewidth = .3) +
  annotate("text", x = 15, y = .335, label = paste("B limit =", paper_number(limit_B, 4)),
           colour = red, size = text_size, hjust = 0) +
  scale_colour_manual(values = c(A = blue, B = red)) +
  scale_shape_manual(values = c(A = 16, B = 17)) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(-.3, 24.4), ylim = c(-.035, 1.12), clip = "off") +
  labs(title = "(a)  Learned values", x = expression("Completed trials, " * italic(t)),
       y = expression(italic(V))) +
  theme_paper(title_gap = 10)

# Figure 2b numbers trials from 0, as Equation 1 does, so the first AB+ trial,
# trial 11 above, is t = 10.
shared_error$t <- shared_error$trial - 1L
error_panel <- ggplot(shared_error, aes(t, delta)) +
  phase_marks(9.5, c(4.5, 14.5)) +
  geom_line(colour = ink, linewidth = .6) +
  geom_point(colour = ink, size = 1.4) +
  geom_point(data = shared_error[11, ], shape = 21,
             size = 2.7, fill = "white", colour = ink, stroke = .7) +
  annotate("segment", x = 13.2, xend = 10.35, y = .49,
           yend = first_error + .025, colour = ink, linewidth = .3) +
  annotate("text", x = 11.1, y = .625, label = "First AB+ trial",
           size = text_size, colour = ink, hjust = 0) +
  annotate("text", x = 11.1, y = .545,
           label = paste("δ =", paper_number(first_error, 4)),
           size = text_size, colour = ink, hjust = 0) +
  scale_x_continuous(breaks = c(0, 5, 10, 15), expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(-.3, 19.3), ylim = c(-.035, 1.12), clip = "off") +
  labs(title = "(b)  Shared prediction error", x = expression("Trial, " * italic(t)),
       y = expression("δ = λ − Σ" * italic(V))) +
  theme_paper(title_gap = 10)

save_figure(values_panel + error_panel + plot_layout(widths = c(1.22, 1)),
            "fig02_blocking", width = 7, height = 3.15)


# Figure 3: the values left by blocking under the two response functions of
# Table 1, each drawn in black and its moved version in wide gray.
V_A_end <- V_end[["A"]]
V_B_end <- V_end[["B"]]
value_line <- "#B0B0B0"   # dotted lines at the two learned values
moved_rule <- "#C4C4C4"   # wide line of the moved rule, drawn under the black one
small_text_size <- 8.5 / .pt
V_grid <- seq(0, 1, length.out = 501)

response_theme <- theme_paper() +
  theme(plot.subtitle = element_text(margin = margin(b = 14)),
        plot.margin = margin(6, 12, 6, 6))
unit_axes <- function() list(
  scale_x_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)),
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)),
  coord_cartesian(xlim = c(-.03, 1.03), ylim = c(-.07, 1.07), clip = "off")
)
# The two learned values, marked the same way in both panels.
value_marks <- function() list(
  annotate("segment", x = c(V_B_end, V_A_end), xend = c(V_B_end, V_A_end),
           y = -.07, yend = 1.07, colour = value_line, linewidth = .3, linetype = "13"),
  annotate("text", x = V_B_end, y = 1.14, label = "B", colour = red,
           size = text_size, fontface = "bold"),
  annotate("text", x = V_A_end, y = 1.14, label = "A", colour = blue,
           size = text_size, fontface = "bold")
)
# Key in the empty lower-right corner, the same in both panels.
rule_key <- function(base, moved) list(
  annotate("segment", x = .45, xend = .55, y = .22, yend = .22,
           colour = ink, linewidth = .6),
  annotate("segment", x = .45, xend = .55, y = .10, yend = .10,
           colour = moved_rule, linewidth = 2.2, lineend = "butt"),
  annotate("text", x = .58, y = .22, label = base, hjust = 0, size = small_text_size,
           colour = ink),
  annotate("text", x = .58, y = .10, label = moved, hjust = 0, size = small_text_size,
           colour = ink)
)
# A point with its probability printed beside it.
labeled_point <- function(V, p, cue, dx, dy, hjust) list(
  annotate("point", x = V, y = p, colour = if (cue == "A") blue else red, size = 2),
  annotate("text", x = V + dx, y = p + dy, label = axis_number(round(p, 2)), hjust = hjust,
           size = small_text_size, colour = ink)
)

sigmoid_panel <- ggplot() +
  value_marks() +
  geom_line(data = data.frame(V = V_grid, p = obs_sigmoid(V_grid, gamma = gamma, theta = .1)),
            aes(V, p), colour = moved_rule, linewidth = 2.2) +
  geom_line(data = data.frame(V = V_grid, p = obs_sigmoid(V_grid, gamma = gamma, theta = .5)),
            aes(V, p), colour = ink, linewidth = .6) +
  labeled_point(V_B_end, table_1["sigmoid", "B"], "B", -.025, .07, 1) +
  labeled_point(V_B_end, table_1["sigmoid_moved", "B"], "B", .03, -.07, 0) +
  labeled_point(V_A_end, table_1["sigmoid", "A"], "A", .03, -.07, 0) +
  labeled_point(V_A_end, table_1["sigmoid_moved", "A"], "A", .03, .085, 0) +
  rule_key("θ = .5", "θ = .1") +
  unit_axes() +
  labs(title = "(a)  Sigmoid",
       subtitle = "γ = 5; midpoint θ = .5 or .1",
       x = expression("Learned value, " * italic(V)),
       y = "Probability of a yes response") +
  response_theme

threshold_panel <- ggplot() +
  value_marks() +
  geom_path(data = data.frame(V = c(0, .1, .1, 1), p = c(0, 0, 1, 1)), aes(V, p),
            colour = moved_rule, linewidth = 2.2, linejoin = "mitre") +
  geom_path(data = data.frame(V = c(0, .3, .3, 1), p = c(0, 0, 1, 1)), aes(V, p),
            colour = ink, linewidth = .6, linejoin = "mitre") +
  labeled_point(V_B_end, table_1["threshold", "B"], "B", .03, .07, 0) +
  labeled_point(V_B_end, table_1["threshold_moved", "B"], "B", .03, -.07, 0) +
  labeled_point(V_A_end, table_1["threshold", "A"], "A", .03, -.07, 0) +
  rule_key("θ = .3", "θ = .1") +
  unit_axes() +
  labs(title = "(b)  Threshold",
       subtitle = expression("Answer yes only when " * italic(V) * " > θ"),
       x = expression("Learned value, " * italic(V)),
       y = "Probability of a yes response") +
  response_theme

save_figure(sigmoid_panel + threshold_panel + plot_layout(widths = c(1, 1)),
            "fig03_response_rules", width = 6.4, height = 3.1)

# Values reported in the paper
stopifnot(
  abs(V_A[2] - .12) < 1e-12,                                         # Section 2.3
  max(abs(V_A[1:11] - (1 - kept_alone^(0:10)))) < 1e-12,             # Equation 3, Figure 2a
  abs(V_A_alone - .7215) < 5e-5,                                     # Equation 4
  abs(first_error - .2785) < 5e-5,                                   # Section 2.3, Figure 2b
  abs(first_gain - .0334) < 5e-5,                                    # Section 2.3
  abs(10 * first_gain - .334) < 5e-4,                                # Section 2.3
  max(abs(shared_error$delta - c(kept_alone^(0:9),
                                 first_error * kept_compound^(0:9)))) < 1e-12,  # Figure 2b
  abs(V_end[["A"]] - .8518) < 5e-5,                                  # Section 2.4, Table 1
  abs(V_end[["B"]] - .1303) < 5e-5,                                  # Section 2.4, Table 1
  abs(limit_B - .1393) < 5e-5,                                       # Section 2.4, Figure 2a
  max(abs(V_B[11:21] - limit_B * (1 - kept_compound^(0:10)))) < 1e-12,  # Figure 2a
  max(abs(table_1 - rbind(c(.85, .14), c(.98, .54), c(1, 0), c(1, 1)))) < .005,  # Table 1
  max(abs(V_separate - .7215)) < 5e-5,                               # Section 2.6
  abs(error_together + .443) < 5e-4,                                 # Section 2.6
  abs(change_together + .0532) < 5e-5,                               # Section 2.6
  abs(V_together[["A"]] - V_together[["B"]]) < 1e-12,                # Section 2.6
  abs(V_together[["A"]] - .5142) < 5e-5,                             # Section 2.6
  max(abs(sigmoid_before_after - c(.75, .52))) < .005,               # Section 2.6
  all(threshold_before_after == 1),                                  # Section 2.6
  max(abs(V_long - .5)) < 1e-6,                                      # Section 2.6
  abs(unequal_before[["A"]] - .922) < 5e-4,                          # Section 2.6
  abs(unequal_before[["B"]] - .472) < 5e-4,                          # Section 2.6
  sum(unequal_before) > 1,                                           # Section 2.6
  abs(diff(unequal_long) - diff(unequal_before)) < 1e-12,            # Section 2.6
  abs(sum(unequal_long) - 1) < 1e-6,                                 # Section 2.6
  abs(unequal_long[["A"]] - .725) < 5e-4,                            # Section 2.6
  abs(unequal_long[["B"]] - .275) < 5e-4,                            # Section 2.6
  abs(loss_boundary - .067) < 5e-4,                                  # Supplement S4.2
  alpha[["A"]] * beta > loss_boundary                                # Supplement S4.2
)
