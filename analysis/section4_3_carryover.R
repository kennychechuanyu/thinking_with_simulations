# Section 4.3: what carries over to the new outcome? A fraction g_V of each
# cue's learned value for O1 (Equation 16) and a fraction g_alpha of its change
# in associability (Equation 14) carry over to learning about O2. Computes both
# models' B-D gaps H(k), compares them after eight updates (Equation 18) as g_V
# varies from 0 to 1, follows both gaps with g_V = .90, checks Equations 15 and
# 17, and draws Figure 7.
#   Rscript analysis/section4_3_carryover.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of Section 3. The gaps are compared after eight updates, where the
# adaptive model's gap is largest without carryover of old value.
lambda <- 1
rw_alpha <- .4
rw_beta <- .3
salience <- .3
alpha_init <- .5
eta <- .05
n_trials <- 10
k_compared <- 8
g_V_example <- .90   # Figure 7b

# Training with O1: A+ then AB+ for the blocked cue B, C+ then DE+ for the
# control cue D.
blocked_design <- c(make_trials("A", lambda, n_trials, "Phase 1"),
                    make_trials(c("A", "B"), lambda, n_trials, "Phase 2"))
control_design <- c(make_trials("C", lambda, n_trials, "Phase 1"),
                    make_trials(c("D", "E"), lambda, n_trials, "Phase 2"))
rw_blocked <- rw_simulate(blocked_design, alpha = c(A = rw_alpha, B = rw_alpha),
                          beta = rw_beta)
rw_control <- rw_simulate(control_design, alpha = c(C = rw_alpha, D = rw_alpha, E = rw_alpha),
                          beta = rw_beta)
adaptive_blocked <- adaptive_simulate(blocked_design, S = c(A = salience, B = salience),
                                      alpha_init = c(A = alpha_init, B = alpha_init),
                                      eta = eta)
adaptive_control <- adaptive_simulate(control_design,
                                      S = c(C = salience, D = salience, E = salience),
                                      alpha_init = c(C = alpha_init, D = alpha_init,
                                                     E = alpha_init),
                                      eta = eta)

# B's and D's values for O1 at the end of training (V_{i,O1}^end in Equation
# 16) and the adaptive model's associabilities (alpha_i^end in Equation 14).
rw_V_end <- c(B = extract_final_V(rw_blocked)[["B"]], D = extract_final_V(rw_control)[["D"]])
adaptive_V_end <- c(B = extract_final_V(adaptive_blocked)[["B"]],
                    D = extract_final_V(adaptive_control)[["D"]])
alpha_end <- c(B = extract_final_alpha(adaptive_blocked)[["B"]],
               D = extract_final_alpha(adaptive_control)[["D"]])

# B and D each learn about O2 alone, starting from the carried-over values and
# associabilities. Returns both models' gaps H(k) = V_D(k) - V_B(k) for
# k = 0, ..., 10 completed updates.
gaps_with_carryover <- function(g_V, g_alpha) {
  B_trials <- make_trials("B", lambda, n_trials, "Phase 3")
  D_trials <- make_trials("D", lambda, n_trials, "Phase 3")
  alpha_start <- alpha_init + g_alpha * (alpha_end - alpha_init)
  rw_B <- rw_simulate(B_trials, alpha = c(B = rw_alpha), beta = rw_beta,
                      V_init = g_V * rw_V_end["B"], cue_names = "B")
  rw_D <- rw_simulate(D_trials, alpha = c(D = rw_alpha), beta = rw_beta,
                      V_init = g_V * rw_V_end["D"], cue_names = "D")
  adaptive_B <- adaptive_simulate(B_trials, S = c(B = salience), alpha_init = alpha_start["B"],
                                  eta = eta,
                                  V_init = g_V * adaptive_V_end["B"], cue_names = "B")
  adaptive_D <- adaptive_simulate(D_trials, S = c(D = salience), alpha_init = alpha_start["D"],
                                  eta = eta,
                                  V_init = g_V * adaptive_V_end["D"], cue_names = "D")
  data.frame(g_V = g_V, g_alpha = g_alpha, k = 0:n_trials,
             H_RW = rw_D$V - rw_B$V, H_adaptive = adaptive_D$V - adaptive_B$V)
}

# Delta H(8) of Equation 18; positive when the adaptive model predicts the
# larger gap.
delta_H <- function(g_V, g_alpha) {
  run <- gaps_with_carryover(g_V, g_alpha)
  run$H_adaptive[run$k == k_compared] - run$H_RW[run$k == k_compared]
}

grid <- expand.grid(g_V = seq(0, 1, by = .01), g_alpha = c(0, .5, 1))
gaps <- do.call(rbind, Map(gaps_with_carryover, grid$g_V, grid$g_alpha))
no_value <- gaps[gaps$g_V == 0, ]
first_update <- no_value[no_value$k == 1, ]
compared <- gaps[gaps$k == k_compared, ]
compared$delta_H <- compared$H_adaptive - compared$H_RW

# Equation 14 with half of the change in associability carried over.
alpha_B_half <- alpha_init + .5 * (alpha_end[["B"]] - alpha_init)
# With full value carryover both Rescorla-Wagner cues start from their values
# for O1 and gain alpha * beta = .12 of their remaining error (Equation 1).
rw_first_gain <- rw_alpha * rw_beta * (lambda - rw_V_end)

# Figure 7a: the gaps after eight updates as g_V varies, with g_alpha = 1, and
# the g_V at which they are equal (the curves cross once, between .3 and 1).
# Figure 7b: both gaps over the ten updates with g_V = .90.
full_alpha <- compared[compared$g_alpha == 1, ]
original <- full_alpha[full_alpha$g_V == 0, ]
full_value <- full_alpha[full_alpha$g_V == 1, ]
crossing_g_V <- uniroot(delta_H, c(.3, 1), g_alpha = 1, tol = 1e-8)$root
crossing_run <- gaps_with_carryover(crossing_g_V, 1)
crossing_gap <- crossing_run$H_RW[crossing_run$k == k_compared]
example <- gaps_with_carryover(g_V_example, 1)


# Figure 7
ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
light <- paper_colours[["light"]]
unit_breaks <- c(0, .25, .5, .75, 1)
unit_labels <- c("0", ".25", ".5", ".75", "1")
gap_breaks <- c(0, .1, .2, .3)
gap_labels <- c("0", ".1", ".2", ".3")

# (a) The gaps after eight updates as old value carries over.
panel_a <- ggplot() +
  geom_line(data = full_alpha, aes(g_V, H_RW), colour = grey,
            linewidth = .7, linetype = "22") +
  geom_line(data = full_alpha, aes(g_V, H_adaptive), colour = ink, linewidth = .7) +
  geom_point(data = original, aes(g_V, H_adaptive), shape = 21, size = 2.4,
             fill = "white", colour = ink, stroke = .6) +
  annotate("text", x = .04, y = original$H_adaptive + .022, label = "original setting",
           size = text_size, colour = ink, hjust = 0) +
  annotate("point", x = crossing_g_V, y = crossing_gap, shape = 21, size = 2.4,
           fill = "white", colour = ink, stroke = .6) +
  annotate("segment", x = crossing_g_V, xend = crossing_g_V, y = crossing_gap - .01,
           yend = .068, colour = ink, linewidth = .3) +
  annotate("text", x = crossing_g_V, y = .052, label = "equal gaps",
           size = text_size, colour = ink) +
  annotate("text", x = crossing_g_V, y = .029,
           label = sprintf("paste('at ', italic(g)[V], ' ≈ %s')", paper_number(crossing_g_V, 3)),
           parse = TRUE, size = text_size, colour = ink) +
  annotate("text", x = .25, y = full_alpha$H_RW[full_alpha$g_V == .25] + .036,
           label = "Rescorla–Wagner", size = text_size, colour = grey, hjust = .5) +
  annotate("text", x = .42, y = full_alpha$H_adaptive[full_alpha$g_V == .42] + .036,
           label = "Adaptive", size = text_size, colour = ink, hjust = .5) +
  scale_x_continuous(breaks = unit_breaks, labels = unit_labels,
                     expand = expansion(add = c(.03, 0))) +
  scale_y_continuous(breaks = gap_breaks, labels = gap_labels, expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-.005, .35), clip = "off") +
  labs(title = "(a)  Each model’s gap after eight updates",
       subtitle = expression("Associability fully carried over, " * italic(g)[italic("α")] * " = 1"),
       x = expression("Old value carried over, " * italic(g)[V]),
       y = expression(italic(H) * "(8) = " * italic(V)[D] * " − " * italic(V)[B])) +
  theme_paper() + theme(plot.margin = margin(6, 22, 6, 6))

# (b) Both gaps over the ten updates with g_V = .90; the dotted line marks
# eight updates.
panel_b <- ggplot(example) +
  annotate("segment", x = k_compared, xend = k_compared, y = -.005, yend = .35,
           colour = light, linewidth = .35, linetype = "13") +
  geom_line(aes(k, H_RW), colour = grey, linewidth = .7, linetype = "22") +
  geom_line(aes(k, H_adaptive), colour = ink, linewidth = .7) +
  annotate("text", x = .4, y = example$H_RW[1] + .004, hjust = 0, vjust = 0,
           label = sprintf("Rescorla–Wagner, %s at the start", paper_number(example$H_RW[1], 3)),
           size = text_size, colour = grey) +
  annotate("text", x = .3, y = example$H_adaptive[1] - .036, hjust = 0,
           label = sprintf("Adaptive, %s at the start", paper_number(example$H_adaptive[1], 3)),
           size = text_size, colour = ink) +
  annotate("text", x = k_compared - .2, y = .012, hjust = 1, vjust = 0,
           label = "8 updates", size = text_size, colour = grey) +
  scale_x_continuous(breaks = c(0, 5, 10), expand = expansion(add = c(.3, 0))) +
  scale_y_continuous(breaks = gap_breaks, labels = gap_labels, expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, n_trials), ylim = c(-.005, .35), clip = "off") +
  labs(title = "(b)  The same later gap, different histories",
       subtitle = expression("Old value carried over, " * italic(g)[V] * " = .90; " *
                               italic(g)[italic("α")] * " = 1"),
       x = expression("Completed updates with the new outcome, " * italic(k)), y = NULL) +
  theme_paper()

figure <- panel_a + plot_spacer() + panel_b + plot_layout(widths = c(1, .06, 1))

cat("Section 4.3, carryover of old value (g_V) and of associability (g_alpha)\n")
cat(sprintf("alpha_B and alpha_D at the start of the new task (g_alpha = 1): %s and %s\n",
            paper_number(alpha_end[["B"]], 3), paper_number(alpha_end[["D"]], 3)))
cat(sprintf("alpha_B at the start of the new task (g_alpha = .5): %s\n",
            paper_number(alpha_B_half, 3)))
cat(sprintf("H_adaptive(1) with g_V = 0: %s (g_alpha = 1), %s (g_alpha = .5)\n",
            paper_number(first_update$H_adaptive[first_update$g_alpha == 1], 3),
            paper_number(first_update$H_adaptive[first_update$g_alpha == .5], 3)))
cat(sprintf("Rescorla-Wagner values for O1: V_B = %s, V_D = %s\n",
            paper_number(rw_V_end[["B"]], 3), paper_number(rw_V_end[["D"]], 3)))
cat(sprintf("First Rescorla-Wagner gains with g_V = 1: %s (B), %s (D)\n",
            paper_number(rw_first_gain[["B"]], 3), paper_number(rw_first_gain[["D"]], 3)))
cat(sprintf("H(8) with g_V = 1 and g_alpha = 1: %s (Rescorla-Wagner), %s (adaptive)\n",
            paper_number(full_value$H_RW, 3), paper_number(full_value$H_adaptive, 3)))
cat(sprintf("The two gaps after eight updates are equal at g_V = %s\n",
            paper_number(crossing_g_V, 3)))
cat(sprintf("g_V = .90: H(0) = %s (Rescorla-Wagner), %s (adaptive); H(8) = %s and %s; H(10) = %s and %s\n",
            paper_number(example$H_RW[1], 3), paper_number(example$H_adaptive[1], 3),
            paper_number(example$H_RW[example$k == 8], 3),
            paper_number(example$H_adaptive[example$k == 8], 3),
            paper_number(example$H_RW[example$k == 10], 3),
            paper_number(example$H_adaptive[example$k == 10], 3)))
save_figure(figure, "fig07_carryover", width = 7, height = 3.35)

# Values reported in the paper
stopifnot(
  abs(alpha_end - c(.235, .5)) < 5e-4,                          # Section 4.3
  abs(alpha_B_half - .368) < 5e-4,
  abs(first_update$H_adaptive[first_update$g_alpha == 1] - .079) < 5e-4,
  abs(first_update$H_adaptive[first_update$g_alpha == .5] - .040) < 5e-4,
  max(abs(no_value$H_adaptive[no_value$g_alpha == 0])) < 1e-12,
  max(abs(first_update$H_adaptive - salience * lambda * first_update$g_alpha *
            (alpha_end[["D"]] - alpha_end[["B"]]))) < 1e-12,    # Equation 15
  abs(rw_V_end - c(.130, .468)) < 5e-4,                         # Box 6, Section 4.3
  abs(rw_first_gain - c(.104, .064)) < 5e-4,                    # Section 4.3
  all(gaps$H_RW[gaps$g_V > 0] > 0),
  max(abs(gaps$H_RW - gaps$g_V * (rw_V_end[["D"]] - rw_V_end[["B"]]) *
            (1 - rw_alpha * rw_beta)^gaps$k)) < 1e-12,          # Equation 17
  abs(original$H_adaptive - .284) < 5e-4,                       # Figure 7a
  abs(full_value$H_RW - .121) < 5e-4,                           # Section 4.3
  abs(full_value$H_adaptive - .092) < 5e-4,                     # Section 4.3
  abs(crossing_g_V - .895) < 5e-4,                              # Section 4.3, Figure 7a
  all(diff(full_alpha$H_RW) > 0), all(diff(full_alpha$H_adaptive) < 0),  # Section 4.3
  # Equation 8 is continuous in the starting value: a tiny g_V barely changes
  # the adaptive model's gap (Supplement S3).
  abs(gaps_with_carryover(1e-6, 1)$H_adaptive[k_compared + 1] - original$H_adaptive) < 1e-6,
  # Section 4.3: retaining less of the associability difference reduces the
  # adaptive model's gap further, and with none retained Rescorla-Wagner
  # predicts the larger gap whenever any old value carries over.
  with(compared, all(tapply(H_adaptive, g_V, function(h) all(diff(h) > 0)))),
  with(compared, all(delta_H[g_alpha == 0 & g_V > 0] < 0)),
  abs(example$H_RW[1] - .304) < 5e-4,                           # Section 4.3, Figure 7b
  abs(example$H_adaptive[1] - .110) < 5e-4,                     # Section 4.3, Figure 7b
  abs(example$H_RW[example$k == 8] - .11) < 5e-3,               # Section 4.3, Figure 7b
  abs(example$H_adaptive[example$k == 8] - .11) < 5e-3,         # Section 4.3, Figure 7b
  example$k[which.max(example$H_adaptive)] == 4,                # Section 4.3
  abs(max(example$H_adaptive) - .138) < 5e-4,
  all(diff(example$H_adaptive[example$k >= 4]) < 0),            # Section 4.3: then shrinks
  # Section 4.3: both gaps remain close after ten updates
  abs(example$H_RW[example$k == 10] - example$H_adaptive[example$k == 10]) < .005
)
