# Section 3: the blocked cue B and its control cue D under Rescorla-Wagner and
# the adaptive model (Equations 7 and 8), then ten trials of each cue alone
# with a new outcome O2. Computes the history effect H(k) of Equation 11 and the
# values reported in Sections 3.2-3.4 and Table 2, and draws Figure 4.
#   Rscript analysis/section3_new_outcome.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of Section 3.2, with lambda = 1 for both outcomes.
rw_alpha <- .4      # Rescorla-Wagner salience alpha_i
rw_beta <- .3       # learning rate beta, shared by all cues
salience <- .3      # adaptive model: fixed salience S_i
alpha_init <- .5    # initial associability
eta <- .05          # associability-change rate


# Phases 1 and 2, with outcome O1: blocking (10 A+, then 10 AB+) and its
# control (10 C+, then 10 DE+).
blocking_design <- c(make_trials("A", 1, 10, "Phase 1"),
                     make_trials(c("A", "B"), 1, 10, "Phase 2"))
control_design <- c(make_trials("C", 1, 10, "Phase 1"),
                    make_trials(c("D", "E"), 1, 10, "Phase 2"))

rw_blocking <- rw_simulate(blocking_design, alpha = c(A = rw_alpha, B = rw_alpha),
                           beta = rw_beta)
rw_control <- rw_simulate(control_design, alpha = c(C = rw_alpha, D = rw_alpha, E = rw_alpha),
                          beta = rw_beta)
adaptive_blocking <- adaptive_simulate(blocking_design, S = c(A = salience, B = salience),
                                       alpha_init = c(A = alpha_init, B = alpha_init),
                                       eta = eta)
adaptive_control <- adaptive_simulate(control_design,
                                      S = c(C = salience, D = salience, E = salience),
                                      alpha_init = c(C = alpha_init, D = alpha_init,
                                                     E = alpha_init),
                                      eta = eta)

# Section 3.2 follows the first compound trials, trials 11 and 12 here:
# rw_simulate() and adaptive_simulate() number trials from 1, whereas Equations
# 1 and 7 number them from 0 (t = 10 and 11). Row t of each cue's run holds its
# value (and associability) at the start of trial t, so rw_gain_B[t] and
# adaptive_gain_B[t] are B's gains on trial t under the two models.
rw_gain_B <- diff(subset(rw_blocking, cue == "B")$V)
rw_gain_D <- diff(subset(rw_control, cue == "D")$V)
blocking_A <- subset(adaptive_blocking, cue == "A")
# Section 3.2: with associability held at .5 (eta = 0), A would reach .803 after
# its ten trials alone; it reaches .848 because its associability rises.
fixed_A <- subset(adaptive_simulate(blocking_design, S = c(A = salience, B = salience),
                                    alpha_init = c(A = alpha_init, B = alpha_init),
                                    eta = 0), cue == "A")
blocking_B <- subset(adaptive_blocking, cue == "B")
control_D <- subset(adaptive_control, cue == "D")
adaptive_gain_B <- diff(blocking_B$V)
adaptive_gain_D <- diff(control_D$V)
alpha_end <- c(extract_final_alpha(adaptive_blocking)["B"],
               extract_final_alpha(adaptive_control)["D"])

cat("Section 3.2, Rescorla-Wagner\n")
cat(sprintf("  first compound trial: B gains %s, D gains %s\n",
            paper_number(rw_gain_B[11], 4), paper_number(rw_gain_D[11], 2)))
cat("Section 3.2, adaptive model\n")
cat(sprintf("  V_A after the A+ trials: %s, so A's error is %s\n",
            paper_number(blocking_A$V[11], 3), paper_number(1 - blocking_A$V[11], 3)))
cat(sprintf("  first compound trial: B gains %s, D gains %s; alpha_B falls to %s\n",
            paper_number(adaptive_gain_B[11], 3), paper_number(adaptive_gain_D[11], 3),
            paper_number(blocking_B$alpha[12], 3)))
cat(sprintf("  second compound trial: B gains %s, D gains %s\n",
            paper_number(adaptive_gain_B[12], 4), paper_number(adaptive_gain_D[12], 4)))
cat(sprintf("  after the compound trials: alpha_B = %s, alpha_D = %s\n",
            paper_number(alpha_end[["B"]], 3), paper_number(alpha_end[["D"]], 3)))

# Table 2: values for O1 after compound training, and the update multipliers
# alpha_i beta (Rescorla-Wagner) and S_i alpha_i (adaptive model).
table_2 <- data.frame(
  V_B = c(extract_final_V(rw_blocking)[["B"]], extract_final_V(adaptive_blocking)[["B"]]),
  V_D = c(extract_final_V(rw_control)[["D"]], extract_final_V(adaptive_control)[["D"]]),
  B_multiplier = c(rw_alpha * rw_beta, salience * alpha_end[["B"]]),
  D_multiplier = c(rw_alpha * rw_beta, salience * alpha_end[["D"]]),
  row.names = c("Rescorla-Wagner", "Adaptive associability")
)
formatted <- table_2
formatted[] <- lapply(table_2, paper_number, digits = 3)
cat("Table 2\n")
print(formatted)


# The transfer phase (Section 3.3): B and D each alone with the new outcome O2
# for ten trials, starting from zero value for O2. Rescorla-Wagner keeps its
# fixed rate; the adaptive model keeps the associability each cue had after
# compound training. Element k + 1 of each V holds the value after k trials.
k <- 0:10
transfer_B <- make_trials("B", 1, 10, "New outcome")
transfer_D <- make_trials("D", 1, 10, "New outcome")
rw_B <- rw_simulate(transfer_B, alpha = c(B = rw_alpha), beta = rw_beta,
                    V_init = c(B = 0), cue_names = "B")
rw_D <- rw_simulate(transfer_D, alpha = c(D = rw_alpha), beta = rw_beta,
                    V_init = c(D = 0), cue_names = "D")
adaptive_B <- adaptive_simulate(transfer_B, S = c(B = salience), alpha_init = alpha_end["B"],
                                eta = eta, V_init = c(B = 0), cue_names = "B")
adaptive_D <- adaptive_simulate(transfer_D, S = c(D = salience), alpha_init = alpha_end["D"],
                                eta = eta, V_init = c(D = 0), cue_names = "D")

# The history effect H_M(k) = V_D(k) - V_B(k) (Equation 11).
H_rw <- rw_D$V - rw_B$V
H_adaptive <- adaptive_D$V - adaptive_B$V
peak <- which.max(H_adaptive)

cat("Sections 3.3-3.4, ten trials of each cue alone with the new outcome O2\n")
cat(sprintf("  first trial (Equation 10): B gains %s, D gains %s\n",
            paper_number(adaptive_B$V[2], 3), paper_number(adaptive_D$V[2], 3)))
cat(sprintf("  H_adaptive(k): %s at k = 1, peak %s at k = %d, %s at k = 10; H_RW(k) = 0\n",
            paper_number(H_adaptive[2], 3), paper_number(H_adaptive[peak], 3), k[peak],
            paper_number(H_adaptive[11], 3)))
# The first update on which B gains more than D, and the associability
# difference alpha_D - alpha_B at the start and after ten updates.
B_ahead <- which(diff(adaptive_B$V) > diff(adaptive_D$V))[1]
alpha_gap <- adaptive_D$alpha - adaptive_B$alpha
cat(sprintf("  update %d is the first on which B gains more: B gains %s, D gains %s\n", B_ahead,
            paper_number(diff(adaptive_B$V)[B_ahead], 3), paper_number(diff(adaptive_D$V)[B_ahead], 3)))
cat(sprintf("  alpha_D - alpha_B: %s at the start, %s after ten updates\n",
            paper_number(alpha_gap[1], 3), paper_number(alpha_gap[11], 3)))
cat(sprintf("  after ten trials: Rescorla-Wagner B = D = %s; adaptive B = %s, D = %s\n",
            paper_number(rw_B$V[11], 3), paper_number(adaptive_B$V[11], 3),
            paper_number(adaptive_D$V[11], 3)))


# Figure 4: (a) the training histories; (b, c) the values for O2 under each
# model; (d) the history effect.
ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
red <- paper_colours[["red"]]
blue <- paper_colours[["blue"]]

# The rows of panel a differ in what B and D are trained beside in Phase 2 (a
# pretrained companion for B, a new one for D); Phase 3, the part plotted in
# panels b-d, is the same for both cues and is shaded.
phase_x <- c(3.35, 6.25, 9.35)
design_panel <- ggplot() +
  annotate("rect", xmin = 7.8, xmax = 10.9, ymin = -.02, ymax = 3.3,
           fill = paper_colours[["tint"]], colour = NA) +
  annotate("text", x = phase_x, y = 2.95,
           label = c("paste('Phase 1: ', italic(O)[1])",
                     "paste('Phase 2: ', italic(O)[1])",
                     "paste('Phase 3: new task, ', italic(O)[2])"),
           parse = TRUE, size = text_size, colour = ink) +
  annotate("segment", x = 0, xend = 10.9, y = 2.62, yend = 2.62,
           linewidth = .3, colour = paper_colours[["light"]]) +
  annotate("text", x = 0, y = 2.2, label = "Blocked cue B", hjust = 0,
           colour = red, size = text_size) +
  annotate("text", x = 0, y = 1.45, label = "Control cue D", hjust = 0,
           colour = blue, size = text_size) +
  annotate("text", x = phase_x, y = 2.2,
           label = c("'10 A+'", "'10 AB+'", "'10 B–' * italic(O)[2]"), parse = TRUE,
           size = text_size, colour = ink) +
  annotate("text", x = phase_x, y = 1.45,
           label = c("'10 C+'", "'10 DE+'", "'10 D–' * italic(O)[2]"), parse = TRUE,
           size = text_size, colour = ink) +
  annotate("segment", x = c(4.25, 4.25, 7.15, 7.15), xend = c(5.25, 5.25, 8.3, 8.3),
           y = c(2.2, 1.45, 2.2, 1.45), yend = c(2.2, 1.45, 2.2, 1.45),
           colour = grey, linewidth = .35,
           arrow = arrow(length = unit(3.5, "pt"), type = "closed")) +
  annotate("text", x = 5.3, y = c(.8, .27), parse = TRUE,
           label = c("paste('B learns alongside A, which already predicts ', italic(O)[1], ';')",
                     "'D learns alongside E, which is new'"),
           size = text_size, colour = grey) +
  annotate("text", x = phase_x[3], y = c(.8, .27),
           label = c("Each cue alone;", "new values start at 0"),
           size = text_size, colour = grey) +
  coord_cartesian(xlim = c(-.05, 10.95), ylim = c(-.02, 3.3), expand = FALSE, clip = "off") +
  labs(title = "(a)  Training histories and the new task") +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 10, face = "bold", colour = ink,
                              margin = margin(b = 6)),
    plot.title.position = "plot",
    plot.margin = margin(6, 9, 10, 6)
  )

x_axis <- scale_x_continuous(breaks = c(0, 5, 10), expand = expansion(mult = 0))
x_title <- expression("Completed updates with the new outcome, " * italic(k))
value_axes <- list(
  x_axis,
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)),
  coord_cartesian(xlim = c(-.3, 14.2), ylim = c(-.02, 1.02), clip = "off"),
  labs(x = x_title, y = expression(italic(V) * " for " * italic(O)[2])),
  theme_paper()
)

# The two Rescorla-Wagner curves are identical (Equation 9). A blue line under
# a red dashed line shows both cues on one path without hiding either color.
rw_panel <- ggplot(data.frame(k = k, V = rw_B$V), aes(k, V)) +
  geom_line(colour = blue, linewidth = .75) +
  geom_line(colour = red, linewidth = .75, linetype = "22") +
  annotate("text", x = 10.5, y = rw_B$V[11],
           label = paste0("B = D\n", paper_number(rw_B$V[11], 3)),
           colour = ink, size = text_size, hjust = 0, lineheight = .95) +
  value_axes +
  labs(title = "(b)  Rescorla–Wagner",
       subtitle = "Same update multiplier, αβ = .12")

adaptive_curves <- data.frame(k = k, V = c(adaptive_B$V, adaptive_D$V),
                              cue = rep(c("B", "D"), each = length(k)))
adaptive_panel <- ggplot(adaptive_curves, aes(k, V, colour = cue)) +
  geom_line(linewidth = .75) +
  annotate("text", x = 10.5, y = adaptive_D$V[11],
           label = paste0("D  ", paper_number(adaptive_D$V[11], 3)),
           colour = blue, size = text_size, hjust = 0) +
  annotate("text", x = 10.5, y = adaptive_B$V[11],
           label = paste0("B  ", paper_number(adaptive_B$V[11], 3)),
           colour = red, size = text_size, hjust = 0) +
  scale_colour_manual(values = c(B = red, D = blue)) +
  value_axes +
  labs(title = "(c)  Adaptive associability",
       subtitle = sprintf("Starting associability: B %s, D %s",
                          paper_number(alpha_end[["B"]], 3), paper_number(alpha_end[["D"]], 3)),
       y = NULL)

gap_panel <- ggplot(data.frame(k = k, H = H_adaptive), aes(k, H)) +
  geom_line(data = data.frame(k = k, H = H_rw),
            colour = grey, linewidth = .65, linetype = "22") +
  geom_line(colour = ink, linewidth = .75) +
  geom_point(data = data.frame(k = k[peak], H = H_adaptive[peak]), shape = 21, fill = "white",
             colour = ink, size = 2.4, stroke = .6) +
  annotate("text", x = k[peak], y = H_adaptive[peak] + .034,
           label = paste("peak", paper_number(H_adaptive[peak], 3)), colour = ink,
           size = text_size) +
  annotate("text", x = 10.5, y = H_adaptive[11], label = paper_number(H_adaptive[11], 3),
           colour = ink, size = text_size, hjust = 0) +
  annotate("text", x = 5.5, y = .092, label = "Adaptive",
           colour = ink, size = text_size, hjust = 0) +
  annotate("text", x = 10, y = .024, label = "Rescorla–Wagner",
           colour = grey, size = text_size, hjust = 1) +
  x_axis +
  scale_y_continuous(breaks = c(0, .1, .2, .3), labels = c("0", ".1", ".2", ".3"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(-.3, 14.2), ylim = c(-.01, .345), clip = "off") +
  labs(title = "(d)  The B–D gap",
       subtitle = "D minus B within each model",
       x = x_title, y = expression(italic(H) * " = " * italic(V)[D] * " − " * italic(V)[B])) +
  theme_paper()

bottom_row <- rw_panel + adaptive_panel + gap_panel +
  plot_layout(widths = c(1, 1.16, .94), axis_titles = "collect_x")
save_figure(design_panel / bottom_row + plot_layout(heights = c(1.2, 2.2)),
            "fig04_new_outcome", width = 7, height = 4.75)


# The adaptive model's value for O2 after k = 0, ..., 10 trials alone, computed
# directly from Equations 7 and 8 rather than by simulation (Section 3.4).
value_alone <- function(alpha_start) {
  V <- numeric(11)
  alpha <- alpha_start
  for (j in 1:10) {
    V[j + 1] <- V[j] + salience * alpha * (1 - V[j])
    alpha <- min(1, alpha + eta * V[j])
  }
  V
}

# Values reported in the paper
stopifnot(
  abs(rw_gain_B[11] - .0334) < 5e-5, abs(rw_gain_D[11] - .12) < 1e-12,  # Section 3.2
  abs(blocking_A$V[11] - .848) < 5e-4,                                # Section 3.2
  abs(fixed_A$V[11] - .803) < 5e-4,                                   # Section 3.2
  abs(1 - blocking_A$V[11] - .152) < 5e-4,                            # Section 3.2
  abs(adaptive_gain_B[11] - .150) < 1e-12,                            # Section 3.2
  abs(adaptive_gain_D[11] - .150) < 1e-12,                            # Section 3.2
  abs(blocking_B$alpha[12] - (.5 + eta * ((1 - blocking_A$V[11]) - 1))) < 1e-12,  # Equation 8
  abs(blocking_B$alpha[12] - .458) < 5e-4,                            # Section 3.2
  abs(adaptive_gain_B[12] - .3 * blocking_B$alpha[12] * .85) < 1e-12, # Section 3.2
  abs(adaptive_gain_B[12] - .117) < 5e-4,                             # Section 3.2
  abs(adaptive_gain_D[12] - .3 * .500 * .85) < 1e-12,                 # Section 3.2: .128
  abs(alpha_end[["B"]] - .235) < 5e-4,                                # Section 3.2
  abs(alpha_end[["D"]] - .500) < 1e-12,                               # Section 3.2
  max(abs(as.matrix(table_2) - rbind(c(.130, .468, .120, .120),
                                     c(.681, .803, .071, .150)))) < 5e-4,  # Table 2
  max(abs(rw_B$V - (1 - (1 - rw_alpha * rw_beta)^k))) < 1e-12,        # Equation 3, Figure 4b
  max(abs(H_rw)) < 1e-12,                                             # Equation 9
  abs(adaptive_B$V[2] - salience * alpha_end[["B"]]) < 1e-12,         # Equation 10
  abs(adaptive_B$V[2] - .071) < 5e-4,                                 # Section 3.3
  abs(adaptive_D$V[2] - salience * alpha_end[["D"]]) < 1e-12,         # Equation 10: .150
  max(abs(adaptive_B$V - value_alone(alpha_end[["B"]]))) < 1e-12,     # Section 3.4, Figure 4c
  max(abs(adaptive_D$V - value_alone(alpha_end[["D"]]))) < 1e-12,     # Section 3.4, Figure 4c
  abs(H_adaptive[2] - .079) < 5e-4,                                   # Section 3.4
  k[peak] == 8, abs(H_adaptive[peak] - .284) < 5e-4,                  # Section 3.4, Figure 4d
  abs(H_adaptive[11] - .272) < 5e-4,                                  # Section 3.4, Figure 4d
  all(diff(adaptive_B$V)[1:8] < diff(adaptive_D$V)[1:8]),             # Section 3.4
  B_ahead == 9,                                                       # Section 3.4
  abs(diff(adaptive_B$V)[9] - .051) < 5e-4,                           # Section 3.4
  abs(diff(adaptive_D$V)[9] - .048) < 5e-4,                           # Section 3.4
  all(diff(adaptive_B$V)[9:10] > diff(adaptive_D$V)[9:10]),           # Section 3.4
  abs(alpha_gap[1] - .265) < 5e-4, abs(alpha_gap[11] - .365) < 5e-4,  # Section 3.4
  diff(alpha_gap)[1] == 0, all(diff(alpha_gap)[2:10] > 0),           # Section 3.4
  abs(rw_B$V[11] - .721) < 5e-4,                                      # Figure 4b
  abs(adaptive_B$V[11] - .576) < 5e-4,                                # Figure 4c, Table 3
  abs(adaptive_D$V[11] - .848) < 5e-4                                 # Figure 4c, Table 3
)
