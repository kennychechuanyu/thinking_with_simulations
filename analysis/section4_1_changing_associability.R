# Section 4.1: checking the explanation by changing the model. Runs the
# new-outcome comparison of Section 3.3 four ways (Original, Prevent, Remove and
# Supply), computes the B-D gap H(k) of Equation 11 for each, and draws Figure 5.
#   Rscript analysis/section4_1_changing_associability.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of Section 3. The new task has ten trials; the runs follow it for
# twenty updates to show how Original and Supply develop beyond it.
n_trials <- 10   # trials in each of Phases 1 and 2, and in the new task
n_followed <- 20 # updates with the new outcome shown in Figure 5b
salience <- .3   # fixed salience S_i
alpha_init <- .5 # initial associability
eta <- .05       # associability-change rate
lambda <- 1

# Original: blocking (10 A+, 10 AB+) and control (10 C+, 10 DE+) histories.
blocked <- adaptive_simulate(
  c(make_trials("A", lambda, n_trials, "Phase 1"),
    make_trials(c("A", "B"), lambda, n_trials, "Phase 2")),
  S = c(A = salience, B = salience), alpha_init = c(A = alpha_init, B = alpha_init),
  eta = eta
)
control <- adaptive_simulate(
  c(make_trials("C", lambda, n_trials, "Phase 1"),
    make_trials(c("D", "E"), lambda, n_trials, "Phase 2")),
  S = c(C = salience, D = salience, E = salience),
  alpha_init = c(C = alpha_init, D = alpha_init, E = alpha_init),
  eta = eta
)
alpha_B <- extract_final_alpha(blocked)[["B"]]
alpha_D <- extract_final_alpha(control)[["D"]]

# Prevent: Phase 1 as in Original; during compound training eta = 0, so values
# update but associabilities do not.
blocked_phase1 <- adaptive_simulate(
  make_trials("A", lambda, n_trials, "Phase 1"),
  S = c(A = salience, B = salience), alpha_init = c(A = alpha_init, B = alpha_init),
  eta = eta, cue_names = c("A", "B")
)
control_phase1 <- adaptive_simulate(
  make_trials("C", lambda, n_trials, "Phase 1"),
  S = c(C = salience, D = salience, E = salience),
  alpha_init = c(C = alpha_init, D = alpha_init, E = alpha_init),
  eta = eta, cue_names = c("C", "D", "E")
)
blocked_fixed <- adaptive_simulate(
  make_trials(c("A", "B"), lambda, n_trials, "Phase 2"),
  S = c(A = salience, B = salience), alpha_init = extract_final_alpha(blocked_phase1),
  eta = 0,
  V_init = extract_final_V(blocked_phase1), cue_names = c("A", "B")
)
control_fixed <- adaptive_simulate(
  make_trials(c("D", "E"), lambda, n_trials, "Phase 2"),
  S = c(C = salience, D = salience, E = salience),
  alpha_init = extract_final_alpha(control_phase1),
  eta = 0,
  V_init = extract_final_V(control_phase1), cue_names = c("C", "D", "E")
)
prevent_alpha_B <- extract_final_alpha(blocked_fixed)[["B"]]
prevent_alpha_D <- extract_final_alpha(control_fixed)[["D"]]

# The new task: a cue trained alone with the new outcome from value 0. Returns
# its value after k = 0, ..., 20 trials. In the adaptive model the cue starts
# with associability alpha, which keeps updating.
adaptive_values <- function(cue, alpha) {
  adaptive_simulate(make_trials(cue, lambda, n_followed, "Phase 3"),
                    S = setNames(salience, cue), alpha_init = setNames(alpha, cue),
                    eta = eta,
                    V_init = setNames(0, cue), cue_names = cue)$V
}

# Supply: a Rescorla-Wagner learner with fixed salience alpha. With beta = S,
# its update multiplier alpha * beta equals the adaptive model's S * alpha at
# the start of the new task.
supplied_values <- function(cue, alpha) {
  rw_simulate(make_trials(cue, lambda, n_followed, "Phase 3"),
              alpha = setNames(alpha, cue), beta = salience,
              V_init = setNames(0, cue), cue_names = cue)$V
}

# The gap H(k) = V_D(k) - V_B(k) of Equation 11 in each run. Remove resets both
# associabilities to .5 before the new task.
gap <- data.frame(
  k = 0:n_followed,
  Original = adaptive_values("D", alpha_D) - adaptive_values("B", alpha_B),
  Prevent = adaptive_values("D", prevent_alpha_D) - adaptive_values("B", prevent_alpha_B),
  Remove = adaptive_values("D", alpha_init) - adaptive_values("B", alpha_init),
  Supply = supplied_values("D", alpha_D) - supplied_values("B", alpha_B)
)

# Gaps after the first update, at the peak of Original, at the end of the
# ten-trial task and after the last update followed.
first <- gap[gap$k == 1, ]
peak <- gap[which.max(gap$Original), ]
task_end <- gap[gap$k == n_trials, ]
last <- gap[gap$k == n_followed, ]


# Figure 5
ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
light <- paper_colours[["light"]]
red <- paper_colours[["red"]]
blue <- paper_colours[["blue"]]

# (a) What each run changes, and B's and D's update multipliers at the start
# of the new task (S * alpha, or alpha * beta for Supply).
runs <- data.frame(
  y = c(3, 2, 1, 0),
  run = c("Original", "Prevent", "Remove", "Supply"),
  change = c("Nothing; associability keeps updating in the new task",
             "Associability held fixed during compound training",
             "Both associabilities reset to .5 when the new task begins",
             "Rescorla–Wagner learner with fixed saliences; multiplier stays fixed"),
  B = paper_number(salience * c(alpha_B, prevent_alpha_B, alpha_init, alpha_B), 3),
  D = paper_number(salience * c(alpha_D, prevent_alpha_D, alpha_init, alpha_D), 3)
)
column_B <- 6
column_D <- 6.6

panel_a <- ggplot() +
  annotate("text", x = .9, y = 4.0, label = "What the run changes",
           hjust = 0, size = text_size, colour = grey) +
  annotate("text", x = (column_B + column_D) / 2, y = 5.1,
           label = "Update multiplier\nentering the new task", lineheight = .95,
           size = text_size, colour = grey) +
  annotate("text", x = column_B, y = 4.0, label = "B", size = text_size, colour = red) +
  annotate("text", x = column_D, y = 4.0, label = "D", size = text_size, colour = blue) +
  annotate("segment", x = 0, xend = 6.75, y = 3.5, yend = 3.5,
           linewidth = .3, colour = light) +
  annotate("text", x = 0, y = runs$y, label = runs$run, hjust = 0,
           size = text_size, colour = ink, fontface = "bold") +
  annotate("text", x = .9, y = runs$y, label = runs$change, hjust = 0,
           size = text_size, colour = ink) +
  annotate("text", x = column_B, y = runs$y, label = runs$B, size = text_size, colour = ink) +
  annotate("text", x = column_D, y = runs$y, label = runs$D, size = text_size, colour = ink) +
  coord_cartesian(xlim = c(0, 6.75), ylim = c(-.3, 5.5), expand = FALSE, clip = "off") +
  labs(title = "(a)  Four runs of the new task",
       subtitle = expression(
         "Each cue is trained alone, so every run uses the same form of error, λ − " *
           italic(V) * "; new values start at 0")) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 10, face = "bold", colour = ink, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, colour = grey, margin = margin(b = 4)),
    plot.title.position = "plot",
    plot.margin = margin(6, 9, 10, 6)
  )

# (b) The gap in each run. Prevent and Remove give the same zero line, drawn
# once. The dotted line marks the end of the ten-trial task.
label_x <- n_followed + .5

panel_b <- ggplot(gap) +
  annotate("segment", x = n_trials, xend = n_trials, y = -.025, yend = .335,
           colour = light, linewidth = .35, linetype = "13") +
  annotate("text", x = n_trials + .25, y = .335, hjust = 0, vjust = 1,
           size = text_size, colour = grey, label = "End of the ten-trial task") +
  geom_line(aes(k, Supply), colour = grey, linewidth = .7, linetype = "22") +
  geom_line(aes(k, Prevent), colour = ink, linewidth = .7) +
  geom_line(aes(k, Original), colour = ink, linewidth = .7) +
  geom_point(data = peak, aes(k, Original), shape = 21, fill = "white",
             colour = ink, size = 2.4, stroke = .6) +
  annotate("text", x = peak$k, y = peak$Original - .032,
           label = paste("peak", paper_number(peak$Original, 3)),
           size = text_size, colour = ink) +
  annotate("segment", x = 1.3, xend = 3.2, y = first$Original - .004, yend = .052,
           colour = ink, linewidth = .3) +
  annotate("text", x = 3.4, y = .045, hjust = 0, size = text_size, colour = ink,
           label = paste0("Original and Supply agree for two updates:\n",
                          paper_number(first$Original, 3), " after the first")) +
  annotate("text", x = label_x, y = last$Supply, hjust = 0, size = text_size, colour = grey,
           label = sprintf("Supply  %s", paper_number(last$Supply, 3))) +
  annotate("text", x = label_x, y = last$Original, hjust = 0, size = text_size, colour = ink,
           label = sprintf("Original  %s", paper_number(last$Original, 3))) +
  annotate("text", x = label_x, y = 0, hjust = 0, size = text_size, colour = ink,
           label = "Prevent, Remove  0") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), expand = expansion(add = c(.5, 0))) +
  scale_y_continuous(breaks = c(0, .1, .2, .3), labels = c("0", ".1", ".2", ".3"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, n_followed), ylim = c(-.025, .345), clip = "off") +
  labs(title = "(b)  The B–D gap in the new task",
       subtitle = "Adaptive model solid; Rescorla–Wagner learner dashed",
       x = expression("Completed updates with the new outcome, " * italic(k)),
       y = expression(italic(H) * " = " * italic(V)[D] * " − " * italic(V)[B])) +
  theme_paper() +
  theme(plot.margin = margin(6, 118, 6, 6))

figure <- panel_a / (panel_b + plot_spacer() + plot_layout(widths = c(1, .12))) +
  plot_layout(heights = c(1, 1.5))

cat("Section 4.1, changing associability: Original, Prevent, Remove and Supply\n")
cat(sprintf("After compound training: alpha_B = %s, alpha_D = %s (Prevent: %s, %s)\n",
            paper_number(alpha_B, 3), paper_number(alpha_D, 3),
            paper_number(prevent_alpha_B, 3), paper_number(prevent_alpha_D, 3)))
cat(sprintf("Update multipliers S * alpha entering the new task: B %s, D %s\n",
            paper_number(salience * alpha_B, 3), paper_number(salience * alpha_D, 3)))
cat("Gap H(k) = V_D(k) - V_B(k) after k trials with the new outcome:\n")
shown <- gap[gap$k %in% c(1, 2, 3, 5, 6, 8, 9, 10, 15, 20), ]
shown[-1] <- lapply(shown[-1], paper_number, digits = 3)
print(shown, row.names = FALSE)
save_figure(figure, "fig05_changing_associability", width = 7, height = 4.6)

# Values reported in the paper
stopifnot(
  abs(alpha_B - .235) < 5e-4, abs(alpha_D - .5) < 1e-12,                 # Sections 3.2 and 4.1
  abs(prevent_alpha_B - .5) < 1e-12, abs(prevent_alpha_D - .5) < 1e-12,  # Section 4.1
  max(abs(gap$Prevent)) < 1e-12, max(abs(gap$Remove)) < 1e-12,           # Section 4.1
  abs(salience * alpha_B - .071) < 5e-4,                                 # Section 4.1, Figure 5a
  abs(salience * alpha_D - .150) < 5e-4,                                 # Section 4.1, Figure 5a
  abs(first$Original - .079) < 5e-4,                                     # Section 3.4, Figure 5b
  peak$k == 8, abs(peak$Original - .284) < 5e-4,                         # Section 3.4, Figure 5b
  max(abs(gap$Supply - gap$Original)[gap$k <= 2]) < 1e-12,               # Section 4.1, Figure 5b
  all((gap$Original > gap$Supply)[gap$k %in% 3:8]),                      # Section 4.1
  abs(max((gap$Original - gap$Supply)[gap$k %in% 3:8]) - .004) < 5e-4,  # Section 4.1
  all((gap$Supply > gap$Original)[gap$k >= 9]),                          # Section 4.1, Figure 5b
  all(diff((gap$Supply - gap$Original)[gap$k >= 9]) > 0),                # Section 4.1
  abs(task_end$Supply - .284) < 5e-4, abs(task_end$Original - .272) < 5e-4,  # Section 4.1
  abs(last$Supply - .192) < 5e-4, abs(last$Original - .068) < 5e-4       # Section 4.1, Figure 5b
)
