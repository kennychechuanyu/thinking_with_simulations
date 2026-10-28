# Section 4.1: checking the explanation of the B-D gap by changing the
# adaptive model. Prevent holds associability fixed during the compound trials,
# Remove resets B's and D's associabilities to .5 before the pear trials, and
# Supply gives a Rescorla-Wagner learner the learning rates with which the
# adaptive model starts the pear trials. Computes the values reported in
# Section 4.1 and draws Figure 4.
#   Rscript analysis/section4_1_interventions.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
light <- paper_colours[["light"]]
red <- paper_colours[["red"]]
blue <- paper_colours[["blue"]]

# Settings of Section 3: salience .3, starting associability .5, eta = .05.
salience <- .3
alpha_init <- .5
eta <- .05


# The adaptive model of Section 3: 10 A+ then 10 AB+ for B, 10 X+ then 10 CD+
# for the comparison light D. B's and D's associabilities and learned values
# for the apple after the compound trials.
blocking_design <- c(make_trials("A", 1, 10, "Phase 1"),
                     make_trials(c("A", "B"), 1, 10, "Phase 2"))
control_design <- c(make_trials("X", 1, 10, "Phase 1"),
                    make_trials(c("C", "D"), 1, 10, "Phase 2"))
blocking_end <- adaptive_final_state(blocking_design, S = c(A = salience, B = salience),
                                     alpha_init = c(A = alpha_init, B = alpha_init), eta = eta)
control_end <- adaptive_final_state(control_design,
                                    S = c(X = salience, C = salience, D = salience),
                                    alpha_init = c(X = alpha_init, C = alpha_init,
                                                   D = alpha_init), eta = eta)
alpha_end <- c(B = blocking_end$alpha[["B"]], D = control_end$alpha[["D"]])
apple_V <- c(B = blocking_end$V[["B"]], D = control_end$V[["D"]])

# pear_curve() gives the values of one light trained alone with the pear for n
# trials, k = 0, ..., n, under the adaptive model, or, with fixed = TRUE, under
# Rescorla-Wagner with the fixed learning rate salience * alpha_start (Supply).
pear_curve <- function(cue, alpha_start, V_start = 0, n = 20, fixed = FALSE) {
  trials <- make_trials(cue, 1, n, "Pear")
  if (fixed) {
    return(rw_simulate(trials, beta = setNames(salience * alpha_start, cue),
                       V_init = setNames(V_start, cue), cue_names = cue)$V)
  }
  adaptive_simulate(trials, S = setNames(salience, cue), alpha_init = setNames(alpha_start, cue),
                    eta = eta, V_init = setNames(V_start, cue), cue_names = cue)$V
}

# Prevent: eta = 0 during the compound trials, so every associability keeps the
# value it had after the first phase, and B and D keep their starting
# associability of .5, while the learned values change as usual.
blocking_phase1 <- adaptive_simulate(make_trials("A", 1, 10, "Phase 1"),
                                     S = c(A = salience, B = salience),
                                     alpha_init = c(A = alpha_init, B = alpha_init),
                                     eta = eta, cue_names = c("A", "B"))
control_phase1 <- adaptive_simulate(make_trials("X", 1, 10, "Phase 1"),
                                    S = c(X = salience, C = salience, D = salience),
                                    alpha_init = c(X = alpha_init, C = alpha_init, D = alpha_init),
                                    eta = eta, cue_names = c("X", "C", "D"))
blocking_held <- adaptive_simulate(make_trials(c("A", "B"), 1, 10, "Phase 2"),
                                   S = c(A = salience, B = salience),
                                   alpha_init = extract_final_alpha(blocking_phase1), eta = 0,
                                   V_init = extract_final_V(blocking_phase1),
                                   cue_names = c("A", "B"))
control_held <- adaptive_simulate(make_trials(c("C", "D"), 1, 10, "Phase 2"),
                                  S = c(X = salience, C = salience, D = salience),
                                  alpha_init = extract_final_alpha(control_phase1), eta = 0,
                                  V_init = extract_final_V(control_phase1),
                                  cue_names = c("X", "C", "D"))
prevent_alpha <- c(B = extract_final_alpha(blocking_held)[["B"]],
                   D = extract_final_alpha(control_held)[["D"]])
prevent_V <- c(B = extract_final_V(blocking_held)[["B"]], D = extract_final_V(control_held)[["D"]])

# The B-D gap in each run, k = 0, ..., 20, with learned values for the pear
# starting at zero.
gap_runs <- data.frame(
  k = 0:20,
  Unchanged = pear_curve("D", alpha_end[["D"]]) - pear_curve("B", alpha_end[["B"]]),
  Prevent = pear_curve("D", prevent_alpha[["D"]]) - pear_curve("B", prevent_alpha[["B"]]),
  Remove = pear_curve("D", alpha_init) - pear_curve("B", alpha_init),
  Supply = pear_curve("D", alpha_end[["D"]], fixed = TRUE) -
    pear_curve("B", alpha_end[["B"]], fixed = TRUE)
)
# Prevent and Remove if the learned values for the apple carried over in full.
prevent_carried <- pear_curve("D", prevent_alpha[["D"]], prevent_V[["D"]]) -
  pear_curve("B", prevent_alpha[["B"]], prevent_V[["B"]])
remove_carried <- pear_curve("D", alpha_init, apple_V[["D"]]) -
  pear_curve("B", alpha_init, apple_V[["B"]])
# The associabilities during the ten pear trials of the Unchanged run.
pear_alpha <- function(cue, alpha_start) {
  adaptive_simulate(make_trials(cue, 1, 10, "Pear"), S = setNames(salience, cue),
                    alpha_init = setNames(alpha_start, cue), eta = eta,
                    V_init = setNames(0, cue), cue_names = cue)$alpha
}
rise_B <- diff(pear_alpha("B", alpha_end[["B"]]))
rise_D <- diff(pear_alpha("D", alpha_end[["D"]]))
unchanged_minus_supply <- gap_runs$Unchanged - gap_runs$Supply   # row k + 1 holds k
peak_row <- which.max(gap_runs$Unchanged)

stopifnot(
  # Prevent: B keeps associability .5 and reaches .803 for the apple, as D
  # does, instead of .681
  all(prevent_alpha == alpha_init),
  paper_number(prevent_V[["B"]], 3) == ".803", prevent_V[["B"]] == prevent_V[["D"]],
  paper_number(apple_V[["B"]], 3) == ".681", paper_number(alpha_end[["B"]], 3) == ".235",
  # Prevent and Remove give no gap; with full carryover of learned value,
  # Prevent still gives none, and Remove leaves B behind by .803 - .681 = .122
  max(abs(gap_runs$Prevent)) < 1e-12, max(abs(gap_runs$Remove)) < 1e-12,
  max(abs(prevent_carried)) < 1e-12, paper_number(remove_carried[1], 3) == ".122",
  # Supply: learning rates .071 and .150; the same gaps as Unchanged after one
  # and two pear trials (.079 after the first), associability unchanged on the
  # first pear trial and rising faster for D on the later ones
  paper_number(salience * alpha_end[["B"]], 3) == ".071",
  paper_number(salience * alpha_end[["D"]], 3) == ".150",
  max(abs(unchanged_minus_supply[1:3])) < 1e-12,
  paper_number(gap_runs$Unchanged[2], 3) == ".079",
  rise_B[1] == 0, rise_D[1] == 0, all(rise_D[2:10] > rise_B[2:10]),
  # over the ten pear trials, the two gaps differ by at most about .012
  paper_number(max(abs(unchanged_minus_supply[1:11])), 3) == ".012",
  # after twenty pear trials, .192 under Supply and .068 under Unchanged;
  # Figure 4b marks the Unchanged peak, .284 after eight
  paper_number(gap_runs$Supply[21], 3) == ".192",
  paper_number(gap_runs$Unchanged[21], 3) == ".068",
  gap_runs$k[peak_row] == 8, paper_number(gap_runs$Unchanged[peak_row], 3) == ".284"
)
cat(sprintf("Largest difference between the Unchanged and Supply gaps over ten pear trials: %.4f\n",
            max(abs(unchanged_minus_supply[1:11]))))


# Figure 4. (a) What each run changes, and B's and D's learning rates at the
# start of the pear trials.
runs <- data.frame(
  y = c(3, 2, 1, 0),
  run = c("Unchanged", "Prevent", "Remove", "Supply"),
  change = c("Nothing (the adaptive model of Section 3)",
             "Associability held fixed during the compound trials",
             "B’s and D’s associabilities reset to .5 before the pear trials",
             "Rescorla–Wagner learner, learning rates fixed at these values"),
  B = paper_number(salience * c(alpha_end[["B"]], prevent_alpha[["B"]], alpha_init,
                                alpha_end[["B"]]), 3),
  D = paper_number(salience * c(alpha_end[["D"]], prevent_alpha[["D"]], alpha_init,
                                alpha_end[["D"]]), 3)
)
column_B <- 6
column_D <- 6.6

runs_panel <- ggplot() +
  annotate("text", x = .9, y = 4.0, label = "What the run changes",
           hjust = 0, size = text_size, colour = grey) +
  annotate("text", x = column_D + .2, y = 5.1,
           label = "Learning rate at the start\nof the pear trials", lineheight = .95,
           hjust = 1, size = text_size, colour = grey) +
  annotate("text", x = column_B, y = 4.0, label = "B", size = text_size, colour = red) +
  annotate("text", x = column_D, y = 4.0, label = "D", size = text_size, colour = blue) +
  annotate("segment", x = 0, xend = 6.75, y = 3.5, yend = 3.5, linewidth = .3, colour = light) +
  annotate("text", x = 0, y = runs$y, label = runs$run, hjust = 0,
           size = text_size, colour = ink, fontface = "bold") +
  annotate("text", x = .9, y = runs$y, label = runs$change, hjust = 0,
           size = text_size, colour = ink) +
  annotate("text", x = column_B, y = runs$y, label = runs$B, size = text_size, colour = ink) +
  annotate("text", x = column_D, y = runs$y, label = runs$D, size = text_size, colour = ink) +
  coord_cartesian(xlim = c(0, 6.75), ylim = c(-.3, 5.5), expand = FALSE, clip = "off") +
  labs(title = "(a)  Four runs of the pear trials",
       subtitle = "Each light is presented alone, and its value for the pear starts at 0") +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 10, face = "bold", colour = ink, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, colour = grey, margin = margin(b = 4)),
    plot.title.position = "plot",
    plot.margin = margin(6, 9, 10, 6)
  )

# (b) The gap in each run. Prevent and Remove give the same zero line, drawn
# once. The shaded region lies beyond the ten pear trials of the task.
x_title <- expression("Completed pear trials, " * italic(k))
gap_end <- gap_runs[gap_runs$k == 20, ]
gap_axis <- scale_y_continuous(breaks = c(0, .1, .2, .3), labels = c("0", ".1", ".2", ".3"),
                               expand = expansion(mult = 0))
interventions_panel <- ggplot(gap_runs) +
  annotate("rect", xmin = 10, xmax = 20, ymin = -.025, ymax = .345,
           fill = paper_colours[["tint"]], colour = NA) +
  annotate("text", x = 10.3, y = .335, hjust = 0, vjust = 1, size = text_size, colour = grey,
           label = "Beyond the ten pear trials") +
  geom_line(aes(k, Supply), colour = grey, linewidth = .7, linetype = "22") +
  geom_line(aes(k, Prevent), colour = ink, linewidth = .7) +
  geom_line(aes(k, Unchanged), colour = ink, linewidth = .7) +
  geom_point(data = gap_runs[peak_row, ], aes(k, Unchanged), shape = 21, fill = "white",
             colour = ink, size = 2.4, stroke = .6) +
  annotate("text", x = gap_runs$k[peak_row], y = gap_runs$Unchanged[peak_row] - .032,
           label = paste("peak", paper_number(gap_runs$Unchanged[peak_row], 3)),
           size = text_size, colour = ink) +
  annotate("segment", x = 1.3, xend = 3.2, y = gap_runs$Unchanged[2] - .004, yend = .052,
           colour = ink, linewidth = .3) +
  annotate("text", x = 3.4, y = .045, hjust = 0, size = text_size, colour = ink,
           lineheight = .95,
           label = paste0("Unchanged and Supply agree\nfor two trials: ",
                          paper_number(gap_runs$Unchanged[2], 3), " after the first")) +
  annotate("text", x = 20.5, y = gap_end$Supply, hjust = 0, size = text_size, colour = grey,
           label = sprintf("Supply  %s", paper_number(gap_end$Supply, 3))) +
  annotate("text", x = 20.5, y = gap_end$Unchanged, hjust = 0, size = text_size, colour = ink,
           label = sprintf("Unchanged  %s", paper_number(gap_end$Unchanged, 3))) +
  annotate("text", x = 20.5, y = 0, hjust = 0, size = text_size, colour = ink,
           label = "Prevent, Remove  0") +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), expand = expansion(add = c(.5, 0))) +
  gap_axis +
  coord_cartesian(xlim = c(0, 20), ylim = c(-.025, .345), clip = "off") +
  labs(title = "(b)  The B–D gap during the pear trials",
       subtitle = "Adaptive model solid, Rescorla–Wagner learner dashed",
       x = x_title, y = expression(italic(V)[D] * " − " * italic(V)[B])) +
  theme_paper() +
  theme(plot.margin = margin(6, 118, 6, 6))

save_figure(runs_panel / (interventions_panel + plot_spacer() + plot_layout(widths = c(1, .12))) +
              plot_layout(heights = c(1, 1.5)),
            "fig04_interventions", width = 7, height = 4.6)
