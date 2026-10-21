# Section 4.4: would the difference appear in an experiment? Applies the
# sigmoid response function (Equation 5, gamma = 8 and theta = .5) and the
# threshold rule (Equation 6, theta = .5) to the adaptive model's values for B
# and D while they learn about O2, computes the predicted response difference
# of Equation 19 and the threshold window of Equation 20, prints Table 3 and
# draws Figure 8.
#   Rscript analysis/section4_4_predicted_responses.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of Section 3 for the adaptive model, and the response functions of
# Section 4.4.
lambda <- 1
salience <- .3
alpha_init <- .5
eta <- .05
n_trials <- 10
gamma <- 8
theta <- .5

# The original setting of Section 3: B and D start learning about O2 with
# value zero and the associabilities left by training with O1.
blocked <- adaptive_simulate(c(make_trials("A", lambda, n_trials, "Phase 1"),
                               make_trials(c("A", "B"), lambda, n_trials, "Phase 2")),
                             S = c(A = salience, B = salience),
                             alpha_init = c(A = alpha_init, B = alpha_init),
                             eta = eta)
control <- adaptive_simulate(c(make_trials("C", lambda, n_trials, "Phase 1"),
                               make_trials(c("D", "E"), lambda, n_trials, "Phase 2")),
                             S = c(C = salience, D = salience, E = salience),
                             alpha_init = c(C = alpha_init, D = alpha_init, E = alpha_init),
                             eta = eta)
transfer_B <- adaptive_simulate(make_trials("B", lambda, n_trials, "Phase 3"), S = c(B = salience),
                                alpha_init = extract_final_alpha(blocked)["B"],
                                eta = eta, V_init = c(B = 0), cue_names = "B")
transfer_D <- adaptive_simulate(make_trials("D", lambda, n_trials, "Phase 3"), S = c(D = salience),
                                alpha_init = extract_final_alpha(control)["D"],
                                eta = eta, V_init = c(D = 0), cue_names = "D")

# Values, their gap H(k) and response probabilities after k = 0, ..., 10
# completed updates, and the response difference p_D - p_B (Equation 19) under
# each rule.
responses <- data.frame(k = 0:n_trials, V_B = transfer_B$V, V_D = transfer_D$V)
responses$H <- responses$V_D - responses$V_B
responses$p_B <- obs_sigmoid(responses$V_B, gamma = gamma, theta = theta)
responses$p_D <- obs_sigmoid(responses$V_D, gamma = gamma, theta = theta)
responses$sigmoid_difference <- responses$p_D - responses$p_B
responses$threshold_difference <- obs_threshold(responses$V_D, theta = theta) -
  obs_threshold(responses$V_B, theta = theta)

# Equation 20: under the threshold rule, B and D produce different responses
# only when V_B(k) <= theta < V_D(k).
threshold_window <- responses$k[responses$threshold_difference != 0]
# Table 3 gives the values after 1, 5 and 10 updates.
table_rows <- responses[responses$k %in% c(1, 5, 10), ]


# Figure 8
ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
tint <- paper_colours[["tint"]]
small_text_size <- 8.5 / .pt
learned_values <- rbind(data.frame(k = responses$k, cue = "B", V = responses$V_B),
                        data.frame(k = responses$k, cue = "D", V = responses$V_D))
on_curve <- rbind(
  data.frame(k = table_rows$k, cue = "B", V = table_rows$V_B, p = table_rows$p_B),
  data.frame(k = table_rows$k, cue = "D", V = table_rows$V_D, p = table_rows$p_D)
)
value_axis <- seq(0, 1, length.out = 501)
sigmoid_curve <- data.frame(V = value_axis,
                            p = obs_sigmoid(value_axis, gamma = gamma, theta = theta))
threshold_step <- data.frame(V = c(0, theta, theta, 1), p = c(0, 0, 1, 1))
window_start <- min(threshold_window) - .5
window_end <- max(threshold_window) + .5

cue_colours <- scale_colour_manual(values = c(B = paper_colours[["red"]],
                                              D = paper_colours[["blue"]]))
updates_x <- scale_x_continuous(breaks = c(0, 5, 10), expand = expansion(mult = 0))
unit_y <- scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                             expand = expansion(mult = 0))
theme_figure <- theme_paper() + theme(plot.margin = margin(6, 8, 6, 6))

# (a) The learned values, with the threshold of the threshold rule; the window
# of Equation 20 is shaded.
panel_a <- ggplot() +
  annotate("rect", xmin = window_start, xmax = window_end, ymin = 0, ymax = 1, fill = tint) +
  geom_hline(yintercept = theta, colour = grey, linewidth = .3) +
  annotate("text", x = .15, y = theta + .045, label = "θ = .5",
           size = small_text_size, colour = grey, hjust = 0) +
  annotate("text", x = 4.3, y = .93, label = "D above .5,\nB below", size = small_text_size,
           colour = grey, hjust = 1, lineheight = .95) +
  geom_line(data = learned_values, aes(k, V, colour = cue), linewidth = .7) +
  geom_point(data = learned_values[learned_values$k %in% table_rows$k, ],
             aes(k, V, colour = cue), size = 1.7) +
  annotate("text", x = 10.45, y = responses$V_D[responses$k == 10], label = "D",
           colour = paper_colours[["blue"]], size = text_size, hjust = 0) +
  annotate("text", x = 10.45, y = responses$V_B[responses$k == 10], label = "B",
           colour = paper_colours[["red"]], size = text_size, hjust = 0) +
  cue_colours + updates_x + unit_y +
  coord_cartesian(xlim = c(-.3, 11.4), ylim = c(-.02, 1.02), clip = "off") +
  labs(title = "(a)  Learned values",
       subtitle = "Adaptive model, original setting",
       x = expression("Completed updates, " * italic(k)),
       y = expression(italic(V) * " for " * italic(O)[2])) +
  theme_figure

# (b) The two response functions, and where B's and D's values fall on the
# sigmoid after the updates of Table 3. Each point is labeled with its number
# of updates.
panel_b <- ggplot() +
  geom_path(data = threshold_step, aes(V, p), colour = grey, linewidth = .45) +
  geom_line(data = sigmoid_curve, aes(V, p), colour = ink, linewidth = .7) +
  geom_point(data = on_curve, aes(V, p, colour = cue), size = 1.7) +
  geom_text(data = transform(on_curve,
                             x = ifelse(k == 10, V + .025, V - .025),
                             y = ifelse(k == 10, p - .07, p + .06),
                             h = ifelse(k == 10, 0, 1)),
            aes(x, y, label = k, colour = cue, hjust = h), size = small_text_size) +
  annotate("text", x = .46, y = .93, label = "threshold", size = text_size,
           colour = grey, hjust = 1) +
  annotate("text", x = .63, y = .40, label = "sigmoid", size = text_size,
           colour = ink, hjust = 0) +
  cue_colours +
  scale_x_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)) +
  unit_y +
  coord_cartesian(xlim = c(-.02, 1.02), ylim = c(-.02, 1.02), clip = "off") +
  labs(title = "(b)  Two response functions",
       subtitle = expression("Same for B and D; numbers = " * italic(k)),
       x = expression("Learned value, " * italic(V)),
       y = expression("Response probability, " * italic(p))) +
  theme_figure

# (c) The response difference. Under the threshold rule it is 1 inside the
# window and 0 elsewhere, drawn as a bar; under Rescorla-Wagner it is 0.
panel_c <- ggplot() +
  annotate("rect", xmin = window_start, xmax = window_end, ymin = 0, ymax = 1, fill = tint,
           colour = grey, linewidth = .3) +
  geom_hline(yintercept = 0, colour = grey, linewidth = .5, linetype = "22") +
  geom_line(data = responses, aes(k, sigmoid_difference), colour = ink, linewidth = .7) +
  geom_point(data = table_rows, aes(k, sigmoid_difference), colour = ink, size = 1.7) +
  annotate("text", x = 1, y = .125, label = paper_number(table_rows$sigmoid_difference[1], 3),
           size = small_text_size, colour = ink) +
  annotate("text", x = 5.3, y = table_rows$sigmoid_difference[2] - .075,
           label = paper_number(table_rows$sigmoid_difference[2], 3),
           size = small_text_size, colour = ink, hjust = 0) +
  annotate("text", x = 10.5, y = responses$sigmoid_difference[responses$k == 10],
           label = paper_number(table_rows$sigmoid_difference[3], 3), size = small_text_size,
           colour = ink, hjust = 0) +
  annotate("text", x = (window_start + window_end) / 2, y = .92, label = "threshold",
           size = text_size, colour = grey) +
  annotate("text", x = 8.9, y = .47, label = "sigmoid", size = text_size,
           colour = ink, hjust = 0) +
  annotate("text", x = 11.3, y = -.062, label = "Rescorla–Wagner", size = small_text_size,
           colour = grey, hjust = 1) +
  updates_x + unit_y +
  coord_cartesian(xlim = c(-.3, 11.4), ylim = c(-.12, 1.02), clip = "off") +
  labs(title = "(c)  Response difference",
       subtitle = "Each function applied to (a)",
       x = expression("Completed updates, " * italic(k)),
       y = expression(italic(p)[D] * " − " * italic(p)[B])) +
  theme_figure

figure <- panel_a + panel_b + panel_c

cat("Table 3: k, V_B, V_D, p_B, p_D, and the sigmoid response difference p_D - p_B\n")
cat(sprintf("%2d  %s  %s  %s  %s  %s\n", table_rows$k,
            paper_number(table_rows$V_B, 3), paper_number(table_rows$V_D, 3),
            paper_number(table_rows$p_B, 4), paper_number(table_rows$p_D, 4),
            paper_number(table_rows$sigmoid_difference, 3)), sep = "")
cat(sprintf("Threshold rule: responses differ only after updates %d-%d\n",
            min(threshold_window), max(threshold_window)))
cat(sprintf("Sigmoid: largest response difference %s, after update %d\n",
            paper_number(max(responses$sigmoid_difference), 3),
            responses$k[which.max(responses$sigmoid_difference)]))
save_figure(figure, "fig08_predicted_responses", width = 7, height = 3.2)

# Values reported in the paper
stopifnot(
  abs(table_rows$V_B - c(.071, .314, .576)) < 5e-4,                   # Table 3
  abs(table_rows$V_D - c(.150, .567, .848)) < 5e-4,
  abs(table_rows$p_B - c(.0312, .1845, .6473)) < 5e-5,
  abs(table_rows$p_D - c(.0573, .6311, .9417)) < 5e-5,
  abs(table_rows$sigmoid_difference - c(.026, .447, .294)) < 5e-4,    # Section 4.4
  all(responses$H[-1] > 0),
  identical(threshold_window, 5:8),                                   # Equation 20
  all(responses$V_D[responses$k < 5] <= theta),
  all(responses$V_B[responses$k > 8] > theta),
  responses$k[which.max(responses$sigmoid_difference)] == 6,          # Figure 8c
  abs(responses$H[responses$k == 8] - .284) < 5e-4                    # Section 3.4
)
