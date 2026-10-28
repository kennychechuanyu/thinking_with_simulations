# Section 5: from model states to predicted behavior. Applies the sigmoid of
# Equation 7 (gamma = 8, theta = .5) and the threshold rule (theta = .5) to the
# adaptive model's values for the pear from Section 3.4, and to both models'
# values in the example of Section 4.2 with g_V = .90. Computes the values
# reported in Sections 5.2 and 5.3 and Table 6, and draws Figure 6.
#   Rscript analysis/section5_responses.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
red <- paper_colours[["red"]]
blue <- paper_colours[["blue"]]

# Settings of Sections 2 and 3, and the response functions of Section 5.2.
rw_rate <- .12
salience <- .3
alpha_init <- .5
eta <- .05
gamma <- 8
theta <- .5


# Both models after the training histories of Section 3: B's and D's learned
# values for the apple, and the adaptive model's associabilities.
blocking_design <- c(make_trials("A", 1, 10, "Phase 1"),
                     make_trials(c("A", "B"), 1, 10, "Phase 2"))
control_design <- c(make_trials("X", 1, 10, "Phase 1"),
                    make_trials(c("C", "D"), 1, 10, "Phase 2"))
rw_apple <- c(B = rw_final(blocking_design, beta = c(A = rw_rate, B = rw_rate))[["B"]],
              D = rw_final(control_design, beta = c(X = rw_rate, C = rw_rate, D = rw_rate))[["D"]])
blocking_end <- adaptive_final_state(blocking_design, S = c(A = salience, B = salience),
                                     alpha_init = c(A = alpha_init, B = alpha_init), eta = eta)
control_end <- adaptive_final_state(control_design,
                                    S = c(X = salience, C = salience, D = salience),
                                    alpha_init = c(X = alpha_init, C = alpha_init,
                                                   D = alpha_init), eta = eta)
alpha_end <- c(B = blocking_end$alpha[["B"]], D = control_end$alpha[["D"]])
apple_V <- c(B = blocking_end$V[["B"]], D = control_end$V[["D"]])

# One light alone with the pear for n trials, k = 0, ..., n, from the value
# V_start, under the adaptive model or under Rescorla-Wagner.
pear_curve <- function(cue, alpha_start, V_start = 0, n = 10) {
  adaptive_simulate(make_trials(cue, 1, n, "Pear"), S = setNames(salience, cue),
                    alpha_init = setNames(alpha_start, cue), eta = eta,
                    V_init = setNames(V_start, cue), cue_names = cue)$V
}
rw_pear <- function(cue, V_start, n = 10) {
  rw_simulate(make_trials(cue, 1, n, "Pear"), beta = setNames(rw_rate, cue),
              V_init = setNames(V_start, cue), cue_names = cue)$V
}


# Table 6. The response on pear trial k + 1 is given before the pear appears,
# so it uses the value after k completed pear trials, and the ten responses use
# k = 0, ..., 9.
responses <- data.frame(k = 0:9,
                        V_B = pear_curve("B", alpha_end[["B"]])[1:10],
                        V_D = pear_curve("D", alpha_end[["D"]])[1:10])
responses$H <- responses$V_D - responses$V_B
responses$p_B <- obs_sigmoid(responses$V_B, gamma = gamma, theta = theta)
responses$p_D <- obs_sigmoid(responses$V_D, gamma = gamma, theta = theta)
responses$delta_p <- responses$p_D - responses$p_B
responses$threshold_B <- obs_threshold(responses$V_B, theta = theta)
responses$threshold_D <- obs_threshold(responses$V_D, theta = theta)
threshold_window <- responses$k[responses$threshold_D != responses$threshold_B]

# Section 5.3: the example of Section 4.2 with g_V = .90, after eight pear
# trials: B's and D's values under Rescorla-Wagner, then under the adaptive
# model, and the differences in yes probability under each model.
carried <- c(rw_pear("B", .9 * rw_apple[["B"]], 8)[9],
             rw_pear("D", .9 * rw_apple[["D"]], 8)[9],
             pear_curve("B", alpha_end[["B"]], .9 * apple_V[["B"]], n = 8)[9],
             pear_curve("D", alpha_end[["D"]], .9 * apple_V[["D"]], n = 8)[9])
carried_delta_p <- c(obs_sigmoid(carried[2], gamma, theta) - obs_sigmoid(carried[1], gamma, theta),
                     obs_sigmoid(carried[4], gamma, theta) - obs_sigmoid(carried[3], gamma, theta))

stopifnot(
  # Table 6, learned values and yes probabilities
  identical(unname(paper_number(responses$V_B, 3)),
            c(".000", ".071", ".136", ".198", ".257", ".314", ".369", ".423", ".476", ".527")),
  # Table 6 rounds D's exact .2775 up, as the text rounds .1275 to .128; the
  # small offset keeps binary rounding error from turning such a tie down.
  identical(unname(paper_number(round(responses$V_D + 1e-12, 3), 3)),
            c(".000", ".150", ".278", ".388", ".483", ".567", ".640", ".704", ".760", ".807")),
  identical(unname(paper_number(responses$p_B, 3)),
            c(".018", ".031", ".052", ".082", ".125", ".184", ".260", ".351", ".451", ".553")),
  identical(unname(paper_number(responses$p_D, 3)),
            c(".018", ".057", ".144", ".289", ".467", ".631", ".755", ".837", ".889", ".921")),
  # Table 6, threshold rule: no for B until the tenth response, yes for D from
  # the sixth, so the predicted responses differ only after 5-8 pear trials
  all(responses$threshold_B == c(rep(0, 9), 1)),
  all(responses$threshold_D == c(rep(0, 5), rep(1, 5))),
  identical(threshold_window, 5:8),
  # 5.2: after five pear trials, .314 and .567 give .184 and .631; the values
  # differ by .253 and the probabilities by .447
  paper_number(obs_sigmoid(.314, gamma, theta), 3) == ".184",
  paper_number(responses$H[6], 3) == ".253", paper_number(responses$delta_p[6], 3) == ".447",
  # 5.3: the difference in yes probability is largest after six pear trials,
  # the B-D gap after eight
  responses$k[which.max(responses$delta_p)] == 6, responses$k[which.max(responses$H)] == 8,
  # 5.3: with g_V = .90, gaps of .109 and .108 after eight pear trials, from
  # .683 and .792 under Rescorla-Wagner and .842 and .950 under the adaptive
  # model, give differences in yes probability of .100 and .034
  paper_number(carried[2] - carried[1], 3) == ".109",
  paper_number(carried[4] - carried[3], 3) == ".108",
  identical(unname(paper_number(carried, 3)), c(".683", ".792", ".842", ".950")),
  identical(unname(paper_number(carried_delta_p, 3)), c(".100", ".034"))
)
cat(sprintf("Largest difference in yes probability: %.3f, after %d pear trials\n",
            max(responses$delta_p), responses$k[which.max(responses$delta_p)]))


# Figure 6. (a) The two response functions, and where B's and D's values fall
# on the sigmoid before the 1st, 6th and 10th responses. (b) The B-D gap and the
# difference in yes probabilities before each of the ten responses. The band
# marks where the threshold rule predicts different responses.
small_text_size <- 8.5 / .pt
value_axis <- seq(0, 1, length.out = 501)
sigmoid_curve <- data.frame(V = value_axis, p = obs_sigmoid(value_axis, gamma = gamma, theta = theta))
threshold_step <- data.frame(V = c(0, theta, theta, 1), p = c(0, 0, 1, 1))
shown <- responses[responses$k %in% c(0, 5, 9), ]
on_curve <- rbind(data.frame(k = shown$k, cue = "B", V = shown$V_B, p = shown$p_B),
                  data.frame(k = shown$k, cue = "D", V = shown$V_D, p = shown$p_D))
# Label positions next to each point; at k = 0 the two points coincide and
# carry one label.
point_labels <- data.frame(
  cue = c("B", "B", "D", "D"), k = c(5, 9, 5, 9),
  x = c(shown$V_B[2] - .03, shown$V_B[3] + .035, shown$V_D[2] - .035, shown$V_D[3] + .03),
  y = c(shown$p_B[2] + .05, shown$p_B[3] - .045, shown$p_D[2] + .02, shown$p_D[3] - .06),
  h = c(1, 0, 1, 0)
)
unit_breaks <- scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                                  expand = expansion(mult = 0))

functions_panel <- ggplot() +
  geom_path(data = threshold_step, aes(V, p), colour = grey, linewidth = .45) +
  geom_line(data = sigmoid_curve, aes(V, p), colour = ink, linewidth = .7) +
  geom_point(data = on_curve, aes(V, p, colour = cue), size = 1.8) +
  geom_text(data = point_labels, aes(x, y, label = k, colour = cue, hjust = h),
            size = small_text_size) +
  annotate("text", x = .035, y = shown$p_B[1] + .06, label = "0", hjust = 0,
           size = small_text_size, colour = ink) +
  annotate("text", x = .46, y = .93, label = "threshold", size = text_size,
           colour = grey, hjust = 1) +
  annotate("text", x = .66, y = .40, label = "sigmoid", size = text_size,
           colour = ink, hjust = 0) +
  annotate("point", x = c(.74, .74), y = c(.15, .07), colour = c(red, blue), size = 1.8) +
  annotate("text", x = c(.77, .77), y = c(.15, .07), label = c("B", "D"),
           colour = c(red, blue), size = text_size, hjust = 0) +
  scale_colour_manual(values = c(B = red, D = blue)) +
  scale_x_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)) +
  unit_breaks +
  coord_cartesian(xlim = c(-.02, 1.02), ylim = c(-.02, 1.02), clip = "off") +
  labs(title = "(a)  Two response functions",
       subtitle = expression("Shared by B and D, with points labeled by " * italic(k)),
       x = expression("Learned value, " * italic(V)),
       y = expression("Probability of a yes response, " * italic(p))) +
  theme_paper() +
  theme(plot.margin = margin(6, 14, 6, 6))

peak_p <- which.max(responses$delta_p)
peak_H <- which.max(responses$H)
differences_panel <- ggplot(responses) +
  annotate("rect", xmin = min(threshold_window) - .5, xmax = max(threshold_window) + .5,
           ymin = 0, ymax = .6, fill = paper_colours[["tint"]]) +
  annotate("text", x = mean(range(threshold_window)), y = .02, vjust = 0, lineheight = .95,
           label = "threshold rule:\nresponses differ", size = small_text_size, colour = grey) +
  geom_line(aes(k, H), colour = grey, linewidth = .7, linetype = "22") +
  geom_line(aes(k, delta_p), colour = ink, linewidth = .7) +
  geom_point(data = responses[peak_p, ], aes(k, delta_p), shape = 21, fill = "white",
             colour = ink, size = 2.3, stroke = .6) +
  geom_point(data = responses[peak_H, ], aes(k, H), shape = 21, fill = "white",
             colour = grey, size = 2.3, stroke = .6) +
  annotate("text", x = responses$k[peak_p], y = responses$delta_p[peak_p] + .04,
           label = paste("largest after", responses$k[peak_p]), size = small_text_size,
           colour = ink) +
  annotate("text", x = responses$k[peak_H], y = responses$H[peak_H] - .045,
           label = paste("largest after", responses$k[peak_H]), size = small_text_size,
           colour = grey) +
  annotate("text", x = 9.35, y = responses$delta_p[10], hjust = 0, parse = TRUE,
           label = "'yes probability'", size = text_size, colour = ink) +
  annotate("text", x = 9.35, y = responses$H[10], hjust = 0, parse = TRUE,
           label = "'learned value'", size = text_size, colour = grey) +
  scale_x_continuous(breaks = 0:9, expand = expansion(add = c(.3, 0))) +
  scale_y_continuous(breaks = c(0, .2, .4, .6), labels = c("0", ".2", ".4", ".6"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, .6), clip = "off") +
  labs(title = "(b)  D minus B before each response",
       subtitle = expression("Ten responses, after " * italic(k) * " = 0 to 9 pear trials"),
       x = expression("Completed pear trials before the response, " * italic(k)),
       y = "D minus B") +
  theme_paper() +
  theme(plot.margin = margin(6, 92, 6, 6))

save_figure(functions_panel + differences_panel + plot_layout(widths = c(1, 1.15)),
            "fig06_responses", width = 7, height = 3.3)
