# Section 4.2: what carries over to the pear trials. A fraction g_alpha of each
# light's change in associability (Equation 4) and a fraction g_V of its
# learned value for the apple (Equation 5) carry over. Computes both models'
# B-D gaps, compares them after eight pear trials as g_V varies from 0 to 1,
# follows both gaps with g_V = .90, and draws Figure 5.
#   Rscript analysis/section4_2_carryover.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
light <- paper_colours[["light"]]

# Settings of Sections 2 and 3: the learning rate .12 under Rescorla-Wagner,
# and salience .3, starting associability .5 and eta = .05 under the adaptive
# model.
rw_rate <- .12
salience <- .3
alpha_init <- .5
eta <- .05


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

# One light trained alone with the pear for n trials, k = 0, ..., n, from the
# value V_start: under the adaptive model from associability alpha_start, or
# under Rescorla-Wagner.
pear_curve <- function(cue, alpha_start, V_start = 0, n = 10) {
  adaptive_simulate(make_trials(cue, 1, n, "Pear"), S = setNames(salience, cue),
                    alpha_init = setNames(alpha_start, cue), eta = eta,
                    V_init = setNames(V_start, cue), cue_names = cue)$V
}
rw_pear <- function(cue, V_start, n = 10) {
  rw_simulate(make_trials(cue, 1, n, "Pear"), beta = setNames(rw_rate, cue),
              V_init = setNames(V_start, cue), cue_names = cue)$V
}

# Both models' gaps V_D(k) - V_B(k), k = 0, ..., n, when a fraction g_V of each
# light's learned value for the apple and a fraction g_alpha of its change in
# associability carry over (Equations 4 and 5).
carry <- function(g_V, g_alpha = 1, n = 10) {
  alpha_start <- alpha_init + g_alpha * (alpha_end - alpha_init)
  data.frame(k = 0:n, g_V = g_V,
             H_RW = rw_pear("D", g_V * rw_apple[["D"]], n) - rw_pear("B", g_V * rw_apple[["B"]], n),
             H_adaptive = pear_curve("D", alpha_start[["D"]], g_V * apple_V[["D"]], n) -
               pear_curve("B", alpha_start[["B"]], g_V * apple_V[["B"]], n))
}
# The gaps after eight pear trials for g_V = 0, .01, ..., 1, with full, half
# and no carryover of associability, the g_V at which the two models' gaps are
# equal under full carryover, and the example of Figure 5b.
g_V_grid <- seq(0, 1, by = .01)
at_eight <- function(g_alpha) do.call(rbind, lapply(g_V_grid, function(g) carry(g, g_alpha)[9, ]))
eight_full <- at_eight(1)
eight_half <- at_eight(.5)
eight_none <- at_eight(0)
crossing <- uniroot(function(g) { run <- carry(g); run$H_adaptive[9] - run$H_RW[9] },
                    c(.3, 1), tol = 1e-10)$root
crossing_gap <- carry(crossing)$H_RW[9]
example <- carry(.90)
# Under Rescorla-Wagner, each light's remaining error shrinks by 12% on every
# pear trial, and so does the difference between the two values.
rw_formula <- function(g_V, k) g_V * (rw_apple[["D"]] - rw_apple[["B"]]) * (1 - rw_rate)^k
peak_example <- which.max(example$H_adaptive)

stopifnot(
  # associability carryover: B starts at .235 with full carryover, .368 with
  # half, .5 with none; half carryover halves the gap after one pear trial,
  # from .079 to .040, and with none there is no gap
  paper_number(alpha_end[["B"]], 3) == ".235", paper_number(alpha_end[["D"]], 3) == ".500",
  paper_number(alpha_init + .5 * (alpha_end[["B"]] - alpha_init), 3) == ".368",
  paper_number(carry(0, 1)$H_adaptive[2], 3) == ".079",
  paper_number(carry(0, .5)$H_adaptive[2], 3) == ".040",
  abs(carry(0, .5)$H_adaptive[2] - salience * .5 * (alpha_end[["D"]] - alpha_end[["B"]])) < 1e-12,
  max(abs(carry(0, 0)$H_adaptive)) < 1e-12,
  # Rescorla-Wagner with full value carryover: B starts at .130 and D at .468,
  # B gains .104 and D .064 on the first pear trial, and the gap shrinks by 12%
  # on every pear trial, so it is largest before the first
  paper_number(rw_apple[["B"]], 3) == ".130", paper_number(rw_apple[["D"]], 3) == ".468",
  paper_number(rw_pear("B", rw_apple[["B"]], 1)[2] - rw_apple[["B"]], 3) == ".104",
  paper_number(rw_pear("D", rw_apple[["D"]], 1)[2] - rw_apple[["D"]], 3) == ".064",
  max(abs(carry(1)$H_RW - rw_formula(1, 0:10))) < 1e-12,
  max(abs(carry(.37)$H_RW - rw_formula(.37, 0:10))) < 1e-12,
  # Figure 5a: as g_V rises, the Rescorla-Wagner gap after eight pear trials
  # grows and the adaptive gap shrinks; they are equal at g_V = .895
  all(diff(eight_full$H_RW) > 0), all(diff(eight_full$H_adaptive) < 0),
  paper_number(crossing, 3) == ".895",
  # with less associability carryover the adaptive gap is smaller still, and
  # without it Rescorla-Wagner gives the larger gap whenever g_V > 0
  all(eight_half$H_adaptive < eight_full$H_adaptive),
  all(eight_none$H_adaptive < eight_half$H_adaptive),
  all((eight_none$H_RW > eight_none$H_adaptive)[g_V_grid > 0]),
  # Figure 5b, g_V = .90: the Rescorla-Wagner gap is .304 before the first
  # pear trial and shrinks; the adaptive gap starts at .110, first grows and
  # then shrinks; after eight pear trials the gaps are .109 and .108
  paper_number(example$H_RW[1], 3) == ".304", all(diff(example$H_RW) < 0),
  paper_number(example$H_adaptive[1], 3) == ".110",
  peak_example > 1, all(diff(example$H_adaptive[peak_example:11]) < 0),
  paper_number(example$H_RW[9], 3) == ".109", paper_number(example$H_adaptive[9], 3) == ".108"
)
cat(sprintf("The two gaps after eight pear trials are equal at g_V = %.4f, where both are %.4f\n",
            crossing, crossing_gap))


# Figure 5. (a) Each model's gap after eight pear trials as g_V varies, with
# g_alpha = 1. (b) Both gaps across the ten pear trials with g_V = .90.
x_title <- expression("Completed pear trials, " * italic(k))
carryover_panel <- ggplot(eight_full) +
  geom_line(aes(g_V, H_RW), colour = grey, linewidth = .7, linetype = "22") +
  geom_line(aes(g_V, H_adaptive), colour = ink, linewidth = .7) +
  annotate("point", x = 0, y = eight_full$H_adaptive[1], shape = 21, size = 2.4,
           fill = "white", colour = ink, stroke = .6) +
  annotate("text", x = .04, y = eight_full$H_adaptive[1] + .022, label = "settings of Section 3",
           size = text_size, colour = ink, hjust = 0) +
  annotate("point", x = crossing, y = crossing_gap, shape = 21, size = 2.4,
           fill = "white", colour = ink, stroke = .6) +
  annotate("segment", x = crossing, xend = crossing, y = crossing_gap - .01, yend = .068,
           colour = ink, linewidth = .3) +
  annotate("text", x = crossing, y = .052, label = "equal gaps", size = text_size, colour = ink) +
  annotate("text", x = crossing, y = .029,
           label = sprintf("paste('at ', italic(g)[V], ' ≈ %s')", paper_number(crossing, 3)),
           parse = TRUE, size = text_size, colour = ink) +
  annotate("text", x = .25, y = eight_full$H_RW[g_V_grid == .25] + .036,
           label = "Rescorla–Wagner", size = text_size, colour = grey) +
  annotate("text", x = .42, y = eight_full$H_adaptive[g_V_grid == .42] + .036,
           label = "Adaptive", size = text_size, colour = ink) +
  scale_x_continuous(breaks = c(0, .25, .5, .75, 1), labels = c("0", ".25", ".5", ".75", "1"),
                     expand = expansion(add = c(.03, 0))) +
  scale_y_continuous(breaks = c(0, .1, .2, .3), labels = c("0", ".1", ".2", ".3"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-.005, .35), clip = "off") +
  labs(title = "(a)  Each model’s gap after eight pear trials",
       subtitle = expression("Associability fully carried over, " * italic(g)[italic("α")] * " = 1"),
       x = expression("Fraction of apple value carried over, " * italic(g)[V]),
       y = expression(italic(V)[D] * "(8) − " * italic(V)[B] * "(8)")) +
  theme_paper() +
  theme(plot.margin = margin(6, 22, 6, 6))

history_panel <- ggplot(example) +
  annotate("segment", x = 8, xend = 8, y = -.005, yend = .35,
           colour = light, linewidth = .35, linetype = "13") +
  geom_line(aes(k, H_RW), colour = grey, linewidth = .7, linetype = "22") +
  geom_line(aes(k, H_adaptive), colour = ink, linewidth = .7) +
  annotate("text", x = .4, y = example$H_RW[1] + .004, hjust = 0, vjust = 0,
           label = sprintf("Rescorla–Wagner, %s at the start", paper_number(example$H_RW[1], 3)),
           size = text_size, colour = grey) +
  annotate("text", x = .3, y = example$H_adaptive[1] - .036, hjust = 0,
           label = sprintf("Adaptive, %s at the start", paper_number(example$H_adaptive[1], 3)),
           size = text_size, colour = ink) +
  annotate("text", x = 7.8, y = .012, hjust = 1, vjust = 0, label = "8 trials",
           size = text_size, colour = grey) +
  scale_x_continuous(breaks = c(0, 5, 10), expand = expansion(add = c(.3, 0))) +
  scale_y_continuous(breaks = c(0, .1, .2, .3), labels = c("0", ".1", ".2", ".3"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, 10), ylim = c(-.005, .35), clip = "off") +
  labs(title = "(b)  Similar later gaps, different time courses",
       subtitle = expression(italic(g)[V] * " = .90, " * italic(g)[italic("α")] * " = 1"),
       x = x_title, y = NULL) +
  theme_paper()

save_figure(carryover_panel + plot_spacer() + history_panel + plot_layout(widths = c(1, .06, 1)),
            "fig05_carryover", width = 7, height = 3.35)
