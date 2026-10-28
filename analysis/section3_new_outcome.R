# Section 3: telling the two explanations apart. Runs both training histories
# of Table 1 under Rescorla-Wagner (Equation 1) and the adaptive model
# (Equations 2 and 3), then the pear trials of Section 3.4 with each light
# alone. Computes the values reported in Sections 3.1-3.4 and Table 3, draws
# Figure 3, and writes the trial-by-trial table of Supplement S4.2 (Table S4)
# to results/supplementS4_adaptive_trials.csv and results/supplement_table_s4.tex.
#   Rscript analysis/section3_new_outcome.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
red <- paper_colours[["red"]]
blue <- paper_colours[["blue"]]

# Settings of Sections 2 and 3: the learning rate .12 under Rescorla-Wagner,
# and salience .3, starting associability .5 and eta = .05 under the adaptive
# model.
rw_rate <- .12
salience <- .3
alpha_init <- .5
eta <- .05


# The two training histories of Table 1, each followed by the apple: 10 A+
# then 10 AB+ (light B with the trained tone A), and 10 X+ then 10 CD+ (the
# comparison light D with the new tone C). The histories share no cues.
blocking_design <- c(make_trials("A", 1, 10, "Phase 1"),
                     make_trials(c("A", "B"), 1, 10, "Phase 2"))
control_design <- c(make_trials("X", 1, 10, "Phase 1"),
                    make_trials(c("C", "D"), 1, 10, "Phase 2"))
rw_blocking <- rw_simulate(blocking_design, beta = c(A = rw_rate, B = rw_rate))
rw_control <- rw_simulate(control_design, beta = c(X = rw_rate, C = rw_rate, D = rw_rate))
adaptive_blocking <- adaptive_simulate(blocking_design, S = c(A = salience, B = salience),
                                       alpha_init = c(A = alpha_init, B = alpha_init), eta = eta)
adaptive_control <- adaptive_simulate(control_design,
                                      S = c(X = salience, C = salience, D = salience),
                                      alpha_init = c(X = alpha_init, C = alpha_init,
                                                     D = alpha_init), eta = eta)
final_value <- function(run, cue) run$V[run$cue == cue & run$trial == max(run$trial)]
alpha_end <- c(extract_final_alpha(adaptive_blocking)["B"],
               extract_final_alpha(adaptive_control)["D"])

# Sections 3.1 and 3.2: the first compound trials of both histories, the
# comparison history under Rescorla-Wagner, and the tone's first trials under
# the adaptive model. B and D start their compound trials at zero, so their
# values after the first compound trial are their first gains.
first_AB <- c(make_trials("A", 1, 10, "Phase 1"), make_trials(c("A", "B"), 1, 1, "Phase 2"))
first_CD <- c(make_trials("X", 1, 10, "Phase 1"), make_trials(c("C", "D"), 1, 1, "Phase 2"))
rw_first_B <- rw_final(first_AB, beta = c(A = rw_rate, B = rw_rate))[["B"]]
rw_first_D <- rw_final(first_CD, beta = c(X = rw_rate, C = rw_rate, D = rw_rate))[["D"]]
tone_two <- adaptive_final_state(make_trials("A", 1, 2, "Phase 1"), S = c(A = salience),
                                 alpha_init = c(A = alpha_init), eta = eta)
tone_ten <- adaptive_final_state(make_trials("A", 1, 10, "Phase 1"), S = c(A = salience),
                                 alpha_init = c(A = alpha_init), eta = eta)
first_compound <- adaptive_final_state(first_AB, S = c(A = salience, B = salience),
                                       alpha_init = c(A = alpha_init, B = alpha_init), eta = eta)
adaptive_first_D <- adaptive_final_state(first_CD, S = c(X = salience, C = salience, D = salience),
                                         alpha_init = c(X = alpha_init, C = alpha_init,
                                                        D = alpha_init), eta = eta)$V[["D"]]
# The second compound trial starts from the value .150 and the error .85 for
# both lights, but from B's lowered associability. Section 3.3 adds that B
# would gain only .137 on its first compound trial if the lowered
# associability took effect at once; tests/verify_models.R checks that it
# does not.
second_gain_B <- salience * first_compound$alpha[["B"]] * (1 - first_compound$V[["B"]])
second_gain_D <- salience * alpha_init * (1 - salience * alpha_init)

stopifnot(paper_number(rw_first_B, 4) == ".0334", abs(rw_first_D - .12) < 1e-12,
          paper_number(final_value(rw_control, "D"), 3) == ".468",
          abs(final_value(rw_control, "D") - (1 - .76^10) / 2) < 1e-12,
          paper_number(final_value(rw_blocking, "B"), 3) == ".130",
          paper_number(final_value(rw_blocking, "B"), 4) == ".1303",
          abs(salience * alpha_init - .15) < 1e-12,
          abs(tone_two$V[["A"]] - .2775) < 1e-12, abs(tone_two$alpha[["A"]] - .5075) < 1e-12,
          paper_number(tone_ten$V[["A"]], 3) == ".848",
          paper_number(1 - tone_ten$V[["A"]], 3) == ".152", paper_number(1 - .85^10, 3) == ".803",
          abs(first_compound$V[["B"]] - .15) < 1e-12, abs(adaptive_first_D - .15) < 1e-12,
          paper_number(first_compound$alpha[["B"]], 3) == ".458",
          paper_number(salience * first_compound$alpha[["B"]], 3) == ".137",
          paper_number(second_gain_B, 3) == ".117", paper_number(second_gain_D, 3) == ".128")


# Section 3.3 and Table 3: the states in which the two models leave the lights
# after the compound trials, as learned values for the apple and learning
# rates (beta under Rescorla-Wagner, S alpha under the adaptive model).
stopifnot(paper_number(alpha_end[["B"]], 3) == ".235", paper_number(alpha_end[["D"]], 3) == ".500",
          paper_number(final_value(adaptive_blocking, "B"), 3) == ".681",
          paper_number(final_value(adaptive_control, "D"), 3) == ".803",
          paper_number(salience * alpha_end[["B"]], 3) == ".071",
          paper_number(salience * alpha_end[["D"]], 3) == ".150")

# Section 3.3: with salience .24, the adaptive model's learning rates also
# start at .12, and B still ends the compound trials with a much lower
# learning rate than D.
salience_24 <- .24
blocking_24 <- adaptive_final_state(blocking_design, S = c(A = salience_24, B = salience_24),
                                    alpha_init = c(A = alpha_init, B = alpha_init), eta = eta)
control_24 <- adaptive_final_state(control_design,
                                   S = c(X = salience_24, C = salience_24, D = salience_24),
                                   alpha_init = c(X = alpha_init, C = alpha_init,
                                                  D = alpha_init), eta = eta)
stopifnot(abs(salience_24 * alpha_init - .12) < 1e-12,
          paper_number(blocking_24$alpha[["B"]], 3) == ".231",
          paper_number(salience_24 * blocking_24$alpha[["B"]], 3) == ".055",
          paper_number(salience_24 * control_24$alpha[["D"]], 3) == ".120")


# Section 3.4: each light alone with the pear for ten trials, starting from
# zero value for the pear. The adaptive model keeps each light's associability
# from the compound trials. Row k + 1 holds the value after k pear trials.
k <- 0:10
pear_B <- make_trials("B", 1, 10, "Pear")
pear_D <- make_trials("D", 1, 10, "Pear")
rw_B <- rw_simulate(pear_B, beta = c(B = rw_rate), V_init = c(B = 0), cue_names = "B")
rw_D <- rw_simulate(pear_D, beta = c(D = rw_rate), V_init = c(D = 0), cue_names = "D")
adaptive_B <- adaptive_simulate(pear_B, S = c(B = salience), alpha_init = alpha_end["B"],
                                eta = eta, V_init = c(B = 0), cue_names = "B")
adaptive_D <- adaptive_simulate(pear_D, S = c(D = salience), alpha_init = alpha_end["D"],
                                eta = eta, V_init = c(D = 0), cue_names = "D")
H_rw <- rw_D$V - rw_B$V
H_adaptive <- adaptive_D$V - adaptive_B$V
peak <- which.max(H_adaptive)

stopifnot(abs(rw_B$V[2] - .12) < 1e-12, all(H_rw == 0),
          paper_number(rw_B$V[11], 4) == ".7215", paper_number(adaptive_D$V[11], 3) == ".848",
          paper_number(adaptive_B$V[2], 3) == ".071", paper_number(adaptive_D$V[2], 3) == ".150",
          paper_number(H_adaptive[2], 3) == ".079",
          k[peak] == 8, paper_number(H_adaptive[peak], 3) == ".284",
          paper_number(H_adaptive[11], 3) == ".272",
          # the ninth pear trial: B gains more than D, and D stays ahead
          paper_number(adaptive_B$V[10] - adaptive_B$V[9], 3) == ".051",
          paper_number(adaptive_D$V[10] - adaptive_D$V[9], 3) == ".048",
          all(H_adaptive[-1] > 0))
cat(sprintf("B-D gap under the adaptive model: %.3f after one pear trial, %.3f after %d, %.3f after ten\n",
            H_adaptive[2], H_adaptive[peak], k[peak], H_adaptive[11]))


# Figure 3. (a) The training histories and the pear trials; (b) and (c) the
# values for the pear under each model; (d) the B-D gap.
phase_x <- c(3.35, 6.25, 9.35)
design_panel <- ggplot() +
  annotate("rect", xmin = 7.8, xmax = 10.9, ymin = -.02, ymax = 3.3,
           fill = paper_colours[["tint"]], colour = NA) +
  annotate("text", x = phase_x, y = 2.95,
           label = c("Phase 1: apple", "Phase 2: apple", "Phase 3: pear"),
           size = text_size, colour = ink) +
  annotate("segment", x = 0, xend = 10.9, y = 2.62, yend = 2.62,
           linewidth = .3, colour = paper_colours[["light"]]) +
  annotate("text", x = 0, y = 2.2, label = "B (light)", hjust = 0,
           colour = red, size = text_size) +
  annotate("text", x = 0, y = 1.45, label = "D (comparison light)", hjust = 0,
           colour = blue, size = text_size) +
  annotate("text", x = phase_x, y = 2.2, label = c("10 A+", "10 AB+", "10 B"),
           size = text_size, colour = ink) +
  annotate("text", x = phase_x, y = 1.45, label = c("10 X+", "10 CD+", "10 D"),
           size = text_size, colour = ink) +
  annotate("segment", x = c(4.25, 4.25, 7.15, 7.15), xend = c(5.25, 5.25, 8.3, 8.3),
           y = c(2.2, 1.45, 2.2, 1.45), yend = c(2.2, 1.45, 2.2, 1.45),
           colour = grey, linewidth = .35,
           arrow = arrow(length = unit(3.5, "pt"), type = "closed")) +
  annotate("text", x = 5.3, y = c(.8, .27),
           label = c("B appears with tone A, which predicts the apple",
                     "D appears with tone C, which is new"),
           size = text_size, colour = grey) +
  annotate("text", x = phase_x[3], y = c(.8, .27),
           label = c("Each light alone, with", "pear values starting at 0"),
           size = text_size, colour = grey) +
  coord_cartesian(xlim = c(-.05, 10.95), ylim = c(-.02, 3.3), expand = FALSE, clip = "off") +
  labs(title = "(a)  Training histories and the pear task") +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title = element_text(size = 10, face = "bold", colour = ink, margin = margin(b = 6)),
    plot.title.position = "plot",
    plot.margin = margin(6, 9, 10, 6)
  )

x_axis <- scale_x_continuous(breaks = c(0, 5, 10), expand = expansion(mult = 0))
x_title <- expression("Completed pear trials, " * italic(k))
value_axes <- list(
  x_axis,
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)),
  coord_cartesian(xlim = c(-.3, 14.2), ylim = c(-.02, 1.02), clip = "off"),
  labs(x = x_title, y = expression(italic(V) * " for the pear")),
  theme_paper()
)

# The two Rescorla-Wagner curves are identical. A blue line under a red dashed
# line shows both lights on one path.
rw_panel <- ggplot(data.frame(k = k, V = rw_B$V), aes(k, V)) +
  geom_line(colour = blue, linewidth = .75) +
  geom_line(colour = red, linewidth = .75, linetype = "22") +
  annotate("text", x = 10.5, y = rw_B$V[11],
           label = paste0("B = D\n", paper_number(rw_B$V[11], 4)),
           colour = ink, size = text_size, hjust = 0, lineheight = .95) +
  value_axes +
  labs(title = "(b)  Rescorla–Wagner",
       subtitle = expression("Same learning rate, " * beta[B] * " = " * beta[D] * " = .12"))

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
  labs(title = "(c)  Adaptive model",
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
  annotate("text", x = 5.5, y = .092, label = "Adaptive model",
           colour = ink, size = text_size, hjust = 0) +
  annotate("text", x = 10, y = .024, label = "Rescorla–Wagner",
           colour = grey, size = text_size, hjust = 1) +
  x_axis +
  scale_y_continuous(breaks = c(0, .1, .2, .3), labels = c("0", ".1", ".2", ".3"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(-.3, 14.2), ylim = c(-.01, .345), clip = "off") +
  labs(title = "(d)  The B–D gap",
       subtitle = "D minus B within each model",
       x = x_title, y = expression(italic(V)[D] * " − " * italic(V)[B])) +
  theme_paper()

bottom_row <- rw_panel + adaptive_panel + gap_panel +
  plot_layout(widths = c(1, 1.16, .94), axis_titles = "collect_x")
save_figure(design_panel / bottom_row + plot_layout(heights = c(1.2, 2.2)),
            "fig03_new_outcome", width = 7, height = 4.75)


# Supplement S4.2 and Table S4: every update of the adaptive model in the two
# training histories and the pear trials, with each cue's learned value and
# associability at the start of the trial, its own error, the error of the
# other cues present (of predicting zero for a cue alone), its gain, and its
# learned value and associability after the trial.
trace_trials <- function(light, design, V, alpha, S) {
  rows <- list()
  for (t in seq_along(design)) {
    cues <- design[[t]]$cues
    V_start <- V
    alpha_start <- alpha
    for (i in cues) {
      others <- setdiff(cues, i)
      e_own <- abs(1 - V_start[[i]])
      e_other <- abs(1 - if (length(others) > 0) sum(V_start[others]) else 0)
      gain <- S[[i]] * alpha_start[[i]] * (1 - V_start[[i]])
      V[[i]] <- V_start[[i]] + gain
      alpha[[i]] <- max(.01, min(1, alpha_start[[i]] + eta * (e_other - e_own)))
      rows[[length(rows) + 1]] <- data.frame(
        light = light, phase = design[[t]]$phase, trial = t, cue = i,
        V_before = V_start[[i]], alpha_before = alpha_start[[i]], e_own = e_own,
        e_other = e_other, gain = gain, V_after = V[[i]], alpha_after = alpha[[i]])
    }
  }
  list(rows = do.call(rbind, rows), V = V, alpha = alpha)
}
trace_history <- function(light, first, pair) {
  cues <- unique(c(first, pair))
  S <- setNames(rep(salience, length(cues)), cues)
  design <- c(make_trials(first, 1, 10, paste0(first, "+")),
              make_trials(pair, 1, 10, paste0(paste(pair, collapse = ""), "+")))
  training <- trace_trials(light, design, setNames(rep(0, length(cues)), cues),
                           setNames(rep(alpha_init, length(cues)), cues), S)
  pear <- trace_trials(light, make_trials(light, 1, 10, "pear"), setNames(0, light),
                       training$alpha[light], S[light])
  list(rows = rbind(training$rows, pear$rows), training = training, pear = pear)
}
trace_B <- trace_history("B", "A", c("A", "B"))
trace_D <- trace_history("D", "X", c("C", "D"))
trials <- rbind(trace_B$rows, trace_D$rows)

# The trace agrees with adaptive_simulate(), and the values that Supplement
# S4.2 and Sections 3.2-3.4 quote can be read from it.
row_of <- function(light, phase, trial, cue) {
  trials[trials$light == light & trials$phase == phase & trials$trial == trial & trials$cue == cue, ]
}
stopifnot(nrow(trials) == 80,
          abs(trace_B$training$alpha[["B"]] - alpha_end[["B"]]) < 1e-12,
          abs(trace_D$training$alpha[["D"]] - alpha_end[["D"]]) < 1e-12,
          abs(trace_B$training$V[["B"]] - final_value(adaptive_blocking, "B")) < 1e-12,
          abs(trace_D$training$V[["D"]] - final_value(adaptive_control, "D")) < 1e-12,
          max(abs(trace_B$pear$rows$V_after - adaptive_B$V[-1])) < 1e-12,
          max(abs(trace_D$pear$rows$V_after - adaptive_D$V[-1])) < 1e-12,
          paper_number(row_of("B", "A+", 10, "A")$V_after, 3) == ".848",
          paper_number(row_of("B", "A+", 2, "A")$alpha_after, 4) == ".5075",
          paper_number(row_of("B", "AB+", 11, "B")$gain, 3) == ".150",
          paper_number(row_of("B", "AB+", 11, "B")$alpha_after, 3) == ".458",
          paper_number(row_of("B", "AB+", 20, "B")$alpha_after, 3) == ".235",
          paper_number(row_of("D", "CD+", 20, "D")$alpha_after, 3) == ".500",
          paper_number(row_of("B", "AB+", 20, "B")$V_after, 3) == ".681",
          paper_number(row_of("D", "CD+", 20, "D")$V_after, 3) == ".803",
          paper_number(row_of("B", "pear", 9, "B")$gain, 3) == ".051",
          paper_number(row_of("D", "pear", 9, "D")$gain, 3) == ".048")

write.csv(trials, "results/supplementS4_adaptive_trials.csv", row.names = FALSE)

# Table S4 as a LaTeX longtable, with the values rounded to four decimals.
four <- function(x) paper_number(x, 4)
phase_names <- c("A+" = "A+", "AB+" = "AB+", "X+" = "X+", "CD+" = "CD+", pear = "Pear")
group_rows <- function(light, heading) {
  d <- trials[trials$light == light, ]
  c(sprintf("\\multicolumn{10}{@{}l}{\\textit{%s}} \\\\*", heading),
    sprintf("%s & %d & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            phase_names[d$phase], d$trial, d$cue, four(d$V_before), four(d$alpha_before),
            four(d$e_own), four(d$e_other), four(d$gain), four(d$V_after), four(d$alpha_after)))
}
header <- paste("\\textbf{Phase} & \\textbf{Trial} & \\textbf{Cue} & $\\boldsymbol{V}$ \\textbf{before} &",
                "$\\boldsymbol{\\alpha}$ \\textbf{before} & \\textbf{Own error} & \\textbf{Other error} &",
                "\\textbf{Gain} & $\\boldsymbol{V}$ \\textbf{after} & $\\boldsymbol{\\alpha}$ \\textbf{after} \\\\")
table_s4 <- c(
  "\\begin{longtable}{@{}lrlrrrrrrr@{}}",
  "\\caption{The adaptive model trial by trial, for the two training histories and the pear trials of main-text Sections~3.1 to~3.4. Each row is one cue on one trial: its learned value and associability before the trial, the size of its own error and of the error made by the other cues' summed prediction (by predicting zero, when the cue appears alone), its gain in learned value, and its learned value and associability after the trial. Trials 1 to 10 are the trials of the first phase and trials 11 to 20 the compound trials. Pear trials are numbered separately for each light, and on these rows $V$ is the learned value for the pear. All cues have salience .3 and start with associability .5, $\\eta=.05$, learned values for the pear start at zero, and the associability acquired during the compound trials carries over in full. Values are rounded to four decimal places.}\\label{tab:s4_trials}\\\\",
  "\\toprule", header, "\\midrule", "\\endfirsthead",
  "\\toprule", header, "\\midrule", "\\endhead",
  "\\bottomrule", "\\endfoot",
  group_rows("B", "B's history: tone A, then tone A with light B, then B with the pear"),
  "\\addlinespace",
  group_rows("D", "D's history: buzzer X, then tone C with light D, then D with the pear"),
  "\\end{longtable}")
writeLines(table_s4, "results/supplement_table_s4.tex")
cat("Saved results/supplementS4_adaptive_trials.csv and results/supplement_table_s4.tex\n")
