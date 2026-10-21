# Section 4.2: how much training, and when should we measure?
# Computes the adaptive model's history effect H(k) = V_D(k) - V_B(k)
# (Equation 11) for 1-20 compound trials and eta = .01-.05, checks Equations 12
# and 13 and that B's associability stays above the lower bound of Equation 8,
# and draws Figure 6.
#   Rscript analysis/section4_2_training_and_rate.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

# Settings of Section 3. The amount of compound training and eta vary, and the
# new task has fifteen trials instead of ten.
n_pretraining <- 10   # A+ and C+ trials before compound training
n_updates <- 15       # trials with the new outcome
salience <- .3        # fixed salience S_i
alpha_init <- .5      # initial associability
lambda <- 1

# The history effect H(k), k = 0, ..., 15, for one setting, with the
# associabilities that B and D bring into the new task.
history_effect <- function(n_compound, eta) {
  blocked <- adaptive_simulate(
    c(make_trials("A", lambda, n_pretraining, "Phase 1"),
      make_trials(c("A", "B"), lambda, n_compound, "Phase 2")),
    S = c(A = salience, B = salience), alpha_init = c(A = alpha_init, B = alpha_init),
    eta = eta
  )
  control <- adaptive_simulate(
    c(make_trials("C", lambda, n_pretraining, "Phase 1"),
      make_trials(c("D", "E"), lambda, n_compound, "Phase 2")),
    S = c(C = salience, D = salience, E = salience),
    alpha_init = c(C = alpha_init, D = alpha_init, E = alpha_init),
    eta = eta
  )
  alpha_B <- extract_final_alpha(blocked)[["B"]]
  alpha_D <- extract_final_alpha(control)[["D"]]
  values <- function(cue, alpha) {
    adaptive_simulate(make_trials(cue, lambda, n_updates, "Phase 3"),
                      S = setNames(salience, cue), alpha_init = setNames(alpha, cue),
                      eta = eta, V_init = setNames(0, cue), cue_names = cue)$V
  }
  data.frame(n_compound = n_compound, eta = eta, k = 0:n_updates,
             alpha_B = alpha_B, alpha_D = alpha_D,
             lowest_alpha_B = min(blocked$alpha[blocked$cue == "B"]),
             H = values("D", alpha_D) - values("B", alpha_B))
}

# 20 amounts of compound training by 5 rates: 100 settings.
grid <- expand.grid(n_compound = 1:20, eta = seq(.01, .05, by = .01))
gaps <- do.call(rbind, Map(history_effect, grid$n_compound, grid$eta))

# One setting's rows, which give its curve H(k). The rates made by seq() carry
# rounding error, so they are compared after rounding to two decimals.
setting <- function(n, rate) gaps[gaps$n_compound == n & round(gaps$eta, 2) == rate, ]
peak_gap <- function(curve) max(curve$H)
peak_update <- function(curve) curve$k[which.max(curve$H)]

original <- setting(10, .05)   # the setting of Section 3
slow <- setting(10, .01)       # ten compound trials at slower rates
middle <- setting(10, .03)

# Results that depend on every setting: the smallest gap after an update, the
# earliest and latest peak, and one row per setting at k = 0.
peaks <- sapply(split(gaps, gaps[c("n_compound", "eta")]), peak_update)
smallest_gap <- min(gaps$H[gaps$k >= 1])
start <- gaps[gaps$k == 0, ]


# Figure 6
ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
mid <- "#8A8A8A"   # curves other than the setting of Section 3

# The panels share the Rescorla-Wagner zero line, the scales and the theme, so
# the setting of Section 3 is drawn identically in both.
rw_zero <- list(
  annotate("segment", x = 0, xend = 15, y = 0, yend = 0, colour = grey,
           linewidth = .6, linetype = "22"),
  annotate("text", x = 15, y = -.035, label = "Rescorla–Wagner",
           size = text_size, colour = grey, hjust = 1)
)
axes <- list(
  scale_x_continuous(breaks = c(0, 5, 10, 15), expand = expansion(add = c(.3, 0))),
  scale_y_continuous(breaks = c(0, .1, .2, .3, .4, .5),
                     labels = c("0", ".1", ".2", ".3", ".4", ".5"),
                     expand = expansion(mult = 0)),
  coord_cartesian(xlim = c(0, 15), ylim = c(-.06, .57), clip = "off"),
  theme_paper()
)
gap_line <- function(curve, colour) {
  geom_line(data = curve, aes(k, H), colour = colour, linewidth = .7)
}
original_peak <- geom_point(data = original[which.max(original$H), ], aes(k, H), shape = 21,
                            size = 2.4, fill = "white", colour = ink, stroke = .6)
# Each curve is labeled above its peak.
peak_label <- function(curve, label, colour, above = .028) {
  annotate("text", x = peak_update(curve), y = peak_gap(curve) + above, label = label,
           size = text_size, colour = colour)
}

panel_a <- ggplot() + rw_zero +
  gap_line(setting(1, .05), mid) + gap_line(setting(5, .05), mid) +
  gap_line(setting(20, .05), mid) + gap_line(original, ink) + original_peak +
  peak_label(setting(20, .05), "20 trials", grey) +
  peak_label(original, "10 trials", ink, above = .034) +
  peak_label(setting(5, .05), "5 trials", grey) +
  peak_label(setting(1, .05), "1 trial", grey) +
  axes +
  labs(title = "(a)  Amount of compound training",
       subtitle = "Associability-change rate held at η = .05",
       x = expression("Completed updates with the new outcome, " * italic(k)),
       y = expression(italic(H) * " = " * italic(V)[D] * " − " * italic(V)[B]))

panel_b <- ggplot() + rw_zero +
  gap_line(slow, mid) + gap_line(middle, mid) + gap_line(original, ink) + original_peak +
  peak_label(original, "η = .05", ink, above = .034) +
  peak_label(middle, "η = .03", grey) +
  peak_label(slow, "η = .01", grey) +
  axes +
  labs(title = "(b)  Rate of associability change",
       subtitle = "Compound training held at 10 trials",
       x = expression("Completed updates with the new outcome, " * italic(k)), y = NULL)

figure <- panel_a + plot_spacer() + panel_b + plot_layout(widths = c(1, .06, 1))

cat("Section 4.2, the B-D gap by amount of compound training and by eta\n")
cat(sprintf("Setting of Section 3: alpha_B = %s, alpha_D = %s, H(1) = %s\n",
            paper_number(original$alpha_B[1], 3), paper_number(original$alpha_D[1], 3),
            paper_number(original$H[original$k == 1], 3)))
for (n in c(1, 5, 10, 20)) {
  cat(sprintf("eta = .05, %d compound %s: peak %s after %d updates\n", n,
              if (n == 1) "trial" else "trials",
              paper_number(peak_gap(setting(n, .05)), 3), peak_update(setting(n, .05))))
}
for (rate in c(.01, .02, .03, .04)) {
  cat(sprintf("Ten compound trials, eta = %s: peak %s after %d updates\n",
              paper_number(rate, 2), paper_number(peak_gap(setting(10, rate)), 3),
              peak_update(setting(10, rate))))
}
cat(sprintf("All %d settings: smallest gap after an update %s, peaks after updates %d-%d\n",
            nrow(start), paper_number(smallest_gap, 4), min(peaks), max(peaks)))
cat(sprintf("B's lowest associability in any setting: %s (lower bound of Equation 8: .01)\n",
            paper_number(min(gaps$lowest_alpha_B), 3)))
save_figure(figure, "fig06_training_and_rate", width = 7, height = 3.35)

# Values reported in the paper
first_update <- gaps[gaps$k == 1, ]
stopifnot(
  max(abs(first_update$H - salience * lambda *                          # Equation 12
            (first_update$alpha_D - first_update$alpha_B))) < 1e-12,
  max(abs(start$alpha_D - alpha_init)) < 1e-12,                         # Section 4.2
  abs(original$alpha_B[1] - .235) < 5e-4,                               # Section 4.2
  abs(original$H[original$k == 1] - .079) < 5e-4,                       # Section 4.2
  abs(peak_gap(setting(5, .05)) - .156) < 5e-4,                         # Section 4.2, Figure 6a
  peak_update(setting(5, .05)) == 7,                                    # Section 4.2
  abs(peak_gap(original) - .284) < 5e-4, peak_update(original) == 8,    # Section 4.2, Figure 6a
  abs(peak_gap(setting(20, .05)) - .512) < 5e-4,                        # Section 4.2, Figure 6a
  peak_update(setting(20, .05)) == 10,                                  # Section 4.2
  abs(peak_gap(slow) - .037) < 5e-4, peak_update(slow) == 6,            # Section 4.2, Figure 6b
  abs(peak_gap(middle) - .136) < 5e-4, peak_update(middle) == 7,        # Section 4.2, Figure 6b
  smallest_gap > 0,                                                     # Equation 13
  min(peaks) == 6, max(peaks) == 10,                                    # Section 4.2
  min(gaps$lowest_alpha_B) > .11                                        # Section 4.2
)
