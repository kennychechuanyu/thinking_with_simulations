# Section 2: what the Rescorla-Wagner model (Equation 1) implies for a light
# trained together with an already trained tone. Computes the values reported
# in Sections 2.2-2.4, including the expected results of the compound check of
# Section 2.3, and draws Figure 2.
#   Rscript analysis/section2_blocking.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")
library(patchwork)

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
red <- paper_colours[["red"]]
blue <- paper_colours[["blue"]]

# Section 2.2: the tone A and the light B both have the learning rate .12.
beta <- c(A = .12, B = .12)


# Ten A+ trials (the tone), then ten AB+ trials (tone and light), each followed
# by the apple. rw_simulate() numbers trials from 1, and row t of each cue holds
# its value at the start of trial t, that is, after t - 1 completed trials.
blocking_design <- c(make_trials("A", 1, 10, "Phase 1"),
                     make_trials(c("A", "B"), 1, 10, "Phase 2"))
blocking <- rw_simulate(blocking_design, beta = beta)
blocking$completed <- blocking$trial - 1L
V_A <- blocking$V[blocking$cue == "A"]
V_B <- blocking$V[blocking$cue == "B"]

# The shared error on trials 1-20, from the values before the update.
shared_error <- data.frame(trial = seq_along(blocking_design))
shared_error$delta <- sapply(shared_error$trial, function(t) {
  start <- blocking[blocking$trial == t, ]
  1 - sum(start$V[start$cue %in% blocking_design[[t]]$cues])
})
first_error <- shared_error$delta[11]   # trial 11, the first AB+ trial
second_error <- shared_error$delta[12]
limit_B <- (1 - V_A[11]) / 2            # the light's limit under continued AB+ trials

# Figure 2a's reference: a new cue trained alone from zero, drawn over the
# AB+ trials.
single_cue <- data.frame(completed = 10:20, V = 1 - (1 - beta[["A"]])^(0:10))

# Section 2.3: the compound check from equal values of .30, and the gains that
# the two mistaken programs would give, calculated by hand: one program uses
# each cue's own error, and one updates the tone before recalculating the error
# for the light. tests/verify_models.R runs both programs.
check_from_30 <- rw_final(make_trials(c("A", "B"), 1, 1, "Check"), beta = beta,
                          V_init = c(A = .3, B = .3))
own_error_gain <- .12 * (1 - .3)
tone_updated_first <- .3 + .12 * (1 - (.3 + .3))
order_mistake_gain <- .12 * (1 - (tone_updated_first + .3))

# Sections 2.2-2.4: the tone's first two trials and its value after ten, the
# first and second compound trials, the values after the compound trials, the
# light's limit, and the expected results of the compound check.
stopifnot(abs(V_A[2] - .12) < 1e-12, abs(V_A[3] - .2256) < 1e-12,
          abs(V_A[11] - (1 - .88^10)) < 1e-12, paper_number(V_A[11], 4) == ".7215",
          paper_number(first_error, 4) == ".2785", paper_number(V_B[12], 4) == ".0334",
          abs(V_B[12] - V_A[12] + V_A[11]) < 1e-12,
          paper_number(V_A[12], 4) == ".7549", paper_number(V_A[12] + V_B[12], 4) == ".7883",
          paper_number(second_error, 4) == ".2117", paper_number(.12 * second_error, 4) == ".0254",
          paper_number(V_A[21], 4) == ".8518", paper_number(V_B[21], 4) == ".1303",
          # 2.4: equal gains on every compound trial, so the tone keeps its lead of .7215
          all(abs(diff(V_A[11:21]) - diff(V_B[11:21])) < 1e-12),
          all(abs((V_A[11:21] - V_B[11:21]) - V_A[11]) < 1e-12),
          paper_number(limit_B, 4) == ".1393", paper_number(limit_B - V_B[21], 3) == ".009",
          all(abs(check_from_30 - .348) < 1e-12), abs(own_error_gain - .084) < 1e-12,
          abs(order_mistake_gain - .04224) < 1e-12)
cat(sprintf("After ten A+ trials: V_A = %.4f. After ten AB+ trials: V_A = %.4f, V_B = %.4f\n",
            V_A[11], V_A[21], V_B[21]))


# Figure 2. (a) Learned values after each completed trial; (b) the shared error
# on each trial.
phase_marks <- function(boundary, centers) {
  list(
    geom_vline(xintercept = boundary, colour = "#C5C5C5",
               linewidth = .3, linetype = "longdash"),
    annotate("text", x = centers, y = 1.075,
             label = c("A+  (tone)", "AB+  (tone and light)"), size = text_size, colour = ink)
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
  annotate("text", x = 20.65, y = V_A[21], label = "A (tone)",
           colour = blue, size = text_size, hjust = 0) +
  annotate("text", x = 20.65, y = tail(single_cue$V, 1) - .055, label = "new cue\nalone, no\ncompanion",
           colour = grey, size = text_size, hjust = 0, lineheight = .9) +
  annotate("text", x = 20.65, y = V_B[21], label = "B (light)",
           colour = red, size = text_size, hjust = 0) +
  annotate("segment", x = 17, xend = 18, y = .285, yend = limit_B + .018,
           colour = red, linewidth = .3) +
  annotate("text", x = 14.6, y = .335, label = paste("B's limit", paper_number(limit_B, 4)),
           colour = red, size = text_size, hjust = 0) +
  scale_colour_manual(values = c(A = blue, B = red)) +
  scale_shape_manual(values = c(A = 16, B = 17)) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(-.3, 25.6), ylim = c(-.035, 1.12), clip = "off") +
  labs(title = "(a)  Learned values", x = expression("Completed trials, " * italic(t)),
       y = expression("Learned value, " * italic(V))) +
  theme_paper(title_gap = 10)

# Panel b numbers the trials from 1. The first AB+ trial is trial 11.
error_panel <- ggplot(shared_error, aes(trial, delta)) +
  phase_marks(10.5, c(5.5, 15.5)) +
  geom_line(colour = ink, linewidth = .6) +
  geom_point(colour = ink, size = 1.4) +
  geom_point(data = shared_error[11, ], shape = 21,
             size = 2.7, fill = "white", colour = ink, stroke = .7) +
  annotate("segment", x = 14.2, xend = 11.35, y = .49,
           yend = first_error + .025, colour = ink, linewidth = .3) +
  annotate("text", x = 12.1, y = .625, label = "First AB+ trial (11)",
           size = text_size, colour = ink, hjust = 0) +
  annotate("text", x = 12.1, y = .545,
           label = paste("error", paper_number(first_error, 4)),
           size = text_size, colour = ink, hjust = 0) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(.7, 20.3), ylim = c(-.035, 1.12), clip = "off") +
  labs(title = "(b)  Shared error", x = "Trial",
       y = "Shared error") +
  theme_paper(title_gap = 10)

save_figure(values_panel + error_panel + plot_layout(widths = c(1.22, 1)),
            "fig02_blocking", width = 7, height = 3.15)
