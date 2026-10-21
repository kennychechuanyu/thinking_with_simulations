# Section 6 and Supplement S1: two settings of the drift diffusion model that
# predict the same accuracy but different decision times. Simulates both
# settings for Table S1 (written to results/section6_ddm_checks.csv) and for the
# check of the time limit, compares them with Equation 22 and the mean decision
# time, and draws Figure 12. A larger simulation also checks two time steps.
#   Rscript analysis/section6_decision_model.R
# Only the time-step check, leaving Figure 12 and the upper block of Table S1
# unchanged:
#   Rscript analysis/section6_decision_model.R --timestep-check

if (!file.exists("R/figure_style.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
library(patchwork)
# R's default random number generators, set here so that a profile that changes
# them does not change the simulated trials.
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# The two settings have the same product va = 0.60. Evidence starts halfway
# between the boundaries, the noise scale is 1, and there is no deadline.
settings <- data.frame(id = c("s1", "s2"), v = c(.30, .60), a = c(2.0, 1.0))
t0 <- .20           # nondecision time
time_step <- .001   # Euler step of the simulation
time_limit <- 6.0   # a numerical safeguard on the decision time, not a deadline


# Calculated results
ddm_accuracy <- function(v, a) 1 / (1 + exp(-v * a))             # Equation 22
ddm_mean_time <- function(v, a) a / (2 * v) * tanh(v * a / 2)    # mean decision time

# Density of the decision time at the lower boundary: the large-time series of
# Navarro and Fuss (2009), with K terms and the start at w * a. The upper
# boundary uses -v and 1 - w, which is also .5 here.
ddm_density_lower <- function(t, v, a, w = .5, K = 300) {
  k <- seq_len(K)
  sapply(t, function(tt) {
    if (tt <= 0) return(0)
    (pi / a^2) * exp(-v * a * w - v^2 * tt / 2) *
      sum(k * exp(-k^2 * pi^2 * tt / (2 * a^2)) * sin(k * pi * w))
  })
}
# Correct and incorrect responses together, as in Figure 12c.
ddm_density <- function(t, v, a) ddm_density_lower(t, v, a) + ddm_density_lower(t, -v, a)

settings$accuracy <- ddm_accuracy(settings$v, settings$a)
settings$mean_time <- ddm_mean_time(settings$v, settings$a)

# The series is checked against three results known in closed form: summed over
# a grid of .001 up to 12, the density integrates to 1, its upper-boundary part
# to Equation 22, and its mean to the mean decision time.
grid <- seq(0, 12, by = .001)
for (i in 1:2) {
  density_both <- ddm_density(grid, settings$v[i], settings$a[i])
  density_upper <- ddm_density_lower(grid, -settings$v[i], settings$a[i])
  stopifnot(abs(sum(density_both) * .001 - 1) < 1e-3,
            abs(sum(density_upper) * .001 - settings$accuracy[i]) < 1e-3,
            abs(sum(grid * density_both) * .001 - settings$mean_time[i]) < 2e-3)
}


# Simulation (Supplement S1). Evidence starts at a/2 and moves in Euler steps;
# a trial ends at a (correct, response 1) or at 0 (error, response 0), and the
# response time is the decision time plus t0. A trial that reaches the time
# limit first has no response.
simulate_ddm <- function(v, a, t0, n_trials, dt, max_t) {
  max_steps <- floor(max_t / dt)
  rt <- numeric(n_trials)
  response <- integer(n_trials)
  for (i in seq_len(n_trials)) {
    x <- a / 2
    step <- 0L
    while (step < max_steps && x > 0 && x < a) {
      step <- step + 1L
      x <- x + v * dt + sqrt(dt) * rnorm(1)
    }
    rt[i] <- step * dt + t0
    response[i] <- if (x >= a) 1L else if (x <= 0) 0L else NA_integer_
  }
  data.frame(rt = rt, response = response)
}

# Supplement S1: both settings with time steps of .001 and .0001, 20,000 trials
# and a fixed seed per run. The time limit is raised to 20; the number of trials
# that reach it is saved.
check_ddm_timestep <- function() {
  jobs <- data.frame(setting = rep(seq_len(nrow(settings)), each = 2),
                     dt = rep(c(.001, .0001), times = nrow(settings)),
                     seed = 602601:602604)
  n_trials <- 20000L
  max_t <- 20
  checks <- do.call(rbind, lapply(seq_len(nrow(jobs)), function(j) {
    i <- jobs$setting[j]
    set.seed(jobs$seed[j])
    trials <- simulate_ddm(settings$v[i], settings$a[i], t0, n_trials,
                           dt = jobs$dt[j], max_t = max_t)
    responded <- !is.na(trials$response)
    responses <- trials$response[responded]
    correct_times <- trials$rt[responded & trials$response == 1L] - t0
    accuracy <- mean(responses)
    mean_time <- mean(correct_times)
    result <- data.frame(v = settings$v[i], a = settings$a[i], dt = jobs$dt[j],
                          seed = jobs$seed[j], n = n_trials, max_t = max_t,
                          n_response = sum(responded), n_correct = length(correct_times),
                          n_limit = sum(!responded),
                          acc_sim = accuracy, acc_calc = settings$accuracy[i],
                          acc_error = accuracy - settings$accuracy[i],
                          acc_mcse = sqrt(accuracy * (1 - accuracy) / length(responses)),
                          time_sim = mean_time, time_calc = settings$mean_time[i],
                          time_error = mean_time - settings$mean_time[i],
                          time_relative_error = mean_time / settings$mean_time[i] - 1,
                          time_mcse = sd(correct_times) / sqrt(length(correct_times)))
    cat(sprintf("Time-step check: v = %.2f, a = %.1f, dt = %g, seed %d; ",
                result$v, result$a, result$dt, result$seed))
    cat(sprintf("accuracy %.4f (MCSE %.4f), mean time %.4f (MCSE %.4f), %d at limit\n",
                result$acc_sim, result$acc_mcse, result$time_sim,
                result$time_mcse, result$n_limit))
    result
  }))
  write.csv(checks, "results/section6_ddm_timestep_checks.csv", row.names = FALSE)
  cat("Saved results/section6_ddm_timestep_checks.csv\n")
  # Values reported in Supplement S1 and the lower block of Table S1
  stopifnot(
    checks$n_limit == 0,
    sprintf("%.4f", checks$acc_sim) == c("0.6523", "0.6442", "0.6481", "0.6501"),
    sprintf("%.4f", checks$time_sim) == c("1.0164", "0.9876", "0.2619", "0.2481"),
    sprintf("%.4f", checks$acc_mcse) == "0.0034",
    sprintf("%.4f", checks$time_mcse) == c("0.0073", "0.0070", "0.0019", "0.0018"),
    sprintf("%.4f", unique(checks$time_calc)) == c("0.9710", "0.2428"),
    sprintf("%.4f", unique(checks$acc_calc)) == "0.6457",
    round(100 * checks$time_relative_error[checks$dt == 1e-4], 1) == c(1.7, 2.2),
    # The finer step moves accuracy closer to the calculated value in the first
    # setting and farther from it in the second.
    abs(checks$acc_error[2]) < abs(checks$acc_error[1]),
    abs(checks$acc_error[4]) > abs(checks$acc_error[3])
  )
  invisible(checks)
}

if ("--timestep-check" %in% commandArgs(trailingOnly = TRUE)) {
  check_ddm_timestep()
  quit(save = "no")
}

# Table S1: 2,000 trials per setting, both settings from one stream with seed
# 2026. Trials without a response are left out of both summaries; acc_se is the
# binomial standard error of the calculated accuracy over the trials with a
# response.
set.seed(2026)
simulated <- lapply(1:2, function(i) {
  simulate_ddm(settings$v[i], settings$a[i], t0, n_trials = 2000, dt = time_step,
               max_t = time_limit)
})
ddm_checks <- do.call(rbind, lapply(1:2, function(i) {
  trials <- simulated[[i]][!is.na(simulated[[i]]$response), ]
  data.frame(v = settings$v[i], a = settings$a[i], n = nrow(simulated[[i]]),
             n_response = nrow(trials),
             acc_sim = mean(trials$response), acc_calc = settings$accuracy[i],
             time_sim = mean(trials$rt[trials$response == 1]) - t0,
             time_calc = settings$mean_time[i])
}))
ddm_checks$acc_se <- sqrt(ddm_checks$acc_calc * (1 - ddm_checks$acc_calc) / ddm_checks$n_response)

# How often the time limit is reached: both settings again with 20,000 trials
# from the same seed.
set.seed(2026)
limit_reached <- sapply(1:2, function(i) {
  trials <- simulate_ddm(settings$v[i], settings$a[i], t0, n_trials = 20000, dt = time_step,
                         max_t = time_limit)
  sum(is.na(trials$response))
})


# Figure 12
ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
setting_colours <- c(s1 = "#5B4B8A", s2 = "#3A8F6A")
label_size <- 8 / .pt
theme_figure <- theme_paper() +
  theme(axis.title.x = element_text(margin = margin(t = 6)), plot.margin = margin(6, 12, 6, 6))
setting_labels <- c(s1 = "paste(italic(v), ' = 0.30, ', italic(a), ' = 2.0')",
                    s2 = "paste(italic(v), ' = 0.60, ', italic(a), ' = 1.0')")

# (a) Evidence is measured from the starting point, so the boundaries are at
# +a/2 (correct) and -a/2 (error) and both settings share one scale. The first
# twenty paths per setting (steps of .001, seed 2026). A path stops at a boundary
# or at the end of the panel; all forty reach a boundary (checked at the end).
evidence_path <- function(v, a, dt, max_t) {
  x <- 0
  t <- 0
  xs <- 0
  ts <- 0
  while (abs(x) < a / 2 && t < max_t) {
    x <- x + v * dt + sqrt(dt) * rnorm(1)
    t <- t + dt
    xs <- c(xs, x)
    ts <- c(ts, t)
  }
  data.frame(t = ts, x = pmin(pmax(xs, -a / 2), a / 2))
}
set.seed(2026)
paths <- do.call(rbind, lapply(1:2, function(i) {
  do.call(rbind, lapply(1:20, function(j) {
    transform(evidence_path(settings$v[i], settings$a[i], dt = time_step, max_t = 3.5),
              id = settings$id[i], path = j)
  }))
}))
path_ends <- do.call(rbind, lapply(split(paths, list(paths$id, paths$path)), function(d) d[nrow(d), ]))
boundaries <- data.frame(id = rep(settings$id, each = 2), y = c(rbind(settings$a, -settings$a)) / 2)
# The arrow shows the average drift from 0 to .6 in both settings.
drift <- data.frame(id = settings$id, t = 0, x = 0, t_end = .6, x_end = .6 * settings$v)
panel_titles <- data.frame(id = settings$id, label = unname(setting_labels),
                           description = c("slower drift, wider boundaries",
                                           "faster drift, narrower boundaries"))
panel_a <- ggplot() +
  geom_hline(data = boundaries, aes(yintercept = y), colour = ink, linewidth = .6) +
  geom_hline(yintercept = 0, colour = "#D0D0D0", linewidth = .3, linetype = "22") +
  geom_path(data = paths, aes(t, x, group = path, colour = id), linewidth = .22,
            alpha = .35) +
  geom_segment(data = drift, aes(t, x, xend = t_end, yend = x_end), colour = ink,
               linewidth = .8, arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  geom_point(data = drift, aes(t, x), colour = ink, size = 1.6) +
  geom_text(data = panel_titles, aes(0, 1.55, label = label, colour = id), parse = TRUE,
            hjust = 0, size = text_size, fontface = "bold") +
  geom_text(data = panel_titles, aes(0, 1.33, label = description), hjust = 0,
            size = label_size, colour = grey) +
  geom_text(data = data.frame(id = settings$id, y = settings$a / 2),
            aes(3.55, y, label = "correct"), hjust = 0, vjust = .5, size = label_size,
            colour = grey) +
  geom_text(data = data.frame(id = settings$id, y = -settings$a / 2),
            aes(3.55, y, label = "error"), hjust = 0, vjust = .5, size = label_size,
            colour = grey) +
  facet_wrap(~ id, nrow = 1) +
  scale_colour_manual(values = setting_colours) +
  scale_x_continuous(breaks = 0:3, expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1),
                     labels = c("−1", "−0.5", "0", "0.5", "1")) +
  coord_cartesian(xlim = c(0, 3.5), ylim = c(-1.05, 1.05), clip = "off") +
  labs(title = "(a)  How evidence accumulates",
       subtitle = paste("Twenty simulated paths per setting, each stopping at the boundary",
                        "it reaches; arrow: average drift"),
       x = "Time (model time units)", y = "Evidence from start") +
  theme_figure +
  theme(strip.text = element_blank(), strip.background = element_blank(),
        panel.spacing.x = unit(42, "pt"), plot.margin = margin(6, 40, 10, 6),
        plot.subtitle = element_text(size = 9, colour = grey, margin = margin(b = 30)))

# (b) Accuracy against drift rate for the two boundary separations; each
# setting lies on the curve for its own separation, at the same height.
drift_rates <- seq(0, 1.2, by = .005)
accuracy_curves <- rbind(data.frame(a = "2", v = drift_rates, p = ddm_accuracy(drift_rates, 2.0)),
                         data.frame(a = "1", v = drift_rates, p = ddm_accuracy(drift_rates, 1.0)))
panel_b <- ggplot() +
  geom_hline(yintercept = settings$accuracy[1], colour = "#BDBDBD", linewidth = .3,
             linetype = "22") +
  geom_line(data = accuracy_curves, aes(v, p, group = a), colour = grey, linewidth = .6) +
  annotate("text", x = 1.23, y = ddm_accuracy(1.2, 2.0), label = "paste(italic(a), ' = 2.0')",
           parse = TRUE, hjust = 0, size = text_size, colour = grey) +
  annotate("text", x = 1.23, y = ddm_accuracy(1.2, 1.0), label = "paste(italic(a), ' = 1.0')",
           parse = TRUE, hjust = 0, size = text_size, colour = grey) +
  annotate("segment", x = c(.08, .74), y = c(.79, .565), xend = c(.29, .61),
           yend = c(.655, .637), colour = setting_colours, linewidth = .3) +
  annotate("text", x = .03, y = .81, label = setting_labels[["s1"]], parse = TRUE, hjust = 0,
           vjust = 0, size = text_size, colour = setting_colours[["s1"]]) +
  annotate("text", x = .66, y = .545, label = setting_labels[["s2"]], parse = TRUE, hjust = 0,
           vjust = 1, size = text_size, colour = setting_colours[["s2"]]) +
  geom_point(data = settings, aes(v, accuracy, colour = id), size = 2.4) +
  annotate("text", x = 1.2, y = settings$accuracy[1] + .008, label = "both .646", hjust = 1,
           vjust = 0, size = text_size, colour = grey) +
  scale_colour_manual(values = setting_colours) +
  scale_x_continuous(breaks = c(0, .3, .6, .9, 1.2), labels = c("0", "0.3", "0.6", "0.9", "1.2"),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(.5, .92), breaks = c(.5, .6, .7, .8, .9),
                     labels = c(".5", ".6", ".7", ".8", ".9"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, 1.2), clip = "off") +
  labs(title = "(b)  The same accuracy",
       subtitle = expression("Equation 22: both settings have " * italic(va) * " = 0.60"),
       x = expression("Drift rate, " * italic(v)),
       y = "Probability correct") +
  theme_figure +
  theme(plot.margin = margin(6, 48, 6, 6))

# (c) Densities of the decision time, both boundaries together, with the means
# marked by dashed lines.
time_grid <- seq(0, 3.5, by = .005)
density_curves <- do.call(rbind, lapply(1:2, function(i) {
  data.frame(id = settings$id[i], t = time_grid,
             f = ddm_density(time_grid, settings$v[i], settings$a[i]))
}))
mean_lines <- data.frame(id = settings$id, time = settings$mean_time,
                         density = mapply(ddm_density, settings$mean_time, settings$v,
                                          settings$a))
mean_labels <- paste("mean", sprintf("%.2f", settings$mean_time))
panel_c <- ggplot(density_curves, aes(t, f, colour = id)) +
  geom_line(linewidth = .8) +
  geom_segment(data = mean_lines, aes(x = time, xend = time, y = 0, yend = density,
                                      colour = id),
               linewidth = .4, linetype = "22") +
  annotate("text", x = 1.3, y = 1.05, label = setting_labels[["s1"]], parse = TRUE, hjust = 0,
           size = text_size, colour = setting_colours[["s1"]]) +
  annotate("text", x = 1.3, y = .8, label = mean_labels[1], hjust = 0, size = text_size,
           colour = setting_colours[["s1"]]) +
  annotate("text", x = .55, y = 2.3, label = setting_labels[["s2"]], parse = TRUE, hjust = 0,
           size = text_size, colour = setting_colours[["s2"]]) +
  annotate("text", x = .55, y = 2.0, label = mean_labels[2], hjust = 0, size = text_size,
           colour = setting_colours[["s2"]]) +
  scale_colour_manual(values = setting_colours) +
  scale_x_continuous(breaks = 0:3, expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, .03))) +
  coord_cartesian(xlim = c(0, 3.5), clip = "off") +
  labs(title = "(c)  Different decision times",
       subtitle = "Density of the decision time, before nondecision time",
       x = "Decision time (model time units)", y = "Density") +
  theme_figure

figure <- panel_a / (panel_b + panel_c + plot_layout(widths = c(1, 1.15))) +
  plot_layout(heights = c(.8, 1))


cat(sprintf("Calculated accuracy (Equation 22): %s in both settings\n",
            paper_number(settings$accuracy[1], 3)))
cat("Table S1, 2,000 trials per setting (simulated, calculated):\n")
cat(sprintf("  v = %.2f, a = %.1f: accuracy %s, %s; mean decision time %.2f, %.2f\n",
            settings$v, settings$a,
            paper_number(ddm_checks$acc_sim, 3),
            paper_number(ddm_checks$acc_calc, 3),
            ddm_checks$time_sim, ddm_checks$time_calc), sep = "")
cat(sprintf("Trials reaching the time limit of %.1f, out of 20,000 per setting: %d and %d\n",
            time_limit, limit_reached[1], limit_reached[2]))
write.csv(ddm_checks, "results/section6_ddm_checks.csv", row.names = FALSE)
cat("Saved results/section6_ddm_checks.csv\n")
save_figure(figure, "fig12_decision_times", width = 7, height = 5.4)

# Values reported in the paper
stopifnot(
  abs(settings$accuracy - .646) < 5e-4,                                  # Section 6
  abs(settings$mean_time - c(.97, .24)) < 5e-3,                          # Figure 12c, Table S1
  abs(ddm_checks$acc_sim - c(.652, .658)) < 5e-4,                        # Table S1
  abs(ddm_checks$time_sim - c(1.00, .25)) < 5e-3,                        # Table S1
  abs(ddm_checks$acc_sim - ddm_checks$acc_calc - c(.006, .012)) < 5e-4,  # Supplement S1
  abs(ddm_checks$acc_se - .011) < 5e-4,                                  # Supplement S1
  abs(ddm_checks$acc_sim - ddm_checks$acc_calc) < 2 * ddm_checks$acc_se, # Supplement S1
  limit_reached == c(17, 0),                                             # Supplement S1
  # Figure 12a: each path ends where it reaches a boundary
  abs(path_ends$x) == settings$a[match(path_ends$id, settings$id)] / 2
)

check_ddm_timestep()
