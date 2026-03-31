# ============================================================================
# Supplementary S1 — DDM Demonstration: The Simulation Thinking Cycle
# ============================================================================
# Demonstrates how the paper's Simulation Thinking Cycle applies to the
# Drift Diffusion Model (DDM). Key teaching point: two configurations with
# identical v*a products produce indistinguishable accuracy, yet very
# different RT distributions — invisible to accuracy-only analysis.
#
# Figures produced:
#   Figures/figS1_ddm_rt_distributions.pdf/.png
#   Figures/figS2_ddm_summary_stats.pdf/.png
# Runtime: ~2-4 minutes (Euler-Maruyama simulation, 2000 trials per config)
# ============================================================================

library(ggplot2)
library(viridis)

set.seed(2026)

dir.create("Figures", showWarnings = FALSE)


# === DDM Simulation Function =================================================
# Euler-Maruyama random walk with absorbing boundaries.
#
# Parameters:
#   v   : drift rate (evidence accumulation rate)
#   a   : boundary separation
#   t0  : non-decision time (seconds)
#   dt  : time step (seconds)
#   max_t : maximum decision time before timeout (seconds)
#   n_trials : number of simulated trials
#
# Returns a data.frame with columns:
#   rt       : response time in seconds (decision time + t0)
#   response : 1 = correct (upper boundary), 0 = error (lower boundary)
#              NA = timeout (excluded from response data but rt = max_t + t0)

simulate_ddm <- function(v, a, t0, dt = 0.001, max_t = 6.0, n_trials = 2000) {

  s         <- 1.0          # diffusion coefficient (fixed)
  max_steps <- floor(max_t / dt)
  x_start   <- a / 2        # symmetric starting point

  rt       <- numeric(n_trials)
  response <- integer(n_trials)

  for (i in seq_len(n_trials)) {
    x    <- x_start
    step <- 0L
    hit  <- NA_integer_

    while (step < max_steps) {
      step <- step + 1L
      x    <- x + v * dt + s * sqrt(dt) * rnorm(1)

      if (x >= a) {          # upper (correct) boundary
        hit <- 1L
        break
      }
      if (x <= 0) {          # lower (error) boundary
        hit <- 0L
        break
      }
    }

    rt[i]       <- step * dt + t0
    response[i] <- if (is.na(hit)) NA_integer_ else hit
  }

  data.frame(rt = rt, response = response)
}


# === Configurations ==========================================================
# Both have v * a = 0.60  →  theoretical accuracy = sigmoid(0.6) ≈ 64.6%.
# They are indistinguishable by accuracy alone; RT distributions differ.

configs <- list(
  A = list(v = 0.30, a = 2.0, t0 = 0.20,
           label = "Config A: v=0.30, a=2.0"),
  B = list(v = 0.60, a = 1.0, t0 = 0.20,
           label = "Config B: v=0.60, a=1.0")
)

theor_acc <- 1 / (1 + exp(-0.6))   # sigmoid(v*a) = sigmoid(0.6)


# === Run Simulations =========================================================

cat("=== DDM Demonstration: Simulation Thinking Cycle ===\n\n")
cat("Both configurations share v*a = 0.60.\n")
cat(sprintf("Theoretical accuracy (sigmoid(0.6)): %.4f\n\n", theor_acc))

cat("Simulating Config A (v=0.30, a=2.0, t0=0.20) — takes ~1-2 minutes...\n")
sim_A <- simulate_ddm(v = configs$A$v, a = configs$A$a, t0 = configs$A$t0)

cat("Simulating Config B (v=0.60, a=1.0, t0=0.20) — takes ~1-2 minutes...\n")
sim_B <- simulate_ddm(v = configs$B$v, a = configs$B$a, t0 = configs$B$t0)


# === Console Summary =========================================================

summarise_config <- function(dat, label) {
  valid    <- dat[!is.na(dat$response), ]
  acc      <- mean(valid$response)
  mean_rt  <- mean(valid$rt)
  mean_rtc <- mean(valid$rt[valid$response == 1])
  cat(sprintf("%s\n", label))
  cat(sprintf("  Accuracy          : %.4f\n", acc))
  cat(sprintf("  Mean RT (all)     : %.4f s\n", mean_rt))
  cat(sprintf("  Mean RT (correct) : %.4f s\n\n", mean_rtc))
}

cat("\n=== Results ===\n\n")
summarise_config(sim_A, configs$A$label)
summarise_config(sim_B, configs$B$label)
cat(sprintf("Theoretical accuracy for both: %.4f\n\n", theor_acc))


# === Combine for Plotting ====================================================

sim_A$config <- "Config A: v=0.30, a=2.0"
sim_B$config <- "Config B: v=0.60, a=1.0"

dat_all <- rbind(sim_A, sim_B)

# Enforce factor ordering: Config A always first
dat_all$config <- factor(dat_all$config,
                         levels = c("Config A: v=0.30, a=2.0",
                                    "Config B: v=0.60, a=1.0"))

# Recode response as labelled factor for faceting
dat_all$resp_label <- factor(
  ifelse(dat_all$response == 1, "Correct", "Error"),
  levels = c("Correct", "Error")
)

# Filter: valid trials only, RT cap for display
dat_plot <- dat_all[!is.na(dat_all$response) & dat_all$rt < 4.0, ]


# === Figure S1: RT Distributions =============================================
# Two-panel density plot faceted by response type.
# Teaching point annotation: both configurations have the same accuracy.

acc_label <- sprintf("Both configurations: accuracy = %.1f%% (theoretical)", theor_acc * 100)

# Check that each config x response combination has enough data for density
cell_counts <- table(dat_plot$config, dat_plot$resp_label)

# Only keep cells with >= 2 observations (geom_density silently drops <2)
keep_cells <- which(cell_counts >= 2, arr.ind = TRUE)
dat_plot_s1 <- dat_plot[
  paste(dat_plot$config, dat_plot$resp_label) %in%
    paste(rownames(cell_counts)[keep_cells[, 1]],
          colnames(cell_counts)[keep_cells[, 2]]),
]

figS1 <- ggplot(dat_plot_s1,
                aes(x = rt, fill = config, colour = config)) +
  geom_density(alpha = 0.35, linewidth = 0.6, bw = "SJ") +
  facet_wrap(~resp_label, scales = "free_y") +
  scale_fill_viridis_d(option = "D", end = 0.75) +
  scale_colour_viridis_d(option = "D", end = 0.75) +
  labs(
    x        = "Response time (s)",
    y        = "Density",
    fill     = NULL,
    colour   = NULL,
    subtitle = acc_label
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 10),
    plot.subtitle    = element_text(size = 9, colour = "#555555",
                                    margin = margin(b = 4))
  )


# === Figure S2: Summary Statistics Bar Chart =================================
# Four-panel bar chart: Accuracy, Mean RT correct, RT SD correct, Mean RT error.

compute_stats <- function(dat, cfg_label) {
  valid    <- dat[!is.na(dat$response), ]
  correct  <- valid[valid$response == 1, ]
  errors   <- valid[valid$response == 0, ]

  mean_rt_err <- if (nrow(errors) >= 5) mean(errors$rt) else NA_real_

  data.frame(
    config = cfg_label,
    metric = c("Accuracy",
               "Mean RT, correct (s)",
               "RT SD, correct (s)",
               "Mean RT, error (s)"),
    value  = c(mean(valid$response),
               mean(correct$rt),
               sd(correct$rt),
               mean_rt_err),
    stringsAsFactors = FALSE
  )
}

stats_A <- compute_stats(sim_A, "Config A: v=0.30, a=2.0")
stats_B <- compute_stats(sim_B, "Config B: v=0.60, a=1.0")

dat_stats <- rbind(stats_A, stats_B)

# Enforce factor ordering
dat_stats$config <- factor(dat_stats$config,
                           levels = c("Config A: v=0.30, a=2.0",
                                      "Config B: v=0.60, a=1.0"))

# Enforce metric ordering for facets
metric_order <- c("Accuracy",
                  "Mean RT, correct (s)",
                  "RT SD, correct (s)",
                  "Mean RT, error (s)")
dat_stats$metric <- factor(dat_stats$metric, levels = metric_order)

figS2 <- ggplot(dat_stats[!is.na(dat_stats$value), ],
                aes(x = config, y = value, fill = config)) +
  geom_col(colour = "white", linewidth = 0.3) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_fill_viridis_d(option = "D", end = 0.75) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold", size = 9),
    axis.text.x      = element_text(angle = 25, hjust = 1, size = 8)
  )


# === Save Figures ============================================================

ggsave("Figures/figS1_ddm_rt_distributions.pdf", figS1,
       width = 7, height = 3.5, device = "pdf")
ggsave("Figures/figS1_ddm_rt_distributions.png", figS1,
       width = 7, height = 3.5, dpi = 300, device = "png")

ggsave("Figures/figS2_ddm_summary_stats.pdf", figS2,
       width = 8, height = 3.0, device = "pdf")
ggsave("Figures/figS2_ddm_summary_stats.png", figS2,
       width = 8, height = 3.0, dpi = 300, device = "png")

cat("Figure S1 saved: Figures/figS1_ddm_rt_distributions\n")
cat("Figure S2 saved: Figures/figS2_ddm_summary_stats\n")


# === Figure S3: Accuracy Surface (Structural Result Visualized) ==============
# Heatmap of P(correct) = sigmoid(v*a) over the (v, a) parameter space.
# The dashed hyperbolic curve marks v*a = 0.60; Config A and Config B both
# lie on this curve, making the accuracy-equivalence structural result visible.

v_grid <- seq(0.05, 1.20, by = 0.025)
a_grid <- seq(0.30, 3.50, by = 0.05)
surf   <- expand.grid(v = v_grid, a = a_grid)
surf$accuracy <- 1 / (1 + exp(-surf$v * surf$a))

va_line <- data.frame(v = seq(0.15, 1.20, by = 0.005))
va_line$a <- 0.60 / va_line$v
va_line   <- va_line[va_line$a >= 0.30 & va_line$a <= 3.50, ]

configs_pts <- data.frame(
  v = c(0.30, 0.60), a = c(2.0, 1.0)
)

figS3 <- ggplot(surf, aes(x = v, y = a)) +
  geom_raster(aes(fill = accuracy), interpolate = TRUE) +
  geom_contour(aes(z = accuracy),
               breaks = seq(0.55, 0.95, by = 0.05),
               colour = "white", alpha = 0.4, linewidth = 0.3) +
  geom_line(data = va_line, aes(x = v, y = a),
            colour = "white", linewidth = 1.0, linetype = "dashed",
            inherit.aes = FALSE) +
  geom_point(data = configs_pts, aes(x = v, y = a),
             colour = "white", size = 3.5, shape = 16, inherit.aes = FALSE) +
  annotate("text", x = 0.30, y = 2.20, label = "Config A",
           colour = "white", size = 3, fontface = "bold") +
  annotate("text", x = 0.60, y = 0.82, label = "Config B",
           colour = "white", size = 3, fontface = "bold") +
  scale_fill_viridis_c(option = "D", name = "P(correct)",
                       limits = c(0.5, 1.0),
                       breaks = seq(0.5, 1.0, by = 0.1)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0.2, 1.2, by = 0.2)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.5, 3.5, by = 0.5)) +
  labs(
    x = "Drift rate (v)",
    y = "Boundary separation (a)"
  ) +
  coord_cartesian(xlim = c(0.05, 1.20), ylim = c(0.30, 3.50)) +
  theme_classic(base_size = 11) +
  theme(legend.position = "right")

ggsave("Figures/figS3_ddm_accuracy_surface.pdf", figS3,
       width = 5.5, height = 4.0, device = "pdf")
ggsave("Figures/figS3_ddm_accuracy_surface.png", figS3,
       width = 5.5, height = 4.0, dpi = 300, device = "png")

cat("Figure S3 saved: Figures/figS3_ddm_accuracy_surface\n")
cat("DDM transfer demonstration complete.\n")
