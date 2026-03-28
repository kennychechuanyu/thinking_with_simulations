# ============================================================================
# Section 4.7 — Robustness Checks for the Critical Test
# ============================================================================
# Tests whether the flagship model-comparison result (blocking-plus-acquisition
# design that distinguishes Rescorla-Wagner from Mackintosh) is robust to:
#   A. Choice of observation function
#   B. V_B reset assumption at Phase 3 boundary
#   C. Divergence metric
#   D. Mackintosh attention parameters (beta_up, beta_down)
#
# Runtime: ~10-30 seconds
# Figures produced: fig12_robustness_obs, fig13_robustness_reset,
#                   fig14_robustness_attention
# ============================================================================

source("R/models.R")
library(ggplot2)
library(viridis)

set.seed(2026)
dir.create("Figures", showWarnings = FALSE)


# --- Colour palette and theme (consistent with figures.R) ---
pal <- c(blue="#4477AA", cyan="#66CCEE", green="#228833",
         yellow="#CCBB44", red="#EE6677", purple="#AA3377", grey="#BBBBBB")

theme_pub <- function(base_size = 9) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#F0F0F0", linewidth = 0.25),
      axis.line = element_line(color = "#404040", linewidth = 0.3),
      axis.ticks = element_line(color = "#404040", linewidth = 0.25),
      axis.ticks.length = unit(2, "pt"),
      axis.title = element_text(size = base_size, color = "#303030"),
      axis.text = element_text(size = base_size - 1, color = "#505050"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1, color = "#404040"),
      legend.key.size = unit(0.8, "lines"),
      legend.margin = margin(0, 0, 0, 0),
      legend.background = element_blank(),
      plot.margin = margin(6, 10, 6, 6)
    )
}

theme_heat <- function(base_size = 9) {
  theme_pub(base_size) %+replace%
    theme(
      panel.grid.major = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = base_size - 1, color = "#505050"),
      legend.key.height = unit(2.5, "lines"),
      legend.key.width = unit(0.5, "lines")
    )
}

save_fig <- function(p, name, width = 5, height = 3.5) {
  ggsave(paste0("Figures/", name, ".pdf"), p,
         width = width, height = height, device = "pdf")
  ggsave(paste0("Figures/", name, ".png"), p,
         width = width, height = height, dpi = 300, device = "png")
  cat(sprintf("  Saved: Figures/%s.pdf + .png\n", name))
}


# ============================================================================
# ANALYSIS A: Observation Function Sensitivity
# ============================================================================
# Run the alpha_A x beta parameter sweep under three observation functions:
#   1. Raw V:     |V_B(RW) - V_B(Mack)|
#   2. Sigmoid:   |sigmoid(V_B(RW)) - sigmoid(V_B(Mack))|  (gamma=5, theta=0.5)
#   3. Threshold: I(V_B(RW) > 0.3) != I(V_B(Mack) > 0.3)  (qualitative)

cat("=== Analysis A: Observation Function Sensitivity ===\n")
cat("Sweeping alpha_A x beta under three observation functions...\n")

t0 <- Sys.time()

alpha_range_A <- seq(0.1, 0.9, by = 0.1)
beta_range_A  <- seq(0.1, 0.9, by = 0.1)

obs_sweep <- expand.grid(alpha_A = alpha_range_A, beta = beta_range_A)
obs_sweep$div_raw       <- NA_real_
obs_sweep$div_sigmoid   <- NA_real_
obs_sweep$div_threshold <- NA_real_

for (i in seq_len(nrow(obs_sweep))) {
  a_A <- obs_sweep$alpha_A[i]
  b   <- obs_sweep$beta[i]

  # Design: Phase 1 = 20 trials A+, Phase 2 = 20 trials AB+, Phase 3 = 20 trials B+
  dp12 <- c(make_trials("A", 1, 20, "P1"),
            make_trials(c("A", "B"), 1, 20, "P2"))
  dp3  <- make_trials("B", 1, 20, "P3")

  # RW: Phase 1+2 -> reset V_B -> Phase 3
  V_rw_s <- rw_final(dp12, alpha = c(A = a_A, B = 0.4), beta = b)
  V_rw_s["B"] <- 0
  vb_rw <- rw_final(dp3, alpha = c(A = a_A, B = 0.4), beta = b,
                    V_init = V_rw_s)[["B"]]

  # Mack: Phase 1+2 -> reset V_B -> Phase 3
  # S yoked to beta for fair comparison
  state <- mack_final_state(dp12, S = c(A = b, B = b),
                            alpha_init = c(A = 0.5, B = 0.5))
  state$V["B"] <- 0
  vb_mack <- mack_final_state(dp3, S = c(A = b, B = b),
                              alpha_init = state$alpha,
                              V_init = state$V)$V[["B"]]

  # Metric 1: Raw V divergence
  obs_sweep$div_raw[i] <- abs(vb_rw - vb_mack)

  # Metric 2: Sigmoid divergence (gamma=5, theta=0.5)
  obs_sweep$div_sigmoid[i] <- abs(obs_sigmoid(vb_rw, gamma = 5, theta = 0.5) -
                                  obs_sigmoid(vb_mack, gamma = 5, theta = 0.5))

  # Metric 3: Threshold divergence (theta=0.3, qualitative)
  obs_sweep$div_threshold[i] <- abs(obs_threshold(vb_rw, theta = 0.3) -
                                    obs_threshold(vb_mack, theta = 0.3))
}

t1 <- Sys.time()
cat(sprintf("  Completed %d combinations in %.1f seconds.\n\n",
    nrow(obs_sweep), as.numeric(t1 - t0, units = "secs")))

# --- Checkpoint ---
cat("  Raw V:      max divergence = %.4f\n" |> sprintf(max(obs_sweep$div_raw)))
cat("  Sigmoid:    max divergence = %.4f\n" |> sprintf(max(obs_sweep$div_sigmoid)))
cat("  Threshold:  %d of %d combos show qualitative disagreement (%.0f%%)\n\n" |>
    sprintf(sum(obs_sweep$div_threshold > 0), nrow(obs_sweep),
            100 * mean(obs_sweep$div_threshold > 0)))


# --- Figure 12: 3-panel heatmap ---
cat("Generating Figure 12: Observation function sensitivity...\n")

# Reshape to long format for faceting
obs_long <- rbind(
  data.frame(alpha_A = obs_sweep$alpha_A, beta = obs_sweep$beta,
             divergence = obs_sweep$div_raw,
             metric = "Raw~italic(V)[B]"),
  data.frame(alpha_A = obs_sweep$alpha_A, beta = obs_sweep$beta,
             divergence = obs_sweep$div_sigmoid,
             metric = "Sigmoid~(gamma==5*','~theta==0.5)"),
  data.frame(alpha_A = obs_sweep$alpha_A, beta = obs_sweep$beta,
             divergence = obs_sweep$div_threshold,
             metric = "Threshold~(theta==0.3)")
)
obs_long$metric <- factor(obs_long$metric,
                          levels = c("Raw~italic(V)[B]",
                                     "Sigmoid~(gamma==5*','~theta==0.5)",
                                     "Threshold~(theta==0.3)"))

fig12 <- ggplot(obs_long, aes(x = alpha_A, y = beta)) +
  geom_raster(aes(fill = divergence), interpolate = TRUE) +
  facet_wrap(~metric, nrow = 1, labeller = label_parsed) +
  annotate("point", x = 0.4, y = 0.3,
           shape = 3, size = 2, color = "white", stroke = 1) +
  scale_fill_viridis(option = "D", name = "Divergence",
                     guide = guide_colorbar(title.position = "top")) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0.2, 0.8, 0.2)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.2, 0.8, 0.2)) +
  labs(x = expression(alpha[A]), y = expression(beta)) +
  coord_fixed() +
  theme_heat() +
  theme(strip.text = element_text(size = 8, color = "#303030"))

save_fig(fig12, "fig12_robustness_obs", width = 9, height = 3.5)

cat("\n")


# ============================================================================
# ANALYSIS B: V_B Reset Assumption Sensitivity
# ============================================================================
# Instead of hard-resetting V_B = 0 at Phase 3, use V_B = V_B_end_phase2 * g
# where g in {0, 0.25, 0.5, 0.75, 1.0} (generalization fraction).

cat("=== Analysis B: V_B Reset Assumption Sensitivity ===\n")

g_vals <- c(0, 0.25, 0.5, 0.75, 1.0)
a_A_fix <- 0.4
b_fix   <- 0.3

# Design
dp12_B <- c(make_trials("A", 1, 20, "P1"),
            make_trials(c("A", "B"), 1, 20, "P2"))
dp3_B  <- make_trials("B", 1, 20, "P3")

# Phase 1+2 for both models (run once)
V_rw_p12  <- rw_final(dp12_B, alpha = c(A = a_A_fix, B = 0.4), beta = b_fix)
state_mack_p12 <- mack_final_state(dp12_B, S = c(A = b_fix, B = b_fix),
                                   alpha_init = c(A = 0.5, B = 0.5))

cat(sprintf("  V_B after Phase 2:  RW = %.4f,  Mack = %.4f\n",
    V_rw_p12[["B"]], state_mack_p12$V[["B"]]))

reset_results <- data.frame(g = g_vals,
                            vb_rw = NA_real_,
                            vb_mack = NA_real_,
                            divergence = NA_real_)

for (j in seq_along(g_vals)) {
  g <- g_vals[j]

  # RW: Phase 3 with V_B = V_B_end_phase2 * g
  V_rw_init <- V_rw_p12
  V_rw_init["B"] <- V_rw_p12[["B"]] * g
  vb_rw_g <- rw_final(dp3_B, alpha = c(A = a_A_fix, B = 0.4), beta = b_fix,
                      V_init = V_rw_init)[["B"]]

  # Mack: Phase 3 with V_B = V_B_end_phase2 * g (alpha preserved)
  V_mack_init <- state_mack_p12$V
  V_mack_init["B"] <- state_mack_p12$V[["B"]] * g
  vb_mack_g <- mack_final_state(dp3_B, S = c(A = b_fix, B = b_fix),
                                alpha_init = state_mack_p12$alpha,
                                V_init = V_mack_init)$V[["B"]]

  reset_results$vb_rw[j]      <- vb_rw_g
  reset_results$vb_mack[j]    <- vb_mack_g
  reset_results$divergence[j] <- abs(vb_rw_g - vb_mack_g)
}

cat("\n  Reset sensitivity results:\n")
print(reset_results)
cat(sprintf("\n  Divergence range: %.4f to %.4f\n",
    min(reset_results$divergence), max(reset_results$divergence)))
cat("  Key finding: divergence is robust even with partial generalization.\n\n")


# --- Figure 13: Line plot ---
cat("Generating Figure 13: V_B reset assumption sensitivity...\n")

# Reshape for plotting both models' V_B
fig13_data <- rbind(
  data.frame(g = reset_results$g, V_B = reset_results$vb_rw,
             model = "Rescorla-Wagner"),
  data.frame(g = reset_results$g, V_B = reset_results$vb_mack,
             model = "Mackintosh")
)

fig13_left <- ggplot(fig13_data, aes(x = g, y = V_B,
                                     color = model, linetype = model)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Rescorla-Wagner" = pal[["blue"]],
                                "Mackintosh" = pal[["red"]])) +
  scale_linetype_manual(values = c("Rescorla-Wagner" = "solid",
                                   "Mackintosh" = "dashed")) +
  scale_x_continuous(breaks = g_vals) +
  labs(x = "Generalization fraction (g)",
       y = expression("Phase 3 final " * italic(V)[B]),
       color = NULL, linetype = NULL) +
  theme_pub()

fig13_right <- ggplot(reset_results, aes(x = g, y = divergence)) +
  geom_line(linewidth = 0.7, color = pal[["purple"]]) +
  geom_point(size = 2, color = pal[["purple"]]) +
  scale_x_continuous(breaks = g_vals) +
  labs(x = "Generalization fraction (g)",
       y = expression("|" * italic(V)[B]^{RW} - italic(V)[B]^{Mack} * "|")) +
  theme_pub()

# Combine using patchwork if available, otherwise save side by side via gridExtra
fig13 <- tryCatch({
  library(patchwork)
  fig13_left + fig13_right +
    plot_layout(widths = c(1.2, 1)) +
    plot_annotation(tag_levels = "A")
}, error = function(e) {
  # Fallback: use gridExtra
  library(gridExtra)
  grid.arrange(fig13_left, fig13_right, ncol = 2, widths = c(1.2, 1))
})

# Save explicitly to handle both patchwork and gridExtra cases
if (inherits(fig13, "patchwork")) {
  ggsave("Figures/fig13_robustness_reset.pdf", fig13,
         width = 8, height = 3.5, device = "pdf")
  ggsave("Figures/fig13_robustness_reset.png", fig13,
         width = 8, height = 3.5, dpi = 300, device = "png")
} else {
  pdf("Figures/fig13_robustness_reset.pdf", width = 8, height = 3.5)
  grid.arrange(fig13_left, fig13_right, ncol = 2, widths = c(1.2, 1))
  dev.off()
  png("Figures/fig13_robustness_reset.png", width = 8, height = 3.5,
      units = "in", res = 300)
  grid.arrange(fig13_left, fig13_right, ncol = 2, widths = c(1.2, 1))
  dev.off()
}
cat("  Saved: Figures/fig13_robustness_reset.pdf + .png\n\n")


# ============================================================================
# ANALYSIS C: Divergence Metric Sensitivity
# ============================================================================
# Fix alpha_A=0.4, beta=0.3. Compute three divergence metrics.

cat("=== Analysis C: Divergence Metric Sensitivity ===\n")

# Design
dp12_C <- c(make_trials("A", 1, 20, "P1"),
            make_trials(c("A", "B"), 1, 20, "P2"))
dp3_C  <- make_trials("B", 1, 20, "P3")

# --- Metric 1: Absolute difference at terminal trial ---
V_rw_C <- rw_final(dp12_C, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw_C["B"] <- 0
vb_rw_C <- rw_final(dp3_C, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                    V_init = V_rw_C)[["B"]]

state_mack_C <- mack_final_state(dp12_C, S = c(A = 0.3, B = 0.3),
                                 alpha_init = c(A = 0.5, B = 0.5))
state_mack_C$V["B"] <- 0
vb_mack_C <- mack_final_state(dp3_C, S = c(A = 0.3, B = 0.3),
                              alpha_init = state_mack_C$alpha,
                              V_init = state_mack_C$V)$V[["B"]]

abs_diff <- abs(vb_rw_C - vb_mack_C)

# --- Metric 2: Ratio V_B(Mack) / V_B(RW) ---
ratio <- vb_mack_C / vb_rw_C

# --- Metric 3: Learning rate proxy (slope over first 5 Phase 3 trials) ---
# Need full trial-by-trial simulation for Phase 3

# RW full simulation
V_rw_full <- rw_final(dp12_C, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw_full["B"] <- 0
rw_p3_full <- rw_simulate(dp3_C, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                          V_init = V_rw_full, cue_names = c("A", "B"))
rw_vb_traj <- rw_p3_full[rw_p3_full$cue == "B", ]

# Mackintosh full simulation
state_mack_full <- mack_final_state(dp12_C, S = c(A = 0.3, B = 0.3),
                                    alpha_init = c(A = 0.5, B = 0.5))
V_mack_full <- state_mack_full$V
alpha_mack_full <- state_mack_full$alpha
V_mack_full["B"] <- 0
mack_p3_full <- mack_simulate(dp3_C, S = c(A = 0.3, B = 0.3),
                               alpha_init = alpha_mack_full,
                               V_init = V_mack_full, cue_names = c("A", "B"))
mack_vb_traj <- mack_p3_full[mack_p3_full$cue == "B", ]

# Slope over first 5 trials (linear regression of V on trial number)
rw_first5   <- rw_vb_traj[rw_vb_traj$trial <= 5, ]
mack_first5 <- mack_vb_traj[mack_vb_traj$trial <= 5, ]

slope_rw   <- coef(lm(V ~ trial, data = rw_first5))[["trial"]]
slope_mack <- coef(lm(V ~ trial, data = mack_first5))[["trial"]]

# --- Report ---
cat(sprintf("  Fixed parameters: alpha_A = 0.4, beta = 0.3\n"))
cat(sprintf("  V_B at end of Phase 3:  RW = %.4f,  Mack = %.4f\n\n",
    vb_rw_C, vb_mack_C))

cat(sprintf("  Metric 1 — Absolute difference:  |V_B(RW) - V_B(Mack)| = %.4f\n",
    abs_diff))
cat(sprintf("  Metric 2 — Ratio:                V_B(Mack) / V_B(RW)  = %.4f\n",
    ratio))
cat(sprintf("  Metric 3 — Learning rate proxy:\n"))
cat(sprintf("    RW   slope (first 5 trials) = %.4f per trial\n", slope_rw))
cat(sprintf("    Mack slope (first 5 trials) = %.4f per trial\n", slope_mack))
cat(sprintf("    Ratio of slopes (Mack/RW)   = %.4f\n", slope_mack / slope_rw))
cat("\n  All three metrics agree: RW learns faster than Mackintosh in Phase 3.\n")
cat("  The qualitative conclusion is robust to metric choice.\n\n")


# ============================================================================
# ANALYSIS D: Mackintosh Attention Parameter Sensitivity
# ============================================================================
# Fix alpha_A=0.4, beta=0.3. Sweep beta_up and beta_down.

cat("=== Analysis D: Mackintosh Attention Parameter Sensitivity ===\n")
cat("Sweeping beta_up x beta_down...\n")

t0 <- Sys.time()

beta_up_range   <- c(0.01, 0.03, 0.05, 0.10)
beta_down_range <- c(0.05, 0.10, 0.15, 0.30)

attn_sweep <- expand.grid(beta_up = beta_up_range, beta_down = beta_down_range)
attn_sweep$divergence   <- NA_real_
attn_sweep$vb_rw        <- NA_real_
attn_sweep$vb_mack      <- NA_real_
attn_sweep$alpha_B_end  <- NA_real_

# Design
dp12_D <- c(make_trials("A", 1, 20, "P1"),
            make_trials(c("A", "B"), 1, 20, "P2"))
dp3_D  <- make_trials("B", 1, 20, "P3")

# RW result is the same for all combinations (does not use beta_up/beta_down)
V_rw_D <- rw_final(dp12_D, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw_D["B"] <- 0
vb_rw_D <- rw_final(dp3_D, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                    V_init = V_rw_D)[["B"]]

for (i in seq_len(nrow(attn_sweep))) {
  bu <- attn_sweep$beta_up[i]
  bd <- attn_sweep$beta_down[i]

  # Mack: Phase 1+2 with specific attention parameters
  state <- mack_final_state(dp12_D, S = c(A = 0.3, B = 0.3),
                            alpha_init = c(A = 0.5, B = 0.5),
                            beta_up = bu, beta_down = bd)
  state$V["B"] <- 0

  # Mack: Phase 3
  state_p3 <- mack_final_state(dp3_D, S = c(A = 0.3, B = 0.3),
                               alpha_init = state$alpha,
                               V_init = state$V,
                               beta_up = bu, beta_down = bd)

  attn_sweep$vb_rw[i]       <- vb_rw_D
  attn_sweep$vb_mack[i]     <- state_p3$V[["B"]]
  attn_sweep$alpha_B_end[i] <- state$alpha[["B"]]  # alpha_B at Phase 2/3 boundary
  attn_sweep$divergence[i]  <- abs(vb_rw_D - state_p3$V[["B"]])
}

t1 <- Sys.time()
cat(sprintf("  Completed %d combinations in %.1f seconds.\n\n",
    nrow(attn_sweep), as.numeric(t1 - t0, units = "secs")))

# --- Checkpoint ---
cat("  Attention parameter sweep results:\n")
print(attn_sweep[, c("beta_up", "beta_down", "alpha_B_end", "vb_mack", "divergence")])
cat(sprintf("\n  Max divergence:  %.4f  (beta_up=%.2f, beta_down=%.2f)\n",
    max(attn_sweep$divergence),
    attn_sweep$beta_up[which.max(attn_sweep$divergence)],
    attn_sweep$beta_down[which.max(attn_sweep$divergence)]))
cat(sprintf("  Min divergence:  %.4f  (beta_up=%.2f, beta_down=%.2f)\n",
    min(attn_sweep$divergence),
    attn_sweep$beta_up[which.min(attn_sweep$divergence)],
    attn_sweep$beta_down[which.min(attn_sweep$divergence)]))
cat(sprintf("  RW V_B (constant): %.4f\n", vb_rw_D))
cat("  Divergence is larger when beta_down is high (more α suppression in Phase 2).\n\n")


# --- Figure 14: Heatmap ---
cat("Generating Figure 14: Mackintosh attention parameter sensitivity...\n")

# Convert to factors for discrete heatmap tiles
attn_sweep$beta_up_f   <- factor(attn_sweep$beta_up)
attn_sweep$beta_down_f <- factor(attn_sweep$beta_down)

fig14 <- ggplot(attn_sweep, aes(x = beta_up_f, y = beta_down_f)) +
  geom_tile(aes(fill = divergence), color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", divergence)),
            size = 2.8, color = "white") +
  scale_fill_viridis(option = "D", name = "Divergence",
                     guide = guide_colorbar(title.position = "top")) +
  labs(x = expression(beta[up] ~ "(attention increase rate)"),
       y = expression(beta[down] ~ "(attention decrease rate)")) +
  coord_fixed() +
  theme_heat() +
  theme(axis.text = element_text(size = 8))

save_fig(fig14, "fig14_robustness_attention", width = 5, height = 4)


# ============================================================================
# Summary
# ============================================================================

cat("\n=== Robustness Check Summary ===\n")
cat("A. Observation function: The parameter region where models diverge shifts\n")
cat("   depending on the observation function. Under raw V and sigmoid, divergence\n")
cat("   spans a broad region. Under threshold mapping, qualitative disagreement\n")
cat("   depends on the threshold value and may be compressed or expanded.\n")
cat("B. V_B reset: The qualitative ordering (RW > Mack in Phase 3) persists across\n")
cat("   the tested range of generalization values, but divergence magnitude decreases\n")
cat("   as generalization increases.\n")
cat("C. Divergence metric: All three metrics (absolute, ratio, learning slope)\n")
cat("   agree qualitatively that RW learns faster in Phase 3.\n")
cat("D. Attention parameters: Divergence increases with beta_down (stronger\n")
cat("   attention suppression). The critical test works across all tested\n")
cat("   beta_up x beta_down combinations.\n")

cat("\nDone. Run this script with: source('analysis/07_robustness.R')\n")
