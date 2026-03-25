# ============================================================================
# Novel Figures — The paper's most original contributions
# ============================================================================
# Figure 9: Overexpectation parameter sweep (the failure example)
# Figure 10: Observation function flipping model comparison
# Runtime: ~10 seconds
# ============================================================================

source("R/models.R")
library(ggplot2)
library(viridis)

set.seed(2026)

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
# FIGURE 9: Overexpectation Parameter Sweep (Section 4 — Failure Example)
# ============================================================================
# Shows that overexpectation is PARAMETRIC, not structural.
# Sweep alpha and beta; measure the magnitude of V decline in Phase 2.

cat("Generating Figure 9: Overexpectation parameter sweep...\n")

alpha_range <- seq(0.05, 0.95, by = 0.05)
beta_range  <- seq(0.05, 0.95, by = 0.05)

oe_sweep <- expand.grid(alpha = alpha_range, beta = beta_range)
oe_sweep$decline <- NA_real_

for (i in seq_len(nrow(oe_sweep))) {
  a <- oe_sweep$alpha[i]
  b <- oe_sweep$beta[i]

  # Phase 1: train A and B separately (alternating, 10 each)
  design_p1 <- unlist(lapply(1:10, function(j) {
    c(make_trials("A", 1, 1, "P1"), make_trials("B", 1, 1, "P1"))
  }), recursive = FALSE)

  # Phase 2: compound AB+ (10 trials)
  design_p2 <- make_trials(c("A", "B"), 1, 10, "P2")

  design <- c(design_p1, design_p2)

  # Run RW model
  V_final <- rw_final(design, alpha = c(A = a, B = a), beta = b)

  # V_A at end of Phase 1 (before compound)
  V_p1 <- rw_final(design_p1, alpha = c(A = a, B = a), beta = b)

  # Decline = V at end of Phase 1 minus V at end of Phase 2
  # Positive = overexpectation occurred (V declined)
  oe_sweep$decline[i] <- V_p1[["A"]] - V_final[["A"]]
}

cat(sprintf("  Max decline: %.3f at alpha=%.2f, beta=%.2f\n",
    max(oe_sweep$decline),
    oe_sweep$alpha[which.max(oe_sweep$decline)],
    oe_sweep$beta[which.max(oe_sweep$decline)]))
cat(sprintf("  Region with decline > 0.05: %.0f%% of parameter space\n",
    100 * mean(oe_sweep$decline > 0.05)))
cat(sprintf("  Region with decline > 0.15: %.0f%% of parameter space\n",
    100 * mean(oe_sweep$decline > 0.15)))

fig9 <- ggplot(oe_sweep, aes(x = alpha, y = beta)) +
  geom_raster(aes(fill = decline), interpolate = TRUE) +
  stat_contour(aes(z = decline), color = "white", linewidth = 0.25,
               alpha = 0.5, bins = 6) +
  annotate("point", x = 0.4, y = 0.3,
           shape = 3, size = 2.5, color = "white", stroke = 1.2) +
  scale_fill_viridis(option = "D",
                     name = expression(Delta * italic(V)[A]),
                     guide = guide_colorbar(title.position = "top")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(alpha), y = expression(beta)) +
  coord_fixed() +
  theme_heat()

save_fig(fig9, "fig9_overexpectation_sweep", width = 4.5, height = 3.8)


# ============================================================================
# FIGURE 10: Observation Function Impact on Model Comparison (Section 3)
# ============================================================================
# Shows Phase 2 V_B trajectories for both models, with a threshold line
# that creates a QUALITATIVE divergence: Mack V_B crosses threshold,
# RW V_B does not.

cat("\nGenerating Figure 10: Observation function impact...\n")

blocking_design <- c(
  make_trials("A", 1, 10, "Phase 1"),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)

# RW simulation
rw_res <- rw_simulate(blocking_design, alpha = c(A = 0.4, B = 0.4), beta = 0.3)

# Mackintosh simulation
mack_res <- mack_simulate(blocking_design, S = c(A = 0.3, B = 0.3),
                           alpha_init = c(A = 0.5, B = 0.5))

# Extract V_B trajectories
rw_vb <- rw_res[rw_res$cue == "B", c("trial", "V")]
rw_vb$model <- "Rescorla-Wagner"

mack_vb <- mack_res[mack_res$cue == "B", c("trial", "V")]
mack_vb$model <- "Mackintosh"

fig10_data <- rbind(rw_vb, mack_vb)

# Threshold line at 0.3
threshold <- 0.3

fig10 <- ggplot(fig10_data, aes(x = trial, y = V, color = model, linetype = model)) +
  geom_vline(xintercept = 10.5, linetype = "longdash",
             color = "#D0D0D0", linewidth = 0.3) +
  geom_hline(yintercept = threshold, linetype = "dotted",
             color = "#808080", linewidth = 0.5) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.0) +
  annotate("text", x = 5, y = 0.62,
           label = "Phase 1: A+", size = 2.5, color = "#999999") +
  annotate("text", x = 16, y = 0.62,
           label = "Phase 2: AB+", size = 2.5, color = "#999999") +
  annotate("text", x = 20.5, y = threshold + 0.03,
           label = expression(theta == 0.3), size = 2.2, color = "#808080") +
  annotate("segment", x = 19, xend = 19, y = 0.52, yend = threshold + 0.02,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
           color = pal["red"], linewidth = 0.3) +
  annotate("text", x = 17.5, y = 0.54,
           label = "Mack: responds", size = 2.0, color = pal["red"],
           fontface = "italic") +
  annotate("segment", x = 19, xend = 19, y = 0.10, yend = threshold - 0.02,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
           color = pal["blue"], linewidth = 0.3) +
  annotate("text", x = 17.3, y = 0.08,
           label = "RW: no response", size = 2.0, color = pal["blue"],
           fontface = "italic") +
  scale_color_manual(values = c("Rescorla-Wagner" = pal[["blue"]],
                                "Mackintosh" = pal[["red"]])) +
  scale_linetype_manual(values = c("Rescorla-Wagner" = "solid",
                                   "Mackintosh" = "dashed")) +
  scale_y_continuous(limits = c(-0.02, 0.65), breaks = seq(0, 0.6, 0.1)) +
  labs(x = "Trial", y = expression(italic(V)[B]),
       color = NULL, linetype = NULL) +
  theme_pub() +
  theme(legend.key.width = unit(1.5, "lines"))

save_fig(fig10, "fig10_obs_function_impact", width = 5.5, height = 3.5)


# Print key values for manuscript
cat("\n=== Key Values for Manuscript ===\n")
rw_vb_end <- rw_res$V[rw_res$cue == "B" & rw_res$trial == max(rw_res$trial)]
mack_vb_end <- mack_res$V[mack_res$cue == "B" & mack_res$trial == max(mack_res$trial)]
cat(sprintf("  RW V_B at end of Phase 2: %.3f (below threshold %.1f → no response)\n",
    rw_vb_end, threshold))
cat(sprintf("  Mack V_B at end of Phase 2: %.3f (above threshold %.1f → responds)\n",
    mack_vb_end, threshold))
cat(sprintf("  Same models, same design, same 'blocking' — but the observation\n"))
cat(sprintf("  function determines which model predicts a behavioral response.\n"))


# ============================================================================
# FIGURE 11: Mechanism Isolation (Section 4.5)
# ============================================================================
# Shows Phase 3 learning under three conditions:
#   1. Rescorla-Wagner (alpha_B intact)
#   2. Full Mackintosh (alpha_B suppressed)
#   3. Mackintosh with frozen alpha (attention mechanism disabled)

cat("\nGenerating Figure 11: Mechanism isolation...\n")

design_p12 <- c(
  make_trials("A", 1, 10, "Phase 1"),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)
design_p3 <- make_trials("B", 1, 10, "Phase 3")

# --- RW: Phase 1+2 -> reset V_B -> Phase 3 ---
rw_p12 <- rw_simulate(design_p12, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw <- extract_final_V(rw_p12); V_rw["B"] <- 0
rw_p3 <- rw_simulate(design_p3, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                     V_init = V_rw, cue_names = c("A", "B"))

# --- Full Mackintosh: Phase 1+2 -> reset V_B -> Phase 3 ---
mack_p12 <- mack_simulate(design_p12, S = c(A = 0.3, B = 0.3),
                           alpha_init = c(A = 0.5, B = 0.5))
V_mack <- extract_final_V(mack_p12); alpha_mack <- extract_final_alpha(mack_p12)
V_mack["B"] <- 0
mack_p3 <- mack_simulate(design_p3, S = c(A = 0.3, B = 0.3),
                          alpha_init = alpha_mack, V_init = V_mack,
                          cue_names = c("A", "B"))

# --- Mackintosh with frozen alpha: alpha_B stays at initial 0.5 ---
# Run Phase 1+2 to get V values, then reset V_B but use initial alpha
V_mack_frozen <- extract_final_V(mack_p12)
V_mack_frozen["B"] <- 0
mack_frozen_p3 <- mack_simulate(design_p3, S = c(A = 0.3, B = 0.3),
                                 alpha_init = c(A = 0.5, B = 0.5),
                                 V_init = V_mack_frozen,
                                 cue_names = c("A", "B"),
                                 beta_up = 0, beta_down = 0)  # freeze alpha

# Extract Phase 3 V_B trajectories
rw_vb_p3 <- rw_p3[rw_p3$cue == "B", c("trial", "V")]
rw_vb_p3$model <- "Rescorla-Wagner"

mack_vb_p3 <- mack_p3[mack_p3$cue == "B", c("trial", "V")]
mack_vb_p3$model <- "Mackintosh"

frozen_vb_p3 <- mack_frozen_p3[mack_frozen_p3$cue == "B", c("trial", "V")]
frozen_vb_p3$model <- "Mack (frozen α)"

fig11_data <- rbind(rw_vb_p3, mack_vb_p3, frozen_vb_p3)

fig11 <- ggplot(fig11_data, aes(x = trial, y = V, color = model, linetype = model)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.0) +
  scale_color_manual(values = c("Rescorla-Wagner" = pal[["blue"]],
                                "Mackintosh" = pal[["red"]],
                                "Mack (frozen α)" = pal[["green"]])) +
  scale_linetype_manual(values = c("Rescorla-Wagner" = "solid",
                                   "Mackintosh" = "dashed",
                                   "Mack (frozen α)" = "dotted")) +
  scale_y_continuous(limits = c(-0.05, 1.0), breaks = seq(0, 1, 0.2)) +
  labs(x = "Phase 3 Trial", y = expression(italic(V)[B]),
       color = NULL, linetype = NULL) +
  theme_pub() +
  theme(legend.key.width = unit(1.5, "lines"))

save_fig(fig11, "fig11_mechanism_isolation", width = 5.5, height = 3.5)

# Print key values
rw_final_vb <- rw_vb_p3$V[rw_vb_p3$trial == max(rw_vb_p3$trial)]
mack_final_vb <- mack_vb_p3$V[mack_vb_p3$trial == max(mack_vb_p3$trial)]
frozen_final_vb <- frozen_vb_p3$V[frozen_vb_p3$trial == max(frozen_vb_p3$trial)]
cat(sprintf("  RW V_B (end Phase 3): %.2f\n", rw_final_vb))
cat(sprintf("  Mack V_B (end Phase 3): %.2f\n", mack_final_vb))
cat(sprintf("  Mack frozen-α V_B (end Phase 3): %.2f\n", frozen_final_vb))
cat("  With attention frozen, Mack learns FASTER than RW — the Phase 3\n")
cat("  slowing is caused entirely by attention suppression.\n")

cat("\nDone.\n")
