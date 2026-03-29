# ============================================================================
# Thinking With Simulations — Figure Generation (Rebuilt)
# ============================================================================
# Produces all figures for the tutorial paper.
# Each figure is generated independently — you can comment out sections
# you don't need to regenerate.
#
# Requirements: ggplot2, viridis
# Runtime: ~3-5 minutes (most time in Section 5 simulations)
# ============================================================================

library(ggplot2)
library(viridis)

source("R/models.R")

set.seed(2026)
dir.create("Figures", showWarnings = FALSE)


# --- Colour Palette (colorblind-friendly) ------------------------------------
pal <- c(
  blue   = "#4477AA",
  cyan   = "#66CCEE",
  green  = "#228833",
  yellow = "#CCBB44",
  red    = "#EE6677",
  purple = "#AA3377",
  grey   = "#BBBBBB"
)

# --- Publication Theme -------------------------------------------------------
theme_pub <- function(base_size = 9) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(color = "#F0F0F0", linewidth = 0.25),
      axis.line          = element_line(color = "#404040", linewidth = 0.3),
      axis.ticks         = element_line(color = "#404040", linewidth = 0.25),
      axis.ticks.length  = unit(2, "pt"),
      axis.title         = element_text(size = base_size, color = "#303030"),
      axis.text          = element_text(size = base_size - 1, color = "#505050"),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = base_size - 1, color = "#404040"),
      legend.key.size    = unit(0.8, "lines"),
      legend.margin      = margin(0, 0, 0, 0),
      legend.background  = element_blank(),
      plot.margin        = margin(6, 10, 6, 6)
    )
}

# Heatmap variant
theme_heat <- function(base_size = 9) {
  theme_pub(base_size) %+replace%
    theme(
      panel.grid.major = element_blank(),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "right",
      legend.title     = element_text(size = base_size - 1, color = "#505050"),
      legend.key.height = unit(2.5, "lines"),
      legend.key.width  = unit(0.5, "lines")
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
# FIGURE 2: Blocking (Section 2.2)
# ============================================================================
cat("Generating Figure 2: Blocking...\n")

blocking_design <- c(
  make_trials("A", 1, 10, "Phase 1"),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)
blocking_rw <- rw_simulate(blocking_design, alpha = c(A = 0.4, B = 0.4), beta = 0.3)

fig2 <- ggplot(blocking_rw, aes(x = trial, y = V, color = cue)) +
  geom_vline(xintercept = 10.5, linetype = "longdash",
             color = "#D0D0D0", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.0) +
  annotate("text", x = 5, y = 1.05,
           label = "Phase 1: A+", size = 2.5, color = "#999999") +
  annotate("text", x = 16, y = 1.05,
           label = "Phase 2: AB+", size = 2.5, color = "#999999") +
  annotate("segment", x = 11.5, xend = 11.5, y = 0.42, yend = 0.12,
           arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
           color = pal["purple"], linewidth = 0.35) +
  annotate("text", x = 14, y = 0.47,
           label = "Prediction\ncheckpoint",
           size = 2.2, color = pal["purple"], fontface = "italic") +
  scale_color_manual(values = c(A = pal[["blue"]], B = pal[["red"]]),
                     labels = c("Cue A", "Cue B")) +
  scale_y_continuous(limits = c(-0.05, 1.1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Trial", y = expression(italic(V)), color = NULL) +
  theme_pub()

save_fig(fig2, "fig2_blocking")


# ============================================================================
# FIGURE 3: Overexpectation (Section 2.4)
# ============================================================================
cat("Generating Figure 3: Overexpectation...\n")

overexpectation_design <- c(
  unlist(lapply(1:10, function(i) {
    c(make_trials("A", 1, 1, "Phase 1"),
      make_trials("B", 1, 1, "Phase 1"))
  }), recursive = FALSE),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)
overexpectation_rw <- rw_simulate(overexpectation_design,
                                   alpha = c(A = 0.4, B = 0.4), beta = 0.3)

phase_boundary <- max(overexpectation_rw$trial[overexpectation_rw$phase == "Phase 1"])

fig3 <- ggplot(overexpectation_rw, aes(x = trial, y = V, color = cue)) +
  geom_vline(xintercept = phase_boundary + 0.5, linetype = "longdash",
             color = "#D0D0D0", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.0) +
  annotate("text", x = phase_boundary / 2, y = 1.08,
           label = "Phase 1: A+ / B+", size = 2.5, color = "#999999") +
  annotate("text", x = phase_boundary + 5.5, y = 1.08,
           label = "Phase 2: AB+", size = 2.5, color = "#999999") +
  scale_color_manual(values = c(A = pal[["blue"]], B = pal[["red"]]),
                     labels = c("Cue A", "Cue B")) +
  scale_y_continuous(limits = c(-0.05, 1.15), breaks = seq(0, 1, 0.25)) +
  labs(x = "Trial", y = expression(italic(V)), color = NULL) +
  theme_pub()

save_fig(fig3, "fig3_overexpectation")


# ============================================================================
# FIGURE 4: Critical Test (Section 3.3)
# ============================================================================
cat("Generating Figure 4: Critical test...\n")

design_p12 <- c(
  make_trials("A", 1, 10, "Phase 1"),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)
design_p3 <- make_trials("B", 1, 10, "Phase 3")

# RW
rw_p12 <- rw_simulate(design_p12, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw <- extract_final_V(rw_p12); V_rw["B"] <- 0
rw_p3 <- rw_simulate(design_p3, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                     V_init = V_rw, cue_names = c("A", "B"))
critical_rw <- combine_phases(rw_p12, rw_p3)

# Mack
mack_p12 <- mack_simulate(design_p12, S = c(A = 0.3, B = 0.3),
                           alpha_init = c(A = 0.5, B = 0.5))
V_mack <- extract_final_V(mack_p12); alpha_mack <- extract_final_alpha(mack_p12)
V_mack["B"] <- 0
mack_p3 <- mack_simulate(design_p3, S = c(A = 0.3, B = 0.3),
                          alpha_init = alpha_mack, V_init = V_mack,
                          cue_names = c("A", "B"))
critical_mack <- combine_phases(mack_p12, mack_p3)

# Combine for plotting
crit_rw       <- critical_rw
crit_rw$model <- "Rescorla-Wagner"
crit_mack       <- critical_mack[, c("trial", "phase", "cue", "V")]
crit_mack$model <- "Mackintosh"
fig4_data <- rbind(crit_rw[crit_rw$cue == "B", ],
                   crit_mack[crit_mack$cue == "B", ])

p1_end <- max(fig4_data$trial[fig4_data$phase == "Phase 1"])
p2_end <- max(fig4_data$trial[fig4_data$phase == "Phase 2"])
p3_mid <- (p2_end + max(fig4_data$trial)) / 2
y_top  <- max(fig4_data$V) * 1.12

# Get final V_B values for endpoint annotations
rw_final_vb  <- tail(fig4_data$V[fig4_data$model == "Rescorla-Wagner"], 1)
mack_final_vb <- tail(fig4_data$V[fig4_data$model == "Mackintosh"], 1)
last_trial    <- max(fig4_data$trial)

# Subtle Phase 3 highlight
fig4 <- ggplot(fig4_data, aes(x = trial, y = V, color = model, linetype = model)) +
  annotate("rect", xmin = p2_end + 0.5, xmax = last_trial + 0.5,
           ymin = -0.05, ymax = y_top + 0.03, fill = "#F5F5FA", alpha = 0.7) +
  geom_vline(xintercept = c(p1_end + 0.5, p2_end + 0.5),
             linetype = "longdash", color = "#D0D0D0", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.0) +
  annotate("text", x = p1_end / 2, y = y_top,
           label = "Phase 1\nA+", size = 2.2, color = "#AAAAAA", lineheight = 0.9) +
  annotate("text", x = (p1_end + p2_end) / 2, y = y_top,
           label = "Phase 2\nAB+", size = 2.2, color = "#AAAAAA", lineheight = 0.9) +
  annotate("text", x = p3_mid, y = y_top,
           label = "Phase 3\nB\u2192new", size = 2.4, color = "#555555",
           fontface = "bold", lineheight = 0.9) +
  # Endpoint value annotations
  annotate("text", x = last_trial + 0.8, y = rw_final_vb,
           label = sprintf("%.2f", rw_final_vb), size = 2.3,
           color = pal[["blue"]], hjust = 0, fontface = "bold") +
  annotate("text", x = last_trial + 0.8, y = mack_final_vb,
           label = sprintf("%.2f", mack_final_vb), size = 2.3,
           color = pal[["red"]], hjust = 0, fontface = "bold") +
  scale_color_manual(values = c("Rescorla-Wagner" = pal[["blue"]],
                                "Mackintosh" = pal[["red"]])) +
  scale_linetype_manual(values = c("Rescorla-Wagner" = "solid",
                                   "Mackintosh" = "dashed")) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  scale_y_continuous(limits = c(-0.05, y_top + 0.05), breaks = seq(0, 1, 0.25)) +
  labs(x = "Trial", y = expression(italic(V)[B]), color = NULL, linetype = NULL) +
  theme_pub() +
  theme(legend.key.width = unit(1.5, "lines"))

save_fig(fig4, "fig4_critical_test", width = 5.5, height = 3.5)


# ============================================================================
# FIGURE 5: Alpha Dynamics (Section 3.3)
# ============================================================================
cat("Generating Figure 5: Alpha dynamics...\n")

fig5_data <- critical_mack[critical_mack$cue == "B", ]

fig5 <- ggplot(fig5_data, aes(x = trial, y = alpha)) +
  geom_vline(xintercept = c(p1_end + 0.5, p2_end + 0.5),
             linetype = "longdash", color = "#D0D0D0", linewidth = 0.3) +
  geom_line(linewidth = 0.7, color = pal[["purple"]]) +
  geom_point(size = 1.0, color = pal[["purple"]]) +
  annotate("text", x = p1_end / 2, y = max(fig5_data$alpha) * 1.06,
           label = "Phase 1", size = 2.5, color = "#999999") +
  annotate("text", x = (p1_end + p2_end) / 2, y = max(fig5_data$alpha) * 1.06,
           label = "Phase 2", size = 2.5, color = "#999999") +
  annotate("text", x = p3_mid, y = max(fig5_data$alpha) * 1.06,
           label = "Phase 3", size = 2.5, color = "#999999") +
  labs(x = "Trial", y = expression(alpha[B])) +
  theme_pub()

save_fig(fig5, "fig5_alpha_dynamics")


# ============================================================================
# FIGURE 6: Parameter Sensitivity Heatmap (Section 4.2)
# ============================================================================
cat("Generating Figure 6: Parameter sweep...\n")

source("analysis/03_parameter_sweep.R")

fig6 <- ggplot(param_sweep, aes(x = alpha_A, y = beta)) +
  geom_raster(aes(fill = divergence), interpolate = TRUE) +
  stat_contour(aes(z = divergence), color = "white", linewidth = 0.25,
               alpha = 0.5, bins = 6) +
  # Section 3 parameters with label
  annotate("point", x = 0.4, y = 0.3,
           shape = 3, size = 3, color = "white", stroke = 1.5) +
  annotate("text", x = 0.4, y = 0.24,
           label = "Sec. 3", size = 2.2, color = "white", fontface = "bold") +
  # Region label for maximum divergence
  annotate("text", x = 0.5, y = 0.15,
           label = "max divergence", size = 2.0, color = "white",
           fontface = "italic", alpha = 0.8) +
  scale_fill_viridis(option = "D", name = expression(Delta * italic(V)[B]),
                     guide = guide_colorbar(title.position = "top")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(alpha[A]), y = expression(beta)) +
  coord_fixed() +
  theme_heat()

save_fig(fig6, "fig6_param_sweep", width = 4.5, height = 3.8)


# ============================================================================
# FIGURE 7: Design Parameter Sweep (Section 4.3)
# ============================================================================
cat("Generating Figure 7: Design sweep...\n")

fig7 <- ggplot(design_sweep, aes(x = n_p1, y = n_p2)) +
  geom_raster(aes(fill = divergence), interpolate = TRUE) +
  stat_contour(aes(z = divergence), color = "white", linewidth = 0.25,
               alpha = 0.5, bins = 6) +
  scale_fill_viridis(option = "D", name = expression(Delta * italic(V)[B]),
                     guide = guide_colorbar(title.position = "top")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Phase 1 trials", y = "Phase 2 trials") +
  coord_fixed() +
  theme_heat()

save_fig(fig7, "fig7_design_sweep", width = 4.5, height = 3.8)


# ============================================================================
# FIGURE 8: Power Curve (Section 5.2)
# ============================================================================
cat("Generating Figure 8: Power curve...\n")

source("analysis/04_virtual_experiment.R")

fig8 <- ggplot(power_results, aes(x = n, y = power)) +
  geom_hline(yintercept = 0.80, linetype = "longdash",
             color = "#D0D0D0", linewidth = 0.3) +
  geom_line(linewidth = 0.7, color = pal[["blue"]]) +
  geom_point(size = 2.0, color = pal[["blue"]]) +
  annotate("text", x = max(sample_sizes) * 0.92, y = 0.77,
           label = "80%", size = 2.5, color = "#999999") +
  scale_x_continuous(breaks = sample_sizes) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x * 100, "%")) +
  labs(x = "Sample size (N)", y = "Power") +
  theme_pub()

save_fig(fig8, "fig8_power_curve")


# ============================================================================
# FIGURES 9-11: Overexpectation sweep, observation function, mechanism isolation
# ============================================================================
cat("Generating Figures 9-11: Novel figures...\n")
source("analysis/05_novel_figures.R")

cat("\nAll figures generated successfully.\n")
