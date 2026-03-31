# ============================================================================
# Section 5 Extension — Model Recovery Analysis
# ============================================================================
# DIDACTIC DEMONSTRATION: This script is a simplified, pedagogical example of
# model recovery, not a publication-ready analysis. It uses a single SSE
# comparison at the population mean rate rather than per-participant MLE/MAP
# estimation with BIC or Bayes factors. The goal is to illustrate the *logic*
# of model recovery (generate from each model, fit both, check the confusion
# matrix) in a way that is transparent and easy to follow.
#
# This script implements the model recovery logic described in Section 5.
# It generates synthetic data from each candidate model (both RW and
# Mackintosh) and checks whether a simple fit-based comparison correctly
# identifies the generating model.
#
# Runtime: ~2-5 minutes
# ============================================================================

source("R/models.R")

set.seed(2026)

cat("=== Model Recovery Analysis ===\n\n")

# === Design ================================================================

design_p12 <- c(make_trials("A", 1, 20, "P1"),
                make_trials(c("A", "B"), 1, 20, "P2"))
design_p3  <- make_trials("B", 1, 10, "P3")

noise_sd <- 1.0
n_subj   <- 30
n_sims   <- 200

# === Helper: generate one participant's Phase 3 V_B ======================

generate_vb <- function(model, s_i) {
  if (model == "RW") {
    V_end <- rw_final(design_p12, alpha = c(A = 0.4, B = 0.4), beta = s_i)
    V_end["B"] <- 0
    vb <- rw_final(design_p3, alpha = c(A = 0.4, B = 0.4), beta = s_i,
                   V_init = V_end)[["B"]]
  } else {
    state <- mack_final_state(design_p12, S = c(A = s_i, B = s_i),
                              alpha_init = c(A = 0.5, B = 0.5))
    state$V["B"] <- 0
    vb <- mack_final_state(design_p3, S = c(A = s_i, B = s_i),
                           alpha_init = state$alpha,
                           V_init = state$V)$V[["B"]]
  }
  return(vb)  # clean V_B; noise added by caller
}

# === Helper: compute reference predictions for a given rate ===============

reference_vb <- function(model, rate) {
  if (model == "RW") {
    V_end <- rw_final(design_p12, alpha = c(A = 0.4, B = 0.4), beta = rate)
    V_end["B"] <- 0
    rw_final(design_p3, alpha = c(A = 0.4, B = 0.4), beta = rate,
             V_init = V_end)[["B"]]
  } else {
    state <- mack_final_state(design_p12, S = c(A = rate, B = rate),
                              alpha_init = c(A = 0.5, B = 0.5))
    state$V["B"] <- 0
    mack_final_state(design_p3, S = c(A = rate, B = rate),
                     alpha_init = state$alpha,
                     V_init = state$V)$V[["B"]]
  }
}

# === Simple SSE-based model comparison ====================================
# For each simulated dataset, compute the sum of squared errors between
# observed V_B values and each model's prediction at the mean rate.
# The model with lower SSE is "selected."

mean_rate <- 0.3  # mean of Beta(3,7)
ref_rw   <- reference_vb("RW", mean_rate)
ref_mack <- reference_vb("Mack", mean_rate)

cat(sprintf("  RW reference V_B:   %.4f\n", ref_rw))
cat(sprintf("  Mack reference V_B: %.4f\n\n", ref_mack))

# === Recovery simulation ==================================================

recovery <- data.frame(
  generating = rep(c("RW", "Mack"), each = n_sims),
  selected   = character(n_sims * 2),
  stringsAsFactors = FALSE
)

t0 <- Sys.time()

for (sim in seq_len(n_sims * 2)) {
  gen_model <- recovery$generating[sim]

  # Generate dataset
  vb_obs <- numeric(n_subj)
  for (i in seq_len(n_subj)) {
    s_i <- rbeta(1, 3, 7)
    vb_obs[i] <- generate_vb(gen_model, s_i) + rnorm(1, 0, noise_sd)
  }

  # Compare SSE
  sse_rw   <- sum((vb_obs - ref_rw)^2)
  sse_mack <- sum((vb_obs - ref_mack)^2)

  recovery$selected[sim] <- ifelse(sse_rw < sse_mack, "RW", "Mack")

  if (sim %% 50 == 0) {
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("  Simulation %d/%d [%.0fs elapsed]\n", sim, n_sims * 2, elapsed))
  }
}

# === Results ==============================================================

cat("\n=== Model Recovery Results ===\n")

# Confusion matrix
rw_gen  <- recovery[recovery$generating == "RW", ]
mack_gen <- recovery[recovery$generating == "Mack", ]

rw_correct   <- mean(rw_gen$selected == "RW")
mack_correct <- mean(mack_gen$selected == "Mack")

cat(sprintf("  When RW generated data:\n"))
cat(sprintf("    Correctly identified as RW:   %.1f%%\n", rw_correct * 100))
cat(sprintf("    Misidentified as Mack:        %.1f%%\n", (1 - rw_correct) * 100))
cat(sprintf("  When Mack generated data:\n"))
cat(sprintf("    Correctly identified as Mack: %.1f%%\n", mack_correct * 100))
cat(sprintf("    Misidentified as RW:          %.1f%%\n", (1 - mack_correct) * 100))
cat(sprintf("\n  Overall recovery rate: %.1f%%\n",
    (rw_correct + mack_correct) / 2 * 100))

# === Recovery across noise levels ==========================================
cat("\n=== Recovery vs. Noise Level ===\n")
noise_levels <- c(0.5, 1.0, 2.0, 3.0, 5.0)
n_sims_noise <- 100

noise_results <- data.frame(sigma = noise_levels, recovery = NA_real_)

for (ni in seq_along(noise_levels)) {
  noise   <- noise_levels[ni]
  correct <- 0L
  total   <- 0L
  for (gen in c("RW", "Mack")) {
    for (sim in seq_len(n_sims_noise)) {
      vb_obs <- numeric(n_subj)
      for (i in seq_len(n_subj)) {
        s_i <- rbeta(1, 3, 7)
        vb_obs[i] <- generate_vb(gen, s_i) + rnorm(1, 0, noise)
      }
      sse_rw   <- sum((vb_obs - ref_rw)^2)
      sse_mack <- sum((vb_obs - ref_mack)^2)
      selected <- ifelse(sse_rw < sse_mack, "RW", "Mack")
      if (selected == gen) correct <- correct + 1L
      total   <- total + 1L
    }
  }
  noise_results$recovery[ni] <- 100 * correct / total
  cat(sprintf("  sigma = %.1f: recovery = %.1f%%\n", noise, noise_results$recovery[ni]))
}


# === Figure S4: Recovery Rate vs. Observation Noise ==========================

library(ggplot2)
dir.create("Figures", showWarnings = FALSE)

figS4 <- ggplot(noise_results, aes(x = sigma, y = recovery)) +
  geom_hline(yintercept = 50, linetype = "dotted",
             colour = "#888888", linewidth = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed",
             colour = "#888888", linewidth = 0.5) +
  geom_line(colour = "#4477AA", linewidth = 0.8) +
  geom_point(colour = "#4477AA", size = 3) +
  annotate("text", x = 4.8, y = 82.5, label = "80%",
           colour = "#888888", size = 3, hjust = 1) +
  annotate("text", x = 4.8, y = 52.5, label = "Chance (50%)",
           colour = "#888888", size = 3, hjust = 1) +
  scale_x_continuous(breaks = noise_levels) +
  scale_y_continuous(limits = c(40, 105),
                     breaks = seq(50, 100, by = 10)) +
  labs(
    x = "Observation noise (sigma)",
    y = "Recovery rate (%)"
  ) +
  theme_classic(base_size = 11)

ggsave("Figures/figS4_model_recovery_noise.pdf", figS4,
       width = 5.0, height = 3.5, device = "pdf")
ggsave("Figures/figS4_model_recovery_noise.png", figS4,
       width = 5.0, height = 3.5, dpi = 300, device = "png")

cat("\nFigure S4 saved: Figures/figS4_model_recovery_noise\n")

