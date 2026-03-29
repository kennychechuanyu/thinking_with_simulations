# ============================================================================
# Section 5 — Virtual Experiment and Power Analysis
# ============================================================================
# Runtime: ~2-5 minutes (Monte Carlo simulations)
# Data used by: figures.R (fig8_power_curve)
# ============================================================================

source("R/models.R")

set.seed(2026)

cat("=== Virtual Experiment: Power Analysis ===\n")
cat("Runtime: ~2-5 minutes.\n\n")


# === Reference prediction under RW ==========================================
# What does the RW model predict for Phase 3 V_B?

power_p12 <- c(make_trials("A", 1, 20, "P1"),
               make_trials(c("A", "B"), 1, 20, "P2"))
power_p3  <- make_trials("B", 1, 10, "P3")

V_rw_pow <- rw_final(power_p12, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw_pow["B"] <- 0
vb_rw_ref <- rw_final(power_p3, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                       V_init = V_rw_pow)[["B"]]

cat(sprintf("  RW reference V_B (end of Phase 3): %.4f\n\n", vb_rw_ref))


# === Generative Model ========================================================
# For each virtual participant:
#   1. Draw individual S_i ~ Beta(3, 7)   [mean ~ 0.3]
#   2. Run Mackintosh through 3 phases
#   3. Add Gaussian observation noise:  Y_i = V_B + ε,  ε ~ N(0, σ²)

noise_sd <- 1.0

# === Power Analysis ==========================================================
# For each sample size N, simulate 500 experiments.
# In each experiment:
#   - Generate N virtual participants under Mackintosh
#   - Test H₀: mean(V_B) = vb_rw_ref  vs  H₁: mean(V_B) < vb_rw_ref
#   - Power = proportion of experiments where p < 0.05

sample_sizes <- c(10, 15, 20, 30, 50, 75, 100)
n_sims       <- 500

power_results <- data.frame(n = sample_sizes, power = NA_real_)

t0 <- Sys.time()

for (j in seq_along(sample_sizes)) {
  n_subj    <- sample_sizes[j]
  sig_count <- 0L

  for (sim in seq_len(n_sims)) {
    vb_obs <- numeric(n_subj)

    for (i in seq_len(n_subj)) {
      s_i <- rbeta(1, 3, 7)   # individual salience, mean ~ 0.3

      # Phase 1+2: learn blocking
      state <- mack_final_state(power_p12, S = c(A = s_i, B = s_i),
                                alpha_init = c(A = 0.5, B = 0.5))
      state$V["B"] <- 0       # new outcome: V_B resets, α_B preserved

      # Phase 3: relearn B
      V_end <- mack_final_state(power_p3, S = c(A = s_i, B = s_i),
                                alpha_init = state$alpha,
                                V_init = state$V)$V

      vb_obs[i] <- V_end[["B"]] + rnorm(1, 0, noise_sd)
    }

    p <- t.test(vb_obs, mu = vb_rw_ref, alternative = "less")$p.value
    if (p < 0.05) sig_count <- sig_count + 1L
  }

  power_results$power[j] <- sig_count / n_sims

  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  cat(sprintf("  N = %3d: power = %.2f  [%d/%d sims, %.0fs elapsed]\n",
      n_subj, power_results$power[j], n_sims, n_sims, elapsed))
}

cat("\n=== Power Analysis Results ===\n")
print(power_results)

# --- Expected Output Checkpoint ----------------------------------------------

# Monte Carlo SE for each estimate
cat("\n=== Monte Carlo Precision ===\n")
for (j in seq_along(sample_sizes)) {
  p_hat <- power_results$power[j]
  se    <- sqrt(p_hat * (1 - p_hat) / n_sims)
  cat(sprintf("  N = %3d: power = %.3f ± %.3f (SE)\n",
      sample_sizes[j], p_hat, se))
}


# === Monte Carlo Convergence Illustration ====================================
# Show how the power estimate stabilizes as n_sim increases

cat("\n=== Monte Carlo Convergence (N = 30, 2000 sims) ===\n")

mc_n_subj  <- 30
mc_n_total <- 2000
mc_checkpoints <- c(50, 100, 200, 500, 1000, 2000)

mc_hits <- logical(mc_n_total)

t0 <- Sys.time()
for (sim in seq_len(mc_n_total)) {
  vb_obs <- numeric(mc_n_subj)
  for (i in seq_len(mc_n_subj)) {
    s_i   <- rbeta(1, 3, 7)
    state <- mack_final_state(power_p12, S = c(A = s_i, B = s_i),
                              alpha_init = c(A = 0.5, B = 0.5))
    state$V["B"] <- 0
    V_end <- mack_final_state(power_p3, S = c(A = s_i, B = s_i),
                              alpha_init = state$alpha,
                              V_init = state$V)$V
    vb_obs[i] <- V_end[["B"]] + rnorm(1, 0, noise_sd)
  }
  mc_hits[sim] <- t.test(vb_obs, mu = vb_rw_ref,
                         alternative = "less")$p.value < 0.05

  if (sim %in% mc_checkpoints) {
    p_hat <- mean(mc_hits[1:sim])
    se    <- sqrt(p_hat * (1 - p_hat) / sim)
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("  %4d sims: power = %.3f ± %.3f  [%.0fs elapsed]\n",
        sim, p_hat, se, elapsed))
  }
}

mc_convergence <- data.frame(
  n_sim = mc_checkpoints,
  power = sapply(mc_checkpoints, function(k) mean(mc_hits[1:k])),
  se    = sapply(mc_checkpoints, function(k) {
    p_hat <- mean(mc_hits[1:k])
    sqrt(p_hat * (1 - p_hat) / k)
  })
)

cat("\n=== Convergence Table ===\n")
print(mc_convergence)


# === TRY THIS exercises =====================================================
cat("\n=== Try This Exercises ===\n")
cat("1. Increase noise to sigma=3.0.\n")
cat("2. Change individual-differences distribution to Beta(2,8).\n")
cat("3. Change Phase 3 from 10 to 5 trials.\n\n")
