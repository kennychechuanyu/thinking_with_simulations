# ============================================================================
# Section 4 — Parameter and Design Sweeps
# ============================================================================
# Runtime: ~5-10 seconds
# Data used by: figures.R (fig6_param_sweep, fig7_design_sweep)
# ============================================================================

source("R/models.R")

set.seed(2026)

cat("=== Parameter Sensitivity Sweep ===\n")

t0 <- Sys.time()

# === Simulation 5: Parameter Sensitivity =====================================
# Sweep α_A and β across [0.1, 0.9]
# For each combination, run the full critical test and measure divergence

alpha_range <- seq(0.1, 0.9, by = 0.05)
beta_range  <- seq(0.1, 0.9, by = 0.05)

param_sweep <- expand.grid(alpha_A = alpha_range, beta = beta_range)
param_sweep$divergence <- NA_real_

for (i in seq_len(nrow(param_sweep))) {
  a_A <- param_sweep$alpha_A[i]
  b   <- param_sweep$beta[i]

  dp12 <- c(make_trials("A", 1, 20, "P1"),
            make_trials(c("A", "B"), 1, 20, "P2"))
  dp3  <- make_trials("B", 1, 20, "P3")

  # RW: Phase 1+2 → reset V_B → Phase 3
  V_rw_s <- rw_final(dp12, alpha = c(A = a_A, B = 0.4), beta = b)
  V_rw_s["B"] <- 0
  vb_rw <- rw_final(dp3, alpha = c(A = a_A, B = 0.4), beta = b,
                    V_init = V_rw_s)[["B"]]

  # Mack: Phase 1+2 → reset V_B → Phase 3
  # Note: S (Mackintosh salience) is yoked to beta (RW learning rate) for fair comparison
  state <- mack_final_state(dp12, S = c(A = b, B = b),
                            alpha_init = c(A = 0.5, B = 0.5))
  state$V["B"] <- 0
  vb_mack <- mack_final_state(dp3, S = c(A = b, B = b),
                              alpha_init = state$alpha,
                              V_init = state$V)$V[["B"]]

  param_sweep$divergence[i] <- abs(vb_rw - vb_mack)
}

t1 <- Sys.time()
cat(sprintf("  Completed %d parameter combinations in %.1f seconds.\n\n",
    nrow(param_sweep), as.numeric(t1 - t0, units = "secs")))

# --- Expected Output Checkpoint ----------------------------------------------
cat("=== Parameter Sweep: Key Findings ===\n")
cat(sprintf("  Max divergence:  %.4f  (at α_A=%.2f, β=%.2f)\n",
    max(param_sweep$divergence),
    param_sweep$alpha_A[which.max(param_sweep$divergence)],
    param_sweep$beta[which.max(param_sweep$divergence)]))
cat(sprintf("  Min divergence:  %.4f\n", min(param_sweep$divergence)))
cat(sprintf("  Divergence at α_A=0.4, β=0.3:  %.4f\n",
    with(param_sweep,
         divergence[abs(alpha_A - 0.4) < 1e-9 & abs(beta - 0.3) < 1e-9])))
cat(sprintf("  Divergence > 0.05 in %.1f%% of the grid.\n",
    100 * mean(param_sweep$divergence > 0.05)))
cat("\n")


# === Simulation 6: Design Parameter Sweep ====================================
# Sweep Phase 1 and Phase 2 trial counts; Phase 3 fixed at 20

cat("=== Design Parameter Sweep ===\n")

t0 <- Sys.time()

n_p1_range <- seq(5, 40, by = 5)
n_p2_range <- seq(5, 40, by = 5)

design_sweep <- expand.grid(n_p1 = n_p1_range, n_p2 = n_p2_range)
design_sweep$divergence <- NA_real_

for (i in seq_len(nrow(design_sweep))) {
  n1 <- design_sweep$n_p1[i]
  n2 <- design_sweep$n_p2[i]

  dp12 <- c(make_trials("A", 1, n1, "P1"),
            make_trials(c("A", "B"), 1, n2, "P2"))
  dp3  <- make_trials("B", 1, 20, "P3")

  # RW
  V_rw_s <- rw_final(dp12, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
  V_rw_s["B"] <- 0
  vb_rw <- rw_final(dp3, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                    V_init = V_rw_s)[["B"]]

  # Mack
  state <- mack_final_state(dp12, S = c(A = 0.3, B = 0.3),
                            alpha_init = c(A = 0.5, B = 0.5))
  state$V["B"] <- 0
  vb_mack <- mack_final_state(dp3, S = c(A = 0.3, B = 0.3),
                              alpha_init = state$alpha,
                              V_init = state$V)$V[["B"]]

  design_sweep$divergence[i] <- abs(vb_rw - vb_mack)
}

t1 <- Sys.time()
cat(sprintf("  Completed %d design combinations in %.1f seconds.\n\n",
    nrow(design_sweep), as.numeric(t1 - t0, units = "secs")))

# --- Expected Output Checkpoint ----------------------------------------------
cat("=== Design Sweep: Key Findings ===\n")
cat(sprintf("  Max divergence:  %.4f  (at n_P1=%d, n_P2=%d)\n",
    max(design_sweep$divergence),
    design_sweep$n_p1[which.max(design_sweep$divergence)],
    design_sweep$n_p2[which.max(design_sweep$divergence)]))
cat("\n")


# === TRY THIS exercises =====================================================
cat("=== Try This Exercises ===\n")
cat("1. Add Phase 3 trial count as a third swept dimension.\n")
cat("2. Use ratio V_B(Mack)/V_B(RW) instead of absolute difference.\n\n")
