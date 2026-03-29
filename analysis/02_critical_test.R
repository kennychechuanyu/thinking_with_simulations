# ============================================================================
# Section 3 — Model Comparison and Critical Test Design
# ============================================================================
# Runtime: < 1 second
# Data used by: figures.R (fig4_critical_test, fig5_alpha_dynamics)
# ============================================================================

source("R/models.R")

set.seed(2026)


# === Model Comparison on Blocking ============================================
# Run BOTH models on the same blocking design

blocking_design <- c(
  make_trials("A", 1, 10, "Phase 1"),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)

# Rescorla-Wagner
comparison_rw <- rw_simulate(
  design = blocking_design,
  alpha  = c(A = 0.4, B = 0.4),
  beta   = 0.3
)

# Mackintosh
comparison_mack <- mack_simulate(
  design     = blocking_design,
  S          = c(A = 0.3, B = 0.3),
  alpha_init = c(A = 0.5, B = 0.5)
)

# --- Expected Output Checkpoint ----------------------------------------------
cat("=== Model Comparison on Blocking ===\n")
rw_vb  <- comparison_rw$V[comparison_rw$cue == "B" & comparison_rw$trial == max(comparison_rw$trial)]
mc_vb  <- comparison_mack$V[comparison_mack$cue == "B" & comparison_mack$trial == max(comparison_mack$trial)]
mc_alb <- comparison_mack$alpha[comparison_mack$cue == "B" & comparison_mack$trial == max(comparison_mack$trial)]
cat(sprintf("  RW  — V_B at end of Phase 2:   %.4f\n", rw_vb))
cat(sprintf("  Mack — V_B at end of Phase 2:  %.4f\n", mc_vb))
cat(sprintf("  Mack — α_B at end of Phase 2:  %.4f\n", mc_alb))
cat("  Note: Both produce 'blocking' but through different mechanisms!\n")
cat("  RW keeps V_B low (error consumed). Mack lets V_B rise but suppresses α_B.\n\n")


# === Critical Test: Three-Phase Design =======================================
# Phase 1: A+ (10 trials)
# Phase 2: AB+ (10 trials)
# Phase 3: B → new outcome (10 trials, V_B reset to 0, α_B preserved)
#
# Key question: How fast does B learn the new outcome?

design_p12 <- c(
  make_trials("A", 1, 10, "Phase 1"),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)
design_p3 <- make_trials("B", 1, 10, "Phase 3")


# --- RW: Phase 1+2 → reset V_B → Phase 3 ---
rw_p12 <- rw_simulate(design_p12, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw <- extract_final_V(rw_p12)
V_rw["B"] <- 0                          # new outcome: V_B resets
rw_p3 <- rw_simulate(design_p3, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                     V_init = V_rw, cue_names = c("A", "B"))
critical_rw <- combine_phases(rw_p12, rw_p3)


# --- Mack: Phase 1+2 → reset V_B (α_B preserved) → Phase 3 ---
mack_p12 <- mack_simulate(design_p12, S = c(A = 0.3, B = 0.3),
                          alpha_init = c(A = 0.5, B = 0.5))
V_mack <- extract_final_V(mack_p12)
alpha_mack <- extract_final_alpha(mack_p12)
V_mack["B"] <- 0                        # new outcome: V_B resets
mack_p3 <- mack_simulate(design_p3, S = c(A = 0.3, B = 0.3),
                         alpha_init = alpha_mack,
                         V_init = V_mack, cue_names = c("A", "B"))
critical_mack <- combine_phases(mack_p12, mack_p3)


# --- Expected Output Checkpoint: Phase 3 boundary state ---
cat("=== Critical Test: State at Phase 2/3 Boundary ===\n")
cat(sprintf("  RW:   V_B → 0 (reset),  α_B = %.2f (intact — α is fixed in RW)\n",
    0.4))
cat(sprintf("  Mack: V_B → 0 (reset),  α_B = %.4f (SUPPRESSED during Phase 2)\n",
    alpha_mack["B"]))

# Phase 3 endpoints
rw_vb_p3   <- critical_rw$V[critical_rw$cue == "B" & critical_rw$trial == max(critical_rw$trial)]
mack_vb_p3 <- critical_mack$V[critical_mack$cue == "B" & critical_mack$trial == max(critical_mack$trial)]
cat(sprintf("\n  After Phase 3 (10 trials of B → new outcome):\n"))
cat(sprintf("    RW   V_B = %.4f  (fast learning — α_B intact)\n", rw_vb_p3))
cat(sprintf("    Mack V_B = %.4f  (slow learning — α_B suppressed)\n", mack_vb_p3))
cat(sprintf("    Divergence = %.4f\n\n", abs(rw_vb_p3 - mack_vb_p3)))


# --- Alpha dynamics (Mackintosh only) ----------------------------------------
cat("=== Mackintosh α_B Trajectory ===\n")
ab <- critical_mack[critical_mack$cue == "B", ]
checkpoints <- c(1, 5, 10, 11, 15, 20, 21, 25, 30)
for (t in checkpoints) {
  row <- ab[ab$trial == t, ]
  if (nrow(row) > 0)
    cat(sprintf("  Trial %2d [%s]: α_B = %.4f, V_B = %.4f\n",
        t, row$phase, row$alpha, row$V))
}


# === TRY THIS exercises =====================================================
cat("\n=== Try This Exercises ===\n")
cat("1. Change Phase 3 to 50 trials instead of 10.\n")
cat("   Do the models eventually converge?\n\n")
cat("2. In the Mackintosh model, set beta_up = beta_down = 0.10.\n")
cat("   How does symmetric attention change the Phase 3 prediction?\n\n")
cat("3. What happens if you skip Phase 1 entirely (just AB+ then B→new)?\n")
cat("   Is blocking necessary for the Phase 3 divergence?\n\n")

cat("Done. Run this script with: source('analysis/02_critical_test.R')\n")
