# ============================================================================
# Section 2 — Blocking and Overexpectation
# ============================================================================
# Runtime: < 1 second
# Figures produced: fig2_blocking, fig3_overexpectation
# ============================================================================

source("R/models.R")

set.seed(2026)


# === Simulation 1: Blocking ==================================================
# Design: Phase 1 — A+ (10 trials), Phase 2 — AB+ (10 trials)
# Question: After Phase 2, what is V_B?

blocking_design <- c(
  make_trials("A", 1, 10, "Phase 1"),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)

blocking_rw <- rw_simulate(
  design = blocking_design,
  alpha  = c(A = 0.4, B = 0.4),
  beta   = 0.3
)

# --- Expected Output Checkpoint ----------------------------------------------
cat("=== Blocking: Key Values ===\n")
b <- blocking_rw
cat(sprintf("  V_A after Phase 1, trial 5:   %.4f\n",
    b$V[b$cue == "A" & b$trial == 5]))
cat(sprintf("  V_A after Phase 1, trial 10:  %.4f\n",
    b$V[b$cue == "A" & b$trial == 10]))
cat(sprintf("  V_B at start of Phase 2:      %.4f\n",
    b$V[b$cue == "B" & b$trial == 11]))
cat(sprintf("  V_B after Phase 2, trial 20:  %.4f\n",
    b$V[b$cue == "B" & b$trial == 20]))
cat(sprintf("  V_A after Phase 2, trial 20:  %.4f\n",
    b$V[b$cue == "A" & b$trial == 20]))
# Expected: V_A ~ 0.72 at end of Phase 1; V_B ~ 0.13 at end of Phase 2

# --- Hand Trace (first 3 trials + first compound trial) ----------------------
cat("\n")
rw_hand_trace(blocking_design, alpha = c(A = 0.4, B = 0.4), beta = 0.3, n_trace = 12)


# === Observation Function Demo ===============================================
# Show how V maps to behavior under different observation functions

cat("\n=== Observation Function Demo ===\n")
V_A_final <- b$V[b$cue == "A" & b$trial == max(b$trial)]
V_B_final <- b$V[b$cue == "B" & b$trial == max(b$trial)]

cat(sprintf("  Raw V:       V_A = %.3f, V_B = %.3f\n", V_A_final, V_B_final))
cat(sprintf("  Sigmoid:     P(CR|A) = %.3f, P(CR|B) = %.3f\n",
    obs_sigmoid(V_A_final), obs_sigmoid(V_B_final)))
cat(sprintf("  Threshold:   P(CR|A) = %.0f, P(CR|B) = %.0f\n",
    obs_threshold(V_A_final, theta = 0.5), obs_threshold(V_B_final, theta = 0.5)))
cat(sprintf("  Luce choice: P(A) = %.3f, P(B) = %.3f\n",
    obs_luce(c(A = V_A_final, B = V_B_final))["A"],
    obs_luce(c(A = V_A_final, B = V_B_final))["B"]))


# === Simulation 2: Overexpectation ==========================================
# Design: Phase 1 — A+ and B+ alternating (10 each), Phase 2 — AB+ (10 trials)
# Question: What happens to V_A and V_B in Phase 2?

overexpectation_design <- c(
  unlist(lapply(1:10, function(i) {
    c(make_trials("A", 1, 1, "Phase 1"),
      make_trials("B", 1, 1, "Phase 1"))
  }), recursive = FALSE),
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)

overexpectation_rw <- rw_simulate(
  design = overexpectation_design,
  alpha  = c(A = 0.4, B = 0.4),
  beta   = 0.3
)

# --- Expected Output Checkpoint ----------------------------------------------
cat("\n=== Overexpectation: Key Values ===\n")
oe <- overexpectation_rw
phase1_end <- max(oe$trial[oe$phase == "Phase 1"])
cat(sprintf("  V_A at end of Phase 1: %.4f\n",
    oe$V[oe$cue == "A" & oe$trial == phase1_end]))
cat(sprintf("  V_B at end of Phase 1: %.4f\n",
    oe$V[oe$cue == "B" & oe$trial == phase1_end]))
cat(sprintf("  V_A at end of Phase 2: %.4f\n",
    oe$V[oe$cue == "A" & oe$trial == max(oe$trial)]))
cat(sprintf("  V_B at end of Phase 2: %.4f\n",
    oe$V[oe$cue == "B" & oe$trial == max(oe$trial)]))
# Expected: V_A and V_B ~ 0.72 after Phase 1 (10 trials each); both DECLINE in Phase 2


# === TRY THIS exercises =====================================================

cat("\n=== Try This Exercises ===\n")
cat("1. Change alpha to c(A=0.9, B=0.9) and rerun blocking.\n")
cat("   Does blocking still occur? Is it stronger or weaker?\n\n")
cat("2. What if beta=0? (No learning at all)\n")
cat("   What if beta=1? (Maximum learning rate)\n\n")
cat("3. Reverse the phase order: AB+ first (10 trials), then A+ (10 trials).\n")
cat("   What happens to V_B?\n\n")
cat("4. In overexpectation, train A for 20 trials but B for only 5.\n")
cat("   Do they decline equally in Phase 2?\n\n")

cat("Done. Run this script with: source('analysis/01_blocking.R')\n")
