# ============================================================================
# Section 2 — Blocking and Overexpectation
# ============================================================================
# Runtime: < 1 second
# Data used by: figures.R (fig2_blocking, fig3_overexpectation)
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
# NOTE: rw_simulate records pre-trial values. trial==N is the state BEFORE
# the Nth update. The terminal row (trial==N+1) holds the post-final-update state.
# Use extract_final_V() for end-of-phase values.
cat("=== Blocking: Key Values ===\n")
b <- blocking_rw
V_final <- extract_final_V(blocking_rw)
cat(sprintf("  V_A end of Phase 1 (10 trials): %.4f\n",
    rw_final(blocking_design[1:10], alpha=c(A=0.4, B=0.4), beta=0.3)[["A"]]))
cat(sprintf("  V_A end of Phase 2 (20 trials): %.4f\n", V_final[["A"]]))
cat(sprintf("  V_B end of Phase 2 (20 trials): %.4f\n", V_final[["B"]]))
# Expected: V_A ~ 0.72 after Phase 1; V_A ~ 0.85, V_B ~ 0.13 after Phase 2

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
cat(sprintf("  Luce choice: P(resp|A) = %.2f, P(resp|B) = %.2f\n",
    obs_luce_respond(V_A_final), obs_luce_respond(V_B_final)))


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
phase1_final <- rw_final(overexpectation_design[1:20],
                         alpha = c(A = 0.4, B = 0.4), beta = 0.3)
phase2_final <- extract_final_V(overexpectation_rw)
cat(sprintf("  V_A at end of Phase 1: %.4f\n",
    phase1_final[["A"]]))
cat(sprintf("  V_B at end of Phase 1: %.4f\n",
    phase1_final[["B"]]))
cat(sprintf("  V_A at end of Phase 2: %.4f\n",
    phase2_final[["A"]]))
cat(sprintf("  V_B at end of Phase 2: %.4f\n",
    phase2_final[["B"]]))
# Expected: V_A and V_B ~ 0.72 after Phase 1 (10 trials each); both DECLINE in Phase 2


# === TRY THIS exercises =====================================================

cat("\n=== Try This Exercises ===\n")
cat("1. Change alpha to c(A=0.9, B=0.9) and rerun blocking.\n")
cat("2. What if beta=0? What if beta=1?\n")
cat("3. Reverse phase order: AB+ first (10 trials), then A+ (10 trials).\n")
cat("4. In overexpectation, train A for 20 trials but B for only 5.\n\n")
