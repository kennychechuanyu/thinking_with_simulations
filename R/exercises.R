# ============================================================================
# Thinking With Simulations — Tutorial Exercises
# ============================================================================
# Work through these exercises after reading each section of the paper.
# Each exercise asks you to CHANGE something and RE-RUN.
#
# Requirements: source("R/models.R") must be run first
# ============================================================================

source("R/models.R")

set.seed(2026)


# ============================================================================
# SECTION 2 EXERCISES: From Words to Code
# ============================================================================

cat("=" , rep("=", 60), "\n")
cat("SECTION 2 EXERCISES\n")
cat(rep("=", 62), "\n\n")

# --- Exercise 2.1: High Salience Blocking ------------------------------------
# QUESTION: Does blocking still occur when alpha is very high (0.9)?
#           Is it stronger or weaker than with alpha = 0.4?

cat("--- Exercise 2.1: High Salience Blocking ---\n")
cat("Question: Change alpha from 0.4 to 0.9. Does blocking survive?\n\n")

blocking_high <- rw_simulate(
  design = c(make_trials("A", 1, 10, "Phase 1"),
             make_trials(c("A", "B"), 1, 10, "Phase 2")),
  alpha  = c(A = 0.9, B = 0.9),
  beta   = 0.3
)

V_B_high <- blocking_high$V[blocking_high$cue == "B" &
                             blocking_high$trial == max(blocking_high$trial)]
cat(sprintf("  V_B with alpha=0.9: %.4f\n", V_B_high))

# Compare with original
blocking_orig <- rw_simulate(
  design = c(make_trials("A", 1, 10, "Phase 1"),
             make_trials(c("A", "B"), 1, 10, "Phase 2")),
  alpha  = c(A = 0.4, B = 0.4),
  beta   = 0.3
)
V_B_orig <- blocking_orig$V[blocking_orig$cue == "B" &
                             blocking_orig$trial == max(blocking_orig$trial)]
cat(sprintf("  V_B with alpha=0.4: %.4f\n", V_B_orig))
cat(sprintf("  Lesson: Blocking %s with higher alpha because A learns\n",
    ifelse(V_B_high < V_B_orig, "is STRONGER", "is WEAKER")))
cat("  faster in Phase 1, leaving even less error for B.\n\n")


# --- Exercise 2.2: Extreme Learning Rates ------------------------------------
# QUESTION: What happens at the extremes of beta?

cat("--- Exercise 2.2: Extreme Learning Rates ---\n")
cat("Question: What if beta = 0? What if beta = 1?\n\n")

for (b in c(0, 0.1, 0.5, 1.0)) {
  res <- rw_simulate(
    design = c(make_trials("A", 1, 10, "Phase 1"),
               make_trials(c("A", "B"), 1, 10, "Phase 2")),
    alpha = c(A = 0.4, B = 0.4), beta = b
  )
  va <- res$V[res$cue == "A" & res$trial == max(res$trial)]
  vb <- res$V[res$cue == "B" & res$trial == max(res$trial)]
  cat(sprintf("  beta = %.1f: V_A = %.4f, V_B = %.4f\n", b, va, vb))
}
cat("  Lesson: beta=0 means no learning at all. beta=1 means V jumps\n")
cat("  to lambda in one trial, leaving no error for B.\n\n")


# --- Exercise 2.3: Reversed Phase Order --------------------------------------
# QUESTION: What if we do AB+ first, THEN A+?

cat("--- Exercise 2.3: Reversed Phase Order ---\n")
cat("Question: AB+ first (10 trials), then A+ (10 trials). What happens?\n\n")

reversed <- rw_simulate(
  design = c(make_trials(c("A", "B"), 1, 10, "Phase 1: AB+"),
             make_trials("A", 1, 10, "Phase 2: A+")),
  alpha = c(A = 0.4, B = 0.4), beta = 0.3
)

va <- reversed$V[reversed$cue == "A" & reversed$trial == max(reversed$trial)]
vb <- reversed$V[reversed$cue == "B" & reversed$trial == max(reversed$trial)]
cat(sprintf("  V_A = %.4f, V_B = %.4f\n", va, vb))
cat("  Lesson: When A and B learn together first, both gain strength.\n")
cat("  Phase order MATTERS — this is a design commitment.\n\n")


# --- Exercise 2.4: Unequal Training in Overexpectation -----------------------
# QUESTION: If A gets 20 trials and B gets 5, do they decline equally?

cat("--- Exercise 2.4: Unequal Training in Overexpectation ---\n\n")

unequal_design <- c(
  make_trials("A", 1, 20, "Phase 1"),     # A gets 20 trials
  make_trials("B", 1, 5, "Phase 1"),      # B gets only 5
  make_trials(c("A", "B"), 1, 10, "Phase 2")
)

unequal <- rw_simulate(unequal_design, alpha = c(A = 0.4, B = 0.4), beta = 0.3)

phase1_end <- 25  # 20 + 5
va_p1 <- unequal$V[unequal$cue == "A" & unequal$trial == phase1_end]
vb_p1 <- unequal$V[unequal$cue == "B" & unequal$trial == phase1_end]
va_end <- unequal$V[unequal$cue == "A" & unequal$trial == max(unequal$trial)]
vb_end <- unequal$V[unequal$cue == "B" & unequal$trial == max(unequal$trial)]

cat(sprintf("  After Phase 1:  V_A = %.4f (20 trials), V_B = %.4f (5 trials)\n",
    va_p1, vb_p1))
cat(sprintf("  After Phase 2:  V_A = %.4f, V_B = %.4f\n", va_end, vb_end))
cat("  Lesson: Both decline, but NOT equally. The model doesn't care about\n")
cat("  'fairness' — it cares about the summed prediction vs. lambda.\n\n")


# --- Exercise 2.5: Observation Function Impact --------------------------------
cat("--- Exercise 2.5: Observation Function Impact ---\n")
cat("Question: Apply different observation functions to blocking results.\n\n")

res <- rw_simulate(
  design = c(make_trials("A", 1, 10, "Phase 1"),
             make_trials(c("A", "B"), 1, 10, "Phase 2")),
  alpha = c(A = 0.4, B = 0.4), beta = 0.3
)
va <- res$V[res$cue == "A" & res$trial == max(res$trial)]
vb <- res$V[res$cue == "B" & res$trial == max(res$trial)]

cat(sprintf("  Raw V:          V_A = %.3f, V_B = %.3f\n", va, vb))
cat(sprintf("  Sigmoid(5,0.5): P(A) = %.3f, P(B) = %.3f  → B barely responds\n",
    obs_sigmoid(va, 5, 0.5), obs_sigmoid(vb, 5, 0.5)))
cat(sprintf("  Sigmoid(2,0.3): P(A) = %.3f, P(B) = %.3f  → softer mapping\n",
    obs_sigmoid(va, 2, 0.3), obs_sigmoid(vb, 2, 0.3)))
cat(sprintf("  Threshold(0.5): P(A) = %d, P(B) = %d  → binary\n",
    obs_threshold(va, 0.5), obs_threshold(vb, 0.5)))
cat(sprintf("  Threshold(0.1): P(A) = %d, P(B) = %d  → even B responds!\n",
    obs_threshold(va, 0.1), obs_threshold(vb, 0.1)))
cat("  Lesson: The SAME internal V can produce DIFFERENT behavioral predictions\n")
cat("  depending on the observation function. This is a theoretical commitment.\n\n")


# ============================================================================
# SECTION 3 EXERCISES: Theory Comparison
# ============================================================================

cat(rep("=", 62), "\n")
cat("SECTION 3 EXERCISES\n")
cat(rep("=", 62), "\n\n")

# --- Exercise 3.1: Symmetric Attention in Mackintosh -------------------------
cat("--- Exercise 3.1: Symmetric Attention ---\n")
cat("Question: What if beta_up = beta_down in Mackintosh?\n\n")

design_p12 <- c(make_trials("A", 1, 10, "Phase 1"),
                make_trials(c("A", "B"), 1, 10, "Phase 2"))
design_p3  <- make_trials("B", 1, 10, "Phase 3")

for (beta_pair in list(c(0.03, 0.15), c(0.10, 0.10), c(0.15, 0.03))) {
  bu <- beta_pair[1]; bd <- beta_pair[2]
  mp12 <- mack_simulate(design_p12, S = c(A = 0.3, B = 0.3),
                         alpha_init = c(A = 0.5, B = 0.5),
                         beta_up = bu, beta_down = bd)
  ab_end <- extract_final_alpha(mp12)["B"]
  cat(sprintf("  beta_up=%.2f, beta_down=%.2f → α_B at Phase 2 end: %.4f\n",
      bu, bd, ab_end))
}
cat("  Lesson: The asymmetry (easier to lose than gain attention) is\n")
cat("  itself a theoretical commitment that affects predictions.\n\n")


# --- Exercise 3.2: Extended Phase 3 ------------------------------------------
cat("--- Exercise 3.2: Extended Phase 3 (50 trials) ---\n")
cat("Question: Do the models converge with enough Phase 3 trials?\n\n")

dp3_long <- make_trials("B", 1, 50, "Phase 3")

# RW
rw_p12 <- rw_simulate(design_p12, alpha = c(A = 0.4, B = 0.4), beta = 0.3)
V_rw <- extract_final_V(rw_p12); V_rw["B"] <- 0
rw_p3 <- rw_simulate(dp3_long, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                     V_init = V_rw, cue_names = c("A", "B"))

# Mack
mack_p12 <- mack_simulate(design_p12, S = c(A = 0.3, B = 0.3),
                           alpha_init = c(A = 0.5, B = 0.5))
V_m <- extract_final_V(mack_p12); alpha_m <- extract_final_alpha(mack_p12)
V_m["B"] <- 0
mack_p3 <- mack_simulate(dp3_long, S = c(A = 0.3, B = 0.3),
                          alpha_init = alpha_m, V_init = V_m,
                          cue_names = c("A", "B"))

for (t in c(10, 20, 30, 50)) {
  trial_num <- max(rw_p12$trial) + t
  vb_r <- rw_p3$V[rw_p3$cue == "B" & rw_p3$trial == (t + 1)]
  vb_m <- mack_p3$V[mack_p3$cue == "B" & mack_p3$trial == (t + 1)]
  cat(sprintf("  After %d Phase 3 trials: RW V_B=%.3f, Mack V_B=%.3f, gap=%.3f\n",
      t, vb_r, vb_m, abs(vb_r - vb_m)))
}
cat("  Lesson: Models converge eventually, but the RATE of convergence\n")
cat("  is the diagnostic signal. The experiment must be long enough to\n")
cat("  detect the rate difference but short enough that both haven't\n")
cat("  already reached ceiling.\n\n")


# ============================================================================
# SECTION 4 & 5 EXERCISES
# ============================================================================

cat(rep("=", 62), "\n")
cat("SECTION 4 & 5 EXERCISES\n")
cat(rep("=", 62), "\n\n")

cat("--- Exercise 4.1: Different Divergence Metric ---\n")
cat("Instead of |V_B(RW) - V_B(Mack)|, try the RATIO V_B(Mack)/V_B(RW).\n")
cat("Rerun the parameter sweep with this metric. Does the landscape change?\n")
cat("[Modify analysis/03_parameter_sweep.R, line computing divergence]\n\n")

cat("--- Exercise 5.1: Higher Noise ---\n")
cat("In analysis/04_virtual_experiment.R, change noise_sd from 1.0 to 3.0.\n")
cat("How many more participants do you need for 80% power?\n\n")

cat("--- Exercise 5.2: Fewer Phase 3 Trials ---\n")
cat("Change Phase 3 from 10 to 5 trials in the power analysis.\n")
cat("Does the effect shrink? Is the experiment still feasible?\n\n")

cat("\nAll exercises complete.\n")
cat("Run with: source('R/exercises.R')\n")
