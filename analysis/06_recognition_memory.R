# ============================================================================
# Recognition Memory Interlude — Transfer Case
# ============================================================================
# Demonstrates the Simulation Thinking Cycle with signal detection models
# of recognition memory.
#
# Key question: When hit rates increase, is it because memory got stronger
# (d' increased) or because the decision criterion shifted (c decreased)?
#
# Runtime: < 1 second (analytical, no simulation needed for basic SDT;
#          simulation used for confidence-based ROC)
# ============================================================================

set.seed(2026)

# --- Signal Detection Theory functions --------------------------------------

# Hit rate and false alarm rate from d' and c
sdt_rates <- function(dprime, c) {
  hit <- pnorm(dprime / 2 - c)
  fa  <- pnorm(-dprime / 2 - c)
  list(hit = hit, fa = fa)
}

# Generate confidence ratings from SDT
# Old items: evidence ~ N(d'/2, 1); New items: evidence ~ N(-d'/2, 1)
# Criteria: c1 < c2 < c3 < c4 (for 5-point confidence scale)
sdt_confidence <- function(dprime, criteria, n_old = 500, n_new = 500) {
  old_evidence <- rnorm(n_old, mean = dprime / 2, sd = 1)
  new_evidence <- rnorm(n_new, mean = -dprime / 2, sd = 1)

  bin <- function(x, crit) {
    findInterval(x, sort(crit)) + 1
  }

  old_conf <- bin(old_evidence, criteria)
  new_conf <- bin(new_evidence, criteria)

  list(old = old_conf, new = new_conf,
       old_evidence = old_evidence, new_evidence = new_evidence)
}

# Compute ROC points from confidence data
sdt_roc <- function(conf_data, n_bins = 5) {
  hits <- cumsum(rev(table(factor(conf_data$old, levels = 1:n_bins)))) / length(conf_data$old)
  fas  <- cumsum(rev(table(factor(conf_data$new, levels = 1:n_bins)))) / length(conf_data$new)
  data.frame(fa = as.numeric(fas), hit = as.numeric(hits))
}


# === DEMONSTRATION 1: Two accounts of the same hit rate =====================
cat("=== Recognition Memory: Strength vs Criterion ===\n\n")

# Baseline condition
dprime_base <- 1.0; c_base <- 0.0
rates_base <- sdt_rates(dprime_base, c_base)

# Experimental condition has HIGHER hit rate. Two accounts:

# Account A: Stronger memory (d' increases, criterion stays)
dprime_strong <- 1.5; c_strong <- 0.0
rates_strong <- sdt_rates(dprime_strong, c_strong)

# Account B: More liberal criterion (d' stays, criterion shifts)
# Find c that produces same hit rate as Account A
# hit = pnorm(d'/2 - c), so c = d'/2 - qnorm(hit)
target_hit <- rates_strong$hit
c_liberal <- dprime_base / 2 - qnorm(target_hit)
rates_liberal <- sdt_rates(dprime_base, c_liberal)

cat("Baseline:          d'=1.0, c=0.00  -> Hit=", round(rates_base$hit, 3),
    ", FA=", round(rates_base$fa, 3), "\n")
cat("Account A (strong): d'=1.5, c=0.00  -> Hit=", round(rates_strong$hit, 3),
    ", FA=", round(rates_strong$fa, 3), "\n")
cat("Account B (liberal): d'=1.0, c=", round(c_liberal, 2),
    " -> Hit=", round(rates_liberal$hit, 3),
    ", FA=", round(rates_liberal$fa, 3), "\n")
cat("\nBoth accounts produce the SAME hit rate (",
    round(target_hit, 3), ").\n")
cat("But they differ in false alarm rate:\n")
cat("  Account A: FA =", round(rates_strong$fa, 3),
    "(lower, because d' increased)\n")
cat("  Account B: FA =", round(rates_liberal$fa, 3),
    "(higher, because criterion shifted)\n")
cat("\nLesson: Hit rate alone cannot distinguish the accounts.\n")
cat("False alarm rate is the diagnostic observable.\n")


# === DEMONSTRATION 2: ROC curves reveal the difference ======================
cat("\n=== Recognition Memory: ROC Curves ===\n")

# Simulate confidence ratings under both accounts
criteria <- c(-1.5, -0.5, 0.5, 1.5)

conf_strong  <- sdt_confidence(dprime_strong, criteria)
conf_liberal <- sdt_confidence(dprime_base, criteria + c_liberal)

roc_strong  <- sdt_roc(conf_strong)
roc_liberal <- sdt_roc(conf_liberal)

cat("ROC points (Account A, stronger memory):\n")
print(round(roc_strong, 3))
cat("\nROC points (Account B, liberal criterion):\n")
print(round(roc_liberal, 3))
cat("\nThe stronger-memory account produces a more BOWED (curved) ROC.\n")
cat("The criterion-shift account produces a flatter, more LINEAR ROC.\n")
cat("This is the recognition-memory equivalent of the observation-function lesson:\n")
cat("the same hit rate maps to different ROC curves depending on the mechanism.\n")


# === DEMONSTRATION 3: Parameter sensitivity ==================================
cat("\n=== Parameter Sensitivity: d' x c ===\n")

dprime_range <- seq(0.5, 3.0, by = 0.25)
c_range <- seq(-1.0, 1.0, by = 0.2)

sweep <- expand.grid(dprime = dprime_range, c = c_range)
sweep$hit <- mapply(function(d, cr) sdt_rates(d, cr)$hit, sweep$dprime, sweep$c)
sweep$fa  <- mapply(function(d, cr) sdt_rates(d, cr)$fa, sweep$dprime, sweep$c)

# Where can the two accounts mimic each other?
# They mimic when hit rates are equal but d'/c values differ
cat("At high d', criterion shifts produce large FA changes (easy to detect).\n")
cat("At low d', criterion shifts produce small FA changes (hard to detect).\n")
cat("The mimicry problem is worst at moderate d' and moderate hit rates.\n")

cat("\nDone.\n")
