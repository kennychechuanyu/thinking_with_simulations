# ============================================================================
# Thinking With Simulations — Core Model Functions
# ============================================================================
# This file contains reusable model implementations and helpers.
# Source this file before running any analysis script.
#
# Models implemented:
#   - Rescorla-Wagner (1972)
#   - Mackintosh (1975) — pedagogical implementation
#
# Requirements: base R only (no packages needed)
# ============================================================================


# --- Rescorla-Wagner Model ---------------------------------------------------
#
# Update rule:  ΔV_i = α_i · β · (λ − ΣV_present)
#
# Key commitments:
#   - Prediction error is GLOBAL: shared across all present cues
#   - Absent cues are NOT updated
#   - α is FIXED (does not change with experience)
#
# Arguments:
#   design     — list of trials, each with $cues, $lambda, $phase
#   alpha      — named numeric vector of per-cue salience (fixed)
#   beta       — learning rate tied to the outcome (scalar)
#   V_init     — optional named vector of initial associative strengths
#   cue_names  — optional character vector of all cue names
#
# Returns: data.frame with columns: trial, phase, cue, V

rw_simulate <- function(design, alpha, beta, V_init = NULL, cue_names = NULL) {
  if (is.null(cue_names))
    cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  n_cues   <- length(cue_names)
  n_trials <- length(design)

  if (!all(cue_names %in% names(alpha)))
    stop("alpha must contain named values for all cues: ", paste(cue_names, collapse=", "))

  if (is.null(V_init)) {
    V <- setNames(rep(0, n_cues), cue_names)
  } else {
    V <- V_init
  }

  # Pre-allocate results: one row per cue per trial, plus final state
  total_rows <- n_cues * (n_trials + 1)
  res_trial <- integer(total_rows)
  res_phase <- character(total_rows)
  res_cue   <- character(total_rows)
  res_V     <- numeric(total_rows)
  row <- 1L

  for (t in seq_along(design)) {
    cues_present <- design[[t]]$cues
    lambda       <- design[[t]]$lambda
    phase        <- design[[t]]$phase

    # Record pre-trial values for ALL cues
    for (cue in cue_names) {
      res_trial[row] <- t
      res_phase[row] <- phase
      res_cue[row]   <- cue
      res_V[row]     <- V[cue]
      row <- row + 1L
    }

    # Compute global prediction error (shared across present cues)
    sum_V <- sum(V[cues_present])
    delta <- lambda - sum_V

    # Update ONLY present cues
    V[cues_present] <- V[cues_present] + alpha[cues_present] * beta * delta
  }

  # Record final state after last trial
  for (cue in cue_names) {
    res_trial[row] <- n_trials + 1L
    res_phase[row] <- design[[n_trials]]$phase
    res_cue[row]   <- cue
    res_V[row]     <- V[cue]
    row <- row + 1L
  }

  data.frame(
    trial = res_trial, phase = res_phase,
    cue = res_cue, V = res_V,
    stringsAsFactors = FALSE
  )
}


# --- Mackintosh Model --------------------------------------------------------
#
# Update rule:  ΔV_i = S_i · α_i · (λ − V_i)
#
# Key commitments:
#   - Prediction error is LOCAL: each cue has its own error (λ − V_i)
#   - α CHANGES with experience based on relative predictive success
#   - Good predictors gain attention; poor predictors lose it
#
# NOTE: This is a pedagogical implementation chosen to sharpen the contrast
# with Rescorla-Wagner. It captures the core mechanism of Mackintosh (1975)
# but simplifies certain details for tutorial clarity. See the paper's
# discussion of pedagogical simplifications.
#
# Alpha update rule:
#   If |λ − V_i| < |λ − ΣV_{¬i}|  →  α_i increases (cue is a good predictor)
#   Otherwise                       →  α_i decreases (cue is a poor predictor)
#
# Arguments:
#   design      — list of trials (same format as rw_simulate)
#   S           — named numeric vector of salience parameters
#   alpha_init  — named numeric vector of initial attention values
#   beta_up     — rate of attention increase (default 0.03)
#   beta_down   — rate of attention decrease (default 0.15)
#   V_init      — optional initial associative strengths
#   cue_names   — optional character vector of cue names
#   alpha_min   — floor for α (default 0.01)
#   alpha_max   — ceiling for α (default 1.0)
#
# Returns: data.frame with columns: trial, phase, cue, V, alpha

mack_simulate <- function(design, S, alpha_init,
                          beta_up = 0.03, beta_down = 0.15,
                          V_init = NULL, cue_names = NULL,
                          alpha_min = 0.01, alpha_max = 1.0) {
  if (is.null(cue_names))
    cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  n_cues   <- length(cue_names)
  n_trials <- length(design)

  if (!all(cue_names %in% names(S)) || !all(cue_names %in% names(alpha_init)))
    stop("S and alpha_init must contain named values for all cues: ", paste(cue_names, collapse=", "))

  if (is.null(V_init)) {
    V <- setNames(rep(0, n_cues), cue_names)
  } else {
    V <- V_init
  }
  alpha <- alpha_init

  # Pre-allocate results
  total_rows <- n_cues * (n_trials + 1)
  res_trial <- integer(total_rows)
  res_phase <- character(total_rows)
  res_cue   <- character(total_rows)
  res_V     <- numeric(total_rows)
  res_alpha <- numeric(total_rows)
  row <- 1L

  for (t in seq_along(design)) {
    cues_present <- design[[t]]$cues
    lambda       <- design[[t]]$lambda
    phase        <- design[[t]]$phase

    # Record pre-trial values
    for (cue in cue_names) {
      res_trial[row] <- t
      res_phase[row] <- phase
      res_cue[row]   <- cue
      res_V[row]     <- V[cue]
      res_alpha[row] <- alpha[cue]
      row <- row + 1L
    }

    V_old <- V  # snapshot for alpha comparison

    # Update V for present cues (LOCAL error: λ − V_i)
    for (cue in cues_present) {
      V[cue] <- V[cue] + S[cue] * alpha[cue] * (lambda - V[cue])
    }

    # Update α: reward good predictors, penalize poor ones
    for (cue in cues_present) {
      others  <- setdiff(cues_present, cue)
      error_i <- abs(lambda - V_old[cue])
      error_others <- if (length(others) > 0) {
        abs(lambda - sum(V_old[others]))
      } else {
        abs(lambda)
      }

      if (error_i < error_others) {
        alpha[cue] <- alpha[cue] + beta_up * (1 - alpha[cue])
      } else {
        alpha[cue] <- alpha[cue] - beta_down * alpha[cue]
      }
      alpha[cue] <- max(alpha_min, min(alpha_max, alpha[cue]))
    }
  }

  # Record final state
  for (cue in cue_names) {
    res_trial[row] <- n_trials + 1L
    res_phase[row] <- design[[n_trials]]$phase
    res_cue[row]   <- cue
    res_V[row]     <- V[cue]
    res_alpha[row] <- alpha[cue]
    row <- row + 1L
  }

  data.frame(
    trial = res_trial, phase = res_phase,
    cue = res_cue, V = res_V, alpha = res_alpha,
    stringsAsFactors = FALSE
  )
}


# --- Lightweight Final-State Functions ----------------------------------------
# These return only the terminal state (no trial-by-trial history).
# Use for parameter sweeps and power analyses where speed matters.

rw_final <- function(design, alpha, beta, V_init = NULL) {
  cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  if (is.null(V_init)) {
    V <- setNames(rep(0, length(cue_names)), cue_names)
  } else {
    V <- V_init
  }
  for (t in seq_along(design)) {
    cues  <- design[[t]]$cues
    delta <- design[[t]]$lambda - sum(V[cues])
    V[cues] <- V[cues] + alpha[cues] * beta * delta
  }
  V
}

mack_final_state <- function(design, S, alpha_init,
                             beta_up = 0.03, beta_down = 0.15,
                             V_init = NULL,
                             alpha_min = 0.01, alpha_max = 1.0) {
  cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  if (is.null(V_init)) {
    V <- setNames(rep(0, length(cue_names)), cue_names)
  } else {
    V <- V_init
  }
  alpha <- alpha_init
  for (t in seq_along(design)) {
    cues   <- design[[t]]$cues
    lambda <- design[[t]]$lambda
    V_old  <- V
    for (cue in cues) {
      V[cue] <- V[cue] + S[cue] * alpha[cue] * (lambda - V[cue])
    }
    for (cue in cues) {
      others <- setdiff(cues, cue)
      err_i  <- abs(lambda - V_old[cue])
      err_o  <- if (length(others) > 0) abs(lambda - sum(V_old[others])) else abs(lambda)
      if (err_i < err_o) {
        alpha[cue] <- alpha[cue] + beta_up * (1 - alpha[cue])
      } else {
        alpha[cue] <- alpha[cue] - beta_down * alpha[cue]
      }
      alpha[cue] <- max(alpha_min, min(alpha_max, alpha[cue]))
    }
  }
  list(V = V, alpha = alpha)
}


# --- Design Helpers -----------------------------------------------------------

# Create a block of identical trials
#   cues        — character vector of cue names present on each trial
#   lambda      — outcome value (1 = reinforced, 0 = unreinforced)
#   n           — number of trials in this block
#   phase_label — label for the phase (e.g., "Phase 1")
make_trials <- function(cues, lambda, n, phase_label) {
  lapply(seq_len(n), function(i) {
    list(cues = cues, lambda = lambda, phase = phase_label)
  })
}

# Extract terminal V values from a simulation result
extract_final_V <- function(results) {
  final <- results[results$trial == max(results$trial), ]
  setNames(final$V, final$cue)
}

# Extract terminal alpha values from a Mackintosh simulation result
extract_final_alpha <- function(results) {
  final <- results[results$trial == max(results$trial), ]
  setNames(final$alpha, final$cue)
}

# Combine two phase results with renumbered trials
combine_phases <- function(results_p12, results_p3) {
  offset <- max(results_p12$trial)
  results_p3$trial <- results_p3$trial + offset
  rbind(results_p12, results_p3)
}


# --- Observation Functions ----------------------------------------------------
# Map internal associative strength V to observable behavior.
# These are theoretical commitments (see paper Section 2.3).

# Sigmoid (logistic) response function
#   V     — associative strength (scalar or vector)
#   gamma — steepness of the sigmoid (higher = sharper)
#   theta — threshold (V value at 50% response probability)
obs_sigmoid <- function(V, gamma = 5, theta = 0.5) {
  1 / (1 + exp(-gamma * (V - theta)))
}

# Threshold (step) function
#   V     — associative strength
#   theta — threshold value
obs_threshold <- function(V, theta = 0.5) {
  as.numeric(V > theta)
}

# Luce choice rule (for two or more options)
#   V   — named vector of associative strengths for each option
#   phi — sensitivity parameter (higher = more deterministic)
obs_luce <- function(V, phi = 3) {
  # Softmax across provided alternatives (A vs B choice)
  exp_vals <- exp(phi * V)
  exp_vals / sum(exp_vals)
}

obs_luce_respond <- function(V, phi = 3) {
  # P(respond | cue) vs no-response baseline (V=0)
  # This matches the paper's Table definition
  exp(phi * V) / (exp(phi * V) + 1)
}


# --- Verification Utility -----------------------------------------------------
# Hand-trace a few trials of the RW model for tutorial verification

rw_hand_trace <- function(design, alpha, beta, n_trace = 5) {
  cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  V <- setNames(rep(0, length(cue_names)), cue_names)

  cat("=== Rescorla-Wagner Hand Trace ===\n\n")
  for (t in seq_len(min(n_trace, length(design)))) {
    cues   <- design[[t]]$cues
    lambda <- design[[t]]$lambda
    phase  <- design[[t]]$phase
    sum_V  <- sum(V[cues])
    delta  <- lambda - sum_V

    cat(sprintf("Trial %d [%s]: cues = {%s}, lambda = %.1f\n",
                t, phase, paste(cues, collapse = ", "), lambda))
    cat(sprintf("  V before:  %s\n",
                paste(sprintf("%s=%.4f", cue_names, V[cue_names]), collapse = ", ")))
    cat(sprintf("  ΣV_present = %.4f\n", sum_V))
    cat(sprintf("  δ = λ - ΣV = %.4f - %.4f = %.4f\n", lambda, sum_V, delta))

    for (cue in cues) {
      dv <- alpha[cue] * beta * delta
      cat(sprintf("  ΔV_%s = α_%s · β · δ = %.2f × %.2f × %.4f = %.4f\n",
                  cue, cue, alpha[cue], beta, delta, dv))
    }

    V[cues] <- V[cues] + alpha[cues] * beta * delta

    cat(sprintf("  V after:   %s\n\n",
                paste(sprintf("%s=%.4f", cue_names, V[cue_names]), collapse = ", ")))
  }
  invisible(V)
}
