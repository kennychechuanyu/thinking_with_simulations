# Learning models of Sections 2-4: Rescorla-Wagner (Equation 1) and the
# adaptive model (Equations 2 and 3), and the response functions of Section 5.
# Base R only. The analyses, tests/verify_models.R, the reference
# implementation in R/inference.R and the companion app use these functions.
#
# A design is a list of trials made by make_trials(). Each trial gives the cues
# present ($cues), the outcome lambda ($lambda, 1 or 0) and a phase label
# ($phase).


# Rescorla-Wagner model (Equation 1). The cues present on a trial share one
# prediction error, lambda minus their summed value, and each adds its
# learning rate times this error to its value. beta holds the learning rates,
# one named value per cue, and they do not change with training. Absent cues
# keep their values. Returns one row per cue and trial, holding the value at
# the start of the trial; trial length(design) + 1 holds the values after the
# last trial.
rw_simulate <- function(design, beta, V_init = NULL, cue_names = NULL) {
  if (is.null(cue_names))
    cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  n_cues   <- length(cue_names)
  n_trials <- length(design)

  if (!all(cue_names %in% names(beta)))
    stop("beta must contain named values for all cues: ", paste(cue_names, collapse = ", "))

  V <- if (is.null(V_init)) setNames(rep(0, n_cues), cue_names) else V_init
  # Column t holds the values at the start of trial t.
  V_by_trial <- matrix(NA_real_, n_cues, n_trials + 1)
  V_by_trial[, 1] <- V[cue_names]

  for (t in seq_along(design)) {
    cues_present <- design[[t]]$cues
    lambda       <- design[[t]]$lambda

    # The error is computed before any value changes, so cues presented
    # together receive the same error (Section 2.2).
    delta <- lambda - sum(V[cues_present])
    V[cues_present] <- V[cues_present] + beta[cues_present] * delta

    V_by_trial[, t + 1] <- V[cue_names]
  }

  phases <- vapply(design, function(x) x$phase, character(1))
  data.frame(
    trial = rep(seq_len(n_trials + 1), each = n_cues),
    phase = rep(c(phases, phases[n_trials]), each = n_cues),
    cue   = rep(cue_names, n_trials + 1),
    V     = as.vector(V_by_trial),
    stringsAsFactors = FALSE
  )
}


# Adaptive model (Equations 2 and 3). Each cue present learns from its own
# error, lambda - V_i, with the learning rate S_i alpha_i: its salience S_i
# times its associability alpha_i. Associability changes by eta times the
# error of the other cues present minus the cue's own error, so it rises when
# the cue predicts more accurately than the other cues, falls when it predicts
# less accurately, and stays unchanged when the errors are equal. A cue
# presented alone is compared with the error of predicting zero, |lambda|.
# Absent cues keep their values and associabilities.
#
# alpha_init and V_init give the state at the start of the run: alpha_init
# and zero before training, or alpha_i(0) (Equation 4) and V_i(0) (Equation 5)
# for a run of pear trials. Associability is kept within
# [alpha_min, alpha_max] = [.01, 1]. Returns one row per cue and trial, holding
# V and alpha at the start of the trial; trial length(design) + 1 holds the
# values after the last trial.
adaptive_simulate <- function(design, S, alpha_init,
                              eta = 0.05,
                              V_init = NULL, cue_names = NULL,
                              alpha_min = 0.01, alpha_max = 1.0) {
  if (is.null(cue_names))
    cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  n_cues   <- length(cue_names)
  n_trials <- length(design)

  if (!all(cue_names %in% names(S)) || !all(cue_names %in% names(alpha_init)))
    stop("S and alpha_init must contain named values for all cues: ",
         paste(cue_names, collapse = ", "))

  V <- if (is.null(V_init)) setNames(rep(0, n_cues), cue_names) else V_init
  alpha <- alpha_init
  # Column t holds the values and associabilities at the start of trial t.
  V_by_trial <- alpha_by_trial <- matrix(NA_real_, n_cues, n_trials + 1)
  V_by_trial[, 1] <- V[cue_names]
  alpha_by_trial[, 1] <- alpha[cue_names]

  for (t in seq_along(design)) {
    cues_present <- design[[t]]$cues
    lambda       <- design[[t]]$lambda

    # Both the value update and the error comparison use the values at the
    # start of the trial, so a change in associability takes effect on the
    # next trial (Section 3.3).
    V_start <- V

    for (cue in cues_present) {
      V[cue] <- V[cue] + S[cue] * alpha[cue] * (lambda - V[cue])
    }

    # e_own and e_other are e_i and e_-i of Equation 3: the size of the cue's
    # own error and of the error of the other cues' summed prediction.
    for (cue in cues_present) {
      others  <- setdiff(cues_present, cue)
      e_own   <- abs(lambda - V_start[cue])
      e_other <- if (length(others) > 0) {
        abs(lambda - sum(V_start[others]))
      } else {
        abs(lambda)
      }

      alpha[cue] <- max(alpha_min, min(alpha_max, alpha[cue] + eta * (e_other - e_own)))
    }

    V_by_trial[, t + 1] <- V[cue_names]
    alpha_by_trial[, t + 1] <- alpha[cue_names]
  }

  phases <- vapply(design, function(x) x$phase, character(1))
  data.frame(
    trial = rep(seq_len(n_trials + 1), each = n_cues),
    phase = rep(c(phases, phases[n_trials]), each = n_cues),
    cue   = rep(cue_names, n_trials + 1),
    V     = as.vector(V_by_trial),
    alpha = as.vector(alpha_by_trial),
    stringsAsFactors = FALSE
  )
}


# The same two models, returning only the state after the last trial: the
# values, and for the adaptive model also the associabilities. Their loops are
# the ones printed in Sections 2.3 and 3.3. They are written separately from
# rw_simulate() and adaptive_simulate() so that tests/verify_models.R can
# compare the two versions (Section 2.3).
rw_final <- function(design, beta, V_init = NULL) {
  cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  V <- if (is.null(V_init)) setNames(rep(0, length(cue_names)), cue_names) else V_init
  for (t in seq_along(design)) {
    cues  <- design[[t]]$cues
    delta <- design[[t]]$lambda - sum(V[cues])
    V[cues] <- V[cues] + beta[cues] * delta
  }
  V
}

adaptive_final_state <- function(design, S, alpha_init, eta = 0.05, V_init = NULL) {
  cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  V <- if (is.null(V_init)) setNames(rep(0, length(cue_names)), cue_names) else V_init
  alpha <- alpha_init
  for (t in seq_along(design)) {
    cues    <- design[[t]]$cues
    lambda  <- design[[t]]$lambda
    e_own   <- abs(lambda - V[cues])
    e_other <- abs(lambda - (sum(V[cues]) - V[cues]))
    V[cues] <- V[cues] + S[cues] * alpha[cues] * (lambda - V[cues])
    alpha[cues] <- alpha[cues] + eta * (e_other - e_own)
    alpha[cues] <- pmin(1, pmax(.01, alpha[cues]))
  }
  list(V = V, alpha = alpha)
}


# n identical trials: the cues present, the outcome lambda and a phase label.
make_trials <- function(cues, lambda, n, phase_label) {
  lapply(seq_len(n), function(i) {
    list(cues = cues, lambda = lambda, phase = phase_label)
  })
}

# Values and associabilities after the last trial of a run of rw_simulate()
# or adaptive_simulate().
extract_final_V <- function(run) {
  final <- run[run$trial == max(run$trial), ]
  setNames(final$V, final$cue)
}

extract_final_alpha <- function(run) {
  final <- run[run$trial == max(run$trial), ]
  setNames(final$alpha, final$cue)
}


# Response functions of Section 5.2, which turn a learned value V into the
# probability of a yes response: the sigmoid of Equation 7, with slope gamma
# and midpoint theta, and the threshold rule, which gives a yes response when
# V exceeds theta and a no response otherwise.
obs_sigmoid <- function(V, gamma = 8, theta = 0.5) {
  1 / (1 + exp(-gamma * (V - theta)))
}

obs_threshold <- function(V, theta = 0.5) {
  as.numeric(V > theta)
}


# Prints the first n_trace trials of a Rescorla-Wagner run step by step, so
# that the calculations of Section 2.2 can be followed by hand. Returns the
# values after those trials.
rw_hand_trace <- function(design, beta, n_trace = 5) {
  cue_names <- unique(unlist(lapply(design, function(x) x$cues)))
  V <- setNames(rep(0, length(cue_names)), cue_names)
  # Numbers as the paper writes them, without a leading zero.
  fmt <- function(x, digits = 4) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))

  for (t in seq_len(min(n_trace, length(design)))) {
    cues   <- design[[t]]$cues
    lambda <- design[[t]]$lambda
    phase  <- design[[t]]$phase
    sum_V  <- sum(V[cues])
    delta  <- lambda - sum_V

    cat(sprintf("Trial %d [%s]: cues = {%s}, lambda = %g\n",
                t, phase, paste(cues, collapse = ", "), lambda))
    cat(sprintf("  V before: %s\n",
                paste(sprintf("%s = %s", cue_names, fmt(V[cue_names])), collapse = ", ")))
    cat(sprintf("  delta = lambda - sum of V present = %g - %s = %s\n",
                lambda, fmt(sum_V), fmt(delta)))
    for (cue in cues) {
      cat(sprintf("  change in V_%s = beta_%s * delta = %s * %s = %s\n",
                  cue, cue, fmt(beta[cue], 2), fmt(delta), fmt(beta[cue] * delta)))
    }

    V[cues] <- V[cues] + beta[cues] * delta

    cat(sprintf("  V after:  %s\n\n",
                paste(sprintf("%s = %s", cue_names, fmt(V[cue_names])), collapse = ", ")))
  }
  invisible(V)
}
