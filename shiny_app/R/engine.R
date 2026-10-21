# Model engine for the companion app
#
# These functions compute every model result the app shows, except the
# simulated experiments of Step 5, which are read from data/inference_cases.rds.
# They call the learning models in models.R (a copy of R/models.R, which the
# scripts for Sections 2 to 4 of the paper use) and add only the bookkeeping the
# app needs: the training design, the carryover assumptions, the changes to the
# adaptive model, the response functions, and the decision-model calculations of
# Step 6.
#
# Checked against the paper's values by tests/verify_app.R.

# From the repository root this is R/models.R; from shiny_app/ it is the copy in
# shiny_app/R/models.R, which the tests require to be identical.
source("R/models.R", local = TRUE)

# The settings of the paper's example.
default_settings <- list(
  rw_alpha = .4, rw_beta = .3,          # Rescorla-Wagner
  S = .3, alpha_init = .5, eta = .05,   # adaptive model
  n_phase1 = 10, n_compound = 10, n_transfer = 10,
  g_V = 0, g_alpha = 1,                 # carryover to the new outcome
  change = "none",                      # none, prevent, remove or supply (Section 4.1)
  e_offset = 0                          # E's salience is S * (1 + e_offset)
)

settings_with <- function(...) modifyList(default_settings, list(...))

# The two-phase training of Section 3.2: A (or C) alone, then A with B (or D
# with E), all with the first outcome O1.
blocking_design <- function(s) c(make_trials("A", 1, s$n_phase1, "Phase 1"),
                                 make_trials(c("A", "B"), 1, s$n_compound, "Phase 2"))
control_design <- function(s) c(make_trials("C", 1, s$n_phase1, "Phase 1"),
                                make_trials(c("D", "E"), 1, s$n_compound, "Phase 2"))


# Phases 1 and 2: what training with O1 leaves behind

trained_states <- function(s = default_settings) {
  blocking <- blocking_design(s)
  control  <- control_design(s)

  rw_b <- rw_final(blocking, alpha = c(A = s$rw_alpha, B = s$rw_alpha), beta = s$rw_beta)
  rw_c <- rw_final(control, alpha = c(C = s$rw_alpha, D = s$rw_alpha, E = s$rw_alpha),
                   beta = s$rw_beta)

  # The adaptive model runs one phase at a time, so that Prevent can hold
  # associability fixed during compound training only (Section 4.1). C's
  # training cannot affect D or E, so the control run starts at Phase 2. The
  # new cues B, D and E start at value zero and associability alpha_init.
  eta_compound <- if (s$change == "prevent") 0 else s$eta
  phase1 <- adaptive_final_state(make_trials("A", 1, s$n_phase1, "Phase 1"),
                                 S = c(A = s$S), alpha_init = c(A = s$alpha_init),
                                 eta = s$eta)
  ad_b <- adaptive_final_state(make_trials(c("A", "B"), 1, s$n_compound, "Phase 2"),
                               S = c(A = s$S, B = s$S),
                               alpha_init = c(A = unname(phase1$alpha), B = s$alpha_init),
                               eta = eta_compound,
                               V_init = c(A = unname(phase1$V), B = 0))
  # A nonzero e_offset makes E slightly more or less salient than D, so that
  # D's error differs slightly from E's (Section 4.5).
  ad_c <- adaptive_final_state(make_trials(c("D", "E"), 1, s$n_compound, "Phase 2"),
                               S = c(D = s$S, E = s$S * (1 + s$e_offset)),
                               alpha_init = c(D = s$alpha_init, E = s$alpha_init),
                               eta = eta_compound,
                               V_init = c(D = 0, E = 0))

  # mult_first is the multiplier on the first compound trial; mult_B and
  # mult_D are the multipliers left at the end of compound training (Table 2).
  list(
    rw = list(V_A = unname(rw_b["A"]), V_B = unname(rw_b["B"]), V_D = unname(rw_c["D"]),
              mult_first = s$rw_alpha * s$rw_beta,
              mult_B = s$rw_alpha * s$rw_beta, mult_D = s$rw_alpha * s$rw_beta),
    adaptive = list(V_A = unname(ad_b$V["A"]), V_B = unname(ad_b$V["B"]),
                    V_D = unname(ad_c$V["D"]),
                    alpha_B = unname(ad_b$alpha["B"]), alpha_D = unname(ad_c$alpha["D"]),
                    mult_first = s$S * s$alpha_init,
                    mult_B = s$S * unname(ad_b$alpha["B"]),
                    mult_D = s$S * unname(ad_c$alpha["D"]))
  )
}

# Compound training trial by trial: what B and D bring into each trial and
# what they gain from it. Rows are the compound trials 1, ..., n_compound. It
# does not apply change or e_offset; the app calls it with the default settings.
compound_trace <- function(s = default_settings) {
  rw_b <- rw_simulate(blocking_design(s), alpha = c(A = s$rw_alpha, B = s$rw_alpha),
                      beta = s$rw_beta)
  rw_c <- rw_simulate(control_design(s), alpha = c(C = s$rw_alpha, D = s$rw_alpha, E = s$rw_alpha),
                      beta = s$rw_beta)
  ad_b <- adaptive_simulate(blocking_design(s), S = c(A = s$S, B = s$S),
                            alpha_init = c(A = s$alpha_init, B = s$alpha_init),
                            eta = s$eta)
  ad_c <- adaptive_simulate(control_design(s), S = c(C = s$S, D = s$S, E = s$S),
                            alpha_init = c(C = s$alpha_init, D = s$alpha_init, E = s$alpha_init),
                            eta = s$eta)
  # The positions of the compound trials in the design. A run holds each cue's
  # state at the start of every trial, so trial t + 1 gives the state after t.
  trials <- s$n_phase1 + seq_len(s$n_compound)
  pick <- function(run, cue, col, t) {
    rows <- run[run$cue == cue, ]
    rows[[col]][match(t, rows$trial)]
  }
  rw <- data.frame(
    trial = seq_len(s$n_compound),
    V_A = pick(rw_b, "A", "V", trials), V_B = pick(rw_b, "B", "V", trials),
    error = 1 - (pick(rw_b, "A", "V", trials) + pick(rw_b, "B", "V", trials)),
    gain_B = pick(rw_b, "B", "V", trials + 1) - pick(rw_b, "B", "V", trials),
    gain_D = pick(rw_c, "D", "V", trials + 1) - pick(rw_c, "D", "V", trials))
  ad <- data.frame(
    trial = seq_len(s$n_compound),
    V_A = pick(ad_b, "A", "V", trials), V_B = pick(ad_b, "B", "V", trials),
    error_A = 1 - pick(ad_b, "A", "V", trials), error_B = 1 - pick(ad_b, "B", "V", trials),
    alpha_B = pick(ad_b, "B", "alpha", trials),
    alpha_B_after = pick(ad_b, "B", "alpha", trials + 1),
    gain_B = pick(ad_b, "B", "V", trials + 1) - pick(ad_b, "B", "V", trials),
    gain_D = pick(ad_c, "D", "V", trials + 1) - pick(ad_c, "D", "V", trials))
  list(rw = rw, adaptive = ad)
}


# Phase 3: each cue learning alone

# One cue trained alone for n_transfer trials, each followed by the outcome,
# under the adaptive model or a fixed-rate learner ("rw"). Given mult, the
# fixed-rate learner uses that multiplier instead of rw_alpha * rw_beta.
# Returns one row per completed update k = 0, ..., n_transfer: V and alpha are
# the values after k updates; error, mult and gain describe update k + 1.
transfer_cue <- function(model, V_start, alpha_start, s, mult = NULL) {
  n <- s$n_transfer
  design <- make_trials("X", 1, n, "Phase 3")
  if (model == "adaptive") {
    run <- adaptive_simulate(design, S = c(X = s$S), alpha_init = c(X = alpha_start),
                             eta = s$eta,
                             V_init = c(X = V_start), cue_names = "X")
    V <- run$V
    alpha <- run$alpha
    multiplier <- s$S * alpha
  } else {
    # One cue under Rescorla-Wagner: the multiplier is passed as its salience,
    # with beta = 1.
    fixed_mult <- if (is.null(mult)) s$rw_alpha * s$rw_beta else mult
    run <- rw_simulate(design, alpha = c(X = fixed_mult), beta = 1,
                       V_init = c(X = V_start), cue_names = "X")
    V <- run$V
    alpha <- rep(NA_real_, n + 1)
    multiplier <- rep(fixed_mult, n + 1)
  }
  data.frame(k = 0:n, V = V, alpha = alpha, mult = multiplier,
             error = c(1 - V[-(n + 1)], NA), gain = c(diff(V), NA))
}

# Both models, both cues, learning the new outcome O2 under the settings in s.
# Each model's gap is the B-D gap, D's value minus B's, after k = 0, ...,
# n_transfer updates (Equation 11).
run_inquiry <- function(s = default_settings) {
  st <- trained_states(s)
  rw <- st$rw
  ad <- st$adaptive

  # Associabilities at the start of O2 (Equation 14). Remove resets both to
  # alpha_init (Section 4.1).
  a_B <- s$alpha_init + s$g_alpha * (ad$alpha_B - s$alpha_init)
  a_D <- s$alpha_init + s$g_alpha * (ad$alpha_D - s$alpha_init)
  if (s$change == "remove") a_B <- a_D <- s$alpha_init

  # Values at the start of O2 are g_V times the final values for O1 (Equation 16).
  rw_B <- transfer_cue("rw", s$g_V * rw$V_B, NA, s)
  rw_D <- transfer_cue("rw", s$g_V * rw$V_D, NA, s)

  if (s$change == "supply") {
    # A fixed-rate learner that starts the new task where the adaptive model
    # starts: the same O2 values and the multipliers S * alpha, which then stay
    # fixed. Each cue is alone, so its error is lambda - V, as in the adaptive
    # model. Only the associability updating differs.
    ad_B <- transfer_cue("rw", s$g_V * ad$V_B, NA, s, mult = s$S * a_B)
    ad_D <- transfer_cue("rw", s$g_V * ad$V_D, NA, s, mult = s$S * a_D)
  } else {
    ad_B <- transfer_cue("adaptive", s$g_V * ad$V_B, a_B, s)
    ad_D <- transfer_cue("adaptive", s$g_V * ad$V_D, a_D, s)
  }

  list(settings = s, trained = st,
       start = list(alpha_B = a_B, alpha_D = a_D),
       rw = list(B = rw_B, D = rw_D, gap = rw_D$V - rw_B$V),
       adaptive = list(B = ad_B, D = ad_D, gap = ad_D$V - ad_B$V))
}

# Both models, both cues, when training continues with the first outcome O1
# (Step 2): each cue alone, starting from the values and associabilities that
# compound training left. Returns the same structure as run_inquiry().
continue_o1 <- function(s = default_settings) {
  st <- trained_states(s)
  rw <- st$rw
  ad <- st$adaptive
  rw_B <- transfer_cue("rw", rw$V_B, NA, s)
  rw_D <- transfer_cue("rw", rw$V_D, NA, s)
  ad_B <- transfer_cue("adaptive", ad$V_B, ad$alpha_B, s)
  ad_D <- transfer_cue("adaptive", ad$V_D, ad$alpha_D, s)
  list(settings = s, trained = st,
       start = list(alpha_B = ad$alpha_B, alpha_D = ad$alpha_D),
       rw = list(B = rw_B, D = rw_D, gap = rw_D$V - rw_B$V),
       adaptive = list(B = ad_B, D = ad_D, gap = ad_D$V - ad_B$V))
}

# The gap after k updates as old-value carryover varies (Figure 7a).
gap_by_value_carryover <- function(k = 8, g_V = seq(0, 1, by = .01),
                                   s = default_settings) {
  rows <- lapply(g_V, function(g) {
    r <- run_inquiry(modifyList(s, list(g_V = g)))
    c(g_V = g, rw = r$rw$gap[k + 1], adaptive = r$adaptive$gap[k + 1])
  })
  as.data.frame(do.call(rbind, rows))
}


# From learned value to a yes response: the response functions of Section 2.5,
# with the slope and midpoint of Section 4.4

response_prob <- function(V, rule = c("sigmoid", "threshold"),
                          gamma = 8, theta = .5) {
  rule <- match.arg(rule)
  if (rule == "sigmoid") obs_sigmoid(V, gamma = gamma, theta = theta)
  else obs_threshold(V, theta = theta)
}


# The decision model of Step 6
#
# A drift diffusion model with noise scale 1, boundaries at 0 and a, start at
# a/2, and no deadline. The accuracy and the mean decision time have closed
# forms; the decision-time density uses the large-time series of Navarro and
# Fuss (2009), as in the paper's Figure 12.

ddm_settings <- data.frame(id = c("first", "second"), v = c(.30, .60), a = c(2.0, 1.0))

ddm_acc <- function(v, a) 1 / (1 + exp(-v * a))                  # Equation 22
ddm_mean <- function(v, a) a / (2 * v) * tanh(v * a / 2)         # mean decision time
# Density at the lower boundary, with the start at w * a and K terms of the series.
ddm_dens_lower <- function(t, v, a, w = .5, K = 300) {
  k <- seq_len(K)
  sapply(t, function(tt) {
    if (tt <= 0) return(0)
    (pi / a^2) * exp(-v * a * w - v^2 * tt / 2) *
      sum(k * exp(-k^2 * pi^2 * tt / (2 * a^2)) * sin(k * pi * w))
  })
}
# Both boundaries together: the upper boundary (the correct response) uses -v
# and 1 - w, which is also .5.
ddm_dens <- function(t, v, a) ddm_dens_lower(t, v, a) + ddm_dens_lower(t, -v, a)

ddm_summary <- function() {
  d <- ddm_settings
  d$acc <- ddm_acc(d$v, d$a)
  d$mean <- ddm_mean(d$v, d$a)
  d
}
