# Model engine for the companion app
#
# These functions compute every model result the app shows, except the
# simulated experiments of Steps 5 and 6, which are read from
# data/inference_cases.rds. They call the learning models in models.R (a copy
# of R/models.R) and add only what the app needs: the two training histories,
# the pear trials, the carryover assumptions, the changes to the adaptive
# model, the rank-based rule, the response functions, and the decision model
# of Step 7.
#
# The cues are named as in the paper: tones A and C, lights B and D, and the
# buzzer X. A is trained first and then appears with B; X is trained first,
# and the new tone C appears with D, the comparison light.
#
# Checked against the paper's values by tests/verify_app.R.

# From the repository root this is R/models.R; from shiny_app/ it is the copy in
# shiny_app/R/models.R, which the tests require to be identical.
source("R/models.R", local = TRUE)

# The settings of the paper's examples.
default_settings <- list(
  beta = .12,                           # Rescorla-Wagner learning rate (Section 2.2)
  S = .3, alpha_init = .5, eta = .05,   # adaptive model (Section 3.2)
  n_first = 10, n_compound = 10, n_pear = 10,
  g_V = 0, g_alpha = 1,                 # carryover to the pear trials (Section 4.2)
  change = "none",                      # none, prevent, remove or supply (Section 4.1)
  c_offset = 0                          # tone C's salience is S * (1 + c_offset) (Section 4.4)
)

settings_with <- function(...) modifyList(default_settings, list(...))

# The two training histories of Table 1, all trials followed by the apple:
# ten A+ then ten AB+, and ten X+ then ten CD+.
trained_tone_history <- function(s) c(make_trials("A", 1, s$n_first, "Earlier trials"),
                                      make_trials(c("A", "B"), 1, s$n_compound, "Compound trials"))
new_tone_history <- function(s) c(make_trials("X", 1, s$n_first, "Earlier trials"),
                                  make_trials(c("C", "D"), 1, s$n_compound, "Compound trials"))
rw_rates <- function(cues, s) setNames(rep(s$beta, length(cues)), cues)


# Section 2: the trained-tone history under Rescorla-Wagner

# One row per trial t = 1, ..., 20: the cues present, the values before the
# trial, their combined prediction, the shared error, each cue's gain and the
# values after the trial. The light is absent on the first ten trials, so its
# gain there is NA.
rw_trials <- function(s = default_settings) {
  design <- trained_tone_history(s)
  run <- rw_simulate(design, beta = rw_rates(c("A", "B"), s))
  n <- length(design)
  A <- run$V[run$cue == "A"]   # A[t] is the value before trial t; A[n + 1] the final value
  B <- run$V[run$cue == "B"]
  light <- vapply(design, function(x) "B" %in% x$cues, logical(1))
  prediction <- A[1:n] + ifelse(light, B[1:n], 0)
  data.frame(trial = seq_len(n), light = light,
             V_A = A[1:n], V_B = B[1:n], prediction = prediction, error = 1 - prediction,
             gain_A = A[-1] - A[1:n], gain_B = ifelse(light, B[-1] - B[1:n], NA),
             V_A_after = A[-1], V_B_after = B[-1])
}

# Section 2.3: one compound trial followed by the apple, starting from the
# values V, as specified and as two mistaken programs would calculate it. The
# specified rule is applied by rw_final(). One mistaken program uses each
# cue's own error; the other updates A first and then recalculates the error
# for B. Returns the gains of A and B.
compound_check <- function(V = c(A = .3, B = .3), s = default_settings) {
  rate <- s$beta
  specified <- rw_final(make_trials(c("A", "B"), 1, 1, "Check"), beta = rw_rates(c("A", "B"), s),
                        V_init = V)
  own <- V + rate * (1 - V)
  one_at_a_time <- V
  one_at_a_time["A"] <- V[["A"]] + rate * (1 - sum(V))
  one_at_a_time["B"] <- V[["B"]] + rate * (1 - (one_at_a_time[["A"]] + V[["B"]]))
  list(specified = specified[c("A", "B")] - V, own = own - V, one_at_a_time = one_at_a_time - V)
}


# Sections 3 and 4: what the compound trials leave behind

trained_states <- function(s = default_settings) {
  rw_b <- rw_final(trained_tone_history(s), beta = rw_rates(c("A", "B"), s))
  rw_c <- rw_final(new_tone_history(s), beta = rw_rates(c("X", "C", "D"), s))

  # The adaptive model runs one phase at a time, so that Prevent can hold
  # associability fixed during the compound trials only (Section 4.1). X's
  # trials cannot affect C or D, so the new-tone history starts at its
  # compound trials. B, C and D start at value zero and associability
  # alpha_init. A nonzero c_offset makes C slightly more or less salient than
  # D (Section 4.4).
  eta_compound <- if (s$change == "prevent") 0 else s$eta
  first <- adaptive_final_state(make_trials("A", 1, s$n_first, "Earlier trials"),
                                S = c(A = s$S), alpha_init = c(A = s$alpha_init), eta = s$eta)
  ad_b <- adaptive_final_state(make_trials(c("A", "B"), 1, s$n_compound, "Compound trials"),
                               S = c(A = s$S, B = s$S),
                               alpha_init = c(A = unname(first$alpha), B = s$alpha_init),
                               eta = eta_compound, V_init = c(A = unname(first$V), B = 0))
  ad_c <- adaptive_final_state(make_trials(c("C", "D"), 1, s$n_compound, "Compound trials"),
                               S = c(C = s$S * (1 + s$c_offset), D = s$S),
                               alpha_init = c(C = s$alpha_init, D = s$alpha_init),
                               eta = eta_compound, V_init = c(C = 0, D = 0))

  # The learning rates of Table 3: fixed under Rescorla-Wagner, S * alpha
  # under the adaptive model.
  list(
    rw = list(V_A = unname(rw_b["A"]), V_B = unname(rw_b["B"]), V_D = unname(rw_c["D"]),
              rate_B = s$beta, rate_D = s$beta),
    adaptive = list(V_A_first = unname(first$V), alpha_A_first = unname(first$alpha),
                    V_A = unname(ad_b$V["A"]), V_B = unname(ad_b$V["B"]),
                    V_D = unname(ad_c$V["D"]),
                    alpha_B = unname(ad_b$alpha["B"]), alpha_D = unname(ad_c$alpha["D"]),
                    rate_B = s$S * unname(ad_b$alpha["B"]), rate_D = s$S * unname(ad_c$alpha["D"]))
  )
}

# The compound trials one at a time, for B and for D under both models: the
# value before the trial, the error the light learns from, its learning rate,
# its gain and, under the adaptive model, its associability before and after
# the trial. Row j describes compound trial j. Uses the default history,
# without changes or a salience difference.
compound_trials <- function(s = default_settings) {
  n1 <- s$n_first
  rw_b <- rw_simulate(trained_tone_history(s), beta = rw_rates(c("A", "B"), s))
  rw_c <- rw_simulate(new_tone_history(s), beta = rw_rates(c("X", "C", "D"), s))
  ad_b <- adaptive_simulate(trained_tone_history(s), S = c(A = s$S, B = s$S),
                            alpha_init = c(A = s$alpha_init, B = s$alpha_init), eta = s$eta)
  ad_c <- adaptive_simulate(new_tone_history(s), S = c(X = s$S, C = s$S, D = s$S),
                            alpha_init = c(X = s$alpha_init, C = s$alpha_init, D = s$alpha_init),
                            eta = s$eta)
  # A run holds each cue's state at the start of every trial, so the row of
  # trial t + 1 gives the state after trial t.
  trials <- n1 + seq_len(s$n_compound)
  state <- function(run, cue, col, t) run[[col]][run$cue == cue][t]
  light <- function(rw, ad, cue, companion) {
    V_rw <- state(rw, cue, "V", trials)
    V_ad <- state(ad, cue, "V", trials)
    data.frame(trial = seq_len(s$n_compound),
      rw_V = V_rw, rw_error = 1 - V_rw - state(rw, companion, "V", trials),
      rw_rate = s$beta, rw_gain = state(rw, cue, "V", trials + 1) - V_rw,
      ad_V = V_ad, ad_error = 1 - V_ad,
      ad_companion_error = 1 - state(ad, companion, "V", trials),
      ad_alpha = state(ad, cue, "alpha", trials),
      ad_rate = s$S * state(ad, cue, "alpha", trials),
      ad_gain = state(ad, cue, "V", trials + 1) - V_ad,
      ad_alpha_after = state(ad, cue, "alpha", trials + 1))
  }
  list(B = light(rw_b, ad_b, "B", "A"), D = light(rw_c, ad_c, "D", "C"))
}

# Tone A trained alone under the adaptive model (Section 3.2): its value and
# associability after each of the earlier trials, k = 0, ..., n_first.
tone_alone <- function(s = default_settings) {
  run <- adaptive_simulate(make_trials("A", 1, s$n_first, "Earlier trials"), S = c(A = s$S),
                           alpha_init = c(A = s$alpha_init), eta = s$eta)
  data.frame(k = 0:s$n_first, V = run$V, alpha = run$alpha)
}


# The pear trials: each light alone, each trial followed by the pear

# One light trained alone on n pear trials, under the adaptive model or with a
# fixed learning rate ("rw"). Given rate, the fixed-rate learner uses that
# learning rate instead of beta. Returns one row per completed pear trial
# k = 0, ..., n: V and alpha are the values after k pear trials; error, rate
# and gain describe pear trial k + 1.
pear_light <- function(model, V_start, alpha_start, s, rate = NULL, n = s$n_pear) {
  design <- make_trials("L", 1, n, "Pear trials")
  if (model == "adaptive") {
    run <- adaptive_simulate(design, S = c(L = s$S), alpha_init = c(L = alpha_start),
                             eta = s$eta, V_init = c(L = V_start), cue_names = "L")
    V <- run$V
    alpha <- run$alpha
    rates <- s$S * alpha
  } else {
    fixed <- if (is.null(rate)) s$beta else rate
    run <- rw_simulate(design, beta = c(L = fixed), V_init = c(L = V_start), cue_names = "L")
    V <- run$V
    alpha <- rep(NA_real_, n + 1)
    rates <- rep(fixed, n + 1)
  }
  data.frame(k = 0:n, V = V, alpha = alpha, rate = rates,
             error = c(1 - V[-(n + 1)], NA), gain = c(diff(V), NA))
}

# Both models, both lights, on the pear trials under the settings in s. Each
# model's gap is the B-D gap, D's learned value for the pear minus B's, after
# k = 0, ..., n pear trials.
run_inquiry <- function(s = default_settings, n = s$n_pear) {
  st <- trained_states(s)
  rw <- st$rw
  ad <- st$adaptive

  # Associabilities at the start of the pear trials (Equation 4). Remove
  # resets both to alpha_init (Section 4.1).
  a_B <- s$alpha_init + s$g_alpha * (ad$alpha_B - s$alpha_init)
  a_D <- s$alpha_init + s$g_alpha * (ad$alpha_D - s$alpha_init)
  if (s$change == "remove") a_B <- a_D <- s$alpha_init

  # Learned values for the pear start at g_V times those for the apple (Equation 5).
  rw_B <- pear_light("rw", s$g_V * rw$V_B, NA, s, n = n)
  rw_D <- pear_light("rw", s$g_V * rw$V_D, NA, s, n = n)

  if (s$change == "supply") {
    # A Rescorla-Wagner learner that starts the pear trials where the adaptive
    # model starts: the same values and the learning rates S * alpha, which
    # then stay fixed. Each light is alone, so its shared error is its own
    # error, as in the adaptive model. Only the change in associability
    # differs.
    ad_B <- pear_light("rw", s$g_V * ad$V_B, NA, s, rate = s$S * a_B, n = n)
    ad_D <- pear_light("rw", s$g_V * ad$V_D, NA, s, rate = s$S * a_D, n = n)
  } else {
    ad_B <- pear_light("adaptive", s$g_V * ad$V_B, a_B, s, n = n)
    ad_D <- pear_light("adaptive", s$g_V * ad$V_D, a_D, s, n = n)
  }

  list(settings = s, trained = st,
       start = list(alpha_B = a_B, alpha_D = a_D),
       rw = list(B = rw_B, D = rw_D, gap = rw_D$V - rw_B$V),
       adaptive = list(B = ad_B, D = ad_D, gap = ad_D$V - ad_B$V))
}

# Both models, both lights, when training continues with the apple (Step 2):
# each light alone, starting from the values and associabilities that the
# compound trials left. Returns the same structure as run_inquiry().
continue_apple <- function(s = default_settings) {
  st <- trained_states(s)
  rw <- st$rw
  ad <- st$adaptive
  rw_B <- pear_light("rw", rw$V_B, NA, s)
  rw_D <- pear_light("rw", rw$V_D, NA, s)
  ad_B <- pear_light("adaptive", ad$V_B, ad$alpha_B, s)
  ad_D <- pear_light("adaptive", ad$V_D, ad$alpha_D, s)
  list(settings = s, trained = st,
       start = list(alpha_B = ad$alpha_B, alpha_D = ad$alpha_D),
       rw = list(B = rw_B, D = rw_D, gap = rw_D$V - rw_B$V),
       adaptive = list(B = ad_B, D = ad_D, gap = ad_D$V - ad_B$V))
}

# Each model's gap after k pear trials as the carryover of learned value
# varies, with g_alpha as in s (Figure 5a).
gap_by_value_carryover <- function(k = 8, g_V = seq(0, 1, by = .01), s = default_settings) {
  rows <- lapply(g_V, function(g) {
    r <- run_inquiry(modifyList(s, list(g_V = g)))
    c(g_V = g, rw = r$rw$gap[k + 1], adaptive = r$adaptive$gap[k + 1])
  })
  as.data.frame(do.call(rbind, rows))
}


# Section 4.4: the rank-based rule (Supplement S3)

# Associability moves the fraction step of the way toward 1 when a cue makes
# the smaller error and toward 0 when it makes the larger error. Errors closer
# than the square root of the machine precision count as equal. Equal errors
# leave associability unchanged or, with ties_lower = TRUE, lower it as a
# larger error does. Learned values change as in Equation 2. Each trial in
# trials gives the cues present; every trial is followed by its outcome.
rank_run <- function(trials, V, alpha, S, step, ties_lower = FALSE) {
  tolerance <- sqrt(.Machine$double.eps)
  for (cues in trials) {
    V_start <- V
    for (i in cues) V[i] <- V[i] + S[i] * alpha[i] * (1 - V_start[i])
    for (i in cues) {
      e_own <- abs(1 - V_start[i])
      e_other <- abs(1 - sum(V_start[setdiff(cues, i)]))
      alpha[i] <- if (e_own + tolerance < e_other) {
        alpha[i] + step * (1 - alpha[i])
      } else if (e_other + tolerance < e_own || ties_lower) {
        alpha[i] * (1 - step)
      } else {
        alpha[i]
      }
    }
  }
  list(V = V, alpha = alpha)
}

# The step that leaves B, after the compound trials, at the associability that
# Equation 3 gives it. B makes the larger error on every compound trial, so
# its associability is multiplied by 1 - step on each of them.
rank_step <- function(s = default_settings) {
  1 - (trained_states(s)$adaptive$alpha_B / s$alpha_init)^(1 / s$n_compound)
}

# D's associability after the compound trials, and the B-D gap over n pear
# trials, under a rule for associability: "proportional" (Equation 3),
# "rank" (rank-based, ties leave associability unchanged) or "rank_ties"
# (rank-based, ties lower it). Tone C's salience is S * (1 + c_offset).
# Learned values for the pear start at zero, and associability carries over in
# full.
rule_comparison <- function(rule = c("proportional", "rank", "rank_ties"), c_offset = 0,
                            s = default_settings, n = 15) {
  rule <- match.arg(rule)
  if (rule == "proportional") {
    r <- run_inquiry(modifyList(s, list(c_offset = c_offset, g_V = 0, g_alpha = 1,
                                        change = "none")), n = n)
    return(list(alpha_B = r$start$alpha_B, alpha_D = r$start$alpha_D, gap = r$adaptive$gap))
  }
  ties <- rule == "rank_ties"
  step <- rank_step(s)
  history <- function(first, pair, S) {
    cues <- unique(c(first, pair))
    state <- rank_run(rep(list(first), s$n_first), setNames(rep(0, length(cues)), cues),
                      setNames(rep(s$alpha_init, length(cues)), cues), S, step, ties)
    rank_run(rep(list(pair), s$n_compound), state$V, state$alpha, S, step, ties)
  }
  pear <- function(alpha_start) {
    state <- list(V = c(L = 0), alpha = c(L = alpha_start))
    values <- 0
    for (m in seq_len(n)) {
      state <- rank_run(list("L"), state$V, state$alpha, c(L = s$S), step, ties)
      values <- c(values, state$V[["L"]])
    }
    values
  }
  S_all <- c(A = s$S, B = s$S, X = s$S, C = s$S * (1 + c_offset), D = s$S)
  alpha_B <- history("A", c("A", "B"), S_all)$alpha[["B"]]
  alpha_D <- history("X", c("C", "D"), S_all)$alpha[["D"]]
  list(alpha_B = alpha_B, alpha_D = alpha_D, gap = pear(alpha_D) - pear(alpha_B))
}


# Section 5: from learned value to a yes response

# The sigmoid of Equation 7, with slope gamma and midpoint theta, and the
# threshold rule, which gives a yes whenever the learned value exceeds theta.
response_prob <- function(V, rule = c("sigmoid", "threshold"), gamma = 8, theta = .5) {
  rule <- match.arg(rule)
  if (rule == "sigmoid") obs_sigmoid(V, gamma = gamma, theta = theta)
  else obs_threshold(V, theta = theta)
}


# The decision model of Step 7
#
# A drift diffusion model with noise standard deviation 1, boundaries at 0 and
# a, start at a/2, and no deadline. The accuracy (Equation 9) and the mean
# decision time have closed forms. The density of the decision time uses the
# large-time series of Navarro and Fuss (2009), as in Figure 10c, and the
# percentage of decisions finished by each time is that density summed by the
# trapezoid rule.

ddm_settings <- data.frame(id = c("first", "second"), v = c(.30, .60), a = c(2.0, 1.0))

ddm_acc <- function(v, a) 1 / (1 + exp(-v * a))                  # Equation 9
ddm_mean <- function(v, a) a / (2 * v) * tanh(v * a / 2)         # mean decision time
# Density at the lower boundary, with the start at w * a and K terms of the series.
ddm_dens_lower <- function(t, v, a, w = .5, K = 300) {
  k <- seq_len(K)
  vapply(t, function(tt) {
    if (tt <= 0) return(0)
    (pi / a^2) * exp(-v * a * w - v^2 * tt / 2) *
      sum(k * exp(-k^2 * pi^2 * tt / (2 * a^2)) * sin(k * pi * w))
  }, numeric(1))
}
# Both boundaries together: the upper boundary (the correct response) uses -v
# and 1 - w, which is also .5.
ddm_dens <- function(t, v, a) ddm_dens_lower(t, v, a) + ddm_dens_lower(t, -v, a)
# The proportion of decisions finished by each time in times, which must start
# at 0 and be evenly spaced.
ddm_finished <- function(times, v, a) {
  f <- ddm_dens(times, v, a)
  c(0, cumsum((head(f, -1) + f[-1]) / 2 * diff(times)))
}

ddm_summary <- function() {
  d <- ddm_settings
  d$acc <- ddm_acc(d$v, d$a)
  d$mean <- ddm_mean(d$v, d$a)
  # Finished by time 0.5, on the grid of .0005 used for Figure 10c.
  d$by_half <- vapply(seq_len(nrow(d)), function(i)
    tail(ddm_finished(seq(0, .5, by = .0005), d$v[i], d$a[i]), 1), numeric(1))
  d
}
