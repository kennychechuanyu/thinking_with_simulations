# Checks of R/models.R against hand calculations and closed-form results: the
# checks of the code described in Sections 2.3 and 3.3, and results of
# Sections 3-5 and Supplement S4.1 that follow from the equations. Stops at the
# first failure.
#   Rscript tests/verify_models.R

local({
  source("R/models.R", local = TRUE)

  n_checks <- 0L

  pass <- function(label) {
    n_checks <<- n_checks + 1L
    cat(sprintf("  PASS  %s\n", label))
  }

  check_close <- function(label, actual, expected, tolerance = 1e-12) {
    comparison <- all.equal(actual, expected, tolerance = tolerance, check.attributes = TRUE)
    if (!isTRUE(comparison))
      stop(sprintf("FAIL  %s: %s", label, paste(comparison, collapse = "; ")), call. = FALSE)
    pass(label)
  }

  check_true <- function(label, condition) {
    if (!isTRUE(condition)) stop(sprintf("FAIL  %s", label), call. = FALSE)
    pass(label)
  }

  final_value <- function(run, cue) unname(extract_final_V(run)[cue])

  cat("Checks of R/models.R\n")

  # Section 2.3. Ten tone-only trials with the learning rate .12 leave the
  # error .88^10, so the tone's value is 1 - .88^10 (about .7215). More
  # generally, a single cue followed by the outcome on every trial reaches
  # lambda - (lambda - V_0) (1 - beta)^n after n trials.
  tone_only <- make_trials("A", lambda = 1, n = 10, "A+")
  check_close("ten tone-only trials give 1 - .88^10 (rw_simulate)",
              final_value(rw_simulate(tone_only, beta = c(A = 0.12)), "A"), 1 - 0.88^10)
  check_close("ten tone-only trials give 1 - .88^10 (rw_final)",
              rw_final(tone_only, beta = c(A = 0.12)), c(A = 1 - 0.88^10))
  check_close("a single cue follows the closed form from a nonzero start",
              rw_final(make_trials("A", lambda = 1, n = 7, "A+"), beta = c(A = 0.3),
                       V_init = c(A = 0.2)),
              c(A = 1 - (1 - 0.2) * (1 - 0.3)^7))

  # The compound check of Section 2.3. With both cues at .30 before a compound
  # trial followed by the outcome, the shared error is 1 - (.30 + .30) = .40,
  # so both cues gain .12 * .40 = .048 and end at .348.
  one_compound <- make_trials(c("A", "B"), lambda = 1, n = 1, "AB+")
  equal_start <- c(A = 0.3, B = 0.3)
  check_close("compound check from .30 and .30: both cues end at .348 (rw_simulate)",
              extract_final_V(rw_simulate(one_compound, beta = c(A = 0.12, B = 0.12),
                                          V_init = equal_start)),
              c(A = 0.348, B = 0.348))
  check_close("compound check from .30 and .30: both cues end at .348 (rw_final)",
              rw_final(one_compound, beta = c(A = 0.12, B = 0.12), V_init = equal_start),
              c(A = 0.348, B = 0.348))

  # The two mistaken programs of Section 2.3, written out: one gives each cue
  # its own error, and one updates the tone first and recalculates the error for
  # the light. Both pass the tone-only check, because a cue presented alone has
  # only one error, and both fail the compound check, giving each cue .084 or
  # the light .04224.
  own_error_program <- function(design, beta, V) {
    for (trial in design) {
      cues <- trial$cues
      V[cues] <- V[cues] + beta[cues] * (trial$lambda - V[cues])
    }
    V
  }
  tone_first_program <- function(design, beta, V) {
    for (trial in design) {
      for (cue in trial$cues) {
        V[cue] <- V[cue] + beta[cue] * (trial$lambda - sum(V[trial$cues]))
      }
    }
    V
  }
  for (program in list(own_error_program, tone_first_program)) {
    check_close("a mistaken program passes the tone-only check",
                program(tone_only, c(A = 0.12), c(A = 0)), c(A = 1 - 0.88^10))
  }
  own_error_result <- own_error_program(one_compound, c(A = 0.12, B = 0.12), equal_start)
  tone_first_result <- tone_first_program(one_compound, c(A = 0.12, B = 0.12), equal_start)
  check_close("the program using each cue's own error gives each cue .084",
              own_error_result - equal_start, c(A = 0.084, B = 0.084))
  check_close("the program updating the tone first gives the light .04224",
              tone_first_result[["B"]] - 0.3, 0.04224)
  check_true("both mistaken programs fail the compound check",
             any(abs(own_error_result - 0.348) > 1e-6) &&
               any(abs(tone_first_result - 0.348) > 1e-6))

  # The first compound trial of Section 2.2, step by step: after ten A+ trials,
  # the shared error is 1 - .7215 = .2785, and both cues gain about .0334.
  blocking_rw <- c(tone_only, make_trials(c("A", "B"), lambda = 1, n = 10, "AB+"))
  trace_output <- capture.output(
    hand_state <- rw_hand_trace(blocking_rw, beta = c(A = 0.12, B = 0.12), n_trace = 11)
  )
  check_close("rw_hand_trace() agrees with the hand calculation of the first compound trial",
              hand_state[c("A", "B")], c(A = 1 - 0.88^10 + 0.12 * 0.88^10, B = 0.12 * 0.88^10))
  check_true("rw_hand_trace() prints the first compound trial",
             any(grepl("Trial 11 [AB+]", trace_output, fixed = TRUE)) &&
               any(grepl("= .12 * .2785 = .0334", trace_output, fixed = TRUE)))

  # Absent cues neither change nor enter the prediction (Section 2.3). If B's
  # value (.4) entered A's error, A would not follow the single-cue closed form
  # from its start of .2.
  B_absent <- rw_simulate(make_trials("A", lambda = 1, n = 3, "A only"),
                          beta = c(A = 0.12, B = 0.06),
                          V_init = c(A = 0.2, B = 0.4), cue_names = c("A", "B"))
  check_close("an absent cue does not change", final_value(B_absent, "B"), 0.4)
  check_close("an absent cue does not enter the prediction",
              final_value(B_absent, "A"), 1 - (1 - 0.2) * (1 - 0.12)^3)
  check_close("an absent cue neither changes nor enters the prediction (rw_final)",
              rw_final(make_trials("A", lambda = 1, n = 3, "A only"),
                       beta = c(A = 0.12, B = 0.06), V_init = c(A = 0.2, B = 0.4)),
              c(A = 1 - (1 - 0.2) * (1 - 0.12)^3, B = 0.4))

  check_close("a cue with learning rate 0 does not learn",
              rw_final(make_trials("A", lambda = 1, n = 5, "A+"), beta = c(A = 0),
                       V_init = c(A = 0.35)),
              c(A = 0.35))

  # The order in which the cues of a compound are listed does not matter
  # (Section 2.3).
  ordered_ab <- rw_final(make_trials(c("A", "B"), lambda = 0.9, n = 1, "AB"),
                         beta = c(A = 0.12, B = 0.075), V_init = c(A = 0.2, B = 0.1))
  ordered_ba <- rw_final(make_trials(c("B", "A"), lambda = 0.9, n = 1, "BA"),
                         beta = c(A = 0.12, B = 0.075), V_init = c(A = 0.2, B = 0.1))
  check_close("the order in which compound cues are listed does not matter",
              sort(ordered_ab), sort(ordered_ba))

  # rw_final(), whose loop is printed in Section 2.3, and rw_simulate(), written
  # separately to record every trial, agree over longer sequences of different
  # kinds.
  agreement_cases <- list(
    acquisition = list(
      design = make_trials("A", lambda = 1, n = 10, "acquisition"),
      beta = c(A = 0.12), V_init = NULL
    ),
    blocking = list(
      design = c(make_trials("A", lambda = 1, n = 12, "single cue"),
                 make_trials(c("A", "B"), lambda = 1, n = 8, "compound")),
      beta = c(A = 0.12, B = 0.06), V_init = NULL
    ),
    extinction = list(
      design = c(make_trials(c("A", "B"), lambda = 1, n = 8, "acquisition"),
                 make_trials(c("A", "B"), lambda = 0, n = 4, "extinction")),
      beta = c(A = 0.12, B = 0.06), V_init = c(A = 0.1, B = 0.2)
    )
  )
  for (case_name in names(agreement_cases)) {
    case <- agreement_cases[[case_name]]
    simulated <- extract_final_V(rw_simulate(case$design, beta = case$beta,
                                             V_init = case$V_init))
    final <- rw_final(case$design, beta = case$beta, V_init = case$V_init)
    check_close(sprintf("rw_simulate() and rw_final() agree: %s", case_name),
                sort(simulated), sort(final))
  }

  # Adaptive model (Equations 2 and 3) with the settings of Section 3.2. The
  # recursion below is written from the two equations, separately from
  # R/models.R, for trials followed by the outcome (lambda = 1) with one or two
  # cues: each cue present learns from its own error, and its associability
  # changes by eta times the other cue's error minus its own, with |lambda| = 1
  # as the other error for a cue presented alone, and stays within [.01, 1].
  recursion <- function(trials, S, alpha, V = setNames(numeric(length(alpha)), names(alpha)),
                        eta = 0.05) {
    for (cues in trials) {
      error <- 1 - V[cues]
      other_error <- if (length(cues) == 2) rev(abs(error)) else 1
      V[cues] <- V[cues] + S[cues] * alpha[cues] * error
      alpha[cues] <- pmin(1, pmax(0.01, alpha[cues] + eta * (other_error - abs(error))))
    }
    list(V = V, alpha = alpha)
  }
  blocking_design <- c(make_trials("A", lambda = 1, n = 10, "Phase 1"),
                       make_trials(c("A", "B"), lambda = 1, n = 10, "Phase 2"))
  control_design <- c(make_trials("X", lambda = 1, n = 10, "Phase 1"),
                      make_trials(c("C", "D"), lambda = 1, n = 10, "Phase 2"))
  adaptive_blocked <- adaptive_simulate(blocking_design, S = c(A = 0.3, B = 0.3),
                                        alpha_init = c(A = 0.5, B = 0.5))
  adaptive_control <- adaptive_simulate(control_design, S = c(X = 0.3, C = 0.3, D = 0.3),
                                        alpha_init = c(X = 0.5, C = 0.5, D = 0.5))
  blocked_V <- extract_final_V(adaptive_blocked)
  blocked_alpha <- extract_final_alpha(adaptive_blocked)
  control_V <- extract_final_V(adaptive_control)
  control_alpha <- extract_final_alpha(adaptive_control)
  blocked_by_hand <- recursion(c(rep(list("A"), 10), rep(list(c("A", "B")), 10)),
                               S = c(A = 0.3, B = 0.3), alpha = c(A = 0.5, B = 0.5))
  control_by_hand <- recursion(c(rep(list("X"), 10), rep(list(c("C", "D")), 10)),
                               S = c(X = 0.3, C = 0.3, D = 0.3),
                               alpha = c(X = 0.5, C = 0.5, D = 0.5))
  check_close("tone A, then A with B: adaptive_simulate() follows Equations 2 and 3",
              c(blocked_V[c("A", "B")], blocked_alpha[c("A", "B")]),
              c(blocked_by_hand$V, blocked_by_hand$alpha))
  check_close("buzzer X, then C with D: adaptive_simulate() follows Equations 2 and 3",
              c(control_V[c("X", "C", "D")], control_alpha[c("X", "C", "D")]),
              c(control_by_hand$V, control_by_hand$alpha))
  check_close("C and D make equal errors and keep associability .5",
              control_alpha[c("C", "D")], c(C = 0.5, D = 0.5))
  check_close("C and D then learn with the fixed learning rate .3 * .5",
              control_V[c("C", "D")],
              c(C = 1 - (1 - 0.3 * 0.5)^10, D = 1 - (1 - 0.3 * 0.5)^10))
  check_true("B ends the compound trials with lower value and associability than D",
             blocked_V["B"] < control_V["D"] && blocked_alpha["B"] < control_alpha["D"])

  # adaptive_final_state(), whose loop is printed in Section 3.3, and
  # adaptive_simulate(), written separately, agree.
  adaptive_final <- adaptive_final_state(blocking_design, S = c(A = 0.3, B = 0.3),
                                         alpha_init = c(A = 0.5, B = 0.5))
  check_close("adaptive_simulate() and adaptive_final_state() agree on V",
              sort(blocked_V), sort(adaptive_final$V))
  check_close("adaptive_simulate() and adaptive_final_state() agree on alpha",
              sort(blocked_alpha), sort(adaptive_final$alpha))

  # The order of the updates (Sections 3.2 and 3.3). On the first compound
  # trial, B gains .3 * .5 * 1 = .150, and its associability changes by
  # eta (e_A - e_B) = .05 ((1 - V_A) - 1), to .458. A program that applied the
  # new associability at once would give B only .3 * .458 = .137. On the second
  # compound trial, B and D both have the value .150 and the error .85, but B
  # gains .3 * .458 * .85, about .117, and D .3 * .5 * .85, about .128.
  first_two <- adaptive_simulate(c(make_trials("A", lambda = 1, n = 10, "Phase 1"),
                                   make_trials(c("A", "B"), lambda = 1, n = 2, "Phase 2")),
                                 S = c(A = 0.3, B = 0.3), alpha_init = c(A = 0.5, B = 0.5))
  B_rows <- first_two[first_two$cue == "B", ]
  V_A10 <- first_two$V[first_two$cue == "A" & first_two$trial == 11]
  alpha_B1 <- 0.5 + 0.05 * ((1 - V_A10) - 1)
  check_close("first compound trial: B gains .150, not .3 times its new associability",
              B_rows$V[12], 0.15)
  check_true("first compound trial: B's new associability rounds to .458, which would give .137",
             sprintf("%.3f", alpha_B1) == "0.458" && sprintf("%.3f", 0.3 * alpha_B1) == "0.137")
  check_close("first compound trial: alpha_B = .5 + eta (e_A - e_B)", B_rows$alpha[12], alpha_B1)
  check_close("second compound trial: B gains .3 alpha_B (1 - .15)",
              B_rows$V[13] - B_rows$V[12], 0.3 * alpha_B1 * 0.85)
  control_two <- adaptive_simulate(c(make_trials("X", lambda = 1, n = 10, "Phase 1"),
                                     make_trials(c("C", "D"), lambda = 1, n = 2, "Phase 2")),
                                   S = c(X = 0.3, C = 0.3, D = 0.3),
                                   alpha_init = c(X = 0.5, C = 0.5, D = 0.5))
  D_rows <- control_two[control_two$cue == "D", ]
  check_close("first two compound trials: D gains .150, then .3 * .5 * .85",
              diff(D_rows$V[11:13]), c(0.15, 0.3 * 0.5 * 0.85))

  # Equation 3 is continuous in the errors: when C is slightly more salient
  # than D, D becomes the poorer predictor and loses associability in
  # proportion to the salience difference (Section 4.4).
  control_alpha_D <- function(S_C) {
    unname(adaptive_final_state(control_design, S = c(X = 0.3, C = S_C, D = 0.3),
                                alpha_init = c(X = 0.5, C = 0.5, D = 0.5))$alpha["D"])
  }
  loss_1e6 <- 0.5 - control_alpha_D(0.3 + 1e-6)
  loss_2e6 <- 0.5 - control_alpha_D(0.3 + 2e-6)
  check_true("C more salient by 1e-6: D's associability falls, by less than 1e-5",
             loss_1e6 > 0 && loss_1e6 < 1e-5)
  check_close("twice the salience difference doubles D's loss of associability",
              loss_2e6 / loss_1e6, 2, tolerance = 1e-4)
  check_true("C less salient by 1e-6: D's associability rises above .5",
             control_alpha_D(0.3 - 1e-6) > 0.5)

  # The bounds of Equation 3: with fast associability change, A's
  # associability stops at 1 during the A+ trials and B's at .01 on the second
  # compound trial.
  fast <- adaptive_final_state(blocking_design, S = c(A = 0.3, B = 0.3),
                               alpha_init = c(A = 0.5, B = 0.5), eta = 0.5)
  check_close("with eta = .5, A's associability ends at 1 and B's at .01 (adaptive_final_state)",
              unname(fast$alpha[c("A", "B")]), c(1, 0.01))
  fast_run <- adaptive_simulate(blocking_design, S = c(A = 0.3, B = 0.3),
                                alpha_init = c(A = 0.5, B = 0.5), eta = 0.5)
  check_close("with eta = .5, A's associability ends at 1 and B's at .01 (adaptive_simulate)",
              unname(extract_final_alpha(fast_run)[c("A", "B")]), c(1, 0.01))

  # Trials without the outcome, on which lambda - V is negative. From V_A = .4
  # and V_B = .2, one AB- trial gives the errors .4 and .2 in size, so A's
  # associability falls by eta * .2 and B's rises by as much. One A- trial
  # compares A's error, .4, with |lambda| = 0, so A's associability falls by
  # eta * .4.
  AB_minus <- make_trials(c("A", "B"), lambda = 0, n = 1, "AB-")
  A_minus <- make_trials("A", lambda = 0, n = 1, "A-")
  check_close("one AB- and one A- trial: associability follows the sizes of the errors (adaptive_simulate)",
              c(extract_final_alpha(adaptive_simulate(AB_minus, S = c(A = 0.3, B = 0.3),
                                                      alpha_init = c(A = 0.5, B = 0.5),
                                                      V_init = c(A = 0.4, B = 0.2))),
                extract_final_alpha(adaptive_simulate(A_minus, S = c(A = 0.3),
                                                      alpha_init = c(A = 0.5),
                                                      V_init = c(A = 0.4)))),
              c(A = 0.49, B = 0.51, A = 0.48))
  check_close("one AB- and one A- trial: associability follows the sizes of the errors (adaptive_final_state)",
              c(adaptive_final_state(AB_minus, S = c(A = 0.3, B = 0.3),
                                     alpha_init = c(A = 0.5, B = 0.5),
                                     V_init = c(A = 0.4, B = 0.2))$alpha,
                adaptive_final_state(A_minus, S = c(A = 0.3), alpha_init = c(A = 0.5),
                                     V_init = c(A = 0.4))$alpha),
              c(A = 0.49, B = 0.51, A = 0.48))

  # The pear trials (Section 3.4). B and D are each presented alone and start at
  # value zero; under the adaptive model they keep the associabilities alpha_B
  # and alpha_D of the compound trials. Under Rescorla-Wagner they learn
  # identically; under the adaptive model the gap after the first pear trial is
  # S (alpha_D - alpha_B).
  alpha_B <- unname(blocked_alpha["B"])
  alpha_D <- unname(control_alpha["D"])
  rw_alone <- function(cue, beta, n, V_init = 0) {
    rw_simulate(make_trials(cue, lambda = 1, n = n, "Pear"),
                beta = setNames(beta, cue),
                V_init = setNames(V_init, cue), cue_names = cue)
  }
  adaptive_alone <- function(cue, alpha, n, ...) {
    adaptive_simulate(make_trials(cue, lambda = 1, n = n, "Pear"),
                      S = setNames(0.3, cue), alpha_init = setNames(alpha, cue),
                      V_init = setNames(0, cue), cue_names = cue, ...)
  }

  check_close("Rescorla-Wagner: B and D both follow 1 - (1 - .12)^k on the pear trials",
              c(rw_alone("B", 0.12, 10)$V, rw_alone("D", 0.12, 10)$V),
              rep(1 - (1 - 0.12)^(0:10), 2))

  first_pear_gap <- final_value(adaptive_alone("D", alpha_D, 1), "D") -
    final_value(adaptive_alone("B", alpha_B, 1), "B")
  expected_first_pear_gap <- 0.3 * 1 * (alpha_D - alpha_B)
  check_close("adaptive model: the gap after the first pear trial is S (alpha_D - alpha_B)",
              first_pear_gap, expected_first_pear_gap)
  check_true("adaptive model: B gains less than D on the first pear trial",
             first_pear_gap > 0)

  # Section 4.1. Prevent: holding associability fixed during the compound
  # trials leaves B and D at .5. Supply: a Rescorla-Wagner learner given the
  # adaptive model's starting learning rates, S alpha_B and S alpha_D, has the
  # same gap after the first pear trial.
  prevent_phase1_B <- adaptive_simulate(make_trials("A", lambda = 1, n = 10, "Phase 1"),
                                        S = c(A = 0.3, B = 0.3),
                                        alpha_init = c(A = 0.5, B = 0.5),
                                        cue_names = c("A", "B"))
  prevent_phase1_D <- adaptive_simulate(make_trials("X", lambda = 1, n = 10, "Phase 1"),
                                        S = c(X = 0.3, C = 0.3, D = 0.3),
                                        alpha_init = c(X = 0.5, C = 0.5, D = 0.5),
                                        cue_names = c("X", "C", "D"))
  prevent_B <- adaptive_simulate(make_trials(c("A", "B"), lambda = 1, n = 10, "Phase 2"),
                                 S = c(A = 0.3, B = 0.3),
                                 alpha_init = extract_final_alpha(prevent_phase1_B),
                                 eta = 0,
                                 V_init = extract_final_V(prevent_phase1_B),
                                 cue_names = c("A", "B"))
  prevent_D <- adaptive_simulate(make_trials(c("C", "D"), lambda = 1, n = 10, "Phase 2"),
                                 S = c(X = 0.3, C = 0.3, D = 0.3),
                                 alpha_init = extract_final_alpha(prevent_phase1_D),
                                 eta = 0,
                                 V_init = extract_final_V(prevent_phase1_D),
                                 cue_names = c("X", "C", "D"))
  check_close("Prevent: B and D end the compound trials with associability .5",
              c(unname(extract_final_alpha(prevent_B)["B"]),
                unname(extract_final_alpha(prevent_D)["D"])),
              c(0.5, 0.5))

  supply_gap <- rw_alone("D", 0.3 * alpha_D, 10)$V - rw_alone("B", 0.3 * alpha_B, 10)$V
  check_close("Supply: the gap after the first pear trial equals the adaptive model's",
              supply_gap[2], expected_first_pear_gap)
  check_true("Supply: D stays ahead of B over ten pear trials", all(supply_gap[-1] > 0))

  # Section 4.2. Under Rescorla-Wagner with a fraction g_V of the learned
  # values for the apple carried over (Equation 5), the gap after k pear trials
  # is g_V (V_D - V_B) (1 - .12)^k. Under the adaptive model with a fraction
  # g_alpha of the change in associability carried over (Equation 4), the gap
  # after the first pear trial is .3 g_alpha (alpha_D - alpha_B). Any g_V, k and
  # g_alpha would do.
  rw_apple_B <- final_value(rw_simulate(blocking_design, beta = c(A = 0.12, B = 0.12)), "B")
  rw_apple_D <- final_value(rw_simulate(control_design, beta = c(X = 0.12, C = 0.12, D = 0.12)),
                            "D")
  g_V <- 0.6
  k <- 5
  check_close("Rescorla-Wagner with carryover of learned value: the gap shrinks by 12% per pear trial",
              final_value(rw_alone("D", 0.12, k, V_init = g_V * rw_apple_D), "D") -
                final_value(rw_alone("B", 0.12, k, V_init = g_V * rw_apple_B), "B"),
              g_V * (rw_apple_D - rw_apple_B) * (1 - 0.12)^k)
  g_alpha <- 0.4
  alpha_init <- 0.5
  carried_B <- adaptive_alone("B", alpha_init + g_alpha * (alpha_B - alpha_init), 1)
  carried_D <- adaptive_alone("D", alpha_init + g_alpha * (alpha_D - alpha_init), 1)
  check_close("adaptive model with associability carryover: the first gap is .3 g_alpha (alpha_D - alpha_B)",
              final_value(carried_D, "D") - final_value(carried_B, "B"),
              0.3 * g_alpha * (alpha_D - alpha_B))

  # Section 4.3 and Supplement S4.1 vary the compound trials from 1 to 20 and
  # eta from .01 to .05, 100 settings in all. In every setting,
  # adaptive_final_state() follows the recursion written from Equations 2 and
  # 3, B's associability falls on every compound trial and stays above the
  # lower bound of .01, D's stays at .5, and the B-D gap is positive after
  # every one of fifteen pear trials, as Supplement S4.1 shows in general.
  grid <- expand.grid(n = 1:20, eta = seq(0.01, 0.05, by = 0.01))
  blocked_state <- function(n, eta) {
    adaptive_final_state(c(make_trials("A", lambda = 1, n = 10, "Phase 1"),
                           make_trials(c("A", "B"), lambda = 1, n = n, "Phase 2")),
                         S = c(A = 0.3, B = 0.3), alpha_init = c(A = 0.5, B = 0.5), eta = eta)
  }
  grid_states <- mapply(function(n, eta) unlist(blocked_state(n, eta)), grid$n, grid$eta)
  grid_by_hand <- mapply(function(n, eta) {
    unlist(recursion(c(rep(list("A"), 10), rep(list(c("A", "B")), n)),
                     S = c(A = 0.3, B = 0.3), alpha = c(A = 0.5, B = 0.5), eta = eta))
  }, grid$n, grid$eta)
  check_close("in all 100 settings, adaptive_final_state() follows Equations 2 and 3",
              grid_states, grid_by_hand)
  check_true("in all 100 settings, B's associability stays above the lower bound of .01",
             all(grid_states["alpha.B", ] > 0.01))
  conditions_hold <- mapply(function(n, eta) {
    blocked <- adaptive_simulate(c(make_trials("A", lambda = 1, n = 10, "Phase 1"),
                                   make_trials(c("A", "B"), lambda = 1, n = n, "Phase 2")),
                                 S = c(A = 0.3, B = 0.3), alpha_init = c(A = 0.5, B = 0.5),
                                 eta = eta)
    control <- adaptive_simulate(c(make_trials("X", lambda = 1, n = 10, "Phase 1"),
                                   make_trials(c("C", "D"), lambda = 1, n = n, "Phase 2")),
                                 S = c(X = 0.3, C = 0.3, D = 0.3),
                                 alpha_init = c(X = 0.5, C = 0.5, D = 0.5), eta = eta)
    alpha_B <- blocked$alpha[blocked$cue == "B" & blocked$phase == "Phase 2"]
    length(alpha_B) == n + 1 && all(diff(alpha_B) < 0) &&
      all(control$alpha[control$cue == "D"] == 0.5)
  }, grid$n, grid$eta)
  check_true("in all 100 settings, B's associability falls on every compound trial and D's stays at .5",
             all(conditions_hold))
  gap_positive <- mapply(function(n, eta) {
    gap <- adaptive_alone("D", 0.5, 15, eta = eta)$V -
      adaptive_alone("B", unname(blocked_state(n, eta)$alpha["B"]), 15, eta = eta)$V
    all(gap[-1] > 0)
  }, grid$n, grid$eta)
  check_true("in all 100 settings, the B-D gap is positive after every pear trial",
             all(gap_positive))

  # A light presented alone (Supplement S4.1): its error is compared with
  # |lambda| = 1, so Equation 3 becomes alpha(k + 1) = min(1, alpha(k) + eta V(k))
  # while V(k) lies in [0, 1]. The second case reaches the upper bound.
  alone_by_hand <- function(alpha, n, eta) {
    V <- a <- numeric(n + 1)
    a[1] <- alpha
    for (j in seq_len(n)) {
      V[j + 1] <- V[j] + 0.3 * a[j] * (1 - V[j])
      a[j + 1] <- min(1, a[j] + eta * V[j])
    }
    c(V, a)
  }
  for (case in list(c(alpha = alpha_B, eta = 0.05), c(alpha = 0.9, eta = 0.2))) {
    alone <- adaptive_alone("B", case[["alpha"]], 15, eta = case[["eta"]])
    check_close(sprintf("a light alone follows alpha(k + 1) = min(1, alpha(k) + eta V(k)) (alpha(0) = %s, eta = %s)",
                        sub("^0", "", sprintf("%.3f", case[["alpha"]])),
                        sub("^0", "", sprintf("%.2f", case[["eta"]]))),
                c(alone$V, alone$alpha), alone_by_hand(case[["alpha"]], 15, case[["eta"]]))
  }

  # Response functions (Section 5.2). The sigmoid of Table 6 (gamma = 8,
  # theta = .5) keeps D's yes probability above B's after every pear trial; the
  # threshold rule at .5 separates them only after five to eight pear trials.
  pear_B <- adaptive_alone("B", alpha_B, 10)
  pear_D <- adaptive_alone("D", alpha_D, 10)
  sigmoid_difference <- obs_sigmoid(pear_D$V, gamma = 8, theta = 0.5) -
    obs_sigmoid(pear_B$V, gamma = 8, theta = 0.5)
  check_true("the sigmoid keeps D's yes probability above B's after every pear trial",
             all(sigmoid_difference[-1] > 0))

  threshold_difference <- obs_threshold(pear_D$V, theta = 0.5) -
    obs_threshold(pear_B$V, theta = 0.5)
  completed <- pear_B$trial - 1L
  check_close("the threshold rule separates B and D only after five to eight pear trials",
              completed[threshold_difference != 0], 5:8)

  cat(sprintf("All %d model checks passed.\n", n_checks))
})
