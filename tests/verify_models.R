# Checks of R/models.R against hand calculations and closed-form results:
# the checks described in Section 2.2, and the results of Sections 3 and 4
# and Supplement S4.1 that follow from the equations. Stops at the first
# failure.
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

  # Section 2.2. A single reinforced cue follows the closed form of Equation 3,
  # V_n = lambda - (lambda - V_0) (1 - alpha beta)^n.
  single_cue <- rw_simulate(make_trials("A", lambda = 1, n = 10, "single cue"),
                            alpha = c(A = 0.4), beta = 0.3)
  check_close("a single cue follows the closed form of Equation 3",
              final_value(single_cue, "A"), 1 - (1 - 0.4 * 0.3)^10)

  # One reinforced AB trial from zero gives both cues .4 * .3 * 1 = .12. A
  # program that updated A first and used its new value for B's error would give
  # B only .12 * .88 = .1056.
  one_compound <- make_trials(c("A", "B"), lambda = 1, n = 1, "compound")
  check_close("one compound trial from zero: both cues gain .12 (rw_simulate)",
              extract_final_V(rw_simulate(one_compound, alpha = c(A = 0.4, B = 0.4), beta = 0.3)),
              c(A = 0.12, B = 0.12))
  check_close("one compound trial from zero: both cues gain .12 (rw_final)",
              rw_final(one_compound, alpha = c(A = 0.4, B = 0.4), beta = 0.3),
              c(A = 0.12, B = 0.12))

  # After A+, A = .12 and B = 0. On the following AB+ trial the shared error is
  # .88, so both cues gain .4 * .3 * .88 = .1056. A program that gave each cue its
  # own error would give B .12, and one that updated A first would give B
  # .12 * .7744 = .092928.
  hand_design <- c(make_trials("A", lambda = 1, n = 1, "single cue"),
                   make_trials(c("A", "B"), lambda = 1, n = 1, "compound"))
  check_close("A+ then AB+: both cues gain .1056 on the compound trial (rw_simulate)",
              extract_final_V(rw_simulate(hand_design, alpha = c(A = 0.4, B = 0.4), beta = 0.3)),
              c(A = 0.2256, B = 0.1056))
  check_close("A+ then AB+: both cues gain .1056 on the compound trial (rw_final)",
              rw_final(hand_design, alpha = c(A = 0.4, B = 0.4), beta = 0.3),
              c(A = 0.2256, B = 0.1056))
  trace_output <- capture.output(
    hand_state <- rw_hand_trace(hand_design, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                                n_trace = 2)
  )
  check_close("A+ then AB+ agrees with the hand calculation",
              hand_state[c("A", "B")], c(A = 0.2256, B = 0.1056))
  check_true("rw_hand_trace() prints the compound trial",
             any(grepl("Trial 2", trace_output, fixed = TRUE)) &&
               any(grepl("cues = {A, B}", trace_output, fixed = TRUE)))
  # The compound check of Section 2.2: from A = .5 and B = .25, one AB+ trial has
  # the shared error 1 - .75 = .25, so both cues gain .4 * .3 * .25 = .03. A
  # program that gave B its own error would give B .09, one that recalculated the
  # error after updating A would give B .0264, and one that left B out of the
  # prediction would use the error .5.
  check_close("both values enter the combined prediction (rw_simulate)",
              extract_final_V(rw_simulate(one_compound, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                                          V_init = c(A = 0.5, B = 0.25))),
              c(A = 0.53, B = 0.28))
  check_close("both values enter the combined prediction (rw_final)",
              rw_final(one_compound, alpha = c(A = 0.4, B = 0.4), beta = 0.3,
                       V_init = c(A = 0.5, B = 0.25)),
              c(A = 0.53, B = 0.28))

  # Absent cues neither change nor enter the prediction (Section 2.2). If B's
  # value (.4) entered A's error, A would not follow the single-cue closed form
  # from its start of .2.
  B_absent <- rw_simulate(make_trials("A", lambda = 1, n = 3, "A only"),
                          alpha = c(A = 0.4, B = 0.2), beta = 0.3,
                          V_init = c(A = 0.2, B = 0.4), cue_names = c("A", "B"))
  check_close("an absent cue does not change", final_value(B_absent, "B"), 0.4)
  check_close("an absent cue does not enter the prediction",
              final_value(B_absent, "A"), 1 - (1 - 0.2) * (1 - 0.4 * 0.3)^3)
  check_close("an absent cue neither changes nor enters the prediction (rw_final)",
              rw_final(make_trials("A", lambda = 1, n = 3, "A only"),
                       alpha = c(A = 0.4, B = 0.2), beta = 0.3, V_init = c(A = 0.2, B = 0.4)),
              c(A = 1 - (1 - 0.2) * (1 - 0.4 * 0.3)^3, B = 0.4))

  alpha_zero <- rw_final(make_trials("A", lambda = 1, n = 5, "alpha zero"),
                         alpha = c(A = 0), beta = 0.3, V_init = c(A = 0.35))
  check_close("a cue with alpha = 0 does not learn", alpha_zero, c(A = 0.35))

  beta_zero <- rw_final(make_trials(c("A", "B"), lambda = 1, n = 5, "beta zero"),
                        alpha = c(A = 0.4, B = 0.2), beta = 0,
                        V_init = c(A = 0.25, B = 0.15))
  check_close("no cue learns when beta = 0", beta_zero, c(A = 0.25, B = 0.15))

  ordered_ab <- rw_final(make_trials(c("A", "B"), lambda = 0.9, n = 1, "AB"),
                         alpha = c(A = 0.4, B = 0.25), beta = 0.3,
                         V_init = c(A = 0.2, B = 0.1))
  ordered_ba <- rw_final(make_trials(c("B", "A"), lambda = 0.9, n = 1, "BA"),
                         alpha = c(A = 0.4, B = 0.25), beta = 0.3,
                         V_init = c(A = 0.2, B = 0.1))
  check_close("the order in which compound cues are listed does not matter",
              sort(ordered_ab), sort(ordered_ba))

  # rw_final(), whose loop is printed in Section 2.2, and rw_simulate(), written
  # separately to record every trial, agree on runs of different kinds.
  agreement_cases <- list(
    acquisition = list(
      design = make_trials("A", lambda = 1, n = 10, "acquisition"),
      alpha = c(A = 0.4), beta = 0.3, V_init = NULL
    ),
    blocking = list(
      design = c(make_trials("A", lambda = 1, n = 12, "single cue"),
                 make_trials(c("A", "B"), lambda = 1, n = 8, "compound")),
      alpha = c(A = 0.4, B = 0.2), beta = 0.3, V_init = NULL
    ),
    extinction = list(
      design = c(make_trials(c("A", "B"), lambda = 1, n = 8, "acquisition"),
                 make_trials(c("A", "B"), lambda = 0, n = 4, "extinction")),
      alpha = c(A = 0.4, B = 0.2), beta = 0.3, V_init = c(A = 0.1, B = 0.2)
    )
  )
  for (case_name in names(agreement_cases)) {
    case <- agreement_cases[[case_name]]
    simulated <- extract_final_V(rw_simulate(case$design, alpha = case$alpha,
                                             beta = case$beta, V_init = case$V_init))
    final <- rw_final(case$design, alpha = case$alpha, beta = case$beta,
                      V_init = case$V_init)
    check_close(sprintf("rw_simulate() and rw_final() agree: %s", case_name),
                sort(simulated), sort(final))
  }

  # Adaptive model (Equations 7 and 8) with the settings of Section 3.2. The
  # recursion below is written from the two equations, separately from
  # R/models.R, for reinforced trials (lambda = 1): each cue present learns
  # from its own error, and its associability changes by eta times the other
  # cue's absolute error minus its own, with |lambda| = 1 as the other error
  # for a cue presented alone, and stays within [.01, 1].
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
  control_design <- c(make_trials("C", lambda = 1, n = 10, "Phase 1"),
                      make_trials(c("D", "E"), lambda = 1, n = 10, "Phase 2"))
  adaptive_blocked <- adaptive_simulate(blocking_design, S = c(A = 0.3, B = 0.3),
                                        alpha_init = c(A = 0.5, B = 0.5))
  adaptive_control <- adaptive_simulate(control_design, S = c(C = 0.3, D = 0.3, E = 0.3),
                                        alpha_init = c(C = 0.5, D = 0.5, E = 0.5))
  blocked_V <- extract_final_V(adaptive_blocked)
  blocked_alpha <- extract_final_alpha(adaptive_blocked)
  control_V <- extract_final_V(adaptive_control)
  control_alpha <- extract_final_alpha(adaptive_control)
  blocked_by_hand <- recursion(c(rep(list("A"), 10), rep(list(c("A", "B")), 10)),
                               S = c(A = 0.3, B = 0.3), alpha = c(A = 0.5, B = 0.5))
  control_by_hand <- recursion(c(rep(list("C"), 10), rep(list(c("D", "E")), 10)),
                               S = c(C = 0.3, D = 0.3, E = 0.3),
                               alpha = c(C = 0.5, D = 0.5, E = 0.5))
  check_close("blocking condition: adaptive_simulate() follows Equations 7 and 8",
              c(blocked_V[c("A", "B")], blocked_alpha[c("A", "B")]),
              c(blocked_by_hand$V, blocked_by_hand$alpha))
  check_close("control condition: adaptive_simulate() follows Equations 7 and 8",
              c(control_V[c("C", "D", "E")], control_alpha[c("C", "D", "E")]),
              c(control_by_hand$V, control_by_hand$alpha))
  check_close("control cues D and E make equal errors and keep associability .5",
              control_alpha[c("D", "E")], c(D = 0.5, E = 0.5))
  check_close("D and E then follow Equation 3 with multiplier .3 * .5",
              control_V[c("D", "E")],
              c(D = 1 - (1 - 0.3 * 0.5)^10, E = 1 - (1 - 0.3 * 0.5)^10))
  check_true("B ends compound training with lower value and associability than D",
             blocked_V["B"] < control_V["D"] && blocked_alpha["B"] < control_alpha["D"])

  # The first two compound trials of Section 3.2, from A's value after ten A+
  # trials: on the first, B and D gain .3 * .5 * 1, and B's associability
  # changes by eta (e_A - e_B) = .05 ((1 - V_A) - 1); on the second, B and D
  # have the same error, 1 - .15, but no longer the same associability.
  first_two <- adaptive_simulate(c(make_trials("A", lambda = 1, n = 10, "Phase 1"),
                                   make_trials(c("A", "B"), lambda = 1, n = 2, "Phase 2")),
                                 S = c(A = 0.3, B = 0.3), alpha_init = c(A = 0.5, B = 0.5))
  B_rows <- first_two[first_two$cue == "B", ]
  V_A10 <- first_two$V[first_two$cue == "A" & first_two$trial == 11]
  alpha_B1 <- 0.5 + 0.05 * ((1 - V_A10) - 1)
  check_close("first compound trial: B gains .3 * .5 * 1 = .15", B_rows$V[12], 0.15)
  check_close("first compound trial: alpha_B = .5 + eta (e_A - e_B)", B_rows$alpha[12], alpha_B1)
  check_close("second compound trial: B gains .3 alpha_B (1 - .15)",
              B_rows$V[13] - B_rows$V[12], 0.3 * alpha_B1 * 0.85)

  # Equation 8 is continuous in the errors: when E is slightly more salient
  # than D, D becomes the poorer predictor and loses associability in
  # proportion to the salience difference.
  control_alpha_D <- function(S_E) {
    unname(adaptive_final_state(control_design, S = c(C = 0.3, D = 0.3, E = S_E),
                                alpha_init = c(C = 0.5, D = 0.5, E = 0.5))$alpha["D"])
  }
  loss_1e6 <- 0.5 - control_alpha_D(0.3 + 1e-6)
  loss_2e6 <- 0.5 - control_alpha_D(0.3 + 2e-6)
  check_true("E more salient by 1e-6: D's associability falls, by less than 1e-5",
             loss_1e6 > 0 && loss_1e6 < 1e-5)
  check_close("twice the salience difference doubles D's loss of associability",
              loss_2e6 / loss_1e6, 2, tolerance = 1e-4)
  check_true("E less salient by 1e-6: D's associability rises above .5",
             control_alpha_D(0.3 - 1e-6) > 0.5)

  # The bounds of Equation 8: with fast associability change, A's
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

  # Nonreinforced trials, on which lambda - V is negative. From V_A = .4 and
  # V_B = .2, one AB- trial gives absolute errors .4 and .2, so A's
  # associability falls by eta * .2 and B's rises by as much. One A- trial
  # compares A's error, .4, with |lambda| = 0, so A's associability falls by
  # eta * .4.
  AB_minus <- make_trials(c("A", "B"), lambda = 0, n = 1, "AB-")
  A_minus <- make_trials("A", lambda = 0, n = 1, "A-")
  check_close("one AB- and one A- trial: associability follows the absolute errors (adaptive_simulate)",
              c(extract_final_alpha(adaptive_simulate(AB_minus, S = c(A = 0.3, B = 0.3),
                                                      alpha_init = c(A = 0.5, B = 0.5),
                                                      V_init = c(A = 0.4, B = 0.2))),
                extract_final_alpha(adaptive_simulate(A_minus, S = c(A = 0.3),
                                                      alpha_init = c(A = 0.5),
                                                      V_init = c(A = 0.4)))),
              c(A = 0.49, B = 0.51, A = 0.48))
  check_close("one AB- and one A- trial: associability follows the absolute errors (adaptive_final_state)",
              c(adaptive_final_state(AB_minus, S = c(A = 0.3, B = 0.3),
                                     alpha_init = c(A = 0.5, B = 0.5),
                                     V_init = c(A = 0.4, B = 0.2))$alpha,
                adaptive_final_state(A_minus, S = c(A = 0.3), alpha_init = c(A = 0.5),
                                     V_init = c(A = 0.4))$alpha),
              c(A = 0.49, B = 0.51, A = 0.48))

  adaptive_final <- adaptive_final_state(blocking_design, S = c(A = 0.3, B = 0.3),
                                         alpha_init = c(A = 0.5, B = 0.5))
  check_close("adaptive_simulate() and adaptive_final_state() agree on V",
              sort(blocked_V), sort(adaptive_final$V))
  check_close("adaptive_simulate() and adaptive_final_state() agree on alpha",
              sort(blocked_alpha), sort(adaptive_final$alpha))

  # The new outcome (Section 3.3). B and D are each trained alone and start at
  # value zero; under the adaptive model they keep the associabilities alpha_B
  # and alpha_D of compound training. Under Rescorla-Wagner they learn
  # identically (Equation 9); under the adaptive model the first-update gap is
  # S (alpha_D - alpha_B) (Equations 10 and 12).
  alpha_B <- unname(blocked_alpha["B"])
  alpha_D <- unname(control_alpha["D"])
  rw_alone <- function(cue, alpha, n, V_init = 0) {
    rw_simulate(make_trials(cue, lambda = 1, n = n, "Phase 3"),
                alpha = setNames(alpha, cue), beta = 0.3,
                V_init = setNames(V_init, cue), cue_names = cue)
  }
  adaptive_alone <- function(cue, alpha, n, ...) {
    adaptive_simulate(make_trials(cue, lambda = 1, n = n, "Phase 3"),
                      S = setNames(0.3, cue), alpha_init = setNames(alpha, cue),
                      V_init = setNames(0, cue), cue_names = cue, ...)
  }

  check_close("Rescorla-Wagner: B and D both follow 1 - (1 - .12)^k with the new outcome (Equation 9)",
              c(rw_alone("B", 0.4, 10)$V, rw_alone("D", 0.4, 10)$V),
              rep(1 - (1 - 0.4 * 0.3)^(0:10), 2))

  first_update_gap <- final_value(adaptive_alone("D", alpha_D, 1), "D") -
    final_value(adaptive_alone("B", alpha_B, 1), "B")
  expected_first_update_gap <- 0.3 * 1 * (alpha_D - alpha_B)
  check_close("adaptive model: the first-update gap is S (alpha_D - alpha_B) (Equation 12)",
              first_update_gap, expected_first_update_gap)
  check_true("adaptive model: B gains less than D on the first update (Equation 10)",
             first_update_gap > 0)

  # Section 4.1. Prevent: holding associability fixed during compound training
  # leaves B and D at .5. Supply: a Rescorla-Wagner learner given the adaptive
  # model's starting multipliers has the same first-update gap. Its saliences
  # are alpha_B and alpha_D and its rate is beta = .3 = S, so its multipliers
  # are S alpha_B and S alpha_D.
  prevent_phase1_B <- adaptive_simulate(make_trials("A", lambda = 1, n = 10, "Phase 1"),
                                        S = c(A = 0.3, B = 0.3),
                                        alpha_init = c(A = 0.5, B = 0.5),
                                        cue_names = c("A", "B"))
  prevent_phase1_D <- adaptive_simulate(make_trials("C", lambda = 1, n = 10, "Phase 1"),
                                        S = c(C = 0.3, D = 0.3, E = 0.3),
                                        alpha_init = c(C = 0.5, D = 0.5, E = 0.5),
                                        cue_names = c("C", "D", "E"))
  prevent_B <- adaptive_simulate(make_trials(c("A", "B"), lambda = 1, n = 10, "Phase 2"),
                                 S = c(A = 0.3, B = 0.3),
                                 alpha_init = extract_final_alpha(prevent_phase1_B),
                                 eta = 0,
                                 V_init = extract_final_V(prevent_phase1_B),
                                 cue_names = c("A", "B"))
  prevent_D <- adaptive_simulate(make_trials(c("D", "E"), lambda = 1, n = 10, "Phase 2"),
                                 S = c(C = 0.3, D = 0.3, E = 0.3),
                                 alpha_init = extract_final_alpha(prevent_phase1_D),
                                 eta = 0,
                                 V_init = extract_final_V(prevent_phase1_D),
                                 cue_names = c("C", "D", "E"))
  check_close("Prevent: B and D end compound training with associability .5",
              c(unname(extract_final_alpha(prevent_B)["B"]),
                unname(extract_final_alpha(prevent_D)["D"])),
              c(0.5, 0.5))

  supply_gap <- rw_alone("D", alpha_D, 10)$V - rw_alone("B", alpha_B, 10)$V
  check_close("Supply: the first-update gap equals the adaptive model's",
              supply_gap[2], expected_first_update_gap)
  check_true("Supply: D stays ahead of B over ten updates", all(supply_gap[-1] > 0))

  # Section 4.2 varies compound training from 1 to 20 trials and eta from .01
  # to .05. Across this grid, adaptive_final_state() follows the recursion
  # written from Equations 7 and 8, and B's associability stays above the
  # lower bound of .01.
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
  check_close("across the Section 4.2 grid, adaptive_final_state() follows Equations 7 and 8",
              grid_states, grid_by_hand)
  blocked_alpha_B <- function(n, eta) unname(blocked_state(n, eta)$alpha["B"])
  check_true("across the Section 4.2 grid, B's associability stays above the lower bound of .01",
             all(grid_states["alpha.B", ] > 0.01))

  # Equation 13 (Supplement S4.1): with equal zero starts, equal saliences and
  # alpha_D > alpha_B > 0, D stays ahead after every update. D keeps its
  # starting associability of .5 through compound training.
  gap_positive <- mapply(function(n, eta) {
    gap <- adaptive_alone("D", 0.5, 15, eta = eta)$V -
      adaptive_alone("B", blocked_alpha_B(n, eta), 15, eta = eta)$V
    all(gap[-1] > 0)
  }, grid$n, grid$eta)
  check_true("the B-D gap is positive after every update across the Section 4.2 grid",
             all(gap_positive))

  # Carryover (Section 4.3). Any g_V and k would do for the Rescorla-Wagner
  # check, and any g_alpha for the adaptive one.
  rw_old_B <- final_value(rw_simulate(blocking_design, alpha = c(A = 0.4, B = 0.4),
                                      beta = 0.3), "B")
  rw_old_D <- final_value(rw_simulate(control_design, alpha = c(C = 0.4, D = 0.4, E = 0.4),
                                      beta = 0.3), "D")
  g_V <- 0.6
  k <- 5
  check_close("Rescorla-Wagner with old-value carryover: the gap follows Equation 17",
              final_value(rw_alone("D", 0.4, k, V_init = g_V * rw_old_D), "D") -
                final_value(rw_alone("B", 0.4, k, V_init = g_V * rw_old_B), "B"),
              g_V * (rw_old_D - rw_old_B) * (1 - 0.4 * 0.3)^k)

  # Starting associabilities alpha_i(0) for the new task, from Equation 14.
  g_alpha <- 0.4
  alpha_init <- 0.5
  carried_B <- adaptive_alone("B", alpha_init + g_alpha * (alpha_B - alpha_init), 1)
  carried_D <- adaptive_alone("D", alpha_init + g_alpha * (alpha_D - alpha_init), 1)
  check_close("adaptive model with associability carryover: the first-update gap follows Equation 15",
              final_value(carried_D, "D") - final_value(carried_B, "B"),
              0.3 * g_alpha * (alpha_D - alpha_B))

  # A cue trained alone with the new outcome (Section 3.4): its error is
  # compared with |lambda| = 1, so Equation 8 becomes
  # alpha(k + 1) = min(1, alpha(k) + eta V(k)) while V(k) lies in [0, 1].
  # The second case reaches the upper bound.
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
    check_close(sprintf("a cue alone follows alpha(k + 1) = min(1, alpha(k) + eta V(k)) (alpha(0) = %s, eta = %s)",
                        sub("^0", "", sprintf("%.3f", case[["alpha"]])),
                        sub("^0", "", sprintf("%.2f", case[["eta"]]))),
                c(alone$V, alone$alpha), alone_by_hand(case[["alpha"]], 15, case[["eta"]]))
  }

  # Response rules (Section 4.4). The sigmoid of Table 3 (gamma = 8,
  # theta = .5) keeps D ahead of B; the threshold rule at .5 separates them
  # only after updates 5 to 8 (Equation 20).
  transfer_B <- adaptive_alone("B", alpha_B, 10)
  transfer_D <- adaptive_alone("D", alpha_D, 10)
  sigmoid_difference <- obs_sigmoid(transfer_D$V, gamma = 8, theta = 0.5) -
    obs_sigmoid(transfer_B$V, gamma = 8, theta = 0.5)
  check_true("the sigmoid keeps D's response probability above B's after every update",
             all(sigmoid_difference[-1] > 0))

  threshold_difference <- obs_threshold(transfer_D$V, theta = 0.5) -
    obs_threshold(transfer_B$V, theta = 0.5)
  completed_updates <- transfer_B$trial - 1L
  check_close("the threshold rule separates B and D only after updates 5 to 8",
              completed_updates[threshold_difference != 0], 5:8)

  cat(sprintf("All %d model checks passed.\n", n_checks))
})
