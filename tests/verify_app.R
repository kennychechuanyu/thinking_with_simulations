# Checks of the companion app (shiny_app/). The app computes its model results
# with shiny_app/R/engine.R, which calls the learning models. These checks
# compare the engine with the values printed in Sections 2 to 5 and 8 of the
# paper and with closed-form results, check that the app's copy of the model
# code is identical to R/models.R, and check the saved experiments of Steps 5
# and 6. They stop at the first failure.
#   Rscript tests/verify_app.R

local({
  source("shiny_app/R/engine.R", local = TRUE)

  n_checks <- 0L
  check <- function(label, ok) {
    if (!isTRUE(ok)) stop(sprintf("FAIL  %s", label), call. = FALSE)
    n_checks <<- n_checks + 1L
    cat(sprintf("  PASS  %s\n", label))
  }
  # x agrees with a value printed to the given number of decimals. Like the
  # paper, a value exactly halfway rounds up (.2775 prints as .278).
  near <- function(x, value, digits = 3) isTRUE(abs(x - value) <= .5 * 10^-digits + 1e-12)
  at <- function(x, k) x[k + 1]  # value after k completed pear trials

  cat("Checks of the companion app\n")
  s <- default_settings

  # Section 2: the tone and the light under Rescorla-Wagner.
  tr <- rw_trials(s)
  check("Section 2.2: the tone gains .12 and then .1056, reaching .2256",
        near(tr$gain_A[1], .12, 12) && near(tr$gain_A[2], .1056, 12) &&
          near(tr$V_A_after[2], .2256, 12))
  check("Section 2.2: after ten tone-only trials the tone has exactly 1 - .88^10 = .7215",
        abs(tr$V_A_after[10] - (1 - .88^10)) < 1e-12 && near(tr$V_A_after[10], .7215, 4))
  check("Section 2.2: first compound trial, error .2785 and each cue gains .0334",
        near(tr$error[11], .2785, 4) && near(tr$gain_A[11], .0334, 4) && near(tr$gain_B[11], .0334, 4))
  check("Section 2.4: second compound trial, .7549 + .0334 = .7883, error .2117, light gains .0254",
        near(tr$V_A[12], .7549, 4) && near(tr$V_B[12], .0334, 4) &&
          near(tr$prediction[12], .7883, 4) && near(tr$error[12], .2117, 4) &&
          near(tr$gain_B[12], .0254, 4))
  check("Section 2.4: the tone and the light gain the same on every compound trial",
        all(abs(tr$gain_A[11:20] - tr$gain_B[11:20]) < 1e-15))
  check("Section 2.4: the shared error is multiplied by .76 on every compound trial",
        all(abs(tr$error[12:20] / tr$error[11:19] - .76) < 1e-12))
  check("Section 2.4: after ten compound trials, .8518 and .1303; limit .1393, within .009",
        near(tr$V_A_after[20], .8518, 4) && near(tr$V_B_after[20], .1303, 4) &&
          near((1 - tr$V_A_after[10]) / 2, .1393, 4) &&
          near((1 - tr$V_A_after[10]) / 2 - tr$V_B_after[20], .009))
  check("Section 2.4: a new cue trained alone reaches .7215 after ten trials",
        abs((1 - (1 - s$beta)^10) - tr$V_A_after[10]) < 1e-12)

  # Section 2.3: the check of the code.
  from30 <- compound_check(c(A = .3, B = .3))
  from0 <- compound_check(c(A = 0, B = 0))
  check("Section 2.3: from .30 and .30, the rule gives each cue .048 (to .348)",
        all(abs(from30$specified - .048) < 1e-12))
  check("Section 2.3: each cue's own error would give .084, and the tone updated first .04224",
        all(abs(from30$own - .084) < 1e-12) && abs(from30$one_at_a_time[["B"]] - .04224) < 1e-12)
  check("Section 2.3: from zero, the own-error program agrees with the rule (.12 each)",
        all(abs(from0$own - from0$specified) < 1e-15) && all(abs(from0$specified - .12) < 1e-12))
  check("Section 2.3: from zero, updating the tone first gives the light .1056",
        abs(from0$one_at_a_time[["B"]] - .1056) < 1e-12)
  own_alone <- 0
  for (t in 1:10) own_alone <- own_alone + s$beta * (1 - own_alone)
  check("Section 2.3: with one cue present, its own error is the shared error",
        abs(own_alone - tr$V_A_after[10]) < 1e-15)

  # Section 3.1: the comparison light under Rescorla-Wagner.
  trained <- trained_states(s)
  check("Section 3.1: D ends at exactly (1 - .76^10)/2 = .468, B at .130",
        abs(trained$rw$V_D - (1 - .76^10) / 2) < 1e-12 && near(trained$rw$V_D, .468) &&
          near(trained$rw$V_B, .130))
  ct <- compound_trials(s)
  check("Section 3.1: first compound trial, B gains .0334 and D .12",
        near(ct$B$rw_gain[1], .0334, 4) && abs(ct$D$rw_gain[1] - .12) < 1e-12)

  # Section 3.2: the adaptive model.
  ta <- tone_alone(s)
  check("Section 3.2: A's first trial leaves associability at .5 and gives .150",
        identical(at(ta$alpha, 1), .5) && abs(at(ta$V, 1) - .15) < 1e-12)
  check("Section 3.2: A's associability rises by .0075 to .5075 on the second trial",
        abs(at(ta$alpha, 2) - .5075) < 1e-12)
  check("Section 3.2: after ten trials A has .848, more than 1 - .85^10 = .803",
        near(at(ta$V, 10), .848) && near(1 - .85^10, .803) && at(ta$V, 10) > 1 - .85^10)
  check("Section 3.2: first compound trial, B and D both gain .150",
        abs(ct$B$ad_gain[1] - .15) < 1e-12 && abs(ct$D$ad_gain[1] - .15) < 1e-12)
  check("Section 3.2: A's error .152, and B's associability falls from .500 to .458 (Equation 3)",
        near(ct$B$ad_companion_error[1], .152) && near(ct$B$ad_alpha_after[1], .458) &&
          abs(ct$B$ad_alpha_after[1] - (.5 + .05 * (ct$B$ad_companion_error[1] - 1))) < 1e-12)
  check("Section 3.2: D and C make equal errors, so D stays at exactly .5",
        all(ct$D$ad_alpha_after == .5))
  check("Section 3.2: second compound trial, value .150 and error .850 for both; gains .117 and .128",
        abs(ct$B$ad_V[2] - .15) < 1e-12 && abs(ct$D$ad_V[2] - .15) < 1e-12 &&
          near(ct$B$ad_gain[2], .117) && near(ct$D$ad_gain[2], .128))
  check("Section 3.3: B gains .150, not the .137 that its new associability would give",
        near(s$S * ct$B$ad_alpha_after[1], .137))
  check("Table 3: Rescorla-Wagner .130 / .468, learning rates .120 / .120",
        near(trained$rw$V_B, .130) && near(trained$rw$V_D, .468) &&
          trained$rw$rate_B == .12 && trained$rw$rate_D == .12)
  check("Table 3: adaptive .681 / .803, learning rates .071 / .150, associabilities .235 / .500",
        near(trained$adaptive$V_B, .681) && near(trained$adaptive$V_D, .803) &&
          near(trained$adaptive$rate_B, .071) && near(trained$adaptive$rate_D, .150) &&
          near(trained$adaptive$alpha_B, .235) && identical(trained$adaptive$alpha_D, .5))
  low <- trained_states(settings_with(S = .24))
  check("Section 3.3: with salience .24, B ends at .231 and learning rate .055, D at .120",
        near(low$adaptive$alpha_B, .231) && near(low$adaptive$rate_B, .055) &&
          near(low$adaptive$rate_D, .120))
  check("compound_trials() ends where trained_states() ends",
        abs(ct$B$ad_alpha_after[10] - trained$adaptive$alpha_B) < 1e-12 &&
          abs(ct$B$rw_V[10] + ct$B$rw_gain[10] - trained$rw$V_B) < 1e-12)

  # Section 3.4: the pear trials.
  base <- run_inquiry(s)
  check("Section 3.4: Rescorla-Wagner's gap is exactly zero, with first gains of .12",
        all(base$rw$gap == 0) && abs(at(base$rw$B$gain, 0) - .12) < 1e-12)
  check("Section 3.4: first pear trial, B gains .071 and D .150",
        near(at(base$adaptive$B$gain, 0), .071) && abs(at(base$adaptive$D$gain, 0) - .15) < 1e-12)
  check("Section 3.4: the gap is .079, .284 and .272 after 1, 8 and 10 pear trials",
        near(at(base$adaptive$gap, 1), .079) && near(at(base$adaptive$gap, 8), .284) &&
          near(at(base$adaptive$gap, 10), .272))
  check("Section 3.4: the gap is largest after eight pear trials",
        which.max(base$adaptive$gap) - 1 == 8)
  check("Section 3.4: on the ninth pear trial, B gains .051 and D .048",
        near(at(base$adaptive$B$gain, 8), .051) && near(at(base$adaptive$D$gain, 8), .048))
  alpha_gap <- base$adaptive$D$alpha - base$adaptive$B$alpha
  check("Step 2: D's associability exceeds B's by .265 at the start and .337 after eight",
        near(at(alpha_gap, 0), .265) && near(at(alpha_gap, 8), .337))
  check("Each gain is the learning rate times the error",
        all(vapply(list(base$rw$B, base$rw$D, base$adaptive$B, base$adaptive$D), function(d) {
          all(abs(head(d$gain, -1) - head(d$rate * d$error, -1)) < 1e-15)
        }, logical(1))))
  check("A light alone, at value zero, keeps its associability on the first pear trial",
        identical(at(base$adaptive$B$alpha, 1), at(base$adaptive$B$alpha, 0)))
  co <- continue_apple(s)
  check("Step 2: continuing with the apple keeps D ahead in both models, with the gap closing",
        all(co$rw$gap > 0) && all(co$adaptive$gap > 0) &&
          at(co$rw$gap, 10) < at(co$rw$gap, 0) && at(co$adaptive$gap, 10) < at(co$adaptive$gap, 0))

  # Section 4.1: Prevent, Remove and Supply.
  runs <- lapply(c(none = "none", prevent = "prevent", remove = "remove", supply = "supply"),
                 function(ch) run_inquiry(settings_with(change = ch), n = 20))
  check("Section 4.1: Prevent and Remove give a zero gap over twenty pear trials",
        all(runs$prevent$adaptive$gap == 0) && all(runs$remove$adaptive$gap == 0))
  check("Section 4.1: under Prevent, B learns .803 about the apple, the same as D",
        near(runs$prevent$trained$adaptive$V_B, .803) &&
          abs(runs$prevent$trained$adaptive$V_B - runs$prevent$trained$adaptive$V_D) < 1e-15)
  carried_remove <- run_inquiry(settings_with(change = "remove", g_V = 1))
  carried_prevent <- run_inquiry(settings_with(change = "prevent", g_V = 1))
  check("Section 4.1: with full value carryover, Remove leaves .122 and Prevent no gap",
        near(at(carried_remove$adaptive$gap, 0), .122) && all(carried_prevent$adaptive$gap == 0))
  supply_diff <- runs$none$adaptive$gap - runs$supply$adaptive$gap
  check("Section 4.1: Supply matches the adaptive model after one and two pear trials (.079)",
        max(abs(supply_diff[1:3])) < 1e-12 && near(at(runs$supply$adaptive$gap, 1), .079))
  check("Section 4.1: over ten pear trials the gaps differ by at most .012",
        near(max(abs(supply_diff[1:11])), .012))
  check("Section 4.1: after twenty pear trials, .192 under Supply and .068 unchanged",
        near(at(runs$supply$adaptive$gap, 20), .192) && near(at(runs$none$adaptive$gap, 20), .068))
  check("Remove restarts both lights at .5 whatever the associability carryover",
        all(vapply(c(0, .25, .5, 1), function(g) identical(unname(unlist(
          run_inquiry(settings_with(change = "remove", g_alpha = g, g_V = .5))$start)), c(.5, .5)),
          logical(1))))

  # Section 4.2: carryover.
  half <- run_inquiry(settings_with(g_alpha = .5))
  check("Section 4.2: half associability carryover, B starts at .368 and the first gap is .040",
        near(half$start$alpha_B, .368) && near(at(half$adaptive$gap, 1), .040))
  none <- run_inquiry(settings_with(g_alpha = 0))
  check("Section 4.2: with no carryover of either kind, both models give no gap",
        all(none$adaptive$gap == 0) && all(none$rw$gap == 0))
  full <- run_inquiry(settings_with(g_V = 1))
  check("Box 3: with full value carryover, Rescorla-Wagner starts B at .130 and D at .468",
        near(at(full$rw$B$V, 0), .130) && near(at(full$rw$D$V, 0), .468))
  check("Box 3: B gains .104 and D .064 on the first pear trial",
        near(at(full$rw$B$gain, 0), .104) && near(at(full$rw$D$gain, 0), .064))
  for (g in c(.25, .5, .9, 1)) {
    r <- run_inquiry(settings_with(g_V = g))
    check(sprintf("g_V = %s: the Rescorla-Wagner gap shrinks by 12%% per pear trial",
                  sub("^0", "", g)),
          all(abs(r$rw$gap - g * (trained$rw$V_D - trained$rw$V_B) * (1 - .12)^(0:10)) < 1e-12))
  }
  cross <- uniroot(function(g) {
    r <- run_inquiry(settings_with(g_V = g))
    r$rw$gap[9] - r$adaptive$gap[9]
  }, c(.5, 1), tol = 1e-8)$root
  check("Section 4.2: the gaps after eight pear trials are equal at g_V = .895", near(cross, .895))
  grid <- gap_by_value_carryover(k = 8)
  check("Figure 5a: the gaps cross once, between .5 and 1",
        sum(diff(sign(grid$rw - grid$adaptive)) != 0) == 1 &&
          grid$g_V[which.min(abs(grid$rw - grid$adaptive))] == .89)
  g9 <- run_inquiry(settings_with(g_V = .9))
  check("Section 4.2: with g_V = .90, the Rescorla-Wagner gap is .304 before the first pear trial",
        near(at(g9$rw$gap, 0), .304) && which.max(g9$rw$gap) == 1)
  check("Section 4.2: with g_V = .90, the gaps after eight pear trials are .109 and .108",
        near(at(g9$rw$gap, 8), .109) && near(at(g9$adaptive$gap, 8), .108))
  p <- function(V) response_prob(V, "sigmoid")
  check("Section 5.3: the learned values are .683 / .792 and .842 / .950 after eight",
        near(at(g9$rw$B$V, 8), .683) && near(at(g9$rw$D$V, 8), .792) &&
          near(at(g9$adaptive$B$V, 8), .842) && near(at(g9$adaptive$D$V, 8), .950))
  check("Section 5.3: the yes probabilities differ by .100 and .034",
        near(p(at(g9$rw$D$V, 8)) - p(at(g9$rw$B$V, 8)), .100) &&
          near(p(at(g9$adaptive$D$V, 8)) - p(at(g9$adaptive$B$V, 8)), .034))

  # Section 4.3: the amount of training and the rate eta, over fifteen pear trials.
  combos <- expand.grid(n = 1:20, eta = seq(.01, .05, by = .01))
  peaks <- t(vapply(seq_len(nrow(combos)), function(i) {
    gap <- run_inquiry(settings_with(n_compound = combos$n[i], eta = combos$eta[i]), n = 15)$adaptive$gap
    c(positive = all(gap[-1] > 0), at = which.max(gap) - 1)
  }, numeric(2)))
  check("Section 4.3: in all 100 combinations, the gap is positive after every pear trial",
        nrow(combos) == 100 && all(peaks[, "positive"] == 1))
  check("Section 4.3: the gap is largest between the sixth and the tenth pear trial",
        identical(range(peaks[, "at"]), c(6, 10)))
  low_eta <- run_inquiry(settings_with(eta = .01), n = 15)
  check("Section 4.3: with eta = .01 the largest gap is .037, after six pear trials",
        near(max(low_eta$adaptive$gap), .037) && which.max(low_eta$adaptive$gap) - 1 == 6)

  # Section 4.4: the rules for associability (Table 5).
  check("Section 4.4: the rank-based step is .0726", near(rank_step(s), .0726, 4))
  prop_1pc <- rule_comparison("proportional", .01)
  check("Section 4.4: C 1% more salient gives D .498, and the gap after eight changes by .001",
        near(prop_1pc$alpha_D, .498) && near(abs(at(prop_1pc$gap, 8) - at(base$adaptive$gap, 8)), .001))
  rule <- function(r, c) rule_comparison(r, c)
  largest <- function(g) c(max(g), which.max(g) - 1)
  for (c in c(0, 1e-6)) {
    pr <- rule("proportional", c)
    check(sprintf("Table 5: proportional rule, C offset %g: D .500, largest gap .284 after 8", c),
          near(pr$alpha_D, .5) && near(largest(pr$gap)[1], .284) && largest(pr$gap)[2] == 8)
  }
  rk0 <- rule("rank", 0)
  check("Table 5: rank-based rule, matched: D stays at .500, largest gap .213 after 5",
        identical(rk0$alpha_D, .5) && near(largest(rk0$gap)[1], .213) && largest(rk0$gap)[2] == 5)
  for (c in c(1e-6, .01)) {
    rk <- rule("rank", c)
    check(sprintf("Table 5: rank-based rule, C offset %g: D falls to .254, largest gap .017 after 6", c),
          near(rk$alpha_D, .254) && rk$alpha_D > rk$alpha_B &&
            near(largest(rk$gap)[1], .017) && largest(rk$gap)[2] == 6)
  }
  check("Section 4.4: under the rank-based rule, D's associability falls on nine of the ten compound trials",
        abs(rule("rank", 1e-6)$alpha_D - .5 * (1 - rank_step(s))^9) < 1e-12)
  for (c in c(0, 1e-6)) {
    rt <- rule("rank_ties", c)
    check(sprintf("Table 5: ties lower associability, C offset %g: B and D both at .235, no gap", c),
          near(rt$alpha_D, .235) && abs(rt$alpha_B - rt$alpha_D) < 1e-12 && max(abs(rt$gap)) < 1e-12)
  }
  check("Table 5: B ends at .235 under every rule",
        all(vapply(c("proportional", "rank", "rank_ties"), function(r)
          near(rule(r, 0)$alpha_B, .235), logical(1))))

  # Section 5: from learned values to responses (Table 6).
  k <- 0:9
  V_B <- at(base$adaptive$B$V, k); V_D <- at(base$adaptive$D$V, k)
  printed <- function(x) sub("^0", "", formatC(x + 1e-12, format = "f", digits = 3))
  check("Table 6: B's learned values",
        identical(printed(V_B), c(".000", ".071", ".136", ".198", ".257", ".314", ".369", ".423",
                                  ".476", ".527")))
  check("Table 6: D's learned values (.2775 rounds up to .278)",
        identical(printed(V_D), c(".000", ".150", ".278", ".388", ".483", ".567", ".640", ".704",
                                  ".760", ".807")))
  check("Table 6: sigmoid yes probabilities for B",
        identical(printed(p(V_B)), c(".018", ".031", ".052", ".082", ".125", ".184", ".260",
                                     ".351", ".451", ".553")))
  check("Table 6: sigmoid yes probabilities for D",
        identical(printed(p(V_D)), c(".018", ".057", ".144", ".289", ".467", ".631", ".755",
                                     ".837", ".889", ".921")))
  check("Table 6: the threshold rule says yes to B only on the tenth response, to D from the sixth",
        identical(response_prob(V_B, "threshold"), c(rep(0, 9), 1)) &&
          identical(response_prob(V_D, "threshold"), c(rep(0, 5), rep(1, 5))))
  check("Box 4: after five pear trials, .314 and .567 give .184 and .631 (.253 against .447)",
        near(V_B[6], .314) && near(V_D[6], .567) && near(p(V_B[6]), .184) && near(p(V_D[6]), .631) &&
          near(V_D[6] - V_B[6], .253) && near(p(V_D[6]) - p(V_B[6]), .447))
  check("Section 5.3: the difference in yes probability is largest after six pear trials",
        k[which.max(p(V_D) - p(V_B))] == 6)
  check("Section 5.3: the threshold rule's responses differ only after five to eight pear trials",
        identical(k[response_prob(V_D, "threshold") != response_prob(V_B, "threshold")], 5:8))
  check("Section 5.2: the first responses are equally likely to be yes (.018)",
        V_B[1] == V_D[1] && near(p(V_B[1]), .018))

  # Section 8: the decision model.
  dd <- ddm_summary()
  check("Section 8.1: both settings predict accuracy .646, exactly equal",
        near(dd$acc[1], .646) && dd$acc[1] == dd$acc[2])
  check("Section 8.2: mean decision times 0.97 and 0.24, exactly four times apart",
        abs(dd$mean[1] - .97) < .005 && abs(dd$mean[2] - .24) < .005 &&
          abs(dd$mean[1] / dd$mean[2] - 4) < 1e-12)
  check("Section 8.2: by time 0.5, about 32% and 90% of the decisions have finished",
        round(100 * dd$by_half[1]) == 32 && round(100 * dd$by_half[2]) == 90)
  tt <- seq(0, 12, by = .001)
  for (i in 1:2) {
    f <- ddm_dens(tt, dd$v[i], dd$a[i])
    check(sprintf("Figure 10c: setting %d finishes all decisions, with the closed-form mean", i),
          abs(tail(ddm_finished(tt, dd$v[i], dd$a[i]), 1) - 1) < 1e-3 &&
            abs(sum(tt * f) * .001 - dd$mean[i]) < 2e-3)
  }

  # The app's own copy of the model code.
  check("shiny_app/R/models.R is identical to R/models.R",
        file.exists("shiny_app/R/models.R") &&
          identical(readLines("shiny_app/R/models.R"), readLines("R/models.R")))

  # Steps 5 and 6: the saved experiments and their analyses.
  inference <- readRDS("shiny_app/data/inference_cases.rds")
  design <- function(src, n, view) inference$cases[[src]][[as.character(n)]]$designs[[view]]
  all_cases <- unlist(inference$cases, recursive = FALSE)
  check("Experiments: three processes, two sizes, three designs each",
        identical(names(inference$cases), c("value", "associability", "reset")) &&
          all(vapply(inference$cases, function(by_size) identical(names(by_size), c("40", "160")),
                     logical(1))) &&
          all(vapply(all_cases, function(cs) identical(names(cs$designs), c("late", "early", "full")),
                     logical(1))) &&
          all(dim(inference$cases$value[["160"]]$y) == c(160, 20)) &&
          all(dim(inference$cases$value[["40"]]$y) == c(40, 20)))
  check("Carried learned value, 160, all ten: the restricted comparison favors carryover; with g_V estimated, it does not",
        design("value", 160, "full")$decision$zero == "carry" &&
          design("value", 160, "full")$decision$estimated != "carry")
  check("Carried learned value, 160: the first-response check rejects equality",
        design("value", 160, "full")$first$p < .05 &&
          identical(design("value", 160, "full")$first, design("value", 160, "early")$first))
  check("Carried associability, 160, all ten: carryover favored in both analyses",
        design("associability", 160, "full")$decision$zero == "carry" &&
          design("associability", 160, "full")$decision$estimated == "carry")
  check("Carried associability, 160: the first-response check does not reject equality",
        design("associability", 160, "full")$first$p >= .05)
  check("Pear trials 8-10 have no first-response check",
        is.null(design("value", 160, "late")$first) &&
          identical(design("value", 160, "late")$cols, 8:10))
  check("Reset associability: no difference detected under any design",
        all(vapply(c("40", "160"), function(n) all(vapply(c("late", "early", "full"),
          function(view) design("reset", n, view)$effect[["detected"]] == 0, logical(1))),
          logical(1))))
  # Section 6.2: the mean difference averages the two counterbalancing groups
  # with equal weight; the app draws each participant's difference.
  check("Each design's mean difference is the equal-weight average of the two groups' means",
        all(unlist(lapply(all_cases, function(cs) lapply(cs$designs, function(des) {
          d <- rowMeans(cs$y[, 10 + des$cols, drop = FALSE]) - rowMeans(cs$y[, des$cols, drop = FALSE])
          abs(mean(tapply(d, cs$allocation, mean)) - des$effect[["estimate"]]) < 1e-12
        })))))
  check("In every design, the group in which light 1 is B has the smaller mean difference",
        all(unlist(lapply(all_cases, function(cs) lapply(cs$designs, function(des) {
          d <- rowMeans(cs$y[, 10 + des$cols, drop = FALSE]) - rowMeans(cs$y[, des$cols, drop = FALSE])
          m <- tapply(d, cs$allocation, mean)
          cs$generating$cue > 0 && m[["1"]] < m[["-1"]]
        })))))
  # Figure 7b's interval for paired proportions (Agresti and Min, 2005), from
  # the saved responses: add 1/2 to each cell of the table of B and D responses.
  agresti_min <- function(y, q) {
    n <- nrow(y)
    d_only <- sum(y[, 10 + q] == 1 & y[, q] == 0)
    b_only <- sum(y[, q] == 1 & y[, 10 + q] == 0)
    p_d <- (d_only + .5) / (n + 2)
    p_b <- (b_only + .5) / (n + 2)
    half <- qnorm(.975) * sqrt((p_d + p_b - (p_d - p_b)^2) / (n + 2))
    c((d_only - b_only) / n, p_d - p_b - half, p_d - p_b + half)
  }
  check("Each analyzed pear trial's difference and interval, as in Figure 7b",
        all(unlist(lapply(all_cases, function(cs) {
          lapply(cs$designs, function(des) {
            bt <- des$by_trial
            identical(as.numeric(bt$trial), as.numeric(des$cols)) &&
              all(vapply(seq_len(nrow(bt)), function(i) {
                max(abs(unlist(bt[i, c("estimate", "lower", "upper")]) -
                          agresti_min(cs$y, bt$trial[i]))) < 1e-12
              }, logical(1)))
          })
        }))))
  r <- inference$rates
  check("The rates cover every process, size and design",
        nrow(r) == 18 && nrow(unique(r[c("scenario", "n", "view")])) == 18)
  outcomes <- r[c("carry", "no_carry", "unresolved", "fit_problem")]
  check("All 400 experiments remain in each denominator, including fitting problems",
        all(r$repetitions == 400) &&
          max(abs(rowSums(outcomes) - 1)) < 1e-12 &&
          max(abs(as.matrix(outcomes) * 400 - round(as.matrix(outcomes) * 400))) < 1e-10)
  check("No experiment had a fitting problem, so the three decisions add to 1",
        all(r$fit_problem == 0) && max(abs(r$carry + r$no_carry + r$unresolved - 1)) < 1e-12)
  check("Step 5: detecting a difference generated by carried associability is easier than identifying it, in every design",
        all(r$detected[r$scenario == "associability"] > r$carry[r$scenario == "associability"]))
  # The app prints these rates; they must read as the paper's macros and tables do.
  macros <- readLines("results/section6/section6_numbers.tex")
  macro <- function(name) sub(".*\\}\\{(.*)\\}$", "\\1",
                              grep(paste0("{\\", name, "}"), macros, fixed = TRUE, value = TRUE))
  shown <- function(x) formatC(100 * x, format = "f", digits = 1)
  z <- inference$zero_rates
  fr <- inference$first_rates
  rr <- inference$response_rates
  rate <- function(src, n, view, col) r[[col]][r$scenario == src & r$n == n & r$view == view]
  check("Rates print as in Sections 6.5 and 7 and Table 9",
        shown(z$carry[z$scenario == "associability"]) == macro("AssocOmittedCarry") &&
          shown(z$carry[z$scenario == "value"]) == macro("ValueOmittedCarry") &&
          shown(rate("associability", 160, "full", "carry")) == macro("AssocFullLargeCarry") &&
          shown(rate("value", 160, "full", "carry")) == macro("ValueFullLargeCarry") &&
          shown(rate("associability", 160, "late", "carry")) == macro("AssocLateLargeCarry") &&
          shown(rate("associability", 160, "late", "no_carry")) == macro("AssocLateLargeNoCarry") &&
          shown(rate("associability", 160, "early", "carry")) == macro("AssocEarlyLargeCarry") &&
          shown(rate("associability", 40, "full", "carry")) == macro("AssocFullSmallCarry") &&
          shown(rate("associability", 160, "full", "detected")) == macro("AssocFullLargeDetected"))
  check("The first-response check prints as in Section 7.2",
        shown(fr$rejection_160[fr$scenario == "associability"]) == macro("AssocInitialRejected") &&
          shown(fr$rejection_160[fr$scenario == "value"]) == macro("ValueInitialRejected") &&
          shown(fr$rejection_given_carry[fr$scenario == "value"]) == macro("ValueWrongFlagged") &&
          round(fr$carry_preferred[fr$scenario == "value"] * 400) == 380 &&
          shown(fr$rejection_160[fr$scenario == "reset"]) == "2.2")
  shared <- rr[rr$policy == "value_allowed", ]
  own <- rr[rr$policy == "response_allowed", ]
  check("A lower response intercept for B: rates as in Section 7.3 and Table 9",
        shown(shared$detected) == macro("ResponseDetected") &&
          shown(shared$carry) == macro("ResponseSharedCarry") &&
          shown(shared$ranked_carry) == macro("ResponseSharedRankedCarry") &&
          shown(shared$unresolved) == "90.0" && shown(shared$no_carry) == "2.8" &&
          shown(own$carry) == macro("ResponseFlexibleCarry") && shown(own$unresolved) == "82.0" &&
          shown(own$no_carry) == "18.0" &&
          shown(fr$rejection_160[fr$scenario == "response"]) == "10.5")

  cat(sprintf("\nAll %d app checks passed.\n\n", n_checks))
})
