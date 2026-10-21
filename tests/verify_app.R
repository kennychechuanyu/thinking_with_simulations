# Checks of the companion app (shiny_app/). The app computes its model results
# with shiny_app/R/engine.R, which calls the learning models. These checks
# compare the engine with the values printed in Sections 2-4 and 6 and with
# closed-form results, check that the app's copy of the model code is identical
# to R/models.R, and check the precomputed experiments of Step 5. They stop at
# the first failure.
#   Rscript tests/verify_app.R

local({
  source("shiny_app/R/engine.R", local = TRUE)

  n_checks <- 0L
  check <- function(label, ok) {
    if (!isTRUE(ok)) stop(sprintf("FAIL  %s", label), call. = FALSE)
    n_checks <<- n_checks + 1L
    cat(sprintf("  PASS  %s\n", label))
  }
  # x agrees with a value printed to the given number of decimals.
  near <- function(x, value, digits = 3) isTRUE(abs(x - value) < .5 * 10^-digits)

  cat("Checks of the companion app\n")
  base <- run_inquiry(default_settings)
  trained <- base$trained
  at <- function(x, k) x[k + 1]  # value after k completed updates

  # Section 2: blocking under Rescorla-Wagner.
  check("RW blocking: V_A = .8518", near(trained$rw$V_A, .8518, 4))
  check("RW blocking: V_B = .1303", near(trained$rw$V_B, .1303, 4))

  # Section 3.2, Table 2.
  check("Table 2: RW V_D = .468", near(trained$rw$V_D, .468))
  check("Table 2: adaptive V_B = .681, V_D = .803",
        near(trained$adaptive$V_B, .681) && near(trained$adaptive$V_D, .803))
  check("Table 2: adaptive multipliers .071 and .150",
        near(trained$adaptive$mult_B, .071) && near(trained$adaptive$mult_D, .150))
  check("Section 3.2: first compound-trial multipliers .12 (RW) and .15 (adaptive)",
        near(trained$rw$mult_first, .12, 12) && near(trained$adaptive$mult_first, .15, 12))
  check("Section 3.2: alpha_B = .235, alpha_D = .500 (equal errors leave D unchanged)",
        near(trained$adaptive$alpha_B, .235) && identical(trained$adaptive$alpha_D, .5))

  # Sections 3.3-3.4: transfer with g_V = 0, g_alpha = 1.
  check("RW gap is exactly zero on every update", all(base$rw$gap == 0))
  check("Adaptive gap .079, .284, .272 after 1, 8, 10 updates",
        near(at(base$adaptive$gap, 1), .079) && near(at(base$adaptive$gap, 8), .284) &&
          near(at(base$adaptive$gap, 10), .272))
  check("Gap peaks after 8 updates", which.max(base$adaptive$gap) - 1 == 8)
  check("Ninth update: B gains .051, D gains .048",
        near(at(base$adaptive$B$gain, 8), .051) && near(at(base$adaptive$D$gain, 8), .048))
  check("First O2 trial: a cue at value 0 errs as much as predicting zero, so associability is unchanged",
        identical(at(base$adaptive$B$alpha, 1), at(base$adaptive$B$alpha, 0)))
  check("Later O2 trials: a cue alone gains eta times its value in associability",
        all(vapply(list(base$adaptive$B, base$adaptive$D), function(d) {
          all(abs(diff(d$alpha) - .05 * head(d$V, -1)) < 1e-12)
        }, logical(1))))
  check("After ten updates: RW B = D = .721; adaptive B .576, D .848",
        near(at(base$rw$B$V, 10), .721) && near(at(base$rw$D$V, 10), .721) &&
          near(at(base$adaptive$B$V, 10), .576) && near(at(base$adaptive$D$V, 10), .848))
  check("Each gain equals multiplier times error",
        all(vapply(list(base$rw$B, base$rw$D, base$adaptive$B, base$adaptive$D), function(d) {
          all(abs(head(d$gain, -1) - head(d$mult * d$error, -1)) < 1e-15)
        }, logical(1))))

  # Section 4.1: changing the model.
  check("Prevent and Remove give a zero gap",
        all(run_inquiry(settings_with(change = "prevent"))$adaptive$gap == 0) &&
          all(run_inquiry(settings_with(change = "remove"))$adaptive$gap == 0))
  check("Remove: both cues restart O2 at alpha_init whatever g_alpha",
        all(vapply(c(0, .25, .5, 1), function(g) identical(unname(unlist(
          run_inquiry(settings_with(change = "remove", g_alpha = g, g_V = .5))$start)), c(.5, .5)),
          logical(1))))
  sup <- run_inquiry(settings_with(change = "supply"))
  check("Supply matches the first two updates and reaches .284 after ten",
        all(abs(sup$adaptive$gap[1:3] - base$adaptive$gap[1:3]) < 1e-12) &&
          near(at(sup$adaptive$gap, 10), .284))
  # With old-value carryover, Supply starts from the adaptive model's own O2
  # values, so only the associability updating differs.
  sup5 <- run_inquiry(settings_with(change = "supply", g_V = .5))
  ad5 <- run_inquiry(settings_with(g_V = .5))
  check("g_V = .5: Supply starts from .5 times the adaptive model's O1 values",
        abs(at(sup5$adaptive$B$V, 0) - .5 * trained$adaptive$V_B) < 1e-15 &&
          abs(at(sup5$adaptive$D$V, 0) - .5 * trained$adaptive$V_D) < 1e-15)
  check("g_V = .5: Supply's gap equals the adaptive gap after 0 and 1 updates",
        all(abs(sup5$adaptive$gap[1:2] - ad5$adaptive$gap[1:2]) < 1e-12))
  check("g_V = .5: the gaps differ after the second update, once associability has changed",
        abs(at(sup5$adaptive$gap, 2) - at(ad5$adaptive$gap, 2)) > 1e-6)

  # Section 4.2: training and eta (fifteen updates, as in Figure 6).
  peak <- function(...) max(run_inquiry(settings_with(n_transfer = 15, ...))$adaptive$gap)
  check("Peaks .156, .284, .512 with 5, 10, 20 compound trials",
        near(peak(n_compound = 5), .156) && near(peak(n_compound = 10), .284) &&
          near(peak(n_compound = 20), .512))
  check("Ten compound trials: peaks .037 at eta = .01 and .136 at eta = .03",
        near(peak(eta = .01), .037) && near(peak(eta = .03), .136))
  peak_update <- unlist(lapply(1:20, function(n) lapply(seq(.01, .05, by = .01), function(e) {
    gap <- run_inquiry(settings_with(n_compound = n, eta = e, n_transfer = 15))$adaptive$gap
    which.max(gap) - 1
  })))
  check("Section 4.2 grid: the gap peaks between the sixth and tenth update",
        identical(range(peak_update), c(6, 10)))

  # Section 4.3: carryover.
  half <- run_inquiry(settings_with(g_alpha = .5))
  check("g_alpha = .5: B starts at .368, first gap .040",
        near(half$start$alpha_B, .368) && near(at(half$adaptive$gap, 1), .040))
  g0 <- run_inquiry(settings_with(g_alpha = 0))
  check("g_alpha = 0: both models give a zero gap",
        all(g0$adaptive$gap == 0) && all(g0$rw$gap == 0))
  full <- run_inquiry(settings_with(g_V = 1))
  check("g_V = 1: RW gains .104 (B) and .064 (D) on the first trial",
        near(at(full$rw$B$gain, 0), .104) && near(at(full$rw$D$gain, 0), .064))
  check("g_V = 1: gaps after eight updates .121 (RW) and .092 (adaptive)",
        near(at(full$rw$gap, 8), .121) && near(at(full$adaptive$gap, 8), .092))
  for (g in c(.25, .5, 1)) {
    r <- run_inquiry(settings_with(g_V = g))
    check(sprintf("g_V = %s: RW gap follows Equation 17 in closed form", sub("^0", "", g)),
          all(abs(r$rw$gap - g * (trained$rw$V_D - trained$rw$V_B) * (1 - .12)^(0:10)) < 1e-12))
  }
  grid <- gap_by_value_carryover(k = 8)
  check("Gaps after eight updates are closest at g_V = .89 on the grid of .01 (equal near .895)",
        grid$g_V[which.min(abs(grid$rw - grid$adaptive))] == .89)

  # Section 4.4: responses (Table 3 and the threshold window).
  p <- function(V) response_prob(V, "sigmoid", 8, .5)
  check("Table 3: p_B and p_D after 1, 5, 10 updates",
        near(p(at(base$adaptive$B$V, 1)), .0312, 4) &&
          near(p(at(base$adaptive$D$V, 1)), .0573, 4) &&
          near(p(at(base$adaptive$B$V, 5)), .1845, 4) &&
          near(p(at(base$adaptive$D$V, 5)), .6311, 4) &&
          near(p(at(base$adaptive$B$V, 10)), .6473, 4) &&
          near(p(at(base$adaptive$D$V, 10)), .9417, 4))
  check("Sigmoid difference after five updates = .447",
        near(p(at(base$adaptive$D$V, 5)) - p(at(base$adaptive$B$V, 5)), .447))
  thr <- response_prob(base$adaptive$D$V, "threshold", theta = .5) -
    response_prob(base$adaptive$B$V, "threshold", theta = .5)
  check("Threshold .5: responses differ only after updates 5 to 8",
        identical(which(thr != 0) - 1L, 5:8))

  # Section 4.5: the matched control.
  up <- run_inquiry(settings_with(e_offset = 1e-6))
  down <- run_inquiry(settings_with(e_offset = -1e-6))
  check("E one part in 10^6 more salient: alpha_D and the gap after eight change by less than 1e-6",
        up$trained$adaptive$alpha_D < .5 && .5 - up$trained$adaptive$alpha_D < 1e-6 &&
          abs(at(up$adaptive$gap, 8) - at(base$adaptive$gap, 8)) < 1e-6)
  check("E one part in 10^6 less salient: alpha_D and the gap after eight change by less than 1e-6",
        down$trained$adaptive$alpha_D > .5 && down$trained$adaptive$alpha_D - .5 < 1e-6 &&
          abs(at(down$adaptive$gap, 8) - at(base$adaptive$gap, 8)) < 1e-6)

  # Step 1: inside the first compound trial, and what training leaves behind.
  trace <- compound_trace(default_settings)
  check("First compound trial: RW shared error .2785 and B gains .0334",
        near(trace$rw$error[1], .2785, 4) && near(trace$rw$gain_B[1], .0334, 4))
  check("First compound trial: adaptive B gains .150, associability .5 to .458",
        near(trace$adaptive$gain_B[1], .150) && near(trace$adaptive$alpha_B_after[1], .458))
  check("First compound trial: B's associability changes by eta (e_A - e_B) (Equation 8)",
        near(trace$adaptive$alpha_B_after[1],
             .5 + .05 * (trace$adaptive$error_A[1] - trace$adaptive$error_B[1]), 12))
  check("First compound trial: A's error .152 (adaptive) and D gains .12 (RW)",
        near(trace$adaptive$error_A[1], .152) && near(trace$rw$gain_D[1], .12, 2))
  check("Second compound trial: adaptive B gains about .117 and D .1275",
        abs(trace$adaptive$gain_B[2] - .117) < 1e-3 && near(trace$adaptive$gain_D[2], .1275, 12))
  check("compound_trace() ends where trained_states() ends",
        near(trace$rw$V_B[10] + trace$rw$gain_B[10], trained_states()$rw$V_B, 12) &&
          near(trace$adaptive$alpha_B_after[10], trained_states()$adaptive$alpha_B, 12))

  # Step 2: continuing with O1 keeps D ahead under both models, with the gap closing.
  co <- continue_o1(default_settings)
  check("Continuing with O1: both models start from the trained values",
        near(at(co$rw$B$V, 0), trace$rw$V_B[10] + trace$rw$gain_B[10], 12) &&
          near(at(co$adaptive$D$V, 0), trained_states()$adaptive$V_D, 12))
  check("Continuing with O1: D stays ahead and the gap closes under both models",
        all(co$rw$gap > 0) && all(co$adaptive$gap > 0) &&
          at(co$rw$gap, 10) < at(co$rw$gap, 0) && at(co$adaptive$gap, 10) < at(co$adaptive$gap, 0))

  # Step 6: the decision model (Section 6 and Figure 12).
  dd <- ddm_summary()
  check("Decision model: both settings predict accuracy .646",
        all(abs(dd$acc - .646) < 5e-4) && abs(dd$acc[1] - dd$acc[2]) < 1e-12)
  check("Decision model: mean decision times 0.97 and 0.24, as in Figure 12",
        all(abs(dd$mean - c(.97, .24)) < .005))
  tt <- seq(0, 12, by = .001)
  for (i in 1:2) {
    f <- ddm_dens(tt, dd$v[i], dd$a[i])
    check(sprintf(paste("Decision model: the density of setting %d integrates to 1",
                        "with the closed-form mean"), i),
          abs(sum(f) * .001 - 1) < 1e-3 && abs(sum(tt * f) * .001 - dd$mean[i]) < 2e-3)
  }

  # The app's own copy of the model code.
  check("shiny_app/R/models.R is identical to R/models.R",
        file.exists("shiny_app/R/models.R") &&
          identical(readLines("shiny_app/R/models.R"), readLines("R/models.R")))

  # Step 5: the simulated experiments and their analyses.
  path <- "shiny_app/data/inference_cases.rds"
  inference <- readRDS(path)
  design <- function(src, n, view) inference$cases[[src]][[as.character(n)]]$designs[[view]]
  check("Experiments: three processes, two sizes, three designs each",
        identical(names(inference$cases), c("value", "associability", "reset")) &&
          all(vapply(inference$cases, function(by_size) identical(names(by_size), c("40", "160")),
                     logical(1))) &&
          all(vapply(unlist(inference$cases, recursive = FALSE), function(cs) {
            identical(names(cs$designs), c("late", "early", "full"))
          }, logical(1))) &&
          all(dim(inference$cases$value[["160"]]$y) == c(160, 20)) &&
          all(dim(inference$cases$value[["40"]]$y) == c(40, 20)))
  check(paste("Value generalization, 160, all ten: g_V fixed at 0 favors carryover;",
              "with g_V estimated, carryover is not favored"),
        design("value", 160, "full")$decision$zero == "carry" &&
          design("value", 160, "full")$decision$estimated != "carry")
  check("Value generalization, 160: first-answer check rejects equality",
        design("value", 160, "full")$first$p < .05 &&
          identical(design("value", 160, "full")$first, design("value", 160, "early")$first))
  check("Carried associability, 160, all ten: carryover favored under both choices",
        design("associability", 160, "full")$decision$zero == "carry" &&
          design("associability", 160, "full")$decision$estimated == "carry")
  check("Carried associability, 160: first-answer check does not reject equality",
        design("associability", 160, "full")$first$p >= .05)
  check("End-of-training records have no first-answer check and begin at presentation 8",
        is.null(design("value", 160, "late")$first) &&
          identical(design("value", 160, "late")$cols, 8:10))
  check("Transfer-only adaptation: no difference detected under any design",
        all(vapply(c("40", "160"), function(n) all(vapply(c("late", "early", "full"),
          function(view) design("reset", n, view)$effect[["detected"]] == 0, logical(1))),
          logical(1))))
  # Figure 9b's interval for paired proportions (Agresti and Min, 2005), from
  # the saved answers: add 1/2 to each cell of the table of B and D answers.
  agresti_min <- function(y, q) {
    n <- nrow(y)
    d_only <- sum(y[, 10 + q] == 1 & y[, q] == 0)
    b_only <- sum(y[, q] == 1 & y[, 10 + q] == 0)
    p_d <- (d_only + .5) / (n + 2)
    p_b <- (b_only + .5) / (n + 2)
    half <- qnorm(.975) * sqrt((p_d + p_b - (p_d - p_b)^2) / (n + 2))
    c((d_only - b_only) / n, p_d - p_b - half, p_d - p_b + half)
  }
  check("Contrast plots: each recorded presentation's difference and interval, as in Figure 9b",
        all(unlist(lapply(unlist(inference$cases, recursive = FALSE), function(cs) {
          lapply(cs$designs, function(des) {
            bp <- des$by_presentation
            identical(as.numeric(bp$presentation), as.numeric(des$cols)) &&
              all(vapply(seq_len(nrow(bp)), function(i) {
                max(abs(unlist(bp[i, c("estimate", "lower", "upper")]) -
                          agresti_min(cs$y, bp$presentation[i]))) < 1e-12
              }, logical(1)))
          })
        }))))
  check("Rates table covers every process, size and design",
        nrow(inference$rates) == 18 &&
          nrow(unique(inference$rates[c("scenario", "n", "view")])) == 18)
  r <- inference$rates
  outcomes <- r[c("carry", "no_carry", "unresolved", "fit_problem")]
  check("All 400 experiments remain in each denominator, including fitting problems",
        all(r$repetitions == 400) &&
          max(abs(rowSums(outcomes) - 1)) < 1e-12 &&
          max(abs(as.matrix(outcomes) * 400 - round(as.matrix(outcomes) * 400))) < 1e-10)
  check("No experiment had a fitting problem, so the three decision columns add to 1",
        all(r$fit_problem == 0) &&
          max(abs(r$carry + r$no_carry + r$unresolved - 1)) < 1e-12)
  # Step 5 says that most of these comparisons stay unresolved.
  reset <- r[r$scenario == "reset", ]
  check("Transfer-only adaptation: most comparisons unresolved in all six designs",
        nrow(reset) == 6 && all(reset$unresolved > .5))
  # The app prints these rates; they must read as the paper's macros do.
  macros <- readLines("results/section5/section5_numbers.tex")
  macro <- function(name) sub(".*\\}\\{(.*)\\}$", "\\1",
                              grep(paste0("{\\", name, "}"), macros, fixed = TRUE, value = TRUE))
  shown <- function(x) formatC(100 * x, format = "f", digits = 1)
  z <- inference$zero_rates
  fr <- inference$first_rates
  rate <- function(src, n, view, col) r[[col]][r$scenario == src & r$n == n & r$view == view]
  check("Rates print as in Section 5 (Figures 10 and 11)",
        shown(z$carry[z$scenario == "associability"]) == macro("AssocOmittedCarry") &&
          shown(z$carry[z$scenario == "value"]) == macro("ValueOmittedCarry") &&
          shown(rate("associability", 160, "full", "carry")) == macro("AssocFullLargeCarry") &&
          shown(rate("value", 160, "full", "carry")) == macro("ValueFullLargeCarry") &&
          shown(rate("associability", 160, "late", "carry")) == macro("AssocLateLargeCarry") &&
          shown(rate("associability", 160, "early", "carry")) == macro("AssocEarlyLargeCarry") &&
          shown(rate("associability", 40, "full", "carry")) == macro("AssocFullSmallCarry") &&
          shown(rate("reset", 160, "full", "detected")) == macro("ResetFullLargeDetected") &&
          shown(fr$initial_rejection[fr$scenario == "associability"]) ==
            macro("AssocInitialRejected") &&
          shown(fr$initial_rejection[fr$scenario == "value"]) == macro("ValueInitialRejected") &&
          shown(fr$rejection_given_carry[fr$scenario == "value"]) == macro("ValueWrongFlagged"))

  cat(sprintf("\nAll %d app checks passed.\n\n", n_checks))
})
