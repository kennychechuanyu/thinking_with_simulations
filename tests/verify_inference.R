# Checks of R/inference.R and R/inference.cpp (Section 5, Supplement S2): the
# compiled probabilities against the reference built from R/models.R, the
# block order, special cases with known answers, the generator, the views, the
# response contrast, the mixture likelihood, its continuity at g_V = 0 and
# d = 0, the starting vectors, fitting, the held-out comparison and its
# decision rule, and the seeds of parallel runs.
# Needs Rcpp and a C++ compiler. Stops at the first failure.
#   Rscript tests/verify_inference.R

local({
  source("R/inference.R", local = TRUE)

  n_checks <- 0L
  check <- function(label, condition) {
    if (!isTRUE(condition)) stop(sprintf("FAIL  %s", label), call. = FALSE)
    n_checks <<- n_checks + 1L
    cat(sprintf("  PASS  %s\n", label))
  }
  all_close <- function(a, b, tol = 1e-10) max(abs(a - b)) < tol

  cat("Checks of R/inference.R and R/inference.cpp\n")
  scenarios <- inference_scenarios()

  # Compiled and reference probabilities for every generating scenario and
  # three physical-cue contrasts; in the adaptive scenarios also for three
  # associability rates, each with g_V = 0 and g_V = .3, so that carried
  # learned values are compared too. eta does not enter the fixed-rate
  # scenarios.
  for (name in names(scenarios)) {
    par <- scenarios[[name]]$par
    adaptive <- scenarios[[name]]$adaptive
    settings <- if (adaptive) {
      expand.grid(d = c(-.3, 0, .3), g_V = c(0, .3), eta = c(.025, .05, .15))
    } else {
      expand.grid(d = c(-.3, 0, .3), g_V = par[["g_value"]], eta = 0)
    }
    for (j in seq_len(nrow(settings))) {
      par[c("cue", "g_value", "eta")] <- unlist(settings[j, ])
      label <- sprintf("%s, eta = %s, g_V = %s, d = %g", name,
                       sub("^0[.]", ".", settings$eta[j]), sub("^0[.]", ".", settings$g_V[j]),
                       settings$d[j])
      check(paste("compiled and reference probabilities agree:", label),
            all_close(inference_prob_cpp(par, adaptive), inference_reference(par, adaptive)))
    }
    # Section 5.1: a trial with one cue changes only that cue's state, so running
    # the D block first gives exactly the same probabilities.
    par <- scenarios[[name]]$par
    check(paste("the block order does not change the probabilities:", name),
          identical(inference_prob_cpp(par, adaptive, d_first = TRUE),
                    inference_prob_cpp(par, adaptive)))
  }

  # Special cases with known answers. With tau = d = 0 and g_V = 0, each cue's
  # first answer precedes that cue's first O2 feedback and has probability
  # plogis(b). D's higher associability makes its second answer more likely
  # than B's.
  par <- scenarios$associability$par
  par[c("spread", "cue")] <- 0
  prob <- inference_prob_cpp(par, TRUE)
  check("the first answers to B and D are equal, and D's second is more likely",
        all_close(prob[, 1], rep(plogis(par[["intercept"]]), 4)) &&
          all_close(prob[, 1], prob[, 11]) && all(prob[, 12] > prob[, 2]))
  par["g_alpha"] <- 0
  prob <- inference_prob_cpp(par, TRUE)
  check("without associability carryover, B and D have the same probabilities",
        all_close(prob[, 1:10], prob[, 11:20]))
  # Row 1 is allocation -1 and the slower type, so B learns at rate
  # .5 logistic(mu - tau - d) and follows Equation 3 with beta = 1.
  par <- scenarios$null$par
  prob <- inference_prob_cpp(par, FALSE)
  rate <- .5 * plogis(par["mu"] - par["spread"] - par["cue"])
  closed_form <- plogis(par["intercept"] + exp(par["log_slope"]) * (1 - (1 - rate)^(0:9)))
  check("fixed-rate probabilities follow the closed form of Equation 3",
        all_close(prob[1, 1:10], closed_form))
  check("counterbalancing cancels the physical-cue difference on average",
        all_close(colMeans(prob[, 1:10]), colMeans(prob[, 11:20])))

  # The generator. Seeds 519001, 519002 and 519101-519104 lie outside the
  # seeds of the study (610001-613600) and of the pilot (510001-510020).
  experiment <- inference_generate(40, scenarios$associability$par, TRUE, 519001)
  check("the analyses receive only the answers, the allocation and the training length",
        identical(sort(names(experiment$observed)), sort(c("y", "allocation", "n_train"))))
  check("a seed reproduces its experiment exactly",
        identical(experiment, inference_generate(40, scenarios$associability$par, TRUE, 519001)))
  check("cue allocation is balanced", sum(experiment$observed$allocation) == 0L)
  check("within each cue allocation, half the participants receive the D block first",
        all(table(experiment$observed$allocation, experiment$truth$d_first) == 10L))
  # Each participant's probabilities are those of their allocation and rate
  # type: under the null scenario B follows Equation 3 at the rate
  # .5 logistic(mu + tau z + x_B d), with x_B = allocation.
  null_par <- scenarios$null$par
  null_experiment <- inference_generate(40, null_par, FALSE, 519002)
  z <- 2L * null_experiment$truth$type - 3L
  rate_B <- .5 * plogis(null_par[["mu"]] + null_par[["spread"]] * z +
                          null_experiment$observed$allocation * null_par[["cue"]])
  check("each participant's probabilities for B are those of their allocation and rate type",
        all_close(null_experiment$truth$probability[, 1:10],
                  plogis(null_par[["intercept"]] + exp(null_par[["log_slope"]]) *
                           (1 - outer(1 - rate_B, 0:9, `^`)))))
  late <- inference_view(experiment$observed, "late")
  check("the late view hides answers 1-7 to each cue and keeps 8-10",
        all(is.na(late$y[, c(1:7, 11:17)])) &&
          identical(late$y[, c(8:10, 18:20)], experiment$observed$y[, c(8:10, 18:20)]))
  early <- inference_view(experiment$observed, "early")
  check("the early view hides answers 7-10 to each cue and keeps 1-6",
        all(is.na(early$y[, c(7:10, 17:20)])) &&
          identical(early$y[, c(1:6, 11:16)], experiment$observed$y[, c(1:6, 11:16)]))
  check("the full view keeps every answer",
        identical(inference_view(experiment$observed, "full"), experiment$observed))

  # The response contrast of Supplement S2 is the average of the two allocation
  # groups' mean differences, and with balanced groups its interval is half as
  # wide as Welch's interval for the difference between the groups.
  effect <- inference_effect(experiment$observed)
  h <- rowMeans(experiment$observed$y[, 11:20]) - rowMeans(experiment$observed$y[, 1:10])
  allocation <- experiment$observed$allocation
  welch <- t.test(h[allocation == 1L], h[allocation == -1L])
  check("the response contrast averages the two allocation groups, with half the width of Welch's interval",
        all_close(effect[["estimate"]], mean(tapply(h, allocation, mean))) &&
          all_close(effect[["upper"]] - effect[["lower"]], diff(welch$conf.int) / 2))

  # The mixture likelihood L_s of Supplement S2, computed directly with dbinom
  # for every participant in each view.
  par <- scenarios$associability$par
  prob <- inference_prob_cpp(par, TRUE)
  for (view in c("full", "early", "late")) {
    observed <- inference_view(experiment$observed, view)
    direct <- vapply(seq_len(nrow(observed$y)), function(s) {
      rows <- 2L * (observed$allocation[s] == 1L) + 1:2
      log_terms <- vapply(rows, function(row) {
        sum(dbinom(observed$y[s, ], 1, prob[row, ], log = TRUE), na.rm = TRUE)
      }, 0.)
      max(log_terms) + log(sum(exp(log_terms - max(log_terms)))) - log(2)
    }, 0.)
    compiled <- inference_loglik_cpp(par, TRUE, observed$y, observed$allocation)
    check(paste("mixture likelihood of every participant's answers:", view, "view"),
          all_close(direct, compiled))
  }

  # Fitting and the held-out comparison. Turning every yes in fold 2 into a no
  # and every no into a yes must leave the fits to fold 1 unchanged.
  fold <- inference_folds(experiment$observed)
  check("each fold holds half of each allocation group",
        all(table(fold, experiment$observed$allocation) == 10L))
  flipped <- experiment$observed
  flipped$y[fold == 2L, ] <- 1 - flipped$y[fold == 2L, ]
  spec <- inference_spec("fixed")
  fit_original <- inference_fit(inference_subset(experiment$observed, which(fold == 1L)),
                                spec, starts = 1L)
  check("estimates stay within the bounds",
        all(fit_original$par[spec$free] >= inference_bounds["lower", spec$free]) &&
          all(fit_original$par[spec$free] <= inference_bounds["upper", spec$free]))
  check("the fit records one objective per search",
        length(fit_original$start_nll) == 1L + fit_original$refined)
  # Stopped after two iterations, no nlminb search converges, so the fit is
  # refined; the Nelder-Mead search converges and keeps the best value.
  unconverged <- inference_fit(inference_subset(experiment$observed, which(fold == 1L)),
                               inference_spec("reset"), starts = 4L, iter_max = 2L)
  check("a fit that nlminb leaves unconverged is refined, converges and keeps the best value",
        unconverged$refined && unconverged$converged &&
          unconverged$nll <= min(head(unconverged$start_nll, -1)))
  comparison <- inference_compare(experiment$observed, starts = 1L)
  # The fits named *_2 are fitted to the participants outside fold 2.
  flipped_comparison <- inference_compare(flipped, starts = 1L)
  trained_on_fold_1 <- paste0(c("fixed", "reset", "carry"), "_2")
  check("answers in fold 2 cannot change the fits to fold 1",
        identical(comparison$fits[trained_on_fold_1], flipped_comparison$fits[trained_on_fold_1]))
  check("in each fold the carryover fit is at least as good as the reset fit",
        all(vapply(1:2, function(f) {
          comparison$fits[[paste0("carry_", f)]]$nll <=
            comparison$fits[[paste0("reset_", f)]]$nll + 1e-5
        }, logical(1))))
  # With one starting vector, the third entry of the carryover fit's start_nll
  # is the reset solution itself, evaluated without a search (Supplement S2).
  check("the reset solution is a candidate of the carryover fit in each fold",
        all(vapply(1:2, function(f) {
          isTRUE(all.equal(comparison$fits[[paste0("carry_", f)]]$start_nll[3],
                           comparison$fits[[paste0("reset_", f)]]$nll, tolerance = 1e-12))
        }, logical(1))))
  # The reset solution is a candidate but not a search. With no iterations
  # allowed, no nlminb search converges, so the carryover fit has to be refined
  # before it counts as converged (Supplement S2).
  unsearched <- inference_fit(inference_subset(experiment$observed, which(fold != 1L)),
                              inference_spec("carry"), starts = 1L, iter_max = 0L,
                              extra_start = comparison$fits$reset_1$par)
  check("the reset solution alone does not count as a converged search",
        unsearched$refined && unsearched$converged)

  # The decision rule of Supplement S2, run through inference_compare() with
  # inference_fit() and inference_loglik_cpp() replaced by stand-ins that give
  # four participants chosen held-out log likelihoods. Carryover minus
  # fixed-rate is (1, 1.1, .9, 1), about 24 standard errors, or (-1, 1, 2, 3),
  # 1.46 standard errors.
  decide <- function(carry_vs_fixed, carry_vs_reset) {
    ll <- cbind(fixed = -carry_vs_fixed, reset = -carry_vs_reset, carry = 0)
    stand_ins <- new.env(parent = environment(inference_compare))
    stand_ins$inference_fit <- function(observed, spec, ...) {
      list(par = spec$model, spec = spec, converged = TRUE)
    }
    stand_ins$inference_loglik_cpp <- function(par, adaptive, y, ...) ll[y[, 1], par]
    compare <- inference_compare
    environment(compare) <- stand_ins
    compare(list(y = cbind(1:4, 0), allocation = c(-1L, 1L, -1L, 1L), n_train = 10L))$decision
  }
  clear <- c(1, 1.1, .9, 1)
  narrow <- c(-1, 1, 2, 3)
  check("the decision rule: carryover, no carryover, and unresolved at 1.46 standard errors",
        decide(clear, clear) == "carry" && decide(-clear, clear) == "no_carry" &&
          decide(narrow, clear) == "unresolved")

  # Equation 8 changes associability in proportion to the difference between
  # the errors, so the likelihood is continuous at g_V = 0 and at d = 0
  # (Supplement S2).
  par <- scenarios$associability$par
  loglik_at <- function(changes) {
    changed <- par
    changed[names(changes)] <- changes
    sum(inference_loglik_cpp(changed, TRUE, experiment$observed$y,
                             experiment$observed$allocation))
  }
  # A jump at zero would make the difference quotient grow as the step shrinks;
  # here it stays the same for steps of 1e-6 and 1e-8.
  quotient <- function(name, step) {
    (loglik_at(setNames(step, name)) - loglik_at(setNames(0, name))) / step
  }
  for (name in c("g_value", "cue")) {
    check(sprintf("the log likelihood has no jump at %s = 0",
                  c(g_value = "g_V", cue = "d")[[name]]),
          abs(quotient(name, 1e-8) / quotient(name, 1e-6) - 1) < 1e-3)
  }

  # The starting vectors of the fits: the four fixed vectors, then points of
  # the Halton sequence within the bounds. A parameter's starting values do not
  # depend on which other parameters a candidate estimates.
  starts <- inference_starts(48L, inference_names)
  within <- vapply(starts, function(x) {
    all(x >= inference_bounds["lower", ] & x <= inference_bounds["upper", ])
  }, logical(1))
  check("48 starting vectors: the four fixed vectors first, all within the bounds",
        length(starts) == 48L && all(within) &&
          identical(starts[1:4], lapply(1:4, function(i) inference_anchors[i, ])))
  check("the fifth starting vector is the first Halton point: 1/2, 1/3, 1/5, ..., 1/23 of each range",
        all_close(starts[[5]], inference_bounds["lower", ] + 1 / c(2, 3, 5, 7, 11, 13, 17, 19, 23) *
                    (inference_bounds["upper", ] - inference_bounds["lower", ])))
  # eta is the sixth parameter the reset candidate estimates with g_V fixed at
  # 0, and the seventh of inference_names.
  reset_free <- inference_spec("reset", value = FALSE)$free
  check("a parameter's starting values are the same whichever parameters are estimated",
        identical(lapply(inference_starts(48L, reset_free), `[`, "eta"),
                  lapply(starts, `[`, "eta")))

  # Each experiment sets its own seed, so the records do not depend on the
  # number of workers. On Windows mclapply runs serially and the check is trivial.
  draw <- function(i) inference_generate(8, scenarios$null$par, FALSE, 519100L + i)
  one_worker <- lapply(1:4, draw)
  two_workers <- parallel::mclapply(1:4, draw, mc.set.seed = FALSE,
                                    mc.cores = if (.Platform$OS.type == "windows") 1L else 2L)
  check("per-experiment seeds give the same records with one or two workers",
        identical(one_worker, two_workers))

  cat(sprintf("All %d inference checks passed.\n", n_checks))
})
