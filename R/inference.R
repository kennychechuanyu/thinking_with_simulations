# Sections 6 and 7 and Supplement S2: the simulated experiments, the
# fixed-rate, reset and carryover candidates with their likelihood and fitting,
# the B-D difference with its interval, and the two-fold held-out comparison.
# The learning recursions are compiled from R/inference.cpp;
# tests/verify_inference.R checks them against inference_reference(), which
# uses R/models.R. Needs Rcpp and a C++ compiler.
source("R/models.R")
if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Sections 6 and 7 require the Rcpp package.")
Rcpp::sourceCpp("R/inference.cpp", cacheDir = file.path(tempdir(), "inference-cache"))

# Parameters, with their symbols and estimation bounds in Supplement S2.5:
#   mu                 mu         midpoint of the baselines        [-3.5, 1]
#   spread             tau        half the distance between types  [0, 1]
#   cue                d          shift of each physical light     [-0.65, 0.65]
#   intercept          b          response intercept               [-8, 2]
#   log_slope          log gamma  response slope                   [log 1.5, log 16]
#   g_value            g_V        carryover of learned value       [0, 1]
#   eta                eta        rate of change of associability  [0, .20]
#   g_alpha            g_alpha    carryover of associability       [0, 1]
#   history_intercept  b_B        intercept added for B            [-2.5, 2.5]
inference_names <- c("mu", "spread", "cue", "intercept", "log_slope",
                     "g_value", "eta", "g_alpha", "history_intercept")
inference_bounds <- rbind(
  lower = c(-3.5, 0, -.65, -8, log(1.5), 0, 0, 0, -2.5),
  upper = c(1, 1, .65, 2, log(16), 1, .20, 1, 2.5)
)
colnames(inference_bounds) <- inference_names

# The parameters a candidate of Table 7 estimates (Supplement S2.5).
# value = FALSE fixes g_V at 0, as in the restricted comparison of Section 7.1,
# and response = TRUE also estimates b_B (Section 7.3).
inference_spec <- function(model = c("fixed", "reset", "carry"),
                           value = TRUE, response = FALSE) {
  model <- match.arg(model)
  free <- c("mu", "spread", "cue", "intercept", "log_slope",
            if (value) "g_value", if (model != "fixed") "eta",
            if (model == "carry") "g_alpha", if (response) "history_intercept")
  list(model = model, adaptive = model != "fixed", value = value,
       response = response, free = free)
}

# Reference version of inference_prob_cpp(), built from the simulators in
# R/models.R. It returns the same matrix of response probabilities: rows are
# counterbalancing group -1 (slower, then faster learning-rate type) and group
# +1 (slower, faster); columns are B's pear trials, then D's. Training here
# runs the A+, X+, AB+ and CD+ trials in four blocks, whereas the compiled
# version alternates A+ with X+ and AB+ with CD+; the two histories share no
# cues, so either order gives every cue the same sequence of updates.
inference_reference <- function(par, adaptive, n_train = 10L, n_pear = 10L) {
  names(par) <- inference_names
  cues <- c("A", "B", "C", "D", "X")
  training <- c(make_trials("A", 1, n_train, "Phase 1"),
                make_trials("X", 1, n_train, "Phase 1"),
                make_trials(c("A", "B"), 1, n_train, "Phase 2"),
                make_trials(c("C", "D"), 1, n_train, "Phase 2"))
  prob <- matrix(NA_real_, 4L, 2L * n_pear)
  for (allocation in c(-1L, 1L)) {
    for (type in c(-1L, 1L)) {
      rates <- setNames(.5 * plogis(par["mu"] + type * par["spread"] +
                                    c(0, allocation, 0, -allocation, 0) * par["cue"]), cues)
      if (adaptive) {
        trained <- adaptive_simulate(training, S = 2 * rates,
                                     alpha_init = setNames(rep(.5, 5), cues),
                                     eta = par["eta"])
        value <- extract_final_V(trained) * par["g_value"]
        associability <- .5 + par["g_alpha"] * (extract_final_alpha(trained) - .5)
      } else {
        trained <- rw_simulate(training, beta = rates)
        value <- extract_final_V(trained) * par["g_value"]
      }
      row <- 2L * (allocation == 1L) + (type == 1L) + 1L
      for (cue in c("B", "D")) {
        pear <- make_trials(cue, 1, n_pear, "Phase 3")
        run <- if (adaptive) {
          adaptive_simulate(pear, S = 2 * rates, alpha_init = associability, V_init = value,
                            cue_names = cues, eta = par["eta"])
        } else {
          rw_simulate(pear, beta = rates, V_init = value, cue_names = cues)
        }
        # The values before each pear trial's outcome; trial n_pear + 1 holds
        # those after the last.
        pear_values <- run$V[run$cue == cue & run$trial <= n_pear]
        col <- seq_len(n_pear) + (cue == "D") * n_pear
        prob[row, col] <- plogis(par["intercept"] + exp(par["log_slope"]) * pear_values +
                                   (cue == "B") * par["history_intercept"])
      }
    }
  }
  prob
}

# One simulated experiment (Section 6.1, Supplement S2): exactly balanced
# counterbalancing groups (allocation), block order balanced within each group,
# an independently drawn learning-rate type for each participant, and yes or no
# responses drawn from Equation S3, which adds b_B for B. The seed fixes the
# experiment. The analyses receive only $observed; $truth keeps what generated
# the responses, with the types coded 1 (slower) and 2 (faster) and d_first
# marking the participants who receive the D block first.
inference_generate <- function(n, par, adaptive, seed, n_train = 10L,
                               n_pear = 10L) {
  stopifnot(n >= 4, n %% 2 == 0, length(par) == 9L)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  allocation <- sample(rep(c(-1L, 1L), each = n / 2L))
  type <- sample.int(2L, n, replace = TRUE)
  # Within each group, every second participant receives the D block first.
  d_first <- ave(seq_len(n), allocation, FUN = seq_along) %% 2L == 0L
  group <- 2L * (allocation == 1L) + type
  probability <- inference_prob_cpp(par, adaptive, n_train, n_pear)[group, , drop = FALSE]
  probability[d_first, ] <-
    inference_prob_cpp(par, adaptive, n_train, n_pear, TRUE)[group[d_first], , drop = FALSE]
  y <- matrix(rbinom(length(probability), 1L, as.vector(probability)), n, 2L * n_pear)
  list(observed = list(y = y, allocation = allocation, n_train = n_train),
       truth = list(par = par, adaptive = adaptive, type = type, d_first = d_first,
                    probability = probability, seed = seed))
}

# The pear trials whose responses an analysis keeps (Supplement S2.4): all ten
# of each light (full), pear trials 1-6 (early) or pear trials 8-10 (late). The
# other responses become NA; every pear trial still updates the learned state.
inference_view <- function(observed, view = c("full", "early", "late")) {
  view <- match.arg(view)
  n_pear <- ncol(observed$y) / 2L
  keep <- switch(view, full = seq_len(n_pear), early = seq_len(min(6L, n_pear)),
                 late = seq.int(max(1L, n_pear - 2L), n_pear))
  observed$y[, setdiff(seq_len(2L * n_pear), c(keep, n_pear + keep))] <- NA_integer_
  observed
}

# The B-D difference H-hat of Section 6.2 and Supplement S2.6: each
# participant's proportion of yes responses to D minus that to B over the kept
# pear trials, averaged over participants (with exactly balanced groups, the
# average of the two counterbalancing groups), and its 95% interval, with the
# variance computed within each group and Welch-Satterthwaite degrees of
# freedom. A positive difference is detected when the interval lies above zero.
inference_effect <- function(observed) {
  n_pear <- ncol(observed$y) / 2L
  y_B <- observed$y[, seq_len(n_pear), drop = FALSE]
  y_D <- observed$y[, n_pear + seq_len(n_pear), drop = FALSE]
  difference <- rowMeans(y_D, na.rm = TRUE) - rowMeans(y_B, na.rm = TRUE)
  groups <- split(difference, observed$allocation)
  variance <- vapply(groups, function(x) var(x) / (4 * length(x)), 0.)
  df <- sum(variance)^2 / sum(variance^2 / (lengths(groups) - 1L))
  se <- sqrt(sum(variance))
  # The Welch degrees of freedom are undefined when neither group varies.
  if (!is.finite(df)) df <- length(difference) - 2L
  ci <- mean(difference) + c(-1, 1) * qt(.975, df) * se
  c(estimate = mean(difference), se = se, lower = ci[1], upper = ci[2],
    detected = as.numeric(ci[1] > 0))
}

# Starting vectors of the fits (Supplement S2.7): four fixed vectors, then the
# points of a Halton sequence, which spreads the remaining starts evenly over
# the estimation bounds. The sequence uses base 2, 3, 5, ..., 23 for the
# parameters in the order of inference_names, so a parameter's starting values
# do not depend on which other parameters a candidate estimates.
inference_anchors <- rbind(
  c(-1.3, .25, .18, -3, log(6), .15, .035, .5, 0),
  c(-.4, .55, -.18, -5, log(10), .55, .12, .85, -.6),
  c(-.8, .65, .35, -4.5, log(9), .65, .10, .3, -.8),
  c(-1.7, .15, -.35, -2, log(4), .05, .025, .7, .3)
)
colnames(inference_anchors) <- inference_names
inference_starts <- function(n, free) {
  starts <- lapply(seq_len(min(n, nrow(inference_anchors))),
                   function(i) inference_anchors[i, free])
  if (n > nrow(inference_anchors)) {
    lower <- inference_bounds["lower", free]
    upper <- inference_bounds["upper", free]
    bases <- c(2, 3, 5, 7, 11, 13, 17, 19, 23)[match(free, inference_names)]
    for (i in seq_len(n - nrow(inference_anchors))) {
      # Radical inverse of i in each base: the i-th point of the sequence.
      point <- vapply(bases, function(base) {
        x <- 0
        f <- 1
        j <- i
        while (j > 0) {
          f <- f / base
          x <- x + f * (j %% base)
          j <- j %/% base
        }
        x
      }, numeric(1))
      starts[[length(starts) + 1L]] <- setNames(lower + point * (upper - lower), free)
    }
  }
  starts
}

# Maximum-likelihood fit of one candidate (Supplement S2.7): one nlminb search
# from each of `starts` starting vectors (48 by default), keeping the best
# search, which a Nelder-Mead search refines when nlminb reports no convergence
# near the best value. A criterion function replaces the negative log
# likelihood (for the profile likelihood and the mimicry fits of Section 6.4),
# and extra_start adds a start, such as the fitted reset solution for the
# carryover candidate.
inference_fit <- function(observed, spec, starts = 48L, iter_max = 600L,
                          criterion = NULL, extra_start = NULL) {
  free <- spec$free
  # The full parameter vector; parameters the candidate does not estimate are 0.
  unpack <- function(x) {
    par <- setNames(numeric(9), inference_names)
    par[free] <- x
    par
  }
  # A non-finite value becomes 1e100, a very poor finite value, so that the
  # optimizer steps back instead of failing.
  objective <- function(x) {
    val <- if (is.null(criterion)) {
      -sum(inference_loglik_cpp(unpack(x), spec$adaptive, observed$y, observed$allocation,
                                observed$n_train))
    } else {
      criterion(unpack(x))
    }
    if (is.finite(val)) val else 1e100
  }
  control <- list(iter.max = iter_max, eval.max = 2L * iter_max, rel.tol = 1e-8, x.tol = 1e-7)
  start_vectors <- inference_starts(starts, free)
  if (!is.null(extra_start)) start_vectors <- c(start_vectors, list(extra_start[free]))
  searches <- lapply(start_vectors, function(x) {
    tryCatch(
      nlminb(x, objective, lower = inference_bounds["lower", free],
             upper = inference_bounds["upper", free], control = control),
      error = function(e) list(objective = Inf, convergence = 99L,
                               message = conditionMessage(e), par = x)
    )
  })
  # The nested solution is itself a candidate, so the carryover fit can never
  # be worse than the reset fit it starts from. It is not the result of a
  # search, so it does not count as a converged search.
  nested <- logical(length(searches))
  if (!is.null(extra_start)) {
    searches[[length(searches) + 1L]] <- list(par = extra_start[free],
      objective = objective(extra_start[free]), convergence = NA_integer_,
      message = "evaluated nested-model candidate")
    nested <- c(nested, TRUE)
  }
  good <- vapply(searches, function(x) is.finite(x$objective) && isTRUE(x$convergence == 0L),
                 logical(1))
  objectives <- vapply(searches, `[[`, numeric(1), "objective")
  # Associability is kept within [.01, 1] (Equation 3), so the likelihood has
  # kinks where an associability reaches a bound, and nlminb can stop near the
  # best value without reporting convergence. If no converged search is within
  # 1e-5 of the smallest value, a Nelder-Mead search refines the best one within
  # the bounds, and its result joins the searches.
  refined <- !any(good & objectives <= min(objectives) + 1e-5)
  if (refined) {
    lower <- inference_bounds["lower", free]
    upper <- inference_bounds["upper", free]
    clamp <- function(x) pmin(upper, pmax(lower, x))
    penalized <- function(x) objective(clamp(x)) + 1e3 * sum(((x - clamp(x)) / (upper - lower))^2)
    search <- optim(searches[[which.min(objectives)]]$par, penalized, method = "Nelder-Mead",
                    control = list(maxit = 5000L, reltol = 1e-10, parscale = (upper - lower) / 20))
    searches[[length(searches) + 1L]] <- list(par = clamp(search$par),
      objective = objective(clamp(search$par)), convergence = search$convergence,
      message = paste("Nelder-Mead refinement, code", search$convergence))
    good <- c(good, search$convergence == 0L)
    objectives <- c(objectives, searches[[length(searches)]]$objective)
    nested <- c(nested, FALSE)
  }
  # The fit counts as converged when a converged search is within 1e-5 of the
  # smallest value. The best converged search or nested solution within 1e-5 is
  # then selected; if no converged search is that close, the best search is
  # kept and reported as not converged.
  near <- objectives <= min(objectives) + 1e-5
  converged <- any(good & near)
  eligible <- which(near & (good | nested))
  chosen <- if (converged) eligible[which.min(objectives[eligible])] else which.min(objectives)
  best <- searches[[chosen]]
  par <- unpack(best$par)
  # Estimates within 0.1% of the bound range from a bound are flagged.
  span <- inference_bounds["upper", free] - inference_bounds["lower", free]
  boundary <- (par[free] - inference_bounds["lower", free]) / span < .001 |
    (inference_bounds["upper", free] - par[free]) / span < .001
  list(par = par, nll = best$objective, converged = converged,
       code = best$convergence, message = best$message, starts_ok = sum(good),
       start_nll = objectives, refined = refined,
       boundary = paste(free[boundary], collapse = ";"), spec = spec)
}

# Participants are assigned to the folds in turn within each counterbalancing
# group, so that the two groups are balanced within each fold (Section 6.3,
# Supplement S2.7).
inference_folds <- function(observed, folds = 2L) {
  fold <- integer(nrow(observed$y))
  for (allocation in c(-1L, 1L)) {
    rows <- which(observed$allocation == allocation)
    fold[rows] <- rep(seq_len(folds), length.out = length(rows))
  }
  fold
}

# The records of the participants in rows.
inference_subset <- function(observed, rows) {
  list(y = observed$y[rows, , drop = FALSE], allocation = observed$allocation[rows],
       n_train = observed$n_train)
}

# Held-out comparison of the three candidates (Section 6.3, Supplement S2.7).
# Each candidate is fitted to the participants outside a fold and scored on
# that fold; the carryover fit also starts from the reset fit to the same
# participants.
inference_compare <- function(observed, value = TRUE, response = FALSE,
                              starts = 48L, folds = 2L) {
  models <- c("fixed", "reset", "carry")
  fold <- inference_folds(observed, folds)
  ll <- matrix(NA_real_, nrow(observed$y), length(models), dimnames = list(NULL, models))
  fits <- list()
  for (model in models) {
    for (f in seq_len(folds)) {
      nested <- if (model == "carry") fits[[paste("reset", f, sep = "_")]]$par else NULL
      fit <- inference_fit(inference_subset(observed, which(fold != f)),
                           inference_spec(model, value, response), starts, extra_start = nested)
      held_out <- which(fold == f)
      ll[held_out, model] <- inference_loglik_cpp(fit$par, fit$spec$adaptive,
        observed$y[held_out, , drop = FALSE], observed$allocation[held_out], observed$n_train)
      fits[[paste(model, f, sep = "_")]] <- fit
    }
  }
  scores <- colSums(ll)
  winner <- names(which.max(scores))
  # The held-out score of one candidate minus that of each of the others, with
  # the participant-level standard error sqrt(N) SD.
  score_differences <- function(candidate, others) {
    t(vapply(others, function(other) {
      d <- ll[, candidate] - ll[, other]
      c(difference = sum(d), se = sqrt(length(d)) * sd(d))
    }, c(difference = 0, se = 0)))
  }
  # The ranking is resolved when the best candidate's held-out score exceeds
  # each other candidate's by more than two participant-level standard errors.
  contrasts <- score_differences(winner, setdiff(models, winner))
  resolved <- all(contrasts[, "difference"] > 2 * contrasts[, "se"])
  # The decision of the comparison (Section 6.3): carryover when it beats both
  # alternatives by this margin, no associability carryover when either
  # alternative beats it by the margin, and otherwise unresolved.
  # model_decision is the winner when the ranking is resolved. A fit that did
  # not converge gives fit_problem.
  carry_contrasts <- score_differences("carry", c("fixed", "reset"))
  mechanism <- if (all(carry_contrasts[, "difference"] > 2 * carry_contrasts[, "se"])) {
    "carry"
  } else if (any(carry_contrasts[, "difference"] < -2 * carry_contrasts[, "se"])) {
    "no_carry"
  } else {
    "unresolved"
  }
  converged <- all(vapply(fits, `[[`, logical(1), "converged"))
  list(loglik = ll, scores = scores, winner = winner,
       decision = if (!converged) "fit_problem" else mechanism,
       model_decision = if (!converged) "fit_problem" else if (resolved) winner else "unresolved",
       contrasts = contrasts, carry_contrasts = carry_contrasts, fits = fits)
}

# Generating processes of Supplement S2.4: carried associability
# (associability), reset associability (reset), carried learned value (value),
# physical-light differences alone (null) and a lower response intercept for B
# (response). Each is built from the base parameter set
# (mu, tau, d, b, gamma, g_V, eta, g_alpha, b_B) =
# (logit .30, 0.35, 0.20, -4, 8, 0, .05, .80, 0); analysis/section6_study.R
# sets the other three parameter sets of each.
inference_scenarios <- function() {
  baseline <- setNames(c(qlogis(.3), .35, .20, -4, log(8), 0, .05, .8, 0),
                       inference_names)
  scenario <- function(adaptive, change = numeric()) {
    par <- baseline
    par[names(change)] <- change
    list(par = par, adaptive = adaptive)
  }
  list(
    associability = scenario(TRUE),
    # Reset associability: associability changes during the pear trials, but
    # nothing that changed during the compound trials carries over
    # (g_alpha = 0). It generates responses as the reset candidate assumes.
    reset = scenario(TRUE, c(g_alpha = 0)),
    value = scenario(FALSE, c(g_value = .65, eta = 0, g_alpha = 0)),
    null = scenario(FALSE, c(eta = 0, g_alpha = 0)),
    response = scenario(FALSE, c(eta = 0, g_alpha = 0, history_intercept = -.9))
  )
}
