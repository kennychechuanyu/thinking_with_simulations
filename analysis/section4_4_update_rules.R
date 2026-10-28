# Section 4.4 and Table 5: which conclusions depend on the rule for
# associability. Runs the comparison history with the new tone C slightly more
# salient than the comparison light D, under Equation 3 and under two
# rank-based rules (Supplement S3), and computes D's associability after the
# compound trials and the largest B-D gap during fifteen pear trials.
#   Rscript analysis/section4_4_update_rules.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")

# Settings of Section 3: salience .3, starting associability .5, eta = .05.
salience <- .3
alpha_init <- .5
eta <- .05

blocking_design <- c(make_trials("A", 1, 10, "Phase 1"),
                     make_trials(c("A", "B"), 1, 10, "Phase 2"))
control_design <- c(make_trials("X", 1, 10, "Phase 1"),
                    make_trials(c("C", "D"), 1, 10, "Phase 2"))
alpha_B <- adaptive_final_state(blocking_design, S = c(A = salience, B = salience),
                                alpha_init = c(A = alpha_init, B = alpha_init),
                                eta = eta)$alpha[["B"]]

# One light alone with the pear for n trials under Equation 3, k = 0, ..., n,
# with its value for the pear starting at zero.
pear_curve <- function(cue, alpha_start, n = 15) {
  adaptive_simulate(make_trials(cue, 1, n, "Pear"), S = setNames(salience, cue),
                    alpha_init = setNames(alpha_start, cue), eta = eta,
                    V_init = setNames(0, cue), cue_names = cue)$V
}


# Equation 3 with tone C's salience set to S_C: D's associability after the
# compound trials and the B-D gap during fifteen pear trials.
equation_3 <- function(S_C) {
  alpha_D <- adaptive_final_state(control_design, S = c(X = salience, C = S_C, D = salience),
                                  alpha_init = c(X = alpha_init, C = alpha_init,
                                                 D = alpha_init), eta = eta)$alpha[["D"]]
  list(alpha_D = alpha_D, gap = pear_curve("D", alpha_D) - pear_curve("B", alpha_B))
}
matched <- equation_3(salience)
one_percent <- equation_3(salience * 1.01)
one_millionth <- equation_3(salience * (1 + 1e-6))

# The rank-based rule of Supplement S3: associability moves the fraction step
# of the way toward 1 when the cue makes the smaller error and toward 0 when it
# makes the larger error. Errors closer than sqrt(machine epsilon) count as
# equal. Equal errors leave associability unchanged or, with ties_lower = TRUE,
# lower it as a larger error does. Learned values change as in Equation 2.
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
# Ten trials with the first cue alone, then ten compound trials, all followed
# by the apple, and then one light alone with the pear.
rank_history <- function(first, pair, step, S, ties_lower = FALSE) {
  cues <- unique(c(first, pair))
  state <- rank_run(rep(list(first), 10), setNames(rep(0, length(cues)), cues),
                    setNames(rep(alpha_init, length(cues)), cues), S, step, ties_lower)
  rank_run(rep(list(pair), 10), state$V, state$alpha, S, step, ties_lower)
}
rank_pear <- function(cue, alpha_start, step, n = 15) {
  state <- list(V = setNames(0, cue), alpha = setNames(alpha_start, cue))
  values <- 0
  for (m in seq_len(n)) {
    state <- rank_run(list(cue), state$V, state$alpha, setNames(salience, cue), step)
    values <- c(values, state$V[[cue]])
  }
  values
}
# B makes the larger error on every compound trial, so its associability ends
# at .5 (1 - step)^10, and the step that gives B the associability it has
# under Equation 3 can be found directly.
step <- 1 - (alpha_B / alpha_init)^(1 / 10)
S_all <- c(A = salience, B = salience, X = salience, C = salience, D = salience)
rank <- function(S_C, ties_lower = FALSE) {
  S <- replace(S_all, "C", S_C)
  B <- rank_history("A", c("A", "B"), step, S, ties_lower)
  D <- rank_history("X", c("C", "D"), step, S, ties_lower)
  list(alpha_B = B$alpha[["B"]], alpha_D = D$alpha[["D"]], V_B = B$V[["B"]], V_D = D$V[["D"]],
       gap = rank_pear("D", D$alpha[["D"]], step) - rank_pear("B", B$alpha[["B"]], step))
}
rank_matched <- rank(salience)
rank_one_millionth <- rank(salience * (1 + 1e-6))
rank_one_percent <- rank(salience * 1.01)
ties_matched <- rank(salience, ties_lower = TRUE)
ties_one_millionth <- rank(salience * (1 + 1e-6), ties_lower = TRUE)

largest <- function(run) paper_number(max(run$gap), 3)
largest_after <- function(run) which.max(run$gap) - 1

stopifnot(
  # Equation 3: D keeps .500 when C and D are matched; with C 1% more salient,
  # D ends at .498 and the gap after eight pear trials changes by .001
  paper_number(alpha_B, 3) == ".235", paper_number(matched$alpha_D, 3) == ".500",
  paper_number(one_percent$alpha_D, 3) == ".498",
  paper_number(abs(one_percent$gap[9] - matched$gap[9]), 3) == ".001",
  # Table 5, first row: .500 and .500, largest gap .284 after eight pear trials
  # with C and D matched and with C more salient by one part in a million
  paper_number(one_millionth$alpha_D, 3) == ".500",
  largest(matched) == ".284", largest_after(matched) == 8,
  largest(one_millionth) == ".284", largest_after(one_millionth) == 8,
  # the rank-based rule with ties unchanged: B again ends at .235; matched, D
  # keeps .500 and the gap is largest after five pear trials (.213); with C more
  # salient by one part in a million or by 1%, D falls to .254 and the largest
  # gap is .017, after six
  abs(rank_matched$alpha_B - alpha_B) < 1e-12, rank_matched$alpha_D == alpha_init,
  largest(rank_matched) == ".213", largest_after(rank_matched) == 5,
  paper_number(rank_one_millionth$alpha_D, 3) == ".254",
  largest(rank_one_millionth) == ".017", largest_after(rank_one_millionth) == 6,
  paper_number(rank_one_percent$alpha_D, 3) == ".254",
  largest(rank_one_percent) == ".017", largest_after(rank_one_percent) == 6,
  # the rank-based rule with ties lowering associability: D loses as much as B,
  # .235 either way, and with equal learned values there is no gap
  paper_number(ties_matched$alpha_D, 3) == ".235",
  paper_number(ties_one_millionth$alpha_D, 3) == ".235",
  abs(ties_matched$alpha_B - ties_matched$alpha_D) < 1e-12,
  abs(ties_one_millionth$alpha_B - ties_one_millionth$alpha_D) < 1e-12,
  abs(ties_matched$V_B - ties_matched$V_D) < 1e-12,
  abs(ties_one_millionth$V_B - ties_one_millionth$V_D) < 1e-12,
  max(abs(ties_matched$gap)) < 1e-12, max(abs(ties_one_millionth$gap)) < 1e-12
)
cat(sprintf(paste("With C more salient by one part in a million, D's associability is %s, %s and %s",
                  "under the three rules of Table 5, and the largest gap %s (%d), %s (%d) and none\n"),
            paper_number(one_millionth$alpha_D, 3), paper_number(rank_one_millionth$alpha_D, 3),
            paper_number(ties_one_millionth$alpha_D, 3),
            largest(one_millionth), largest_after(one_millionth),
            largest(rank_one_millionth), largest_after(rank_one_millionth)))
