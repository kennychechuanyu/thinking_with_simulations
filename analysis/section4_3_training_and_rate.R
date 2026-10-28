# Section 4.3: how much training, and when to measure. Runs the adaptive model
# with 1-20 compound trials and eta = .01-.05, 100 settings in all, follows each
# for fifteen pear trials, and computes the values reported in Section 4.3.
# tests/verify_models.R checks the conditions of Supplement S4.1 in the same
# settings.
#   Rscript analysis/section4_3_training_and_rate.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")

# Settings of Section 3, apart from the number of compound trials and eta.
salience <- .3
alpha_init <- .5


# The B-D gap V_D(k) - V_B(k), k = 0, ..., n, after n_compound compound trials
# with eta, the rate at which associability changes, which also applies during
# the pear trials. The lights' learned values for the pear start at zero, and
# their associabilities carry over in full.
gap_for <- function(n_compound, eta, n = 15) {
  blocking_end <- adaptive_final_state(c(make_trials("A", 1, 10, "Phase 1"),
                                         make_trials(c("A", "B"), 1, n_compound, "Phase 2")),
                                       S = c(A = salience, B = salience),
                                       alpha_init = c(A = alpha_init, B = alpha_init), eta = eta)
  control_end <- adaptive_final_state(c(make_trials("X", 1, 10, "Phase 1"),
                                        make_trials(c("C", "D"), 1, n_compound, "Phase 2")),
                                      S = c(X = salience, C = salience, D = salience),
                                      alpha_init = c(X = alpha_init, C = alpha_init,
                                                     D = alpha_init), eta = eta)
  pear <- function(cue, alpha_start) {
    adaptive_simulate(make_trials(cue, 1, n, "Pear"), S = setNames(salience, cue),
                      alpha_init = setNames(alpha_start, cue), eta = eta,
                      V_init = setNames(0, cue), cue_names = cue)$V
  }
  pear("D", control_end$alpha[["D"]]) - pear("B", blocking_end$alpha[["B"]])
}
settings <- expand.grid(n_compound = 1:20, eta = (1:5) / 100)
gaps <- Map(gap_for, settings$n_compound, settings$eta)
settings$largest_gap <- sapply(gaps, max)
settings$largest_after <- sapply(gaps, which.max) - 1
setting <- function(n, eta) which(settings$n_compound == n & abs(settings$eta - eta) < 1e-9)

stopifnot(
  nrow(settings) == 100,
  # the gap is positive after every pear trial in every setting
  all(sapply(gaps, function(H) all(H[-1] > 0))),
  # it is largest between the sixth and the tenth pear trial
  min(settings$largest_after) == 6, max(settings$largest_after) == 10,
  # with ten compound trials, lowering eta from .05 to .01 reduces the largest
  # gap from .284 to .037 and moves it from the eighth to the sixth pear trial
  paper_number(settings$largest_gap[setting(10, .05)], 3) == ".284",
  settings$largest_after[setting(10, .05)] == 8,
  paper_number(settings$largest_gap[setting(10, .01)], 3) == ".037",
  settings$largest_after[setting(10, .01)] == 6
)
cat(sprintf("Over the 100 settings, the largest B-D gap ranges from %.3f to %.3f and occurs after %d to %d pear trials\n",
            min(settings$largest_gap), max(settings$largest_gap),
            min(settings$largest_after), max(settings$largest_after)))
