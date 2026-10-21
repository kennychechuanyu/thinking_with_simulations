# Section 4.5: which conclusions depend on the specification? The control
# condition matches D and E. Computes D's associability after compound training
# and the B-D gap after eight updates with O2 when E's salience equals D's, is
# one part in 10^6 higher or lower, or is one part in 100 higher. Supplement S3
# makes the same comparison under a rank-based update.
#   Rscript analysis/section4_5_matched_control.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/models.R")
source("R/figure_style.R")  # paper_number()

# Settings of Section 3 for the adaptive model, and the relative differences
# between E's and D's saliences.
lambda <- 1
salience <- .3
alpha_init <- .5
eta <- .05
n_trials <- 10
k_compared <- 8
mismatches <- c(equal = 0, higher_1e6 = 1e-6, lower_1e6 = -1e-6, higher_1e2 = .01)

# B's run does not involve E, so it is the same in all cases: A+ then AB+, then
# eight trials alone with O2.
blocked <- adaptive_simulate(c(make_trials("A", lambda, n_trials, "Phase 1"),
                               make_trials(c("A", "B"), lambda, n_trials, "Phase 2")),
                             S = c(A = salience, B = salience),
                             alpha_init = c(A = alpha_init, B = alpha_init),
                             eta = eta)
transfer_B <- adaptive_simulate(make_trials("B", lambda, k_compared, "Phase 3"),
                                S = c(B = salience),
                                alpha_init = extract_final_alpha(blocked)["B"],
                                eta = eta, V_init = c(B = 0), cue_names = "B")

# D's associability after C+ then DE+ training with E's salience set to
# salience_E, and the gap H(8) once B and D have learned about O2 alone.
alpha_D_and_gap <- function(salience_E) {
  control <- adaptive_simulate(c(make_trials("C", lambda, n_trials, "Phase 1"),
                                 make_trials(c("D", "E"), lambda, n_trials, "Phase 2")),
                               S = c(C = salience, D = salience, E = salience_E),
                               alpha_init = c(C = alpha_init, D = alpha_init, E = alpha_init),
                               eta = eta)
  transfer_D <- adaptive_simulate(make_trials("D", lambda, k_compared, "Phase 3"),
                                  S = c(D = salience),
                                  alpha_init = extract_final_alpha(control)["D"],
                                  eta = eta, V_init = c(D = 0), cue_names = "D")
  c(alpha_D = extract_final_alpha(control)[["D"]],
    H8 = extract_final_V(transfer_D)[["D"]] - extract_final_V(transfer_B)[["B"]])
}

matched_control <- t(sapply(mismatches, function(m) alpha_D_and_gap(salience * (1 + m))))
change <- sweep(matched_control, 2, matched_control["equal", ])

cat("Section 4.5, E's salience compared with D's: alpha_D after compound training, H(8)\n")
cat(sprintf("%-28s alpha_D = %s   H(8) = %s   change in H(8) = %s\n",
            c("equal", "one part in 10^6 higher", "one part in 10^6 lower",
              "one part in 100 higher"),
            paper_number(matched_control[, "alpha_D"], 7),
            paper_number(matched_control[, "H8"], 7),
            formatC(change[, "H8"], format = "e", digits = 1)), sep = "")

# Values reported in the paper
stopifnot(
  abs(matched_control["equal", ] - c(.5, .284)) < 5e-4,           # Section 4.5
  matched_control["higher_1e6", "alpha_D"] < .5,                  # Section 4.5
  matched_control["lower_1e6", "alpha_D"] > .5,
  # Section 4.5: one part in 10^6 changes each value by less than one part in
  # 10^6; one part in 100 (S_E = .303) changes the gap by about .001
  max(abs(sweep(change[c("higher_1e6", "lower_1e6"), ], 2, matched_control["equal", ], "/"))) < 1e-6,
  abs(abs(change["higher_1e2", "H8"]) - .001) < 5e-4
)
