# The model results the page shows, computed once when the app starts. Shiny
# sources the files in R/ in alphabetical order, so this file comes after
# engine.R, which defines the functions used here.

at <- function(x, k) x[k + 1]                # value after k completed pear trials

# Step 1: the trained-tone history under Rescorla-Wagner, and the code check
# of Section 2.3 from starting values of .30 and of zero.
rw_tr   <- rw_trials(default_settings)
checks  <- list(thirty = compound_check(c(A = .3, B = .3)),
                zero = compound_check(c(A = 0, B = 0)))

# Step 2: the two histories under both models, and the pear trials with the
# settings of Section 3.4.
base     <- run_inquiry(default_settings)
trained  <- base$trained
tone     <- tone_alone(default_settings)
compound <- compound_trials(default_settings)
low_S    <- trained_states(settings_with(S = .24))   # learning rates that start at .12
n_more   <- 30                                       # compound trials in the longer training
more     <- trained_states(settings_with(n_compound = n_more))
co       <- continue_apple(default_settings)
peak_k   <- which.max(base$adaptive$gap) - 1
ad_B     <- base$adaptive$B
ad_D     <- base$adaptive$D

# Step 3: the interventions over twenty pear trials (Section 4.1), the
# carryover examples (Section 4.2) and the rules for associability (Section 4.4).
n_long    <- 20
runs      <- lapply(c(none = "none", prevent = "prevent", remove = "remove", supply = "supply"),
                    function(ch) run_inquiry(settings_with(change = ch), n = n_long))
remove_carried <- run_inquiry(settings_with(change = "remove", g_V = 1))
ga_half   <- run_inquiry(settings_with(g_alpha = .5))
# The gap over fifteen pear trials with the default settings and with eta = .01
# (Section 4.3).
base15    <- run_inquiry(default_settings, n = 15)
eta_low   <- run_inquiry(settings_with(eta = .01), n = 15)
gv1       <- run_inquiry(settings_with(g_V = 1))
gv90      <- run_inquiry(settings_with(g_V = .9))
# The gaps after eight pear trials as g_V varies, and the g_V at which the two
# models' gaps are equal (Figure 5a; the curves cross once, between .5 and 1).
gv_grid   <- gap_by_value_carryover(k = 8)
gv_cross  <- uniroot(function(g) {
  r <- run_inquiry(settings_with(g_V = g))
  r$rw$gap[9] - r$adaptive$gap[9]
}, c(.5, 1), tol = 1e-8)$root
# Table 5: D's associability and the gap over fifteen pear trials under each
# rule, with C matched to D and with C one part in a million more salient; and
# Equation 3 with C 1% more salient.
rule_names <- c(proportional = "Equation 3", rank = "Rank-based, ties unchanged",
                rank_ties = "Rank-based, ties lower associability")
rule_runs <- lapply(setNames(names(rule_names), names(rule_names)), function(rule)
  list(matched = rule_comparison(rule, 0), mismatched = rule_comparison(rule, 1e-6)))
one_percent <- rule_comparison("proportional", .01)

# Step 4: the adaptive model's learned values before each of the ten responses,
# k = 0, ..., 9, and the predicted behavior under both response functions.
responses <- data.frame(k = 0:9, V_B = at(ad_B$V, 0:9), V_D = at(ad_D$V, 0:9))
responses$p_B <- response_prob(responses$V_B, "sigmoid")
responses$p_D <- response_prob(responses$V_D, "sigmoid")
responses$t_B <- response_prob(responses$V_B, "threshold")
responses$t_D <- response_prob(responses$V_D, "threshold")
# The pear trials after which the threshold rule gives different responses, and
# the pear trial after which the sigmoid's difference is largest.
threshold_window <- responses$k[responses$t_D != responses$t_B]
peak_yes <- responses$k[which.max(responses$p_D - responses$p_B)]

# Steps 5 and 6: the simulated experiments, written by
# shiny_app/make_inference_cases.R.
inference   <- readRDS("data/inference_cases.rds")
cases       <- inference$cases
rates       <- inference$rates
zero_rates  <- inference$zero_rates
first_rates <- inference$first_rates
response_rates <- inference$response_rates

# Step 7: the two settings of the decision model.
dd <- ddm_summary()
