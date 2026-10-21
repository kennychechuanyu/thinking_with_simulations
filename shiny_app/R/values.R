# The model results the page shows, computed once when the app starts. Shiny
# sources the files in R/ in alphabetical order, so this file comes after
# engine.R, which defines the functions used here.

base     <- run_inquiry(default_settings)
trained  <- base$trained
trace    <- compound_trace(default_settings)
n_base   <- default_settings$n_compound
n_more   <- 30                               # compound trials in Step 2's longer training
more     <- trained_states(settings_with(n_compound = n_more))
co       <- continue_o1(default_settings)
# The gaps after eight updates as g_V varies, and the g_V at which the two
# models' gaps are equal (Figure 7a; the curves cross once, between .5 and 1).
gv_grid  <- gap_by_value_carryover(k = 8)
gv_cross <- uniroot(function(g) {
  r <- run_inquiry(settings_with(g_V = g))
  r$rw$gap[9] - r$adaptive$gap[9]
}, c(.5, 1), tol = 1e-8)$root
gv1      <- run_inquiry(settings_with(g_V = 1))
# Half of the associability change carried over to O2 (Section 4.3).
ga_half  <- run_inquiry(settings_with(g_alpha = .5))
# E one part in a million more or less salient than D (Section 4.5).
e_up     <- run_inquiry(settings_with(e_offset = 1e-6))
e_down   <- run_inquiry(settings_with(e_offset = -1e-6))
peak_k   <- which.max(base$adaptive$gap) - 1
at       <- function(x, k) x[k + 1]          # value after k completed updates
ad_B     <- base$adaptive$B
ad_D     <- base$adaptive$D

# The simulated experiments of Step 5, written by shiny_app/make_inference_cases.R.
inference   <- readRDS("data/inference_cases.rds")
cases       <- inference$cases
rates       <- inference$rates
zero_rates  <- inference$zero_rates
first_rates <- inference$first_rates
