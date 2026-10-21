# Supplementary Figure S2 (Supplement S2): the worked example of Section 5.2
# for B and D separately, with the observed yes proportions, the generating
# probabilities and the three candidates fitted to all 80 participants. Reads
# the worked example saved by analysis/section5_worked_example.R.
#   Rscript analysis/section5_figS2_fit_by_cue.R

if (!file.exists("R/figure_style.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
source("R/inference.R")
library(patchwork)

example <- readRDS("results/section5/worked_example.rds")
answers <- example$observed$y         # one row per participant; columns 1-10 B, 11-20 D
cue_columns <- list(B = 1:10, D = 11:20)

# Response probabilities under the generating parameters ("truth") or a fitted
# candidate: one row per combination of cue allocation and rate type.
probabilities <- function(model) {
  if (model == "truth") return(inference_prob_cpp(example$truth$par, example$truth$adaptive))
  fit <- example$fits[[model]]
  inference_prob_cpp(fit$par, fit$spec$adaptive)
}
observed <- do.call(rbind, lapply(names(cue_columns), function(cue) {
  data.frame(cue = cue, q = 1:10, p = colMeans(answers[, cue_columns[[cue]]]))
}))
# Predicted yes proportions, averaged with equal weight over the two cue
# allocations and the two rate types.
curves <- do.call(rbind, lapply(c("truth", "carry", "reset", "fixed"), function(model) {
  p <- probabilities(model)
  do.call(rbind, lapply(names(cue_columns), function(cue) {
    data.frame(model = model, cue = cue, q = 1:10, p = colMeans(p[, cue_columns[[cue]]]))
  }))
}))

ink <- paper_colours[["ink"]]
panel <- function(cue_name, title) {
  ggplot() +
    geom_line(data = subset(curves, model == "truth" & cue == cue_name), aes(q, p),
              colour = "#D4D4D4", linewidth = 2.4, lineend = "round") +
    geom_point(data = subset(observed, cue == cue_name), aes(q, p), colour = ink, size = 1.3) +
    geom_line(data = subset(curves, model != "truth" & cue == cue_name),
              aes(q, p, colour = model), linewidth = .7) +
    scale_colour_manual(values = paper_colours[c("fixed", "reset", "carry")],
                        labels = c(carry = "Carryover", reset = "Reset", fixed = "Fixed-rate"),
                        breaks = c("carry", "reset", "fixed")) +
    scale_x_continuous(breaks = c(1, 5, 10)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    labs(title = title, x = expression("Presentation with the new outcome, " * italic(q)),
         y = "Proportion yes") +
    theme_supplement()
}
figure <- panel("B", "(a)  Blocked cue B") + panel("D", "(b)  Control cue D") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_figure(figure, "figS2_fit_by_cue", 6.27, 3.4)

# Values reported in the paper
stopifnot(
  # Figure S2 caption and Supplement S2: 80 participants, seed 8123
  nrow(answers) == 80, example$truth$seed == 8123
)
