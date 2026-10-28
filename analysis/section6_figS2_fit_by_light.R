# Supplementary Figure S2 (Section 6.3, Supplement S2.8): the worked example
# for B and D separately, with the observed proportions of yes responses, the
# generating probabilities and the three candidates fitted to all 80
# participants. Reads the worked example saved by
# analysis/section6_worked_example.R and refits nothing.
#   Rscript analysis/section6_figS2_fit_by_light.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
source("R/inference.R")
library(patchwork)

ink <- paper_colours[["ink"]]
light <- paper_colours[["light"]]
theme_figure <- theme_paper() + theme(axis.title.x = element_text(margin = margin(t = 6)))

example <- readRDS("results/section6/worked_example.rds")
responses <- example$observed$y       # one row per participant; columns 1-10 B, 11-20 D
light_columns <- list(B = 1:10, D = 11:20)
# Response probabilities under the generating parameters ("truth") or a fitted
# candidate: one row per combination of counterbalancing group and
# learning-rate type.
probabilities <- function(model) {
  if (model == "truth") return(inference_prob_cpp(example$truth$par, example$truth$adaptive))
  fit <- example$fits[[model]]
  inference_prob_cpp(fit$par, fit$spec$adaptive)
}
observed <- do.call(rbind, lapply(names(light_columns), function(light_name) {
  data.frame(light = light_name, trial = 1:10,
             p = colMeans(responses[, light_columns[[light_name]]]))
}))
# Predicted proportions, averaged with equal weight over the two
# counterbalancing groups and the two learning-rate types.
curves <- do.call(rbind, lapply(c("truth", "carry", "reset", "fixed"), function(model) {
  p <- probabilities(model)
  do.call(rbind, lapply(names(light_columns), function(light_name) {
    data.frame(model = model, light = light_name, trial = 1:10,
               p = colMeans(p[, light_columns[[light_name]]]))
  }))
}))
candidate_names <- c(carry = "Carryover", reset = "Reset", fixed = "Fixed-rate")
fitted_curves <- subset(curves, model != "truth")
fitted_curves$candidate <- factor(candidate_names[fitted_curves$model], levels = candidate_names)

# One panel per light: the generating probabilities as a thick gray line, the
# fitted candidates as black lines, and the observed proportions as points.
panel_fit <- function(light_name, title) {
  ggplot() +
    geom_line(data = subset(curves, model == "truth" & light == light_name), aes(trial, p),
              colour = light, linewidth = 2.2, lineend = "round") +
    geom_line(data = subset(fitted_curves, light == light_name),
              aes(trial, p, linetype = candidate), colour = ink, linewidth = .5) +
    geom_point(data = subset(observed, light == light_name), aes(trial, p),
               colour = ink, size = 1.3) +
    scale_linetype_manual(values = c(Carryover = "solid", Reset = "22", `Fixed-rate` = "13"),
                          name = NULL) +
    scale_x_continuous(breaks = c(1, 5, 10)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    labs(title = title, x = "Pear trial", y = "Proportion of yes responses") +
    theme_figure + theme(legend.position = "bottom", legend.text = element_text(size = 9),
                         legend.key.width = unit(22, "pt"))
}
figure <- panel_fit("B", "(a)  Responses to B") + panel_fit("D", "(b)  Responses to D") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_figure(figure, "figS2_fit_by_light", 6.27, 3.4)

# Values reported in the paper
stopifnot(
  # Figure S2 caption and Supplement S2.4: 80 participants, seed 8123
  nrow(responses) == 80, example$truth$seed == 8123,
  # Supplement S2.8: no participant responded yes to either light on the first
  # pear trial
  observed$p[observed$trial == 1] == 0,
  # Every curve is a proportion
  all(curves$p >= 0 & curves$p <= 1)
)
