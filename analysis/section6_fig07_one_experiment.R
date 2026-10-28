# Figure 7 (Sections 6.2 and 6.3): one simulated experiment, from the
# responses to the held-out comparison. Reads the worked example saved by
# analysis/section6_worked_example.R, refits nothing, and writes the observed
# differences drawn in panel b, with their intervals (Supplement S2.8), to
# results/section6/worked_example_contrasts.csv. The numbers that the paper
# prints about the figure are checked against the data drawn and against the
# macros in results/section6/section6_numbers.tex.
#   Rscript analysis/section6_fig07_one_experiment.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
source("R/inference.R")
library(patchwork)

# Numbers printed in the text, named by their macros in section6_numbers.tex.
macro_lines <- grep("^\\\\newcommand", readLines("results/section6/section6_numbers.tex"),
                    value = TRUE)
paper <- setNames(sub("^.*\\}\\{(.*)\\}$", "\\1", macro_lines),
                  sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1", macro_lines))

example <- readRDS("results/section6/worked_example.rds")
responses <- example$observed$y       # one row per participant; columns 1-10 B, 11-20 D
n_participants <- nrow(responses)
# Mean difference in the proportion of yes responses, D minus B, with its 95%
# interval (Section 6.2, Supplement S2.6).
mean_contrast <- inference_effect(example$observed)

# Response probabilities under the generating parameters ("truth") or a fitted
# candidate: one row per combination of counterbalancing group and
# learning-rate type.
probabilities <- function(model) {
  if (model == "truth") return(inference_prob_cpp(example$truth$par, example$truth$adaptive))
  fit <- example$fits[[model]]
  inference_prob_cpp(fit$par, fit$spec$adaptive)
}
# Predicted D-minus-B difference on each pear trial, averaged with equal weight
# over the two counterbalancing groups and the two learning-rate types.
curves <- do.call(rbind, lapply(c("fixed", "reset", "carry", "truth"), function(model) {
  p <- probabilities(model)
  data.frame(model = model, trial = 1:10, contrast = colMeans(p[, 11:20] - p[, 1:10]))
}))

# Observed D-minus-B difference in the proportion of yes responses on each pear
# trial, with the interval of Agresti and Min (2005) for paired proportions
# (Supplement S2.8).
observed <- do.call(rbind, lapply(1:10, function(q) {
  yes_b <- responses[, q]
  yes_d <- responses[, q + 10]
  n_d_only <- sum(yes_d == 1 & yes_b == 0)
  n_b_only <- sum(yes_b == 1 & yes_d == 0)
  m <- n_participants + 2
  p_d <- (n_d_only + .5) / m
  p_b <- (n_b_only + .5) / m
  se <- sqrt((p_d + p_b - (p_d - p_b)^2) / m)
  data.frame(trial = q, estimate = (n_d_only - n_b_only) / n_participants,
             n_d_only = n_d_only, n_b_only = n_b_only,
             lower = p_d - p_b - qnorm(.975) * se, upper = p_d - p_b + qnorm(.975) * se)
}))

# Held-out log score of the carryover candidate minus each alternative, with
# two participant-level standard errors on either side: the decision margin.
held_out <- data.frame(
  alternative = factor(c("fixed", "reset"), levels = c("reset", "fixed")),
  difference = example$comparison$carry_contrasts[c("fixed", "reset"), "difference"],
  se = example$comparison$carry_contrasts[c("fixed", "reset"), "se"]
)
held_out$lower <- held_out$difference - 2 * held_out$se
held_out$upper <- held_out$difference + 2 * held_out$se

# The generating model's mean difference within each counterbalancing group.
# The model makes one physical light more salient. Group 1 (allocation 1 in the
# saved data) gives that light the role of B, and group -1 the role of D, in
# rows 3-4 and 1-2 of the probability matrix (checked below).
group_names <- c(`1` = "B more salient", `-1` = "D more salient")
truth_probabilities <- probabilities("truth")
generating_by_group <- setNames(
  c(mean(truth_probabilities[1:2, 11:20] - truth_probabilities[1:2, 1:10]),
    mean(truth_probabilities[3:4, 11:20] - truth_probabilities[3:4, 1:10])),
  group_names[c("-1", "1")])

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
theme_figure <- theme_paper() + theme(axis.title.x = element_text(margin = margin(t = 6)))
# Legends below panels a and b, each starting at the left edge of its panel.
legend_theme <- theme(legend.position = "bottom", legend.justification = "left",
                      legend.title = element_blank(), legend.box = "vertical",
                      legend.box.just = "left", legend.spacing.y = unit(2, "pt"),
                      legend.key.height = unit(9, "pt"), legend.key.width = unit(16, "pt"),
                      legend.text = element_text(size = 9, colour = ink, margin = margin(l = 3, r = 8)),
                      legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(4, "pt"))

# Panel a: every participant's proportion of yes responses to D minus that to
# B, one dot per participant, stacked by value, with the two counterbalancing
# groups in two colors. Below the dots, the mean difference with its 95%
# interval, and a short dashed line at the generating model's mean difference,
# the average of its curve in panel b.
differences <- data.frame(
  value = (rowSums(responses[, 11:20]) - rowSums(responses[, 1:10])) / 10,
  group = factor(group_names[as.character(example$observed$allocation)], levels = group_names)
)
differences <- differences[order(differences$value, differences$group), , drop = FALSE]
differences$stack <- ave(differences$value, differences$value, FUN = seq_along)
n_no_more_yes_to_d <- sum(differences$value <= 0)
generating_mean <- mean(curves$contrast[curves$model == "truth"])
summary_y <- -1.6
panel_a <- ggplot(differences) +
  annotate("segment", x = 0, xend = 0, y = summary_y - .8, yend = max(differences$stack) + .6,
           colour = "#BDBDBD", linewidth = .3) +
  annotate("segment", x = generating_mean, xend = generating_mean, y = summary_y - 1.3,
           yend = summary_y + 1.3, colour = grey, linewidth = .5, linetype = "22") +
  geom_point(aes(value, stack, colour = group), size = 1.45) +
  annotate("segment", x = mean_contrast[["lower"]], xend = mean_contrast[["upper"]],
           y = summary_y, yend = summary_y, colour = ink, linewidth = .6) +
  annotate("point", x = mean_contrast[["estimate"]], y = summary_y, colour = ink, fill = "white",
           shape = 21, size = 1.7, stroke = .6) +
  scale_colour_manual(values = c("B more salient" = "#3A8F6A", "D more salient" = "#5B4B8A")) +
  scale_x_continuous(breaks = c(-.2, 0, .2, .4, .6), labels = axis_number,
                     limits = c(-.38, .68), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(summary_y - 1.4, max(differences$stack) + .8),
                     expand = expansion(mult = 0)) +
  labs(title = "(a)  Each participant", x = "Proportion yes,\nD minus B", y = NULL) +
  theme_figure + legend_theme +
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.direction = "vertical",
        legend.key.width = unit(9, "pt"))

# Panel b: one small panel per candidate, with its fitted curve (solid) and the
# generating model's curve (dashed). The observed differences and the generating
# curve have no candidate column, so they are drawn in every panel. In the
# legend, an observed difference is a point on its interval.
draw_key_observed <- function(data, params, size) {
  grid::grobTree(
    grid::segmentsGrob(.5, .05, .5, .95, gp = grid::gpar(col = "#A6A6A6", lwd = .35 * .pt)),
    draw_key_point(data, params, size)
  )
}
curve_names <- c(fitted = "Fitted candidate", truth = "Generating model")
curve_style <- function(fitted, truth) setNames(c(fitted, truth), curve_names)
curves$curve <- curve_names[ifelse(curves$model == "truth", "truth", "fitted")]
candidate_names <- c(carry = "Carryover", reset = "Reset", fixed = "Fixed-rate")
fitted <- subset(curves, model != "truth")
fitted$candidate <- factor(candidate_names[fitted$model], levels = candidate_names)
# The vertical range holds every interval and curve.
b_range <- c(min(observed$lower, 0) - .02, max(observed$upper, curves$contrast) + .02)
panel_b <- ggplot() +
  geom_hline(yintercept = 0, colour = grey, linewidth = .3) +
  geom_line(data = subset(curves, model == "truth"),
            aes(trial, contrast, colour = curve, linetype = curve, linewidth = curve)) +
  geom_line(data = fitted, aes(trial, contrast, colour = curve, linetype = curve, linewidth = curve)) +
  geom_errorbar(data = observed, aes(trial, ymin = lower, ymax = upper),
                width = 0, colour = "#A6A6A6", linewidth = .35) +
  geom_point(data = observed, aes(trial, estimate, shape = "Observed, with 95% interval"),
             colour = ink, size = 1.2, key_glyph = draw_key_observed) +
  facet_wrap(~ candidate, nrow = 1) +
  scale_colour_manual(values = curve_style(ink, "#8C8C8C")) +
  scale_linetype_manual(values = curve_style("solid", "22")) +
  scale_linewidth_manual(values = curve_style(.6, .45)) +
  scale_shape_manual(values = 16) +
  guides(shape = guide_legend(order = 1), colour = guide_legend(order = 2),
         linetype = guide_legend(order = 2), linewidth = guide_legend(order = 2)) +
  scale_x_continuous(breaks = c(1, 5, 10), expand = expansion(add = .5)) +
  scale_y_continuous(breaks = c(0, .2, .4), labels = c("0", ".2", ".4"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(ylim = b_range, clip = "off") +
  labs(title = "(b)  Each candidate fitted to all 80 participants",
       x = "Pear trial", y = "Proportion yes, D minus B") +
  theme_figure + legend_theme +
  theme(strip.text = element_text(size = 9, hjust = 0, colour = ink, margin = margin(b = 4)),
        strip.background = element_blank(), panel.spacing.x = unit(10, "pt"))

# Panel c: the held-out comparison. A bar that lies entirely to the right of
# zero shows a difference larger than its decision margin.
panel_c <- ggplot(held_out, aes(difference, alternative)) +
  geom_vline(xintercept = 0, colour = grey, linewidth = .3) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, orientation = "y",
                colour = ink, linewidth = .5) +
  geom_point(colour = ink, size = 1.6) +
  scale_y_discrete(labels = c(fixed = "vs fixed-rate", reset = "vs reset")) +
  scale_x_continuous(breaks = c(0, 10, 20, 30), limits = c(-1, 35),
                     expand = expansion(mult = 0)) +
  coord_cartesian(ylim = c(.5, 2.5), clip = "off") +
  labs(title = "(c)  Held-out participants",
       x = "Held-out log score,\ncarryover minus alternative", y = NULL) +
  theme_figure +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.y = element_text(size = 9, colour = ink, hjust = 1),
        plot.margin = margin(6, 14, 6, 6))

# One row, in the order of the text: participants, pear trials, held-out
# participants.
figure <- panel_a + panel_b + panel_c + plot_layout(widths = c(1.3, 2.75, 1.2))

cat(sprintf("Mean difference, D minus B, %s, 95%% interval %s to %s\n",
            paper_number(mean_contrast[["estimate"]], 3),
            paper_number(mean_contrast[["lower"]], 3),
            paper_number(mean_contrast[["upper"]], 3)))
cat(sprintf("Carryover of learned value g_V: fixed-rate %s, reset %s\n",
            paper_number(example$fits$fixed$par[["g_value"]], 2),
            paper_number(example$fits$reset$par[["g_value"]], 2)))
cat(sprintf("Held-out log score, carryover minus %s: %.1f (SE %.1f)\n",
            c("fixed-rate", "reset"), held_out$difference, held_out$se), sep = "")
save_figure(figure, "fig07_one_experiment", 7.2, 3.1)
write.csv(observed, "results/section6/worked_example_contrasts.csv", row.names = FALSE)
cat("Saved results/section6/worked_example_contrasts.csv\n")

# Values reported in the paper
stopifnot(
  # Section 6.2 and Supplement S2.4: 80 participants and seed 8123. Supplement
  # S2.8: no yes response to either light on the first pear trial, so the
  # observed difference there is zero (Section 6.3)
  n_participants == 80, example$truth$seed == 8123, all(responses[, c(1, 11)] == 0),
  # Section 6.2, Figure 7a: twenty of the 80 participants give D no more yes
  # responses than B, although the generating model gives D the higher
  # probability of a yes response on most of the pear trials
  n_no_more_yes_to_d == 20, sum(curves$contrast[curves$model == "truth"] > 0) > 5,
  # Section 6.2, Figure 7a: the mean difference averages the differences drawn,
  # one per participant (the two counterbalancing groups have 40 each, so equal
  # weights give the plain mean)
  nrow(differences) == n_participants, table(example$observed$allocation) == 40,
  abs(mean(differences$value) - mean_contrast[["estimate"]]) < 1e-12,
  # Caption, Figure 7a: the generating model makes one physical light more
  # salient, so its predicted difference is larger in the group in which that
  # light is D, and the colors name the groups accordingly
  generating_by_group[["D more salient"]] > generating_by_group[["B more salient"]],
  # Section 6.2: the mean difference and its 95% interval, as printed
  paper_number(mean_contrast[c("estimate", "lower", "upper")], 3) ==
    paper[c("ExampleContrast", "ExampleLower", "ExampleUpper")],
  # Section 6.3: the fractions g_V estimated by the fixed-rate and reset
  # candidates, as printed
  paper_number(c(example$fits$fixed$par[["g_value"]], example$fits$reset$par[["g_value"]]),
               2) == paper[c("ExampleFixedValueCarry", "ExampleResetValueCarry")],
  # Section 6.3, Figure 7c: the differences in held-out log score and their
  # standard errors, as printed, both beyond the decision margin, so the
  # analysis favors associability carryover
  sprintf("%.1f", held_out$difference) == paper[c("ExampleCarryVsFixed", "ExampleCarryVsReset")],
  sprintf("%.1f", held_out$se) == paper[c("ExampleCarryVsFixedSE", "ExampleCarryVsResetSE")],
  held_out$lower > 0, example$comparison$decision == "carry",
  # Section 6.3, Figure 7b: the fixed-rate and reset curves lie above the
  # observed interval on the first pear trial and below it on the last three
  all(curves$contrast[curves$model %in% c("fixed", "reset") & curves$trial == 1] >
        observed$upper[1]),
  all(sapply(c("fixed", "reset"), function(m)
    all(curves$contrast[curves$model == m][8:10] < observed$lower[8:10]))),
  # Section 6.3, Figure 7b: the carryover curve follows the observed differences
  # most closely
  sum((curves$contrast[curves$model == "carry"] - observed$estimate)^2) <
    min(sapply(c("fixed", "reset"), function(m) {
      sum((curves$contrast[curves$model == m] - observed$estimate)^2)
    }))
)
