# Figure 9 (Section 5.2): one simulated experiment, from the answers to the
# held-out comparison. Reads the worked example saved by
# analysis/section5_worked_example.R, refits nothing, and writes the observed
# contrasts drawn in panel b to results/section5/worked_example_contrasts.csv.
#   Rscript analysis/section5_fig09_one_experiment.R

if (!file.exists("R/figure_style.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
source("R/inference.R")
library(patchwork)

example <- readRDS("results/section5/worked_example.rds")
answers <- example$observed$y         # one row per participant; columns 1-10 B, 11-20 D
n_participants <- nrow(answers)
# Mean D-minus-B response contrast with its 95% interval (Supplement S2).
mean_contrast <- inference_effect(example$observed)

# Response probabilities under the generating parameters ("truth") or a fitted
# candidate: one row per combination of cue allocation and rate type.
probabilities <- function(model) {
  if (model == "truth") return(inference_prob_cpp(example$truth$par, example$truth$adaptive))
  fit <- example$fits[[model]]
  inference_prob_cpp(fit$par, fit$spec$adaptive)
}
# Predicted D-minus-B difference at each presentation, averaged with equal
# weight over the two cue allocations and the two rate types.
curves <- do.call(rbind, lapply(c("fixed", "reset", "carry", "truth"), function(model) {
  p <- probabilities(model)
  data.frame(model = model, trial = 1:10, contrast = colMeans(p[, 11:20] - p[, 1:10]))
}))

# Observed D-minus-B difference in yes proportions at each presentation q, with
# the interval of Agresti and Min (2005) for paired proportions (Supplement S2).
observed <- do.call(rbind, lapply(1:10, function(q) {
  yes_b <- answers[, q]
  yes_d <- answers[, q + 10]
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

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
carry_colour <- paper_colours[["carry"]]
theme_figure <- theme_paper() + theme(axis.title.x = element_text(margin = margin(t = 6)))

# Panel a: the first twelve participants generated, sorted by their number of
# yes answers to D minus yes answers to B, which the column on the right gives.
# The dashed line separates those with no more yes answers to D than to B.
first_twelve <- data.frame(
  id = 1:12, d_minus_b = rowSums(answers[1:12, 11:20]) - rowSums(answers[1:12, 1:10]))
first_twelve$row <- 13 - match(first_twelve$id,
                               order(-first_twelve$d_minus_b, first_twelve$id))
first_twelve$label <- ifelse(first_twelve$d_minus_b > 0, paste0("+", first_twelve$d_minus_b),
                             ifelse(first_twelve$d_minus_b < 0,
                                    paste0("−", -first_twelve$d_minus_b), "0"))
n_no_more_yes_to_d <- sum(first_twelve$d_minus_b <= 0)
d_offset <- 11.2
count_x <- 10 + d_offset + 1.5        # the D-minus-B column
cells <- do.call(rbind, lapply(1:12, function(s) data.frame(
  row = first_twelve$row[s], x = c(1:10, 1:10 + d_offset),
  fill = ifelse(answers[s, ] == 1, rep(c("B", "D"), each = 10), "no"))))
panel_a <- ggplot(cells, aes(x, row)) +
  geom_tile(aes(fill = fill), width = .84, height = .78) +
  scale_fill_manual(values = c(B = paper_colours[["red"]], D = paper_colours[["blue"]],
                               no = paper_colours[["tint"]])) +
  annotate("segment", x = .4, xend = count_x + .5,
           y = n_no_more_yes_to_d + .5, yend = n_no_more_yes_to_d + .5,
           colour = "#B8B8B8", linewidth = .3, linetype = "22") +
  geom_text(data = first_twelve, aes(count_x, row, label = label),
            colour = ifelse(first_twelve$d_minus_b > 0, ink, "#8C8C8C"), size = 8 / .pt) +
  annotate("text", x = 5.5, y = 13.2, label = "Blocked cue B", colour = paper_colours[["red"]],
           size = text_size, vjust = 0) +
  annotate("text", x = 5.5 + d_offset, y = 13.2, label = "Control cue D",
           colour = paper_colours[["blue"]], size = text_size, vjust = 0) +
  annotate("text", x = count_x, y = 13.2, label = "D − B", colour = ink,
           size = text_size, vjust = 0) +
  scale_x_continuous(breaks = c(1, 5, 10, c(1, 5, 10) + d_offset),
                     labels = rep(c("1", "5", "10"), 2), expand = expansion(add = 0)) +
  scale_y_continuous(expand = expansion(add = 0)) +
  coord_cartesian(xlim = c(.4, count_x + .6), ylim = c(.5, 14), clip = "off") +
  labs(title = "(a)  Twelve of the 80 simulated participants’ answers",
       subtitle = paste("Each row is one participant, and filled cells are",
                        "yes answers given before feedback"),
       x = expression("Presentation with the new outcome, " * italic(q)), y = NULL) +
  theme_figure +
  theme(axis.line = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Panel b: one small panel per candidate. The observed contrasts and the
# generating curve have no candidate column, so they are drawn in every panel.
candidate_names <- c(carry = "Carryover", reset = "Reset", fixed = "Fixed-rate")
fitted <- subset(curves, model != "truth")
fitted$candidate <- factor(fitted$model, levels = names(candidate_names))
panel_names <- data.frame(
  candidate = factor(names(candidate_names), levels = names(candidate_names)),
  label = candidate_names)
# The vertical range holds every interval and curve.
b_range <- c(min(observed$lower, 0) - .02, max(observed$upper, curves$contrast) + .02)
panel_b <- ggplot() +
  geom_hline(yintercept = 0, colour = grey, linewidth = .3) +
  geom_line(data = subset(curves, model == "truth"), aes(trial, contrast),
            colour = "#D4D4D4", linewidth = 2.4, lineend = "round") +
  geom_errorbar(data = observed, aes(trial, ymin = lower, ymax = upper),
                width = 0, colour = "#B3B3B3", linewidth = .35) +
  geom_point(data = observed, aes(trial, estimate), colour = ink, size = 1.2) +
  geom_line(data = fitted, aes(trial, contrast, colour = model), linewidth = .8) +
  geom_text(data = panel_names, aes(x = .6, y = b_range[2], label = label, colour = candidate),
            hjust = 0, vjust = 1, size = text_size, fontface = "bold") +
  scale_colour_manual(values = paper_colours[c("fixed", "reset", "carry")]) +
  facet_wrap(~ candidate, nrow = 1) +
  scale_x_continuous(breaks = c(1, 5, 10), expand = expansion(add = .5)) +
  scale_y_continuous(breaks = c(0, .2, .4), labels = c("0", ".2", ".4"),
                     expand = expansion(mult = 0)) +
  coord_cartesian(ylim = b_range, clip = "off") +
  labs(title = "(b)  Observed difference and fits to all 80 participants",
       subtitle = "Points: observed, with 95% intervals; gray line: generating model",
       x = expression("Presentation with the new outcome, " * italic(q)),
       y = "Proportion yes, D minus B") +
  theme_figure +
  theme(strip.text = element_blank(), strip.background = element_blank(),
        panel.spacing.x = unit(10, "pt"))

# Panel c: the held-out comparison.
panel_c <- ggplot(held_out, aes(difference, alternative)) +
  geom_vline(xintercept = 0, colour = grey, linewidth = .3) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, orientation = "y",
                colour = carry_colour, linewidth = .7) +
  geom_point(colour = carry_colour, size = 2.1) +
  annotate("text", x = held_out$difference, y = as.numeric(held_out$alternative) + .3,
           label = sprintf("%.1f", held_out$difference), size = text_size, colour = ink) +
  annotate("text", x = 1, y = 2.95, label = "carryover predicts better", hjust = 0,
           size = 8 / .pt, colour = grey) +
  annotate("segment", x = 1, xend = 14, y = 2.7, yend = 2.7, colour = grey,
           linewidth = .3, arrow = arrow(length = unit(3, "pt"), type = "closed")) +
  scale_y_discrete(labels = c(fixed = "vs fixed-rate", reset = "vs reset")) +
  scale_x_continuous(breaks = c(0, 10, 20, 30), limits = c(-1, 35),
                     expand = expansion(mult = 0)) +
  coord_cartesian(ylim = c(.5, 3.1), clip = "off") +
  labs(title = "(c)  Predicting held-out participants",
       subtitle = "Bars: ±2 standard errors",
       x = "Held-out log score,\ncarryover minus alternative", y = NULL) +
  theme_figure +
  theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.y = element_text(size = 9, colour = ink, hjust = 1))

figure <- panel_a / (panel_b + panel_c + plot_layout(widths = c(2.6, 1))) +
  plot_layout(heights = c(1, 1.05))

cat(sprintf("Response contrast %s, 95%% interval %s to %s\n",
            paper_number(mean_contrast[["estimate"]], 3),
            paper_number(mean_contrast[["lower"]], 3),
            paper_number(mean_contrast[["upper"]], 3)))
cat(sprintf("Old-value carryover g_V: fixed-rate %s, reset %s\n",
            paper_number(example$fits$fixed$par[["g_value"]], 2),
            paper_number(example$fits$reset$par[["g_value"]], 2)))
cat(sprintf("Held-out log score, carryover minus %s: %.1f (SE %.1f)\n",
            c("fixed-rate", "reset"), held_out$difference, held_out$se), sep = "")
save_figure(figure, "fig09_one_experiment", 7.2, 5.6)
write.csv(observed, "results/section5/worked_example_contrasts.csv", row.names = FALSE)
cat("Saved results/section5/worked_example_contrasts.csv\n")

# Values reported in the paper
stopifnot(
  # Supplement S2: 80 participants, seed 8123, and no yes to either cue at the
  # first presentation
  n_participants == 80, example$truth$seed == 8123, all(answers[, c(1, 11)] == 0),
  # Section 5.2: mean contrast .212, 95% interval .174 to .251, as printed
  sprintf("%.3f", mean_contrast[c("estimate", "lower", "upper")]) ==
    c("0.212", "0.174", "0.251"),
  # Section 5.2, Figure 9a: three of the twelve give D no more yes answers than B
  n_no_more_yes_to_d == 3,
  # Section 5.2: old-value carryover about .70 (fixed-rate) and .50 (reset)
  abs(example$fits$fixed$par[["g_value"]] - .70) < .005,
  abs(example$fits$reset$par[["g_value"]] - .50) < .005,
  # Section 5.2, Figure 9c: 20.5 (SE 5.9) and 21.1 (SE 5.1), both beyond the
  # decision margin, so the analysis favors carryover
  abs(held_out$difference - c(20.5, 21.1)) < .05, abs(held_out$se - c(5.9, 5.1)) < .05,
  held_out$lower > 0, example$comparison$decision == "carry",
  # Section 5.2: the fixed-rate and reset curves lie above the observed
  # interval at the first presentation and below it on the last three
  all(curves$contrast[curves$model %in% c("fixed", "reset") & curves$trial == 1] >
        observed$upper[1]),
  all(sapply(c("fixed", "reset"), function(m)
    all(curves$contrast[curves$model == m][8:10] < observed$lower[8:10]))),
  # Section 5.2: the carryover curve follows the observed contrasts most closely
  sum((curves$contrast[curves$model == "carry"] - observed$estimate)^2) <
    min(sapply(c("fixed", "reset"), function(m) {
      sum((curves$contrast[curves$model == m] - observed$estimate)^2)
    })),
  # Section 5.2: the carryover curve, like the generating model's curve, lies
  # above the observed contrasts at presentations 3-5 and below them at 8-10
  curves$contrast[curves$model == "carry"][3:5] > observed$estimate[3:5],
  curves$contrast[curves$model == "carry"][8:10] < observed$estimate[8:10],
  curves$contrast[curves$model == "truth"][3:5] > observed$estimate[3:5],
  curves$contrast[curves$model == "truth"][8:10] < observed$estimate[8:10]
)
