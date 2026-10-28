# Figure 8 (Section 6.5): for each generating process, set of analyzed pear
# trials and number of participants, how often a positive B-D difference is
# detected and the evidence on which the held-out comparison decides. Reads the
# study results (results.csv) and the summary that analysis/section6_report.R
# writes (summary.csv). The numbers that the paper prints about the figure are
# checked against the data drawn and against the macros in
# results/section6/section6_numbers.tex.
#   Rscript analysis/section6_fig08_detection.R

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
library(patchwork)

# Numbers printed in the text, named by their macros in section6_numbers.tex.
macro_lines <- grep("^\\\\newcommand", readLines("results/section6/section6_numbers.tex"),
                    value = TRUE)
paper <- setNames(sub("^.*\\}\\{(.*)\\}$", "\\1", macro_lines),
                  sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1", macro_lines))

# Analyses in which every candidate estimates the carryover of learned value
# g_V (policy value_allowed).
study <- read.csv("results/section6/summary.csv")
study <- study[study$policy == "value_allowed", ]
results <- read.csv("results/section6/results.csv")
results <- results[results$policy == "value_allowed", ]

# The evidence of each experiment: the carryover candidate's held-out score
# minus each alternative's, divided by its standard error, and the smaller of
# the two. A difference of zero with a standard error of zero, where the fitted
# carryover candidate equals the reset candidate, counts as zero. The decision
# rule favors associability carryover above 2 and another candidate below -2.
standardized <- function(difference, se) ifelse(difference == 0 & se == 0, 0, difference / se)
results$evidence <- pmin(standardized(results$carry_vs_fixed, results$carry_vs_fixed_se),
                         standardized(results$carry_vs_reset, results$carry_vs_reset_se))
decision_of <- function(evidence) {
  ifelse(evidence > 2, "carry", ifelse(evidence < -2, "no_carry", "unresolved"))
}

# One row per condition, from the top: carried associability with each set of
# pear trials and 160 and then 40 participants, the other two processes with
# all ten pear trials and 160 participants, and then the top row's experiments
# divided by parameter set, in the order of their rates in Section 6.5 (column
# config: 1 base, 3 less steep response function, 2 slower learning, 4 weaker
# process). The column view holds the pear trials analyzed: full 1-10, early
# 1-6, late 8-10. y is the row's height in the figure; the larger steps
# separate the groups.
rows <- data.frame(
  scenario = c(rep("associability", 6), "value", "reset", rep("associability", 4)),
  view = c("full", "early", "late", "full", "early", "late", rep("full", 6)),
  n = c(160, 160, 160, 40, 40, 40, rep(160, 6)),
  config = c(rep(NA, 8), 1, 3, 2, 4),
  y = -c(1, 2, 3, 4.25, 5.25, 6.25, 7.75, 9, 11, 12, 13, 14)
)
experiments <- lapply(seq_len(nrow(rows)), function(i) {
  d <- results[results$scenario == rows$scenario[i] & results$view == rows$view[i] &
                 results$n == rows$n[i], ]
  if (is.na(rows$config[i])) d else d[d$config == rows$config[i], ]
})
rows$experiments <- sapply(experiments, nrow)
rows$detected <- sapply(experiments, function(d) mean(d$detected == 1))
for (decision in c("carry", "unresolved", "no_carry")) {
  rows[[decision]] <- sapply(experiments, function(d) mean(d$decision == decision))
}
# Responses analyzed in each experiment: two lights, with ten, six or three
# responses to each.
rows$responses <- rows$n * 2 * c(full = 10, early = 6, late = 3)[rows$view]

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
# The column headings and the panel titles share one line above the first row.
header_y <- max(rows$y) + 1.1
y_scale <- scale_y_continuous(limits = c(min(rows$y) - .5, header_y + .3),
                              expand = expansion(mult = 0))
theme_figure8 <- theme_paper() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9.5, margin = margin(t = 5)))
panel_title <- function(label, x) {
  annotate("text", x = x, y = header_y, label = label, hjust = 0, size = 10 / .pt,
           fontface = "bold", colour = ink)
}

# The row labels form a table of four columns, placed in inches from the left.
process_names <- c(associability = "Carried associability", value = "Carried learned value",
                   reset = "Reset associability")
set_names <- c("Base", "Slower learning", "Less steep response", "Weaker process")
view_names <- c(full = "1–10", early = "1–6", late = "8–10")
by_process <- rows[is.na(rows$config), ]
by_set <- rows[!is.na(rows$config), ]
row_labels <- ggplot(rows) +
  geom_text(data = by_process[!duplicated(by_process$scenario), ],
            aes(0, y, label = process_names[scenario]), hjust = 0, size = text_size, colour = ink) +
  annotate("text", x = 0, y = max(by_set$y) + .95, hjust = 0, size = text_size, colour = grey,
           label = "Carried associability by parameter set") +
  geom_text(data = by_set, aes(.1, y, label = set_names[config]), hjust = 0, size = text_size,
            colour = ink) +
  geom_text(aes(1.36, y, label = view_names[view]), hjust = 0, size = text_size, colour = ink) +
  geom_text(aes(2.1, y, label = n), hjust = 1, size = text_size, colour = ink) +
  geom_text(aes(2.86, y, label = format(responses, big.mark = ",", trim = TRUE)), hjust = 1, size = text_size,
            colour = ink) +
  annotate("text", x = c(0, 1.36, 2.1, 2.86), y = header_y, hjust = c(0, 0, 1, 1),
           label = c("Generating process", "Pear trials", "N", "Responses"), size = text_size,
           colour = grey, fontface = c("plain", "plain", "italic", "plain")) +
  scale_x_continuous(limits = c(0, 3), expand = expansion(mult = 0)) + y_scale +
  coord_cartesian(clip = "off") +
  theme_void() + theme(plot.margin = margin(4, 0, 4, 4))

# Panel a: one percentage per row, drawn as a point.
panel_a <- ggplot(rows) +
  geom_segment(aes(x = 0, xend = 1, y = y, yend = y), colour = "#E3E3E3", linewidth = .35) +
  geom_point(aes(detected, y), colour = ink, size = 1.9) +
  panel_title("(a)  Detected", 0) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = c("0%", "50%", "100%"),
                     expand = expansion(mult = 0)) +
  y_scale + coord_cartesian(clip = "off") +
  labs(x = "Share of experiments") +
  theme_figure8 + theme(plot.margin = margin(4, 22, 4, 4))

# Panel b: each row's experiments by their evidence, in bins of half a standard
# error, as numbers of experiments, so the four parameter-set rows add up to the
# top row. Bin edges fall at -2 and 2, so the three shades give the shares of
# the three decisions (checked below).
evidence_limits <- c(-6, 6)
breaks <- seq(evidence_limits[1], evidence_limits[2], .5)
bins <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
  counts <- table(cut(experiments[[i]]$evidence, breaks))
  data.frame(row = i, y = rows$y[i], xmin = head(breaks, -1), xmax = breaks[-1],
             count = as.numeric(counts), share = as.numeric(counts) / rows$experiments[i])
}))
bins$decision <- factor(decision_of((bins$xmin + bins$xmax) / 2),
                        levels = c("carry", "unresolved", "no_carry"))
# The tallest bin fills 85% of the space between two rows.
bar_scale <- .85 / max(bins$count)
decisions <- c(carry = "Associability carryover favored", unresolved = "Unresolved",
               no_carry = "Another candidate favored")
rule_y <- c(min(rows$y) - .5, max(rows$y) + .5)
panel_b <- ggplot(bins[bins$share > 0, ]) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = y - .42, ymax = y - .42 + bar_scale * count,
                fill = decision)) +
  geom_segment(data = rows, aes(x = evidence_limits[1], xend = evidence_limits[2],
                                y = y - .42, yend = y - .42), colour = "#BDBDBD", linewidth = .25) +
  annotate("segment", x = 0, xend = 0, y = rule_y[1], yend = rule_y[2],
           colour = "#9E9E9E", linewidth = .3, linetype = "22") +
  annotate("segment", x = c(-2, 2), xend = c(-2, 2), y = rule_y[1], yend = rule_y[2],
           colour = ink, linewidth = .3) +
  panel_title("(b)  Held-out comparison", evidence_limits[1]) +
  scale_fill_manual(values = c(carry = paper_colours[["carry"]], unresolved = "#D0D0D0",
                               no_carry = "#5E5E5E"), labels = decisions, name = NULL,
                    drop = FALSE) +
  scale_x_continuous(limits = evidence_limits, breaks = c(-4, -2, 0, 2, 4),
                     labels = c("−4", "−2", "0", "2", "4"), expand = expansion(mult = 0)) +
  y_scale + coord_cartesian(clip = "off") +
  labs(x = "Advantage of the carryover\ncandidate, in standard errors") +
  theme_figure8 + theme(plot.margin = margin(4, 8, 4, 4))

figure <- row_labels + panel_a + panel_b +
  plot_layout(widths = unit(c(3, 1, 1), c("in", "in", "null")), guides = "collect") &
  theme(legend.position = "bottom", legend.key.size = unit(8, "pt"),
        legend.text = element_text(size = 9, margin = margin(l = 3, r = 12)),
        legend.margin = margin(0, 0, 0, 0))

cat("Detected, carryover favored, unresolved, another candidate favored:\n")
cat(sprintf("  %s, pear trials %s, N = %d%s: %.1f%%, %.1f%%, %.1f%%, %.1f%%\n",
            process_names[rows$scenario], c(full = "1-10", early = "1-6", late = "8-10")[rows$view],
            rows$n, ifelse(is.na(rows$config), "", paste(", parameter set", rows$config)),
            100 * rows$detected, 100 * rows$carry, 100 * rows$unresolved, 100 * rows$no_carry),
    sep = "")
save_figure(figure, "fig08_detection", 7, 4.3)

# The macro for a row and a measure, such as AssocFullLargeCarry for carried
# associability, all ten pear trials and 160 participants.
macro <- function(rows, measure) {
  paper[paste0(c(associability = "Assoc", value = "Value", reset = "Reset")[rows$scenario],
               c(late = "Late", early = "Early", full = "Full")[rows$view],
               ifelse(rows$n == 160, "Large", "Small"), measure)]
}
first_eight <- rows[is.na(rows$config), ]
summary_rows <- study[match(paste(first_eight$scenario, first_eight$view, first_eight$n),
                            paste(study$scenario, study$view, study$n)), ]
evidence_of <- function(scenario, view, n) {
  results$evidence[results$scenario == scenario & results$view == view & results$n == n]
}
# The share of each row's experiments in each shade of panel b.
shaded <- tapply(bins$share, list(bins$row, bins$decision), sum)

# Values reported in the paper
stopifnot(
  # Caption: 400 simulated experiments in each of the first eight rows, as
  # printed, and 100 in each parameter-set row, and every comparison ended in
  # one of the three decisions
  paper[["StudyReplications"]] == "400",
  rows$experiments == c(rep(400, 8), rep(100, 4)),
  summary_rows$repetitions == 400, summary_rows$fit_problem == 0,
  abs(rows$carry + rows$unresolved + rows$no_carry - 1) < 1e-9,
  # The rates of the first eight rows equal those of the study summary
  abs(first_eight$detected - summary_rows$detected) < 1e-9,
  abs(first_eight$carry - summary_rows$carry) < 1e-9,
  abs(first_eight$unresolved - summary_rows$unresolved) < 1e-9,
  abs(first_eight$no_carry - summary_rows$no_carry) < 1e-9,
  # Section 6.5, Table 9 and Table S2: the percentages of the first eight rows,
  # as printed
  sprintf("%.1f", 100 * first_eight$detected) == macro(first_eight, "Detected"),
  sprintf("%.1f", 100 * first_eight$carry) == macro(first_eight, "Carry"),
  sprintf("%.1f", 100 * first_eight$unresolved) == macro(first_eight, "Unresolved"),
  sprintf("%.1f", 100 * first_eight$no_carry) == macro(first_eight, "NoCarry"),
  # Caption: the decision rule is a threshold on the evidence drawn in panel b,
  # every value lies within the axis, and the three shades give the shares of
  # the three decisions
  sapply(experiments, function(d) all(decision_of(d$evidence) == d$decision)),
  sapply(experiments, function(d) all(abs(d$evidence) < 6)),
  abs(shaded[, "carry"] - rows$carry) < 1e-9,
  abs(shaded[, "unresolved"] - rows$unresolved) < 1e-9,
  abs(shaded[, "no_carry"] - rows$no_carry) < 1e-9,
  # Caption: the bars of the four parameter-set rows add up to those of the top
  # row
  rowSums(sapply(which(!is.na(rows$config)), function(i) bins$count[bins$row == i])) ==
    bins$count[bins$row == 1],
  # Caption and Section 6.5: to the right of zero, the carryover candidate has
  # the highest held-out score, as in 94.5% of the top row's experiments and 21
  # of the 400 generated by carried learned value
  sapply(experiments, function(d) all((d$evidence > 0) == (d$winner == "carry"))),
  sprintf("%.1f", 100 * mean(evidence_of("associability", "full", 160) > 0)) ==
    paper[["AssocFullLargeRankedCarry"]],
  sum(evidence_of("value", "full", 160) > 0) == as.numeric(paper[["ValueFullLargeRankedCarryCount"]]),
  # Section 6.5: in the top row, nearly all of the experiments in which
  # associability carryover is not favored remain unresolved, among them the
  # 35.0% in which the carryover candidate comes first without exceeding both
  # margins (between the dashed line and 2 in panel b)
  rows$unresolved[1] / (1 - rows$carry[1]) > .95,
  sprintf("%.1f", 100 * with(experiments[[1]], mean(evidence > 0 & evidence <= 2))) == "35.0",
  # Section 6.5: associability carryover favored in 91.0, 76.0, 56.0 and 15.0%
  # of the experiments from the four parameter sets, in the order drawn
  sprintf("%.1f", 100 * by_set$carry) ==
    c(paper[["AssocConfigurationMax"]], "76.0", "56.0", paper[["AssocConfigurationMin"]]),
  # Section 6.5: with all ten pear trials, the evidence moves to the right from
  # 40 to 160 participants, and with pear trials 8-10 most experiments lie to
  # the left of zero with either number
  median(evidence_of("associability", "full", 160)) >
    median(evidence_of("associability", "full", 40)),
  mean(evidence_of("associability", "late", 40) < 0) > .5,
  mean(evidence_of("associability", "late", 160) < 0) > .5,
  # Section 6.5: associability carryover is favored more often with 40
  # participants and all ten pear trials than with 160 participants and pear
  # trials 8-10, although the analysis uses fewer responses, 800 against 960
  as.numeric(paper[["AssocFullSmallCarry"]]) > as.numeric(paper[["AssocLateLargeCarry"]]),
  rows$responses[is.na(rows$config) & rows$scenario == "associability" & rows$view == "full" &
                   rows$n == 40] == 800,
  rows$responses[is.na(rows$config) & rows$scenario == "associability" & rows$view == "late" &
                   rows$n == 160] == 960
)
