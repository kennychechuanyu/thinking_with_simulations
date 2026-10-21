# Figure 10 (Section 5.5): how often the B-D response contrast is detected and
# how often the held-out comparison favors associability carryover, from the
# study summary written by analysis/section5_report.R. Each drawn percentage is
# checked against the number the paper prints (section5_numbers.tex).
#   Rscript analysis/section5_fig10_detection_and_identification.R

if (!file.exists("R/figure_style.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
library(patchwork)

# Analyses in which every candidate estimates old-value carryover g_V.
study <- read.csv("results/section5/summary.csv")
study <- study[study$policy == "value_allowed", ]

# Panel a: 160 participants and all ten responses, one row per generating process.
by_process <- study[study$n == 160 & study$view == "full" &
                      study$scenario %in% c("associability", "value", "reset"), ]
by_process$row <- c(associability = 3, value = 2, reset = 1)[by_process$scenario]
by_process <- by_process[order(-by_process$row), ]
process_rows <- data.frame(
  row = 3:1,
  name = c("Carried associability", "Value generalization", "Transfer-only adaptation"),
  note = c("carryover is the right answer", "carryover would be wrong",
           "no history effect to find")
)
# Panel b: carried associability, by view and number of participants.
# The two sample sizes are drawn slightly apart so that the late points, which
# coincide, both stay visible.
by_view <- study[study$scenario == "associability", ]
by_view$group <- ifelse(by_view$n == 160, "N160", "N40")
by_view$x <- c(late = 1, early = 2, full = 3)[by_view$view] +
  ifelse(by_view$group == "N160", -.06, .06)
by_view <- by_view[order(by_view$group, by_view$x), ]

ink <- paper_colours[["ink"]]
grey <- paper_colours[["grey"]]
carry_colour <- paper_colours[["carry"]]
small_text_size <- 8.5 / .pt
# This figure uses slightly smaller type and narrower margins than the others.
theme_figure <- theme_paper(title_gap = 3) +
  theme(axis.text = element_text(size = 8.5), axis.title = element_text(size = 9.5),
        axis.title.x = element_text(margin = margin(t = 6)),
        plot.title = element_text(size = 9.5),
        plot.subtitle = element_text(size = 8.5, margin = margin(b = 10)),
        plot.margin = margin(5, 5, 5, 5))

# Panel a has no axis of its own: the scale from 0 to 100 is drawn below the
# rows. The row names sit left of the scale and the key above the rows; both
# start at x = -88.
left <- -88
panel_a <- ggplot(by_process) +
  annotate("segment", x = 0, xend = 100, y = .55, yend = .55, colour = ink, linewidth = .3) +
  annotate("segment", x = c(0, 50, 100), xend = c(0, 50, 100), y = .55, yend = .49,
           colour = ink, linewidth = .3) +
  annotate("segment", x = 50, xend = 50, y = .55, yend = 3.45,
           colour = paper_colours[["tint"]], linewidth = .35) +
  annotate("text", x = c(0, 50, 100), y = .36, label = c("0", "50", "100"),
           size = small_text_size, colour = ink) +
  annotate("text", x = 50, y = -.02, label = "Percentage of 400 simulated experiments",
           size = 9.5 / .pt, colour = ink) +
  geom_segment(aes(x = 100 * carry, xend = 100 * detected, y = row, yend = row),
               colour = "#C8C8C8", linewidth = 1.6) +
  geom_point(aes(100 * detected, row), shape = 21, fill = "white", colour = ink,
             size = 2.6, stroke = .7) +
  geom_point(aes(100 * carry, row), colour = carry_colour, size = 2.6) +
  geom_text(aes(100 * detected, row + .29, label = sprintf("%.1f%%", 100 * detected)),
            size = small_text_size, colour = ink) +
  geom_text(aes(100 * carry, row - .26, label = sprintf("%.1f%%", 100 * carry)),
            size = small_text_size, colour = carry_colour) +
  geom_text(data = process_rows, aes(left, row + .11, label = name), hjust = 0,
            size = text_size, colour = ink, fontface = "bold") +
  geom_text(data = process_rows, aes(left, row - .16, label = note), hjust = 0,
            size = small_text_size, colour = grey) +
  annotate("point", x = left + 2, y = 3.72, shape = 21, fill = "white",
           colour = ink, size = 2.3, stroke = .7) +
  annotate("text", x = left + 6, y = 3.72, label = "B–D difference detected",
           hjust = 0, size = small_text_size, colour = ink) +
  annotate("point", x = left + 88, y = 3.72, colour = carry_colour, size = 2.3) +
  annotate("text", x = left + 92, y = 3.72, label = "Carryover preferred", hjust = 0,
           size = small_text_size, colour = carry_colour) +
  scale_x_continuous(limits = c(left, 108), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(-.15, 3.9), expand = expansion(mult = 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "(a)  Detecting a difference is not identifying its source",
       subtitle = "160 simulated participants, all ten responses per cue", x = NULL, y = NULL) +
  theme_figure +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank())

# Panel b: points without connecting lines, because the three views differ in
# which presentations they keep and in how many answers they contain. Each point
# has its own label, above the N = 160 points and below the N = 40 points; the
# late and early N = 160 labels are moved left, clear of the N = 40 points.
point_labels <- by_view
point_labels$y <- 100 * point_labels$carry + ifelse(point_labels$group == "N160", 7, -7)
point_labels$x[point_labels$view == "late" & point_labels$group == "N160"] <- 1 - .1
point_labels$x[point_labels$view == "early" & point_labels$group == "N160"] <- 2 - .16
panel_b <- ggplot(by_view, aes(x, 100 * carry)) +
  geom_point(aes(shape = group), colour = carry_colour, fill = "white", size = 2.4,
             stroke = .8) +
  geom_text(data = point_labels, aes(x, y, label = sprintf("%.1f", 100 * carry)),
            size = small_text_size, colour = ink) +
  annotate("text", x = 3.14,
           y = 100 * by_view$carry[by_view$group == "N160" & by_view$view == "full"],
           label = "italic(N) * ' = 160'", parse = TRUE, hjust = 0, size = small_text_size,
           colour = carry_colour) +
  annotate("text", x = 3.14,
           y = 100 * by_view$carry[by_view$group == "N40" & by_view$view == "full"],
           label = "italic(N) * ' = 40'", parse = TRUE, hjust = 0, size = small_text_size,
           colour = carry_colour) +
  scale_shape_manual(values = c(N160 = 19, N40 = 21)) +
  scale_x_continuous(breaks = 1:3, limits = c(.7, 3.55),
                     labels = c("8–10\nlate\n3 answers", "1–6\nearly\n6 answers", "1–10\nfull\n10 answers"),
                     expand = expansion(mult = 0)) +
  # Room below zero for the label of the late N = 40 point; the axis line
  # still runs only from 0 to 100.
  scale_y_continuous(limits = c(-13, 100), breaks = c(0, 50, 100),
                     expand = expansion(mult = c(0, .02))) +
  guides(y = guide_axis(cap = "both")) +
  coord_cartesian(clip = "off") +
  labs(title = "(b)  Which responses are analyzed",
       subtitle = "Carried associability; carryover preferred (%)",
       x = "Presentations kept for each cue", y = NULL) +
  theme_figure +
  theme(panel.grid.major.y = element_line(colour = paper_colours[["tint"]], linewidth = .35))

figure <- panel_a + panel_b + plot_layout(widths = c(1.35, 1))

cat("160 participants, all ten responses:\n")
cat(sprintf("  %s: detected %.1f%%, carryover preferred %.1f%%\n", process_rows$name,
            100 * by_process$detected, 100 * by_process$carry), sep = "")
cat("Carried associability, carryover preferred in presentations 8-10, 1-6, 1-10:\n")
for (n in c(40, 160)) {
  cat(sprintf("  N = %d: %s\n", n,
              paste(sprintf("%.1f%%", 100 * by_view$carry[by_view$n == n]), collapse = ", ")))
}
save_figure(figure, "fig10_detection_and_identification", 7, 3)

# Numbers printed in the text, named by their macros in section5_numbers.tex.
macro_lines <- grep("^\\\\newcommand", readLines("results/section5/section5_numbers.tex"),
                    value = TRUE)
paper <- setNames(sub("^.*\\}\\{(.*)\\}$", "\\1", macro_lines),
                  sub("^\\\\newcommand\\{\\\\([A-Za-z]+)\\}.*$", "\\1", macro_lines))
# The macro for a row and a measure, such as AssocFullLargeCarry for carried
# associability, all ten responses and 160 participants.
macro <- function(rows, measure) {
  paper[paste0(c(associability = "Assoc", value = "Value", reset = "Reset")[rows$scenario],
               c(late = "Late", early = "Early", full = "Full")[rows$view],
               ifelse(rows$n == 160, "Large", "Small"), measure)]
}

# Values reported in the paper
stopifnot(
  # Caption and panel a: 400 simulated experiments per point
  by_process$repetitions == 400, by_view$repetitions == 400,
  # Section 5.5 and Table S2: the percentages drawn, as printed
  sprintf("%.1f", 100 * by_process$detected) == macro(by_process, "Detected"),
  sprintf("%.1f", 100 * by_process$carry) == macro(by_process, "Carry"),
  sprintf("%.1f", 100 * by_view$carry) == macro(by_view, "Carry")
)
