# Plots for the companion app

library(ggplot2)

# The colors of the paper's figures (R/figure_style.R), repeated here because
# shiny_app/ is self-contained. B is red and D blue.
ink   <- "#252525"
grey  <- "#696969"
light <- "#BFBFBF"
col_B <- "#B64B41"
col_D <- "#35658D"

theme_app <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      text = element_text(colour = ink),
      axis.text = element_text(colour = ink, size = base_size - 2),
      axis.line = element_line(linewidth = .3, colour = ink),
      axis.ticks = element_line(linewidth = .3, colour = ink),
      plot.title = element_text(size = base_size, face = "bold", margin = margin(b = 4)),
      plot.subtitle = element_text(size = base_size - 2, colour = grey, margin = margin(b = 8)),
      plot.title.position = "plot",
      strip.background = element_blank(),
      strip.text = element_text(size = base_size - 1, face = "bold", hjust = 0),
      strip.clip = "off",
      legend.position = "none",
      plot.margin = margin(8, 14, 8, 8)
    )
}

# Positions for the labels at the ends of the lines: at least min_gap apart, in
# the order of y, with the same mean as y.
spread_labels <- function(y, min_gap) {
  o <- order(y)
  s <- y[o]
  for (i in seq_along(s)[-1]) s[i] <- max(s[i], s[i - 1] + min_gap)
  y[o] <- s - (mean(s) - mean(y[o]))
  y
}

# A zero line that stops at the last update, so that it does not run through
# the end labels.
zero_line <- function(k_max)
  annotate("segment", x = -Inf, xend = k_max, y = 0, yend = 0, colour = light, linewidth = .3)


# B and D learning curves, one panel per model, as in Figure 4b and c.
plot_curves <- function(r, labels = c(rw = "Rescorla–Wagner", adaptive = "Adaptive model"),
                        xlab = "Completed updates with O2", ylab = "Learned value for O2") {
  models <- c("rw", "adaptive")
  d <- do.call(rbind, lapply(models, function(m) rbind(
    data.frame(model = labels[[m]], cue = "B", k = r[[m]]$B$k, V = r[[m]]$B$V),
    data.frame(model = labels[[m]], cue = "D", k = r[[m]]$D$k, V = r[[m]]$D$V))))
  d$model <- factor(d$model, levels = unname(labels[models]))
  # When B and D coincide, draw D dashed over B and label the pair once.
  same <- vapply(models, function(m) max(abs(r[[m]]$B$V - r[[m]]$D$V)) < 1e-12, logical(1))
  same_labels <- unname(labels[models][same])
  d$dash <- d$cue == "D" & d$model %in% same_labels
  ends <- d[d$k == max(d$k) & !(d$cue == "B" & d$model %in% same_labels), ]
  ends$label <- ifelse(ends$model %in% same_labels, "B, D", ends$cue)
  ends$col <- ifelse(ends$model %in% same_labels, ink, c(B = col_B, D = col_D)[ends$cue])
  ends$y <- ave(ends$V, ends$model, FUN = function(v) spread_labels(v, .07))
  ggplot(d, aes(k, V, colour = cue)) +
    geom_line(aes(linetype = dash), linewidth = .8) +
    geom_point(size = 1.6) +
    geom_text(data = ends, aes(y = y, label = label), colour = ends$col, hjust = -.25,
              size = 4.2, fontface = "bold") +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22")) +
    facet_wrap(~model, nrow = 1) +
    scale_colour_manual(values = c(B = col_B, D = col_D)) +
    scale_x_continuous(breaks = c(0, 5, 10), expand = expansion(mult = c(.02, .16))) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +   # end labels may run past the panel
    labs(x = xlab, y = ylab) +
    theme_app() +
    theme(panel.spacing.x = unit(1.6, "lines"))
}

# The B-D gap, D minus B, for both models, as in Figure 4d. "ref" adds the
# unchanged adaptive model as a light line, for comparison with a changed one.
plot_gap <- function(r, title = "The B–D gap (D minus B)", ylim = NULL,
                     second = "Adaptive model", ref = NULL, ref_label = "No change",
                     xlab = "Completed updates with O2") {
  d <- rbind(data.frame(model = "Rescorla–Wagner", k = r$rw$B$k, gap = r$rw$gap),
             data.frame(model = second, k = r$adaptive$B$k, gap = r$adaptive$gap))
  d$model <- factor(d$model, levels = c("Rescorla–Wagner", second))
  ends <- data.frame(label = c("Rescorla–Wagner", second), gap = d$gap[d$k == max(d$k)],
                     col = c(grey, ink))
  if (!is.null(ref)) {
    ref_gap <- data.frame(k = ref$adaptive$B$k, gap = ref$adaptive$gap)
    ends <- rbind(ends, data.frame(label = ref_label, gap = ref_gap$gap[nrow(ref_gap)],
                                   col = grey))
  }
  y_range <- if (is.null(ylim)) range(d$gap, ends$gap, 0) else ylim
  ends$y <- spread_labels(ends$gap, .075 * diff(y_range))
  ends$k <- max(d$k)
  p <- ggplot(d, aes(k, gap)) + zero_line(max(d$k))
  if (!is.null(ref)) p <- p + geom_line(data = ref_gap, colour = light, linewidth = .8)
  p +
    geom_line(aes(linetype = model, colour = model), linewidth = .8) +
    geom_point(aes(colour = model), size = 1.6) +
    geom_text(data = ends, aes(k, y, label = label), colour = ends$col, hjust = 0,
              nudge_x = .3, size = 3.8) +
    scale_colour_manual(values = setNames(c(grey, ink), c("Rescorla–Wagner", second))) +
    scale_linetype_manual(values = setNames(c("22", "solid"), c("Rescorla–Wagner", second))) +
    scale_x_continuous(breaks = c(0, 5, 10), expand = expansion(mult = c(.02, .02))) +
    scale_y_continuous(labels = axis_num) +
    coord_cartesian(xlim = c(0, 15.5), ylim = ylim, clip = "off") +
    labs(x = xlab, y = "Gap in learned value", title = title) +
    theme_app()
}

# Gap after eight updates as old-value carryover varies (Figure 7a). As in that
# figure, an open circle marks the adaptive model's original setting, g_V = 0.
plot_gap_by_gV <- function(grid, g_now, cross = NULL) {
  d <- rbind(data.frame(model = "Rescorla–Wagner", g_V = grid$g_V, gap = grid$rw),
             data.frame(model = "Adaptive model", g_V = grid$g_V, gap = grid$adaptive))
  origin <- d$model == "Adaptive model" & d$g_V == 0
  now <- d[abs(d$g_V - g_now) < 1e-9, ]
  ends <- d[d$g_V == max(d$g_V), ]
  p <- ggplot(d, aes(g_V, gap, colour = model, linetype = model)) +
    geom_line(linewidth = .8) +
    geom_point(data = d[origin, ], shape = 21, fill = "white", size = 2.6, stroke = .8) +
    geom_point(data = now, size = 3) +
    geom_text(data = ends, aes(label = model), hjust = 0, nudge_x = .02, size = 3.8) +
    scale_colour_manual(values = c("Rescorla–Wagner" = grey, "Adaptive model" = ink)) +
    scale_linetype_manual(values = c("Rescorla–Wagner" = "22", "Adaptive model" = "solid")) +
    scale_x_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    scale_y_continuous(labels = axis_num) +
    coord_cartesian(xlim = c(0, 1.45), ylim = c(0, .32), clip = "off") +
    labs(x = expression("Old-value carryover, " * italic(g)[V]),
         y = "Gap after eight updates",
         title = "Changing old-value carryover") +
    theme_app() +
    theme(plot.margin = margin(8, 24, 8, 8))
  if (!is.null(cross)) p <- p +
    annotate("segment", x = cross, xend = cross, y = 0,
             yend = grid$rw[which.min(abs(grid$g_V - cross))], colour = light, linewidth = .4) +
    annotate("text", x = cross, y = .02, label = paste("equal gaps at", num(cross)),
             hjust = -.06, size = 3.5, colour = grey)
  p
}

# Step 4: the adaptive model's response-probability difference, D minus B,
# under both rules, with the chosen rule in ink. Figure 8c draws the same
# differences against completed updates; here the axis counts presentations,
# and the response on presentation q reflects q - 1 updates.
plot_response_gap <- function(r, rule, gamma = 8, theta = .5, mark = NULL) {
  rows <- lapply(c("sigmoid", "threshold"), function(f) {
    p_B <- response_prob(r$adaptive$B$V, f, gamma, theta)
    p_D <- response_prob(r$adaptive$D$V, f, gamma, theta)
    data.frame(rule = f, q = r$adaptive$B$k + 1, difference = p_D - p_B)
  })
  d <- do.call(rbind, rows)
  d <- d[d$q <= 10, ]
  d$chosen <- d$rule == rule
  lab <- c(sigmoid = "Sigmoid", threshold = "Threshold")
  ends <- d[d$q == max(d$q), ]
  p <- ggplot(d, aes(q, difference, group = rule)) +
    annotate("segment", x = 1, xend = 10, y = 0, yend = 0, colour = light, linewidth = .3) +
    geom_line(aes(colour = chosen, linewidth = chosen)) +
    geom_point(aes(colour = chosen), size = 1.4) +
    geom_text(data = ends, aes(label = lab[rule]), colour = ifelse(ends$chosen, ink, grey),
              hjust = 0, nudge_x = .3, size = 3.8) +
    scale_colour_manual(values = c(`TRUE` = ink, `FALSE` = light)) +
    scale_linewidth_manual(values = c(`TRUE` = .9, `FALSE` = .5)) +
    scale_x_continuous(breaks = c(1, 6, 10), expand = expansion(mult = c(.02, .02))) +
    scale_y_continuous(limits = c(-.05, 1.05), breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(xlim = c(1, 13), clip = "off") +
    labs(x = "Presentation", y = "Probability of a yes, D minus B",
         title = "Response difference in the adaptive model",
         subtitle = if (!is.null(mark)) "Dotted line: the presentation you chose") +
    theme_app()
  if (!is.null(mark)) p <- p + geom_vline(xintercept = mark, colour = grey, linewidth = .4,
                                          linetype = "dotted")
  p
}

# Step 5: answers of the first 24 participants in a case, colored as in
# Figure 9a. Presentations outside cols were not recorded.
plot_records <- function(case, cols = 1:10, rows = 1:24) {
  y <- case$y[rows, , drop = FALSE]
  d <- expand.grid(participant = seq_along(rows), col = 1:20)
  d$yes <- as.vector(y)
  d$cue <- ifelse(d$col <= 10, "B", "D")
  d$presentation <- ifelse(d$col <= 10, d$col, d$col - 10)
  d$fill <- ifelse(!(d$presentation %in% cols), "blank", ifelse(d$yes == 1, d$cue, "no"))
  ggplot(d, aes(presentation, participant)) +
    geom_tile(aes(fill = fill), colour = "white", linewidth = .6) +
    facet_wrap(~cue, nrow = 1, labeller = as_labeller(c(B = "Answers to B", D = "Answers to D"))) +
    scale_fill_manual(values = c(no = "#C9C9C9", B = col_B, D = col_D, blank = "#FAFAFA")) +
    scale_x_continuous(breaks = c(1, 5, 10), expand = expansion(0)) +
    scale_y_reverse(breaks = NULL, expand = expansion(0)) +
    labs(x = "Presentation", y = paste0("Participants 1–", length(rows)),
         subtitle = "Colored: yes · Gray: no · Blank: not recorded") +
    theme_app() +
    theme(axis.line = element_blank(), axis.ticks.y = element_blank())
}

# Step 5: the observed difference, yes to D minus yes to B, at each recorded
# presentation, with 95% intervals. With truth = TRUE it adds the generating
# probability difference as a wide grey line, as in Figure 9b.
plot_contrast <- function(case, design, truth = FALSE) {
  d <- design$by_presentation
  p <- ggplot(d, aes(presentation, estimate)) +
    geom_hline(yintercept = 0, colour = light, linewidth = .3)
  if (truth) {
    generating <- data.frame(presentation = 1:10, difference = case$true_contrast)
    p <- p + geom_line(data = generating, aes(presentation, difference), colour = "#D4D4D4",
                       linewidth = 2.4, lineend = "round")
  }
  p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, colour = grey) +
    geom_point(size = 2.2, colour = ink) +
    scale_x_continuous(breaks = 1:10, limits = c(.5, 10.5)) +
    scale_y_continuous(labels = axis_num) +
    expand_limits(y = c(-.2, .45)) +
    labs(x = "Presentation", y = "Yes to D minus yes to B",
         subtitle = if (truth)
           "Points: observed, with 95% intervals\nGray line: the generating probability difference"
         else "Observed response difference, with 95% intervals") +
    theme_app()
}

# Step 5: held-out log score of the carryover candidate minus each alternative,
# with bars two standard errors either side, as in Figure 9c.
plot_scores <- function(scores) {
  d <- scores
  # Keep a common scale for the saved cases, expanding it to include wider intervals.
  score_range <- range(-70, 130, 0, d$difference - 2 * d$se, d$difference + 2 * d$se)
  # ggplot draws the first level at the bottom, so listing reset first puts
  # fixed-rate at the top, as in Figure 9c.
  d$label <- factor(c(fixed = "Carryover minus\nfixed-rate", reset = "Carryover minus\nreset")[d$against],
                    levels = c("Carryover minus\nreset", "Carryover minus\nfixed-rate"))
  ggplot(d, aes(difference, label)) +
    geom_vline(xintercept = 0, colour = light, linewidth = .3) +
    geom_errorbar(aes(xmin = difference - 2 * se, xmax = difference + 2 * se),
                  width = 0, colour = grey, linewidth = .8, orientation = "y") +
    geom_point(size = 3, colour = ink) +
    coord_cartesian(xlim = score_range) +
    labs(x = "Difference in held-out\nlog score", y = NULL,
         subtitle = "Right of 0: carryover predicted better") +
    theme_app() +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())
}

# Explore: the response-probability difference, D minus B, for both models
# under the chosen rule.
plot_response_models <- function(r, rule, gamma, theta, second = "Adaptive model") {
  first <- "Rescorla–Wagner"
  rows <- lapply(c("rw", "adaptive"), function(m) data.frame(
    model = c(rw = first, adaptive = second)[[m]], q = r[[m]]$B$k + 1,
    difference = response_prob(r[[m]]$D$V, rule, gamma, theta) -
                 response_prob(r[[m]]$B$V, rule, gamma, theta)))
  d <- do.call(rbind, rows)
  d <- d[d$q <= 10, ]   # the answer on presentation q follows q - 1 updates
  d$model <- factor(d$model, levels = c(first, second))
  ends <- d[d$q == max(d$q), ]
  # The same scale as plot_response_gap(), extended only if a setting needs it.
  ylim <- range(-.05, 1.05, d$difference)
  ends$y <- spread_labels(ends$difference, .075 * diff(ylim))
  ggplot(d, aes(q, difference, colour = model, linetype = model)) +
    zero_line(max(d$q)) +
    geom_line(linewidth = .8) +
    geom_point(size = 1.6) +
    geom_text(data = ends, aes(y = y, label = model), hjust = 0, nudge_x = .3, size = 3.8) +
    scale_colour_manual(values = setNames(c(grey, ink), c(first, second))) +
    scale_linetype_manual(values = setNames(c("22", "solid"), c(first, second))) +
    scale_x_continuous(breaks = c(1, 5, 10), expand = expansion(mult = c(.02, .02))) +
    scale_y_continuous(labels = axis_num) +
    coord_cartesian(xlim = c(1, 15.5), ylim = ylim, clip = "off") +
    labs(x = "Presentation", y = "Probability of a yes, D minus B",
         title = "Response difference under the chosen rule") +
    theme_app()
}

# Step 6: decision-time densities of the two settings, with their means, as in
# Figure 12c. The labels are placed by hand beside their curves.
plot_ddm <- function(d = ddm_summary()) {
  times <- seq(0, 3.5, by = .005)
  curves <- do.call(rbind, lapply(seq_len(nrow(d)), function(i)
    data.frame(id = d$id[i], t = times, f = ddm_dens(times, d$v[i], d$a[i]))))
  means <- data.frame(id = d$id, t = d$mean,
                      f = vapply(seq_len(nrow(d)), function(i) ddm_dens(d$mean[i], d$v[i], d$a[i]), 0))
  mean_labels <- data.frame(id = d$id, x = c(1.3, .55), y = c(1.05, 2.3),
                            label = c(paste0("First setting: mean ", num0(d$mean[1], 2)),
                                      paste0("Second setting: mean ", num0(d$mean[2], 2))))
  ggplot(curves, aes(t, f, colour = id)) +
    geom_line(linewidth = .8) +
    geom_segment(data = means, aes(x = t, xend = t, y = 0, yend = f), linewidth = .4,
                 linetype = "22") +
    geom_text(data = mean_labels, aes(x, y, label = label), hjust = 0, size = 3.8,
              show.legend = FALSE) +
    scale_colour_manual(values = c(first = ink, second = grey)) +
    scale_x_continuous(breaks = 0:3, expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, .03))) +
    coord_cartesian(xlim = c(0, 3.5), clip = "off") +
    labs(x = "Decision time (model time units)", y = "Density",
         title = "Predicted decision times",
         subtitle = "Dashed lines mark the means") +
    theme_app()
}
