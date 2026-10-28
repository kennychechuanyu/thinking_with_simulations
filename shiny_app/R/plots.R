# Plots for the companion app

library(ggplot2)

# The colors of the paper's figures (R/figure_style.R), repeated here because
# shiny_app/ is self-contained. B is red and D blue; the two counterbalancing
# groups take the colors of Figure 7a.
ink   <- "#252525"
grey  <- "#696969"
light <- "#BFBFBF"
tint  <- "#EDEDED"
col_B <- "#B64B41"
col_D <- "#35658D"
col_groups <- c(`1` = "#3A8F6A", `-1` = "#5B4B8A")

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

# A zero line that stops at x_max, so that it does not run through the end labels.
zero_line <- function(x_max, x_min = -Inf)
  annotate("segment", x = x_min, xend = x_max, y = 0, yend = 0, colour = light, linewidth = .3)


# Step 1: the tone and the light under Rescorla-Wagner, as in Figure 2a. Values
# after each completed trial; the gray dashed curve is a new cue trained alone
# over the compound trials, and the dotted line the light's limit.
plot_blocking_values <- function(tr) {
  n <- nrow(tr)
  n1 <- sum(!tr$light)
  d <- rbind(data.frame(cue = "A", t = 0:n, V = c(tr$V_A[1], tr$V_A_after)),
             data.frame(cue = "B", t = 0:n, V = c(tr$V_B[1], tr$V_B_after)))
  alone <- data.frame(t = n1:n, V = 1 - (1 - default_settings$beta)^(0:(n - n1)))
  limit <- (1 - tr$V_A_after[n1]) / 2
  ends <- data.frame(label = c("A (tone)", "B (light)", "new cue alone"),
                     y = c(tail(d$V[d$cue == "A"], 1), tail(d$V[d$cue == "B"], 1), tail(alone$V, 1)),
                     col = c(ink, col_B, grey))
  ends$y <- spread_labels(ends$y, .09)
  ggplot(d, aes(t, V)) +
    annotate("segment", x = n1, xend = n1, y = 0, yend = 1, colour = light, linewidth = .3,
             linetype = "longdash") +
    annotate("text", x = n1 / 2, y = 1.07, label = "A+", size = 3.8, colour = grey) +
    annotate("text", x = n1 + (n - n1) / 2, y = 1.07, label = "AB+", size = 3.8, colour = grey) +
    geom_line(data = alone, colour = grey, linetype = "22", linewidth = .6) +
    annotate("segment", x = n1, xend = n, y = limit, yend = limit, colour = col_B,
             linewidth = .4, linetype = "dotted") +
    geom_line(aes(colour = cue), linewidth = .8) +
    geom_point(aes(colour = cue), size = 1.3) +
    geom_text(data = ends, aes(x = n + .6, y = y, label = label), colour = ends$col, hjust = 0,
              size = 3.8) +
    scale_colour_manual(values = c(A = ink, B = col_B)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20), expand = expansion(mult = c(.01, .02))) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(xlim = c(0, n + 5.5), ylim = c(0, 1.1), clip = "off") +
    labs(x = "Completed trials", y = "Learned value",
         title = "Learned values after each trial",
         subtitle = paste0("Dashed line: the start of the compound trials. Dotted line: B's limit, ",
                           num(limit, 4))) +
    theme_app()
}

# Step 1: the shared error on each trial, as in Figure 2b, with the first
# compound trial circled and, if given, the trial chosen in the table marked.
plot_shared_error <- function(tr, mark = NULL) {
  n1 <- sum(!tr$light)
  p <- ggplot(tr, aes(trial, error)) +
    annotate("segment", x = n1 + .5, xend = n1 + .5, y = 0, yend = 1, colour = light,
             linewidth = .3, linetype = "longdash") +
    geom_line(colour = ink, linewidth = .6) +
    geom_point(colour = ink, size = 1.4) +
    geom_point(data = tr[n1 + 1, ], shape = 21, size = 3, fill = "white", colour = ink, stroke = .7)
  if (!is.null(mark)) p <- p + geom_vline(xintercept = mark, colour = grey, linewidth = .4,
                                          linetype = "dotted")
  p + scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Trial", y = "Shared error", title = "The shared error on each trial",
         subtitle = paste0("Dashed line: the start of the compound trials. Open circle: trial ",
                           n1 + 1, if (!is.null(mark)) paste0(". Dotted line: trial ", mark))) +
    theme_app()
}


# B and D learning curves, one panel per model, as in Figure 3b and c.
plot_curves <- function(r, labels = c(rw = "Rescorla–Wagner", adaptive = "Adaptive model"),
                        xlab = "Completed pear trials", ylab = "Learned value for the pear") {
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

# The B-D gap, D minus B, for both models, as in Figure 3d. "ref" adds the
# unchanged adaptive model as a light line, for comparison with a changed one.
plot_gap <- function(r, title = "The B–D gap (D minus B)", ylim = NULL,
                     second = "Adaptive model", ref = NULL, ref_label = "Unchanged",
                     xlab = "Completed pear trials") {
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
  k_max <- max(d$k)
  p <- ggplot(d, aes(k, gap)) + zero_line(k_max)
  if (!is.null(ref)) p <- p + geom_line(data = ref_gap, colour = light, linewidth = .8)
  p +
    geom_line(aes(linetype = model, colour = model), linewidth = .8) +
    geom_point(aes(colour = model), size = 1.6) +
    geom_text(data = ends, aes(k_max, y, label = label), colour = ends$col, hjust = 0,
              nudge_x = .03 * k_max, size = 3.8) +
    scale_colour_manual(values = setNames(c(grey, ink), c("Rescorla–Wagner", second))) +
    scale_linetype_manual(values = setNames(c("22", "solid"), c("Rescorla–Wagner", second))) +
    scale_x_continuous(breaks = seq(0, k_max, by = 5), expand = expansion(mult = c(.02, .02))) +
    scale_y_continuous(labels = axis_num) +
    coord_cartesian(xlim = c(0, 1.55 * k_max), ylim = ylim, clip = "off") +
    labs(x = xlab, y = "Gap in learned value", title = title) +
    theme_app()
}

# Step 3: the adaptive model's gap under several runs, drawn over k_max pear
# trials, with the trials beyond the task's ten shaded, as in Figure 4b. gaps is
# a named list of gap vectors for k = 0, ..., k_max; the first is drawn in ink,
# the others in gray with their own line types.
plot_gap_runs <- function(gaps, title, shade_after = NULL, ylim = NULL,
                          xlab = "Completed pear trials", mark_peak = FALSE, subtitle = NULL) {
  k_max <- length(gaps[[1]]) - 1
  d <- do.call(rbind, lapply(names(gaps), function(nm)
    data.frame(run = nm, k = 0:k_max, gap = gaps[[nm]])))
  d$run <- factor(d$run, levels = names(gaps))
  types <- setNames(c("solid", "22", "42", "12", "1343")[seq_along(gaps)], names(gaps))
  cols <- setNames(c(ink, rep(grey, length(gaps) - 1)), names(gaps))
  ends <- data.frame(label = names(gaps), gap = vapply(gaps, function(g) g[k_max + 1], 0),
                     col = unname(cols))
  y_range <- if (is.null(ylim)) range(d$gap, 0) else ylim
  ends$y <- spread_labels(ends$gap, .08 * diff(y_range))
  p <- ggplot(d, aes(k, gap))
  if (!is.null(shade_after)) p <- p + annotate("rect", xmin = shade_after, xmax = k_max,
                                               ymin = -Inf, ymax = Inf, fill = tint)
  p <- p + zero_line(k_max) +
    geom_line(aes(colour = run, linetype = run), linewidth = .8)
  if (mark_peak) {
    top <- d[d$run == names(gaps)[1], ]
    top <- top[which.max(top$gap), ]
    p <- p + geom_point(data = top, shape = 21, size = 3, fill = "white", colour = ink, stroke = .7)
  }
  p + geom_text(data = ends, aes(k_max, y, label = label), colour = ends$col, hjust = 0,
                nudge_x = .03 * k_max, size = 3.8) +
    scale_colour_manual(values = cols) +
    scale_linetype_manual(values = types) +
    scale_x_continuous(breaks = seq(0, k_max, by = 5), expand = expansion(mult = c(.02, .02))) +
    scale_y_continuous(labels = axis_num) +
    coord_cartesian(xlim = c(0, 1.5 * k_max), ylim = ylim, clip = "off") +
    labs(x = xlab, y = "B–D gap in learned value", title = title, subtitle = subtitle) +
    theme_app()
}

# Gap after eight pear trials as the carryover of learned value varies (Figure
# 5a). As in that figure, an open circle marks the adaptive model at g_V = 0,
# and a filled point each model at the value chosen.
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
    labs(x = expression("Carryover of learned value, " * italic(g)[V]),
         y = "Gap after eight pear trials",
         title = "The gap after eight pear trials as more value carries over",
         subtitle = "Open circle: the adaptive model without value carryover, as in Step 2. Filled points: your value") +
    theme_app() +
    theme(plot.margin = margin(8, 24, 8, 8))
  if (!is.null(cross)) p <- p +
    annotate("segment", x = cross, xend = cross, y = 0,
             yend = grid$rw[which.min(abs(grid$g_V - cross))], colour = light, linewidth = .4) +
    annotate("text", x = cross, y = .02, label = paste("equal gaps at", num(cross)),
             hjust = 1.06, size = 3.5, colour = grey)
  p
}

# Step 4: the two response functions, as in Figure 6a; with points = TRUE, also
# B's and D's learned values before the responses on pear trials 1, 6 and 10.
plot_response_functions <- function(resp, ks = c(0, 5, 9), points = TRUE) {
  V <- seq(0, 1, by = .005)
  curves <- rbind(data.frame(rule = "Sigmoid", V = V, p = response_prob(V, "sigmoid")),
                  data.frame(rule = "Threshold", V = V, p = response_prob(V, "threshold")))
  marks <- resp[resp$k %in% ks, ]
  pts <- rbind(data.frame(cue = "B", k = marks$k, V = marks$V_B, p = marks$p_B),
               data.frame(cue = "D", k = marks$k, V = marks$V_D, p = marks$p_D))
  pts <- pts[!(pts$k == 0 & pts$cue == "D"), ]
  pts$label <- paste0(pts$cue, ", pear trial ", pts$k + 1)
  pts$label[pts$k == 0] <- "B and D, pear trial 1"
  if (!points) pts <- pts[0, ]
  # B's labels sit below and to the right of the points, D's above and to the
  # left; the shared first point is labeled above.
  pts$hjust <- ifelse(pts$k == 0, -.1, ifelse(pts$cue == "B", -.15, 1.15))
  pts$vjust <- ifelse(pts$k == 0, -1, ifelse(pts$cue == "B", 1.4, -.6))
  marked <- if (nrow(pts)) list(
    geom_point(data = pts, aes(colour = cue), size = 2.4),
    geom_text(data = pts, aes(label = label, colour = cue, hjust = hjust, vjust = vjust), size = 3.4),
    scale_colour_manual(values = c(B = col_B, D = col_D)))
  ggplot(curves, aes(V, p)) +
    geom_line(aes(linetype = rule), colour = ink, linewidth = .7) +
    marked +
    annotate("text", x = .98, y = .9, label = "threshold", hjust = 1, size = 3.4, colour = grey) +
    annotate("text", x = .7, y = .62, label = "sigmoid", hjust = 0, size = 3.4, colour = grey) +
    scale_linetype_manual(values = c(Sigmoid = "solid", Threshold = "22")) +
    scale_x_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    labs(x = "Learned value for the pear", y = "Probability of a yes",
         title = "Two response functions",
         subtitle = if (points) "Points: B's and D's learned values before the responses on pear trials 1, 6 and 10") +
    theme_app()
}

# Step 4: D minus B before each of the ten responses, as in Figure 6b: the gap
# in learned values (dashed), the difference in yes probabilities under the
# sigmoid (solid), and, shaded, the responses on which the threshold rule
# predicts different responses. "mark" adds a dotted line at a response.
plot_response_differences <- function(resp, mark = NULL) {
  d <- rbind(data.frame(what = "Gap in learned value", q = resp$k + 1, y = resp$V_D - resp$V_B),
             data.frame(what = "Difference in yes probability, sigmoid", q = resp$k + 1,
                        y = resp$p_D - resp$p_B))
  band <- range(resp$k[resp$t_D != resp$t_B]) + 1
  ends <- d[d$q == 10, ]
  ends$y <- spread_labels(ends$y, .06)
  p <- ggplot(d, aes(q, y)) +
    annotate("rect", xmin = band[1] - .5, xmax = band[2] + .5, ymin = -Inf, ymax = Inf, fill = tint) +
    annotate("text", x = mean(band), y = .62, label = "threshold rule:\nresponses differ",
             size = 3.3, colour = grey, lineheight = .9) +
    zero_line(10, 1) +
    geom_line(aes(linetype = what), colour = ink, linewidth = .8) +
    geom_point(colour = ink, size = 1.5)
  if (!is.null(mark)) p <- p + geom_vline(xintercept = mark, colour = grey, linewidth = .4,
                                          linetype = "dotted")
  p + geom_text(data = ends, aes(x = 10.3, y = y, label = c("gap in\nlearned value",
                                                             "difference in yes\nprobability")[match(what, unique(d$what))]),
                hjust = 0, size = 3.4, colour = ink, lineheight = .9) +
    scale_linetype_manual(values = c("Gap in learned value" = "22",
                                     "Difference in yes probability, sigmoid" = "solid")) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(breaks = c(0, .25, .5), labels = c("0", ".25", ".5")) +
    coord_cartesian(xlim = c(1, 12.6), ylim = c(-.02, .66), clip = "off") +
    labs(x = "Response on pear trial", y = "D minus B",
         title = "Differences between D and B before each response",
         subtitle = "Shaded: the responses on which the threshold rule predicts different answers") +
    theme_app()
}

# Step 5: responses of the first 24 participants in a case. Pear trials outside
# cols were not analyzed.
plot_records <- function(case, cols = 1:10, rows = 1:24) {
  y <- case$y[rows, , drop = FALSE]
  d <- expand.grid(participant = seq_along(rows), col = 1:20)
  d$yes <- as.vector(y)
  d$cue <- ifelse(d$col <= 10, "B", "D")
  d$trial <- ifelse(d$col <= 10, d$col, d$col - 10)
  d$fill <- ifelse(!(d$trial %in% cols), "blank", ifelse(d$yes == 1, d$cue, "no"))
  ggplot(d, aes(trial, participant)) +
    geom_tile(aes(fill = fill), colour = "white", linewidth = .6) +
    facet_wrap(~cue, nrow = 1, labeller = as_labeller(c(B = "Responses to B", D = "Responses to D"))) +
    scale_fill_manual(values = c(no = "#C9C9C9", B = col_B, D = col_D, blank = "#FAFAFA")) +
    scale_x_continuous(breaks = c(1, 5, 10), expand = expansion(0)) +
    scale_y_reverse(breaks = NULL, expand = expansion(0)) +
    labs(x = "Pear trial", y = paste0("Participants 1–", length(rows)),
         subtitle = "Colored: yes · Gray: no · Blank: not analyzed") +
    theme_app() +
    theme(axis.line = element_blank(), axis.ticks.y = element_blank())
}

# Step 5: each participant's proportion of yes responses to D minus that to B,
# over the analyzed pear trials.
participant_differences <- function(case, cols) {
  y <- case$y
  data.frame(participant = seq_len(nrow(y)), group = case$allocation,
             difference = rowMeans(y[, 10 + cols, drop = FALSE]) - rowMeans(y[, cols, drop = FALSE]))
}
# The differences drawn as in Figure 7a: dots stacked by value and colored by
# counterbalancing group, and below them the mean difference with its 95%
# interval.
group_labels <- c(`1` = "Light 1 is B", `-1` = "Light 2 is B")
plot_participants <- function(case, design) {
  d <- participant_differences(case, design$cols)
  d$value <- round(d$difference, 10)
  d <- d[order(d$value, -d$group), ]
  d$stack <- ave(d$value, d$value, FUN = seq_along)
  e <- design$effect
  top <- max(d$stack)
  step <- 1 / length(design$cols)
  ggplot(d, aes(value, stack)) +
    annotate("segment", x = 0, xend = 0, y = -.18 * top, yend = top + 1, colour = light, linewidth = .3) +
    geom_point(aes(colour = factor(group, levels = c("1", "-1"))), size = if (nrow(d) > 80) 1.5 else 2.2) +
    annotate("segment", x = e[["lower"]], xend = e[["upper"]], y = -.1 * top, yend = -.1 * top,
             colour = ink, linewidth = .8) +
    annotate("point", x = e[["estimate"]], y = -.1 * top, shape = 21, fill = "white",
             colour = ink, size = 3, stroke = .8) +
    scale_colour_manual(values = col_groups, labels = group_labels, name = NULL) +
    scale_x_continuous(labels = axis_num, breaks = seq(-1, 1, by = .2)) +
    scale_y_continuous(breaks = NULL) +
    coord_cartesian(xlim = range(-.4, .8, d$value + c(-step, step))) +
    labs(x = "Proportion of yes responses, D minus B", y = NULL,
         title = paste0("Each participant's difference (", nrow(d), " participants)"),
         subtitle = "Circle and line: the mean difference and its 95% interval") +
    theme_app() +
    theme(axis.line.y = element_blank(), legend.position = "bottom",
          legend.justification = "left", legend.margin = margin(0, 0, 0, 0))
}

# Step 5: the observed difference, yes to D minus yes to B, on each analyzed
# pear trial, with 95% intervals. With truth = TRUE it adds the generating
# probability difference as a wide gray line, as the reveal of Step 6 does.
plot_contrast <- function(case, design, truth = FALSE) {
  d <- design$by_trial
  p <- ggplot(d, aes(trial, estimate)) +
    geom_hline(yintercept = 0, colour = light, linewidth = .3)
  if (truth) {
    generating <- data.frame(trial = 1:10, difference = case$true_contrast)
    p <- p + geom_line(data = generating, aes(trial, difference), colour = "#D4D4D4",
                       linewidth = 2.4, lineend = "round")
  }
  p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, colour = grey) +
    geom_point(size = 2.2, colour = ink) +
    scale_x_continuous(breaks = 1:10, limits = c(.5, 10.5)) +
    scale_y_continuous(labels = axis_num) +
    expand_limits(y = c(-.2, .45)) +
    labs(x = "Pear trial", y = "Yes to D minus yes to B",
         subtitle = if (truth)
           "Points: observed, with approximate 95% intervals\nGray line: the generating model's difference in yes probability"
         else "Observed difference on each pear trial, with approximate 95% intervals") +
    theme_app()
}

# Step 5: held-out log score of the carryover candidate minus each alternative,
# with bars two standard errors either side, as in Figure 7c.
plot_scores <- function(scores) {
  d <- scores
  # Keep a common scale for the saved cases, expanding it to include wider intervals.
  score_range <- range(-70, 130, 0, d$difference - 2 * d$se, d$difference + 2 * d$se)
  # ggplot draws the first level at the bottom, so listing reset first puts
  # fixed-rate at the top, as in Figure 7c.
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

# Explore: the difference in yes probability, D minus B, for both models
# under the chosen response function.
plot_response_models <- function(r, rule, gamma, theta, second = "Adaptive model") {
  first <- "Rescorla–Wagner"
  rows <- lapply(c("rw", "adaptive"), function(m) data.frame(
    model = c(rw = first, adaptive = second)[[m]], q = r[[m]]$B$k + 1,
    difference = response_prob(r[[m]]$D$V, rule, gamma, theta) -
                 response_prob(r[[m]]$B$V, rule, gamma, theta)))
  d <- do.call(rbind, rows)
  d <- d[d$q <= 10, ]   # the response on pear trial q follows q - 1 completed pear trials
  d$model <- factor(d$model, levels = c(first, second))
  ends <- d[d$q == max(d$q), ]
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
    labs(x = "Response (pear trial)", y = "Yes probability, D minus B",
         title = "Difference in yes probability under the chosen function") +
    theme_app()
}

# Step 7: the percentage of decisions finished by each time, both boundaries
# together, as in Figure 10c. Dashed lines mark the means and points the
# percentages finished by time 0.5.
plot_ddm_finished <- function(d = ddm_summary()) {
  times <- seq(0, 3.5, by = .0025)
  curves <- do.call(rbind, lapply(seq_len(nrow(d)), function(i)
    data.frame(id = d$id[i], t = times, p = ddm_finished(times, d$v[i], d$a[i]))))
  at_mean <- vapply(seq_len(nrow(d)), function(i)
    approx(curves$t[curves$id == d$id[i]], curves$p[curves$id == d$id[i]], xout = d$mean[i])$y, 0)
  means <- data.frame(id = d$id, t = d$mean, p = at_mean)
  half <- data.frame(id = d$id, t = .5, p = d$by_half)
  labels <- data.frame(id = d$id, x = c(1.6, .75), y = c(.6, .86),
                       label = c(paste0("First setting: mean ", num0(d$mean[1], 2)),
                                 paste0("Second setting: mean ", num0(d$mean[2], 2))))
  ggplot(curves, aes(t, p, colour = id)) +
    annotate("segment", x = .5, xend = .5, y = 0, yend = 1, colour = light, linewidth = .4,
             linetype = "22") +
    geom_line(linewidth = .8) +
    geom_segment(data = means, aes(x = t, xend = t, y = 0, yend = p), linewidth = .4,
                 linetype = "22") +
    geom_point(data = half, size = 2.4) +
    geom_text(data = half, aes(label = paste0(round(100 * p), "%")), hjust = -.3, vjust = 1.2,
              size = 4, show.legend = FALSE) +
    geom_text(data = labels, aes(x, y, label = label), hjust = 0, size = 4.2, show.legend = FALSE) +
    scale_colour_manual(values = c(first = ink, second = grey)) +
    scale_x_continuous(breaks = c(0, .5, 1, 2, 3), labels = c("0", "0.5", "1", "2", "3"),
                       expand = expansion(mult = 0)) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0%", "50%", "100%"),
                       expand = expansion(mult = c(0, .03))) +
    coord_cartesian(xlim = c(0, 3.5), ylim = c(0, 1), clip = "off") +
    labs(x = "Decision time (model time units)", y = "Decisions finished",
         title = "Percentage of decisions finished by each time",
         subtitle = "Vertical lines: the mean decision times. Points: decisions finished by time 0.5") +
    theme_app()
}
