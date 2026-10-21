# Supplementary Figure S1 (Sections 5.3 and 5.4, Supplement S2): (a) the KL
# divergence between the generating probabilities and each alternative's best
# fit, by view; (b) the profile likelihood of associability carryover g_alpha.
# Reads landscape.csv and carryover_profile.csv.
#   Rscript analysis/section5_figS1_mimicry_and_profile.R

if (!file.exists("R/figure_style.R")) stop("Run this script from the repository root.")
source("R/figure_style.R")
library(patchwork)

mimicry <- read.csv("results/section5/landscape.csv")
mimicry <- mimicry[mimicry$scenario == "associability" & mimicry$model != "carry", ]
mimicry$view_label <- factor(mimicry$view, levels = c("late", "early", "full"),
                         labels = c("8–10\nlate\n3 answers", "1–6\nearly\n6 answers", "1–10\nfull\n10 answers"))
profile <- read.csv("results/section5/carryover_profile.csv")
fitted_kl <- function(model, view) {
  mimicry$fitted_kl[mimicry$model == model & mimicry$view == view]
}

panel_a <- ggplot(mimicry, aes(view_label, fitted_kl, colour = model, group = model)) +
  geom_line(linewidth = .7) +
  geom_point(size = 2) +
  scale_colour_manual(values = paper_colours[c("fixed", "reset")],
                      labels = c(fixed = "Fixed-rate", reset = "Reset")) +
  # KL divergence can exceed 1, so its axis keeps the leading zero.
  scale_y_continuous(labels = function(x) format(x, trim = TRUE, drop0trailing = TRUE)) +
  labs(x = "Presentations kept for each cue", y = "KL divergence per participant",
       title = "(a)  Imitating the generating process") +
  theme_supplement()

# Twice the loss in log likelihood relative to the best value of g_alpha.
panel_b <- ggplot(profile, aes(g_alpha, 2 * (nll - min(nll)))) +
  geom_line(colour = paper_colours[["carry"]], linewidth = .8) +
  geom_point(colour = paper_colours[["carry"]], size = 1.5) +
  scale_x_continuous(breaks = c(0, .25, .5, .75, 1), labels = c("0", ".25", ".5", ".75", "1")) +
  labs(x = expression("Associability carryover, " * italic(g)[italic("α")]),
       y = "Twice the log-likelihood loss", title = "(b)  Profile likelihood") +
  theme_supplement()

cat(sprintf("KL divergence per participant, full record: fixed-rate %.3f, reset %.3f\n",
            fitted_kl("fixed", "full"), fitted_kl("reset", "full")))
cat(sprintf("KL divergence per participant, presentations 8-10: fixed-rate %.4f, reset %.4f\n",
            fitted_kl("fixed", "late"), fitted_kl("reset", "late")))
cat(sprintf("Profile likelihood of g_alpha: maximum at %s\n",
            format(profile$g_alpha[which.min(profile$nll)])))
save_figure(panel_a | panel_b, "figS1_mimicry_and_profile", 6.27, 3.6, dpi = 300)

# Values reported in the paper
stopifnot(
  # Section 5.4, Supplement S2: KL divergence 0.119 (fixed-rate) and 0.143
  # (reset) over the full record, and 0.0019 and 0.0015 on presentations 8-10
  abs(fitted_kl("fixed", "full") - .119) < 5e-4,
  abs(fitted_kl("reset", "full") - .143) < 5e-4,
  abs(fitted_kl("fixed", "late") - .0019) < 5e-5,
  abs(fitted_kl("reset", "late") - .0015) < 5e-5,
  # Figure S1 caption: both alternatives come especially close in the late view
  max(fitted_kl("fixed", "late"), fitted_kl("reset", "late")) <
    min(mimicry$fitted_kl[mimicry$view != "late"]),
  # Supplement S2: g_alpha fixed at 0, .1, ..., 1
  isTRUE(all.equal(profile$g_alpha, seq(0, 1, .1))),
  # Section 5.3: maximum at full carryover; the fit changes little down to
  # g_alpha = .5 (twice the loss stays below 1 in panel b) and worsens below it
  profile$g_alpha[which.min(profile$nll)] == 1,
  max(2 * (profile$nll[profile$g_alpha >= .5] - min(profile$nll))) < 1,
  2 * (profile$nll[abs(profile$g_alpha - .4) < 1e-9] - min(profile$nll)) > 5
)
