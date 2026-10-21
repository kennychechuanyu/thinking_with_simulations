# Shared text, number formats and small building blocks

# Names as the page writes them, in HTML.
RW <- "Rescorla&ndash;Wagner"
gV <- "<i>g</i><sub>V</sub>"
gA <- "<i>g</i><sub>&alpha;</sub>"

P <- function(...) p(HTML(paste0(...)))                  # a paragraph with simple markup
fig <- function(id, height) div(class = "fig", plotOutput(id, height = height))
step_head <- function(n, title) h2(class = "step", span(class = "step-n", n), title)

# One step of the inquiry, shown once the condition "show" holds. It is a
# <details> element, so that a completed step can be folded and reopened.
# "done" names the output that is TRUE once the step is complete; from then on
# the heading also shows "summary", and a button leads to the next step.
scene <- function(id, n, title, show, done, summary, ...) {
  next_step <- as.integer(n) + 1L
  conditionalPanel(show,
    tags$details(id = id, class = "inquiry-scene", open = NA, `data-done` = done,
      tags$summary(step_head(n, title),
        p(class = "scene-summary", summary),
        span(class = "scene-review", "Hide details")),
      div(class = "scene-content", ...,
        conditionalPanel(paste0("output.", done),
          tags$button(type = "button", class = "btn-submit scene-continue",
            `data-next` = paste0("scene-", next_step),
            paste0("Continue to Step ", next_step))))))
}
# Where the tutorial treats the same point, for readers who want more.
aside <- function(...) p(class = "aside", HTML(paste0("In the tutorial: ", ...)))

# num() drops the leading zero, as in the paper, for quantities that cannot
# exceed 1. num0() keeps it for quantities that can (times, drift rates,
# log-score differences) and drops the minus sign of a value that rounds to 0.
num <- function(x, digits = 3) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))
num0 <- function(x, digits) sub("^-(0[.]0*)$", "\\1", formatC(x, format = "f", digits = digits))
p_text <- function(p) {
  if (p < .001) "p < .001" else if (p > .999) "p > .999" else paste("p =", num(p))
}
# Axis labels and parameter values, without leading or trailing zeros: "0",
# ".05", ".1".
axis_num <- function(x) sub("^(-?)0[.]", "\\1.", format(x, trim = TRUE, drop0trailing = TRUE))
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
# Exact 0 and 1 (a zero gap, or the threshold rule's probabilities) print as such.
prob <- function(x) ifelse(x == 0 | abs(x) == 1, as.character(x), num(x))
updates <- function(k) paste(c("one", "two", "three", "four", "five", "six", "seven", "eight",
                                "nine", "ten")[k], if (k == 1) "update" else "updates")

# Settings lines for the notes: the simulation of Steps 1 to 4 (without Phase 3
# for results taken at the end of compound training), and the response function of
# Step 4.
first_set <- function(s = default_settings, phase3 = TRUE) paste0(
  RW, ": shared prediction error, alpha = ", axis_num(s$rw_alpha),
  ", beta = ", axis_num(s$rw_beta),
  "; adaptive model: individual prediction error, S = ", axis_num(s$S),
  ", alpha_init = ", axis_num(s$alpha_init), ", eta = ", axis_num(s$eta),
  "; initial values = 0; outcome magnitude = 1; Phase 1: ", s$n_phase1,
  " A+ and ", s$n_phase1, " C+; Phase 2: ", s$n_compound, " AB+ and ",
  s$n_compound, " DE+",
  if (phase3) paste0("; Phase 3: ", s$n_transfer, " B+ and ", s$n_transfer, " D+ with O2; ",
                     gV, " = ", axis_num(s$g_V), ", ", gA, " = ", axis_num(s$g_alpha)),
  "; E salience = S * (1 + ", format(s$e_offset, scientific = s$e_offset != 0),
  "); adaptive-model change = ", s$change)
rule_set <- function(rule) if (rule == "sigmoid") "sigmoid, &gamma; = 8, &theta; = .5" else
  "threshold, &theta; = .5"

# The training design of Step 1; with third = TRUE, also the O2 phase that
# Step 2 adds.
design_table <- function(third = FALSE) {
  tags$table(class = "design",
    tags$thead(tags$tr(tags$th(""), tags$th("Phase 1 (O1)"), tags$th("Phase 2 (O1)"),
                       if (third) tags$th("Phase 3 (new outcome O2)"))),
    tags$tbody(
      tags$tr(tags$td(class = "cue-B", "Blocking"), tags$td("10 A+"), tags$td("10 AB+"),
              if (third) tags$td("10 B alone")),
      tags$tr(tags$td(class = "cue-D", "Control"), tags$td("10 C+"), tags$td("10 DE+"),
              if (third) tags$td("10 D alone"))))
}

# A table row whose cells may contain simple markup, and a table header that
# ends with B and D columns.
vrow <- function(...) tags$tr(lapply(list(...), function(x) tags$td(HTML(x))))
cue_head <- function(...) tags$thead(tags$tr(tags$th("Model"), lapply(list(...), tags$th),
  tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D")))

# The choices for a prediction of the B-D gap, in Steps 2 and 3.
gap_choices <- c(zero = "Zero after every update.", pos = "Positive (D ahead of B).",
                 neg = "Negative (B ahead of D).")

# Step 5 labels: the three saved cases, the recording windows, and the two choices for g_V.
set_labels <- c(value = "Case 1", associability = "Case 2", reset = "Case 3")
view_labels <- c(late = "presentations 8–10", early = "presentations 1–6",
                 full = "presentations 1–10")
policy_label <- c(zero = paste0(gV, " fixed at 0"), estimated = paste0(gV, " estimated"))

# What the decision rule of Section 5.2 gave, worded to follow "the comparison".
decision_text <- c(carry = "favors associability carryover",
                   no_carry = "favors at least one of the models without associability carryover",
                   unresolved = "is unresolved")

# TRUE when the held-out scores of the carryover and reset candidates differ by
# less than .01, with a standard error below .01.
near_reset_scores <- function(scores) {
  r <- scores[scores$against == "reset", , drop = FALSE]
  nrow(r) == 1L && isTRUE(abs(r$difference) < .01 && r$se < .01)
}
