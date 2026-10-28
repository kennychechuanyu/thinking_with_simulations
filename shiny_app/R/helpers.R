# Shared text, number formats and small building blocks

# Names as the page writes them, in HTML.
RW <- "Rescorla&ndash;Wagner"
gV <- "<i>g</i><sub>V</sub>"
gA <- "<i>g</i><sub>&alpha;</sub>"
# B and D glossed, as at their first use in each step.
B_gloss <- "B (the light that appeared with the trained tone)"
D_gloss <- "D (the comparison light)"

P <- function(...) p(HTML(paste0(...)))                  # a paragraph with simple markup
fig <- function(id, height) div(class = "fig", plotOutput(id, height = height))
step_head <- function(n, title) h2(class = "step", span(class = "step-n", n), title)

# One step of the inquiry, shown once the condition "show" holds. It is a
# <details> element, so that a completed step can be folded and reopened.
# "done" names the output that is TRUE once the step is complete; from then on
# the heading also shows "summary", and a button leads to the next step. The
# last step has no button.
scene <- function(id, n, title, show, done, summary, ..., last = FALSE) {
  next_step <- as.integer(n) + 1L
  conditionalPanel(show,
    tags$details(id = id, class = "inquiry-scene", open = NA, `data-done` = done,
      tags$summary(step_head(n, title),
        p(class = "scene-summary", HTML(summary)),
        span(class = "scene-review", "Hide details")),
      div(class = "scene-content", ...,
        if (!last) conditionalPanel(paste0("output.", done),
          tags$button(type = "button", class = "btn-submit scene-continue",
            `data-next` = paste0("scene-", next_step),
            paste0("Continue to Step ", next_step))))))
}
# Where the tutorial treats the same point, for readers who want more.
aside <- function(...) p(class = "aside", HTML(paste0("In the tutorial: ", ...)))
# What a step has settled and what it leaves open.
settled <- function(settled_text, open_text) div(class = "settled",
  P("<b>Settled.</b> ", settled_text),
  P("<b>Still open.</b> ", open_text))

# num() drops the leading zero, as in the paper, for quantities that cannot
# exceed 1. Like the paper, it rounds a value exactly halfway up (.2775 to
# .278); the small offset keeps binary rounding error from turning such a value
# down. num0() keeps the leading zero for quantities that can exceed 1 (times,
# drift rates, log-score differences). Both print a negative value with a minus
# sign rather than a hyphen, and a value that rounds to zero without a sign.
num <- function(x, digits = 3) {
  s <- sub("^-(0[.]0*)$", "\\1", formatC(x + sign(x) * 1e-12, format = "f", digits = digits))
  sub("^-", "\u2212", sub("^(-?)0\\.", "\\1.", s))
}
num0 <- function(x, digits) sub("^-", "\u2212",
  sub("^-(0[.]0*)$", "\\1", formatC(x, format = "f", digits = digits)))
p_text <- function(p) {
  if (p < .001) "p < .001" else if (p > .999) "p > .999" else paste("p =", num(p))
}
# Axis labels and parameter values, without leading or trailing zeros: "0",
# ".05", ".1".
axis_num <- function(x) sub("^-", "\u2212",
  sub("^(-?)0[.]", "\\1.", format(x, trim = TRUE, drop0trailing = TRUE)))
# Percentages to one decimal, as the paper's tables print them.
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
# Exact 0 and 1 (a zero gap, an error of 1, or the threshold rule's
# probabilities) print as such.
prob <- function(x, digits = 3) ifelse(x == 0 | abs(x) == 1, as.character(x), num(x, digits))
number_word <- function(k) c("one", "two", "three", "four", "five", "six", "seven", "eight",
                             "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                             "fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
                             "twenty")[k]
pear_trials <- function(k) paste(number_word(k), if (k == 1) "pear trial" else "pear trials")
ordinal <- function(k) c("first", "second", "third", "fourth", "fifth", "sixth", "seventh",
                         "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth",
                         "fourteenth", "fifteenth")[k]

# Settings lines for the notes: the calculations of Steps 1 to 4 (without the
# pear trials for results taken at the end of the compound trials).
first_set <- function(s = default_settings, pear = TRUE) paste0(
  RW, ": shared error, learning rate ", axis_num(s$beta),
  "; adaptive model: own error, salience S = ", axis_num(s$S),
  ", starting associability ", axis_num(s$alpha_init), ", eta = ", axis_num(s$eta),
  "; learned values start at 0; apple and pear = 1; earlier trials: ", s$n_first,
  " A+ and ", s$n_first, " X+; compound trials: ", s$n_compound, " AB+ and ",
  s$n_compound, " CD+",
  if (pear) paste0("; pear trials: ", s$n_pear, " with B alone and ", s$n_pear,
                   " with D alone; ", gV, " = ", axis_num(s$g_V), ", ", gA, " = ",
                   axis_num(s$g_alpha)),
  "; salience of C = S * (1 + ", format(s$c_offset, scientific = s$c_offset != 0),
  "); change to the adaptive model: ", s$change)
rule_set <- function(rule) if (rule == "sigmoid") "sigmoid, &gamma; = 8, &theta; = .5" else
  "threshold, &theta; = .5"

# The two training histories (Table 1); with pear = TRUE, also the pear
# trials that Step 2 adds.
design_table <- function(pear = FALSE) {
  tags$table(class = "design",
    tags$thead(tags$tr(tags$th("History"), tags$th("Earlier trials"), tags$th("Compound trials"),
                       if (pear) tags$th("Pear trials"))),
    tags$tbody(
      tags$tr(tags$td("Trained tone"), tags$td("10 A+"), tags$td("10 AB+"),
              if (pear) tags$td(class = "cue-B", "10 B alone")),
      tags$tr(tags$td("New tone"), tags$td("10 X+"), tags$td("10 CD+"),
              if (pear) tags$td(class = "cue-D", "10 D alone"))))
}

# A table row whose cells may contain simple markup, and a table header that
# ends with B and D columns.
vrow <- function(...) tags$tr(lapply(list(...), function(x) tags$td(HTML(x))))
cue_head <- function(...) tags$thead(tags$tr(lapply(list(...), tags$th),
  tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D")))

# Steps 5 and 6: the three saved cases, the analyzed pear trials, and the two
# treatments of g_V.
set_labels <- c(value = "Case 1", associability = "Case 2", reset = "Case 3")
process_names <- c(associability = "carried associability", value = "carried learned value",
                   reset = "reset associability")
view_labels <- c(late = "pear trials 8–10", early = "pear trials 1–6", full = "all ten pear trials")
policy_label <- c(estimated = paste0(gV, " estimated"), zero = paste0(gV, " fixed at zero"))

# What the decision rule of Section 6.3 gave, worded to follow "the comparison".
decision_text <- c(carry = "favors associability carryover",
                   no_carry = "favors a candidate without associability carryover",
                   unresolved = "is unresolved")

# TRUE when the held-out scores of the carryover and reset candidates differ by
# less than .01, with a standard error below .01.
near_reset_scores <- function(scores) {
  r <- scores[scores$against == "reset", , drop = FALSE]
  nrow(r) == 1L && isTRUE(abs(r$difference) < .01 && r$se < .01)
}
