# Step 4: from model states to behavior (Section 5)
# What would be recorded, the question of Box 4, the sigmoid and threshold
# response functions, where the difference in behavior is largest (with Table
# 6 shown in full once that question is answered), and whether more responses
# would show the difference.

behavior_ui <- function() {
  scene("scene-4", "4", "From model states to behavior", "output.done_q3e", "done_q4c",
    "A response function can enlarge, reduce or hide a difference in learned values. With the lights starting at zero, the first responses are equally likely to be yes under any shared response function.",
    p("Learned values, associabilities and the B–D gap are quantities inside a model, and we
       cannot observe any of them directly. In the task, we would instead show a light, ask
       whether the pear will follow, and record a yes or a no before it appears."),
    h3("What we would observe"),
    P("We continue with the pear trials of Step 2, in which ", B_gloss, " and ", D_gloss, " are
       each presented alone on ten trials. On each trial, before the pear appears, the
       participant answers whether they expect the pear, and we record the response, yes or no.
       In the models, learned values change when the pear appears, not when the question is
       asked, although in a real experiment being asked might itself change what participants
       learn."),
    p("To predict these responses, we need a response function that turns a learned value into
       the probability of a yes response. Each response is numbered by the pear trial on which
       it is given. The response on pear trial 7, for example, comes before the seventh pear
       appears, so it depends on the learned value after six completed pear trials. The response
       function belongs to the observation bridge, which links model states to responses and
       also covers what is recorded and when. The learning rules specify how learned values
       change, but not how they relate to a yes response."),
    p("For now, we assume that the same function applies to both lights, and that the
       probability of a yes response depends only on the light's learned value. The learning
       models do not imply either assumption. A participant might be more inclined to answer yes
       to one light than to the other at the same learned value. Step 6 returns to this
       possibility."),
    div(class = "question", uiOutput("q4a")),
    conditionalPanel("output.done_q4a",
      h3("Two response functions, different predicted behavior"),
      p("A response function may be part of a psychological theory of responding, as in the
         choice rule of Luce (1959), or an additional assumption linking learned values to a
         particular measure. In either case, its form and parameters need support from a theory
         of responding, from independent data, or from both. For example, in these models a cue
         that has never been followed by an outcome has learned value zero. Yes responses to
         such a cue could help estimate the probability of a yes at learned value zero, provided
         the same function applies to it. We consider two forms:"),
      tags$ul(
        tags$li("The sigmoid: the probability of a yes rises gradually with the learned value.
                 It is .5 at the learned value θ = .5, and γ = 8 sets how steeply it rises around
                 that value."),
        tags$li("The threshold rule: the participant answers yes whenever the learned value
                 exceeds θ = .5, and no otherwise.")),
      response_formulas_ui(),
      fig("p4_functions", "320px"),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Learned values and predicted behavior", uiOutput("resp_table")),
      h3("Which behavior to measure, and when"),
      div(class = "question", uiOutput("q4b"))),
    conditionalPanel("output.done_q4b",
      P("On pear trial 1, both lights have learned value zero for the pear. Under any response
         function shared by the two lights, they therefore have the same probability of a yes on
         their first pear trial, even though their associabilities differ. Under the sigmoid this
         probability is about ", num(responses$p_B[1]), ", not zero. Associability affects only
         how much a light learns from the pear, and the first response comes before the pear has
         appeared with that light. If learned values for the apple carried over, as in Step 3,
         the lights could already differ on their first pear trial."),
      fig("p4_differences", "300px"),
      P("We calculate the difference in yes probabilities by applying the response function to
         each light's learned value and then subtracting, not by applying the function to the
         B&ndash;D gap. Under the adaptive model, the gap in learned value is largest after ",
         pear_trials(peak_k), ", just before the response on pear trial ", peak_k + 1, ", whereas
         under the sigmoid the difference in yes probabilities is largest at the response on pear
         trial ", peak_yes + 1, ", after ", pear_trials(peak_yes), ". The figure numbers the
         responses by pear trial, so the two peaks appear at ", peak_k + 1, " and ", peak_yes + 1,
         "."),
      P("The sigmoid is flatter near its lower and upper ends, so the same difference in learned
         value produces a smaller difference in probability there. After six pear trials, B's
         value (", num(responses$V_B[7]), ") and D's (", num(responses$V_D[7]), ") lie on either
         side of the steep middle of the sigmoid. After eight, D's value (",
         num(responses$V_D[9]), ") has moved onto the flatter upper part. The pear trial on
         which this difference peaks therefore depends on &theta; and &gamma;. Under the
         threshold rule, the two lights' predicted responses differ only on pear trials ",
         min(threshold_window) + 1, " to ", max(threshold_window) + 1, ", the shaded band in the
         figure."),
      P("Equal B&ndash;D gaps need not give equal differences in behavior. With ", gV, " = .90
         (Step 3), the two models had nearly equal gaps after eight pear trials, about ",
         num(at(gv90$rw$gap, 8)), " under ", RW, " and ", num(at(gv90$adaptive$gap, 8)), " under
         the adaptive model. The learned values themselves differ, however. They are about ",
         num(at(gv90$rw$B$V, 8)), " for B and ", num(at(gv90$rw$D$V, 8)), " for D under ", RW,
         ", and ", num(at(gv90$adaptive$B$V, 8)), " and ", num(at(gv90$adaptive$D$V, 8)), " under
         the adaptive model. With the sigmoid, the yes probabilities then differ by about ",
         num(yes_gap(gv90$rw, 8)), " under ", RW, " but by only about ",
         num(yes_gap(gv90$adaptive, 8)), " under the adaptive model, whose values lie on the
         flatter upper part of the curve."),
      div(class = "question", uiOutput("q4c"))),
    conditionalPanel("output.done_q4c",
      tags$details(class = "worked-example",
        tags$summary("Try another response function or response"),
        p(class = "muted", "Each try is added to the table below and to your notes."),
        div(class = "controls-inline",
          radioButtons("q4_try_rule", "Response function", inline = TRUE,
                       choices = c("Sigmoid" = "sigmoid", "Threshold" = "threshold")),
          sliderInput("q4_try_q", "Response on pear trial", min = 1, max = 10, value = 1, step = 1,
                      ticks = FALSE, width = "260px")),
        actionButton("q4_try_go", "Show", class = "btn-submit"),
        uiOutput("q4_tries")),
      settled("A response function can enlarge, reduce or hide a difference in learned values,
               and where the difference in behavior is largest depends on it. With the lights
               starting the pear trials at zero, their first responses are equally likely to be
               yes under any shared response function.",
              "What an experiment could establish from responses, each a random yes or no.
               Step 5 simulates such an experiment."),
      aside("Section 5, Equations 6 and 7, Table 6, Figure 6 and Box 4."))
  )
}

# The difference in sigmoid yes probabilities, D minus B, after k pear trials.
yes_gap <- function(m, k) response_prob(at(m$D$V, k), "sigmoid") - response_prob(at(m$B$V, k), "sigmoid")


behavior_server <- function(input, output, answers, add_note) {

  # Box 4: after five pear trials.
  five <- responses[responses$k == 5, ]
  likely_choices <- c(
    depends = "It depends on how learned values are turned into responses. Under a function that rises with learned value, a yes to D is at least as likely as a yes to B.",
    value = paste0("B gets a yes with probability ", num(five$V_B), ", and D with probability ",
                   num(five$V_D), "."),
    same = "They are equally likely, because both lights started the pear trials at zero.")
  must_choices <- c(
    yes = "Yes, because B's learned value is below .5 and D's above it.",
    only = "Only if the response function is deterministic, for example a yes for every learned value above .5 and a no below it.",
    never = "No. A response is never certain, whatever the rule.")
  q4a_items <- list(
    item("likely", paste0("After five pear trials, the adaptive model gives B a learned value of
      about ", num(five$V_B), " and D about ", num(five$V_D), ". How likely is a participant to
      answer yes to each light on the next pear trial?"), likely_choices),
    item("must", "Must the participant answer no to B and yes to D?", must_choices))
  q4a_text <- paste0("Under the sigmoid described below, with slope &gamma; = 8 and midpoint
    &theta; = .5, B's value of ", num(five$V_B), " gives a yes probability of 1/{1 +
    exp[&minus;8(", num(five$V_B), " &minus; .5)]} &asymp; ", num(five$p_B), ", and D's value of ",
    num(five$V_D), " gives about ", num(five$p_D), ". The learned values differ by about ",
    num(five$V_D - five$V_B), ", whereas the probabilities differ by about ",
    num(five$p_D - five$p_B), ". A participant may still answer yes to B, although the
    probability is only ", num(five$p_B), ", or answer no to D. Under a threshold rule at .5, the
    participant answers no to B and yes to D.")
  ask_server("q4a", input, output, answers, q4a_items,
    reason = "What else would you need to assume to answer these questions? (optional)",
    button = "See two response functions",
    feedback = function(a) tagList(
      if (a$likely == "value") p("A learned value is not a probability of a yes, just as the
        light's value of .1303 in Step 1 did not imply a 13% probability of a yes.")
      else if (a$likely == "same") p("The lights started equal, but after five pear trials their
        learned values differ."),
      if (a$must == "yes") p("Only a deterministic rule, such as a threshold at .5, makes these
        responses certain. Under a response function that rises gradually, either response can
        occur.")
      else if (a$must == "never") p("A threshold rule does make them certain. Whether a response
        is certain depends on the response function."),
      P(q4a_text),
      p("To answer either question, we have to assume a response function.")),
    on_submit = function(a) add_note("Step 4", "Would a difference in learning be visible?",
      q4a_items[[1]]$label,
      paste0(likely_choices[[a$likely]], " Must: ", must_choices[[a$must]],
             if (nzchar(a$reason)) paste0(" Further assumption: ", a$reason)),
      result = q4a_text, settings = paste0(first_set(), "; sigmoid with gamma = 8, theta = .5")))

  # Table 6. Until the question about the largest difference is answered, it
  # shows the learned values only.
  output$resp_table <- renderUI({
    req(answers$q4a)
    full <- !is.null(answers$q4b)
    yn <- function(x) ifelse(x == 1, "yes", "no")
    pair <- function(b, d) paste0(b, " / ", d)
    tagList(
      p(class = "table-note", if (full) "The adaptive model: learned values and predicted
        behavior before each response. Each cell gives B first and D second." else "The
        adaptive model: learned values before each response. Each cell gives B first and D
        second."),
      tags$table(class = "values",
        tags$thead(tags$tr(tags$th("Response on pear trial"), tags$th("Completed pear trials"),
                           tags$th("Learned value"),
                           if (full) tagList(tags$th("Yes probability, sigmoid"),
                                             tags$th("Predicted response, threshold")))),
        tags$tbody(lapply(seq_len(nrow(responses)), function(i) {
          r <- responses[i, ]
          do.call(vrow, c(list(r$k + 1, r$k, pair(num(r$V_B), num(r$V_D))),
            if (full) list(pair(num(r$p_B), num(r$p_D)), pair(yn(r$t_B), yn(r$t_D)))))
        }))),
      p(class = "table-note", "Probabilities are calculated from unrounded learned values. The
        response on each pear trial is recorded before the pear appears on that trial."))
  })
  output$p4_functions <- renderPlot({
    req(answers$q4a)
    plot_response_functions(responses, points = !is.null(answers$q4b))
  })

  # Where the difference in behavior is largest.
  rule_choices <- c(sigmoid = "Sigmoid", threshold = "Threshold")
  peak_choices <- c(`1` = "On pear trial 1.", `6` = "On pear trial 6.", `10` = "On pear trial 10.",
                    same = "The same on all three.")
  q4b_items <- list(
    item("rule", "Choose a response function.", rule_choices),
    item("peak", "Of the responses on pear trials 1, 6 and 10, where is the difference between
      D's and B's yes probabilities largest?", peak_choices))
  # The differences on pear trials 1, 6 and 10, and where the largest is
  # ("same" when all three are equal).
  largest_at <- function(rule) {
    q <- c(1, 6, 10)
    p <- function(V) response_prob(V, rule)
    d <- p(responses$V_D[q]) - p(responses$V_B[q])
    top <- as.character(q[abs(d - max(d)) < 1e-12])
    list(d = d, top = if (length(top) == length(q)) "same" else top)
  }
  q4b_feedback <- function(a) {
    pattern <- largest_at(a$rule)
    tagList(
      if (!(a$peak %in% pattern$top)) p(switch(a$peak,
        `1` = "Associability affects learning from the pear. It does not give either light a
          value for the pear before the first pear trial. Both start at zero, so the first yes
          probabilities are equal.",
        `10` = if (a$rule == "sigmoid") "By pear trial 10, D's yes probability is near the top
          of the sigmoid, while B's is rising through the steep middle, so the probabilities
          come closer." else "By pear trial 10, both values are above .5, so the threshold rule
          gives a yes to both lights.",
        same = "The values and their position on the response function change during the pear
          trials, so the difference changes too.",
        `6` = "")),
      P("Under the ", if (a$rule == "sigmoid") "sigmoid" else "threshold rule",
        ", the difference is ", prob(pattern$d[1]), " on pear trial 1, ", prob(pattern$d[2]),
        " on pear trial 6 and ", prob(pattern$d[3]), " on pear trial 10.",
        if (a$rule == "sigmoid") paste0(" Across all ten responses it is largest on pear trial ",
          peak_yes + 1, ".") else paste0(" Across all ten responses, the threshold rule gives
          different responses only on pear trials ", min(threshold_window) + 1, " to ",
          max(threshold_window) + 1, "."),
        " The table and the plot of the response functions now show both lights' yes
        probabilities."))
  }
  ask_server("q4b", input, output, answers, q4b_items, button = "See the differences",
    feedback = q4b_feedback,
    on_submit = function(a) {
      pattern <- largest_at(a$rule)
      add_note("Step 4", "Where the difference in behavior is largest",
        paste0("With the ", if (a$rule == "sigmoid") "sigmoid" else "threshold rule", ": ",
               q4b_items[[2]]$label),
        peak_choices[[a$peak]],
        result = paste0("Difference in yes probability, D minus B: ", prob(pattern$d[1]),
          " on pear trial 1, ", prob(pattern$d[2]), " on pear trial 6 and ", prob(pattern$d[3]),
          " on pear trial 10."),
        settings = paste0(first_set(), "; ", rule_set(a$rule)))
    })
  output$p4_differences <- renderPlot({ req(answers$q4b); plot_response_differences(responses) })

  # Would more responses show the difference?
  more_choices <- c(yes = "Yes.", no = "No.")
  q4c_items <- list(
    item("sig", "Under the sigmoid, could responses from more participants on pear trial 10 show
      the difference between B and D?", more_choices),
    item("thr", "Under the threshold rule?", more_choices))
  ten <- responses[responses$k == 9, ]
  q4c_text <- paste0("Under the sigmoid, which always rises as the learned value increases,
    different learned values give different yes probabilities: ", num(ten$p_B), " for B and ",
    num(ten$p_D), " for D on pear trial 10. Each response is a random draw, so in a small
    sample D need not get more yes responses than B, but with more responses from comparable
    participants we could estimate even a small difference more precisely. Under the threshold
    rule, both lights have a yes probability of 1 on pear trial 10, so more responses from
    participants who learn like this would show no difference at that point, although earlier
    responses could. A different measure, such as a rating of how strongly the participant
    expects the pear, could also show the difference, if its relationship to learned value were
    specified and justified.")
  ask_server("q4c", input, output, answers, q4c_items,
    feedback = function(a) tagList(
      if (a$sig == "no") p("Under the sigmoid the two probabilities differ, so more responses
        estimate that difference more precisely."),
      if (a$thr == "yes") p("With these learned values, the threshold rule gives a yes to both
        lights on pear trial 10, so more participants who learn like this add only more yes
        responses."),
      P(q4c_text)),
    on_submit = function(a) add_note("Step 4", "Would more responses show the difference?",
      "On pear trial 10, could more responses show the difference between B and D?",
      paste0("Sigmoid: ", more_choices[[a$sig]], " Threshold rule: ", more_choices[[a$thr]]),
      result = q4c_text, settings = first_set(),
      open = "What could an experiment with random yes-or-no responses establish?"))

  # Further tries of a function and a response.
  q4_tries <- reactiveVal(list())
  try_values <- function(rule, q) {
    V_B <- responses$V_B[q]; V_D <- responses$V_D[q]
    list(rule = rule, q = q, V_B = V_B, V_D = V_D,
         p_B = response_prob(V_B, rule), p_D = response_prob(V_D, rule))
  }
  observeEvent(input$q4_try_go, {
    req(answers$q4c, input$q4_try_rule, input$q4_try_q)
    v <- try_values(input$q4_try_rule, input$q4_try_q)
    q4_tries(c(q4_tries(), list(v)))
    add_note("Step 4", "Another function or response",
      paste0(rule_choices[[v$rule]], ", response on pear trial ", v$q, "."), "Viewed.",
      result = paste0("Probability of a yes ", prob(v$p_B), " for B and ", prob(v$p_D),
                      " for D, a difference of ", prob(v$p_D - v$p_B), "."),
      settings = paste0(first_set(), "; ", rule_set(v$rule), "; response on pear trial ", v$q))
  })
  output$q4_tries <- renderUI({
    tries <- q4_tries()
    if (!length(tries)) return(NULL)
    tagList(
      p(class = "table-note", "Probability of a yes at the response"),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Probabilities of a yes for each try", tags$table(class = "values",
        tags$thead(tags$tr(tags$th("Function"), tags$th("Pear trial"),
                           tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D"),
                           tags$th("D minus B"))),
        tags$tbody(lapply(tries, function(v) tags$tr(
          tags$td(rule_choices[[v$rule]]), tags$td(v$q), tags$td(prob(v$p_B)),
          tags$td(prob(v$p_D)), tags$td(prob(v$p_D - v$p_B))))))),
      p(class = "table-note", "Differences are calculated from unrounded values."))
  })

  # Starting the inquiry again clears the answers and the tries but keeps the
  # notes.
  list(reset = function() {
    clear_answers(answers, c("q4a", "q4b", "q4c"))
    q4_tries(list())
  })
}
