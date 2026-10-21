# Step 4: from learned values to predicted responses (Section 4.4)
# The sigmoid and threshold rules applied to the adaptive model's learning
# curves of Step 2, the question whether more participants would reveal a
# difference, and further tries of a rule and a presentation.

responses_ui <- function() {
  scene("scene-4", "4", "What would you observe?", "output.done_q3c", "done_q4b",
    "A response function can preserve a difference in learned values, reduce it, or hide it at a given presentation.",
    p("How would the learned values appear in an experiment? Imagine using pictures
       as cues. You present B and D ten times each. Before each presentation's O2
       feedback, the participant answers “Do you expect O2?” A response function links
       the cue's current learned value to the probability of a yes. This is the
       observation bridge between the model and the recorded answer."),
    p("For this comparison, return to the adaptive model's curves from Step 2:
       no old-value carryover and full associability carryover. Keep those learning
       curves fixed while changing the response function. Each answer reflects learning
       before that presentation's feedback: the first comes before any O2 update,
       and the sixth comes after five updates."),
    p("Two response functions, each applied to both cues:"),
    tags$ul(
      tags$li(HTML("Under the sigmoid rule, the probability of a yes rises smoothly with learned
                    value and is .5 at a value of .5.")),
      tags$li(HTML("Under the threshold rule, the answer is yes whenever learned value is above
                    .5, and no otherwise."))),
    response_formulas_ui(),
    p(class = "muted", "Try both rules with these learning curves. The simulated
       experiment in the next step uses the sigmoid rule throughout, whichever rule
       you try here."),
    div(class = "question", uiOutput("q4a")),
    conditionalPanel("output.done_q4a",
      uiOutput("p4_values"),
      fig("p4_resp", "280px"),
      div(class = "question", uiOutput("q4b"))),
    conditionalPanel("output.done_q4b",
      div(class = "question small",
        p(class = "item-label", "Try another rule or presentation"),
        p(class = "muted", "Each try is added to the table below and to your notes."),
        div(class = "controls-inline",
          radioButtons("q4_try_rule", "Rule", inline = TRUE,
                       choices = c("Sigmoid" = "sigmoid", "Threshold" = "threshold")),
          radioButtons("q4_try_time", "Presentation", inline = TRUE,
                       choices = c("1" = "1", "6" = "6", "10" = "10"))),
        actionButton("q4_try_go", "Show", class = "btn-submit"),
        uiOutput("q4_tries")),
      aside("Section 4.4.")))
}


responses_server <- function(input, output, answers, add_note) {

  # The response functions
  rule_choices <- c(sigmoid = "Sigmoid", threshold = "Threshold")
  time_choices <- c(`1` = "Presentation 1 (before any update)",
                    `6` = "Presentation 6 (after five updates)",
                    `10` = "Presentation 10 (after nine updates)")
  test_points <- as.integer(names(time_choices))
  dir_choices <- c(D = "Higher for D", same = "The same", B = "Higher for B")
  peak_choices <- c(`1` = "At the first answer", `6` = "At the sixth", `10` = "At the last",
                    same = "The same at all three")
  peak_short <- c(`1` = "largest at the first answer", `6` = "largest at the sixth",
                  `10` = "largest at the last", same = "the same at all three")
  q4a_items <- list(
    item("rule", "Response function", rule_choices),
    item("time", "Which presentation will you look at?", time_choices),
    item("dir", "At this presentation, which cue has the higher probability of a yes?",
         dir_choices),
    item("peak", "Of presentations 1, 6 and 10, where do you expect the largest response difference?", peak_choices))

  # The adaptive model's learned values for O2, and the probabilities of a yes,
  # for B and D at presentation q under a response function. The answer on
  # presentation q reflects q - 1 updates.
  test_values <- function(rule, q) {
    q <- as.integer(q); k <- q - 1
    V_B <- at(ad_B$V, k); V_D <- at(ad_D$V, k)
    p_B <- response_prob(V_B, rule); p_D <- response_prob(V_D, rule)
    d <- p_D - p_B
    list(rule = rule, q = q, k = k, V_B = V_B, V_D = V_D, p_B = p_B, p_D = p_D, d = d,
         dir = if (abs(d) < 1e-12) "same" else if (d > 0) "D" else "B")
  }
  # The response differences at presentations 1, 6 and 10, and where the
  # largest is ("same" when all three are equal).
  largest_at <- function(rule) {
    d <- vapply(test_points, function(q) test_values(rule, q)$d, numeric(1))
    top <- as.character(test_points[abs(d - max(d)) < 1e-12])
    list(d = d, top = if (length(top) == length(test_points)) "same" else top)
  }
  test_set <- function(rule, q)
    paste0(first_set(), "; ", rule_set(rule), "; presentation ", q,
           "; response before feedback, after ", as.integer(q) - 1, " updates")
  nth_answer <- function(q) c(`1` = "the first answer", `6` = "the sixth answer",
                              `10` = "the last answer")[[as.character(q)]]
  # The presentations on which the threshold rule answers differently for B and
  # D: B's value at or below .5 and D's above it (Equation 20).
  threshold_window <- which(at(ad_B$V, 0:9) <= .5 & at(ad_D$V, 0:9) > .5)

  ask_server("q4a", input, output, answers, q4a_items, button = "See the predicted responses",
    feedback = function(a) {
      v <- test_values(a$rule, a$time)
      pattern <- largest_at(a$rule)
      rule <- tolower(rule_choices[[a$rule]])
      tagList(
        if (a$dir != v$dir) {
          expected <- if (a$dir == "same") "You expected equal yes probabilities. " else
            paste0("You expected a higher probability of a yes for ", a$dir, ". ")
          P(expected,
            if (v$k == 0)
              "Neither cue has received O2 feedback yet. Both start at value 0,
               and the same response function gives them the same yes probability."
            else if (v$dir == "same")
              "D has the higher learned value, but both values are above .5.
               The threshold gives a yes for either cue, hiding their difference."
            else if (a$dir == "B")
              "D has the higher learned value. This response function never gives a
               lower value a higher yes probability, so it cannot put B ahead."
            else if (a$rule == "sigmoid")
              "D has the higher learned value, and the sigmoid increases with
               value. The two yes probabilities therefore differ."
            else
              "D's value is above .5 while B's is at or below .5. The threshold
               therefore gives a yes for D and a no for B.")
        },
        P("At ", nth_answer(v$q), " (",
          if (v$k == 0) "before any update" else paste0("after ", updates(v$k)),
          "), ", if (v$V_D == v$V_B) "B and D have the same learned value" else
            paste0("D's learned value is ahead by ", prob(v$V_D - v$V_B)),
          ". The ", rule, " rule turns that into ",
          if (v$dir != "same")
            paste0("a difference of ", prob(v$d), " in the probability of a yes.")
          else if (v$p_B %in% c(0, 1))
            paste0("the same answer for both cues: every participant answers ",
                   if (v$p_B == 0) "no" else "yes", " to B and to D.")
          else paste0("the same probability of a yes for both cues, ", prob(v$p_B), ".")),
        if (!(a$peak %in% pattern$top))
          p(switch(a$peak,
            `1` = "You expected the largest difference at the first answer.
              Associability affects learning from feedback; it does not give either
              cue an O2 value before that feedback. Both start at 0, so the first
              response probabilities are equal.",
            `10` = if (a$rule == "sigmoid")
              "You expected the largest difference at the last answer. By then,
               D's yes probability is near the ceiling while B's is rising through
               the steep middle, bringing the probabilities closer. At the sixth
               presentation, B's value is below the midpoint and D's above it, where
               the sigmoid is steepest."
              else "You expected the largest difference at the last answer.
               By then both values are above .5, so the threshold gives a yes for
               both. At the sixth presentation, the values fall on opposite sides
               of the threshold.",
            same = if (a$rule == "sigmoid")
              "You expected the same response difference at all three presentations.
               Both the values and their position on the sigmoid change during
               learning, so the response difference changes too."
              else "You expected the same response difference at all three presentations.
               The threshold gives the same answer for both cues at the first and
               last presentations, but different answers at the sixth.")),
        P("Under the ", rule, " rule the difference is ", prob(pattern$d[1]),
          " at the first answer, ", prob(pattern$d[2]), " at the sixth and ",
          prob(pattern$d[3]), " at the last, so it is ",
          if (identical(pattern$top, "same")) "the same at all three." else
            paste0(peak_short[[pattern$top[1]]], ".")),
        p(if (a$rule == "sigmoid") "The sigmoid is steepest at learned value .5.
            A difference in values is easier to see in responses near that midpoint
            than near the floor or ceiling."
          else paste0("The threshold rule gives different answers only when B is at or below
            .5 and D above it. That happens only on presentations ",
            min(threshold_window), " to ", max(threshold_window), ". Before that both cues
            are below .5, and afterward both are above it.")))
    },
    on_submit = function(a) {
      v <- test_values(a$rule, a$time)
      pattern <- largest_at(a$rule)
      add_note("Step 4", "Response function",
        paste0("Presentation ", v$q, " with the ", tolower(rule_choices[[a$rule]]),
               " rule. Is the probability of a yes higher for D than for B? Where is the",
               " difference largest?"),
        paste0(dir_choices[[a$dir]], ". ", peak_choices[[a$peak]], "."),
        result = paste0("Probability of a yes ", prob(v$p_B), " for B and ",
          prob(v$p_D), " for D, a difference of ", prob(v$d), " (learned values ", prob(v$V_B),
          " and ", prob(v$V_D), "). The difference is ", prob(pattern$d[1]),
          " at the first answer, ", prob(pattern$d[2]), " at the sixth and ",
          prob(pattern$d[3]), " at the last."),
        settings = test_set(a$rule, v$q))
    })

  output$p4_values <- renderUI({
    req(answers$q4a)
    v <- test_values(answers$q4a$rule, answers$q4a$time)
    tagList(tags$table(class = "values",
      tags$thead(tags$tr(tags$th(""), tags$th(class = "cue-B", "B"),
                         tags$th(class = "cue-D", "D"), tags$th("D minus B"))),
      tags$tbody(
        tags$tr(tags$td(paste0("Learned value at presentation ", v$q)),
                tags$td(prob(v$V_B)), tags$td(prob(v$V_D)), tags$td(prob(v$V_D - v$V_B))),
        tags$tr(tags$td("Probability of a yes"), tags$td(prob(v$p_B)),
                tags$td(prob(v$p_D)), tags$td(prob(v$d))))),
      p(class = "table-note", "Differences are calculated from unrounded values."))
  })
  output$p4_resp <- renderPlot({
    req(answers$q4a)
    plot_response_gap(base, answers$q4a$rule, mark = as.integer(answers$q4a$time))
  })

  q4b_items <- list(item("more",
    "Under this rule, could more participants at this presentation reveal the learned-value difference in their responses?",
    c(yes = "Yes.", no = "No.")))
  ask_server("q4b", input, output, answers, q4b_items,
    feedback = function(a) {
      v <- test_values(answers$q4a$rule, answers$q4a$time)
      right <- if (v$dir == "same") "no" else "yes"
      certain <- right == "yes" && all(c(v$p_B, v$p_D) %in% c(0, 1))
      tagList(
        if (a$more == "yes" && right == "no")
          p("A larger sample can reduce sampling noise. It cannot change the fact
            that this rule gives both cues the same response probability.")
        else if (a$more == "no" && right == "yes")
          p("Here the rule does preserve a response difference."),
        p(if (v$V_D == v$V_B) "At this presentation B and D have the same learned value, so
            there is no difference for more participants to reveal. A later presentation could show one."
          else if (right == "no") "Under this rule B and D have the same probability of a yes at
            this presentation. More participants cannot recover a difference that the
            measure does not express. Another presentation, or a different measure, could."
          else if (certain) paste0("Under this rule every participant answers ",
            if (v$p_B == 0) "no" else "yes", " to B and ", if (v$p_D == 0) "no" else "yes",
            " to D at this presentation, so even a few participants show the difference.")
          else paste0("The two probabilities differ by ", num(v$d), ". Each answer is a
            random draw, so in a finite sample D need not get more yes answers than B. With
            more participants, the difference is estimated more precisely.")))
    },
    on_submit = function(a) {
      v <- test_values(answers$q4a$rule, answers$q4a$time)
      add_note("Step 4", "More participants",
        "Under this rule, could more participants reveal the learned-value difference in their responses?",
        q4b_items[[1]]$choices[[a$more]],
        result = paste0("Probability of a yes ", prob(v$p_B), " for B and ",
                        prob(v$p_D), " for D at this presentation, a difference of ", prob(v$d), "."),
        settings = test_set(v$rule, v$q))
    })

  q4_tries <- reactiveVal(list())
  observeEvent(input$q4_try_go, {
    req(answers$q4a, input$q4_try_rule, input$q4_try_time)
    v <- test_values(input$q4_try_rule, input$q4_try_time)
    q4_tries(c(q4_tries(), list(v)))
    add_note("Step 4", "Another rule or presentation",
      paste0(rule_choices[[v$rule]], " rule, presentation ", v$q, "."),
      "Viewed.",
      result = paste0("Probability of a yes ", prob(v$p_B), " for B and ",
                      prob(v$p_D), " for D, a difference of ", prob(v$d), "."),
      settings = test_set(v$rule, v$q))
  })
  output$q4_tries <- renderUI({
    tries <- q4_tries()
    if (!length(tries)) return(NULL)
    tagList(
      p(class = "table-note", "Probability of a yes at the presentation"),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Probabilities of a yes for each try", tags$table(class = "values",
        tags$thead(tags$tr(tags$th("Rule"), tags$th("Presentation"),
                           tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D"),
                           tags$th("D minus B"))),
        tags$tbody(lapply(tries, function(v) tags$tr(
          tags$td(rule_choices[[v$rule]]), tags$td(v$q), tags$td(prob(v$p_B)),
          tags$td(prob(v$p_D)), tags$td(prob(v$d))))))),
      p(class = "table-note", "Differences are calculated from unrounded values."))
  })

  # Starting the inquiry again clears the answers and the tries but keeps the
  # notes.
  list(reset = function() {
    clear_answers(answers, c("q4a", "q4b"))
    q4_tries(list())
  })
}
