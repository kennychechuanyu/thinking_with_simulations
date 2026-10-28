# Step 1: what one learning rule implies (Section 2)
# The Rescorla-Wagner calculation for the tone and the light, the question of
# the second compound trial (Box 1), the check of the code (Section 2.3), and
# what the comparison with a cue trained alone can show.

rule_ui <- function() {
  scene("scene-1", "1", "What one learning rule implies", "true", "done_q1c",
    "Under Rescorla–Wagner the light ends at .1303, gaining little because the shared error is small. One compound trial with both cues starting at .30 checks the code.",
    h3("Working out the prediction"),
    p("Suppose expectations change when an outcome differs from what was expected. This
       difference is called the prediction error. When the tone and the light appear
       together, each could learn from the error in its own prediction, or both could learn
       from the error in their combined prediction. The Rescorla–Wagner model uses the second.
       It adds the predictions of the signals present and compares their sum with the
       outcome."),
    P("The model keeps a learned value for each signal, also called its associative strength.
       Learned values are quantities calculated within the model, not measurements of a
       participant's expectations. The apple is represented by 1, and both learned values
       start at zero. On a trial followed by the apple, each signal present changes its value
       by a fixed fraction of the prediction error. This fraction is called the learning rate,
       and here it is ", axis_num(default_settings$beta), " for both the tone and the light."),
    P("On the tone's first trial its value is zero, so the error is 1 and the value rises by
       .12 &times; 1 = .12. On the second trial the error is 1 &minus; .12 = .88, and the value
       rises by .12 &times; .88 = .1056, to .2256. Each trial leaves 88% of the error, so after
       ten tone-only trials the value is 1 &minus; .88<sup>10</sup> &asymp; ",
       num(rw_tr$V_A_after[default_settings$n_first], 4), ". When the light is added, the combined prediction is ",
       num(rw_tr$prediction[11], 4), " + 0, which leaves an error of ", num(rw_tr$error[11], 4),
       ". Both signals learn from this shared error, and each gains .12 &times; ",
       num(rw_tr$error[11], 4), " &asymp; ", num(rw_tr$gain_B[11], 4), ". A new signal trained
       alone would gain .12 on its first trial."),
    p("We write A for the tone and B for the light. Signals such as these are called cues, and a
       trial on which two cues appear together is a compound trial. The training is written
       10 A+ followed by 10 AB+, where the plus sign means that the apple follows."),
    div(class = "scroll-x", tabindex = "0", role = "region",
        `aria-label` = "The first trials, worked by hand", uiOutput("first_trials")),
    rw_formula_ui(),
    div(class = "question", uiOutput("q1a")),
    conditionalPanel("output.done_q1a",
      h3("Checking that the code follows the rule"),
      p("A plausible-looking output can still come from the wrong calculation, so before
         following the remaining trials we check that the program applies the rule. Checking
         that an implementation matches the model's specification is called verification. In
         R, the rule can be written as a short loop:"),
      tags$pre(class = "code", "for (t in seq_along(design)) {
  cues  <- design[[t]]$cues
  delta <- design[[t]]$lambda - sum(V[cues])
  V[cues] <- V[cues] + beta[cues] * delta
}"),
      p("V holds the current learned values, beta the learning rates, and design the cues and
         the outcome of each trial. The program calculates the shared error, delta, from the
         combined prediction before either cue changes value. Two mistakes would be easy to
         make: calculating each cue's own error instead of the shared error, or updating the
         tone first and then recalculating the error for the light."),
      div(class = "question", uiOutput("q1b")),
      conditionalPanel("output.done_q1b",
        h3("The second compound trial"),
        uiOutput("q1a_result"),
        fig("p1_values", "330px"),
        fig("p1_error", "260px"),
        tags$details(class = "worked-example",
          tags$summary("Follow the calculation trial by trial"),
          p("Choose a trial to see the values before it, the shared error and each cue's gain."),
          sliderInput("t1_trial", "Trial", min = 1, max = 20, value = 12, step = 1,
                      ticks = FALSE, width = "320px"),
          div(class = "scroll-x", tabindex = "0", role = "region",
              `aria-label` = "One trial in detail", uiOutput("t1_table"))),
        h3("What we can conclude, and what remains open"),
        P("After ten compound trials, the tone has a learned value of about ",
          num(rw_tr$V_A_after[20], 4), " and the light about ", num(rw_tr$V_B_after[20], 4),
          ". Because the two cues gain the same amount on every compound trial, the tone keeps
          the lead of ", num(rw_tr$V_A_after[10], 4), " that it held when the light was
          introduced. Together they remove 24% of the shared error on each compound trial, so
          their combined prediction approaches 1. The light therefore approaches (1 &minus; ",
          num(rw_tr$V_A_after[10], 4), ")/2 &asymp; ", num((1 - rw_tr$V_A_after[10]) / 2, 4),
          ", and after ten compound trials it is already within about ",
          num((1 - rw_tr$V_A_after[10]) / 2 - rw_tr$V_B_after[20]), " of this limit. The limit
          holds for continued compound training with the apple and equal fixed learning
          rates."),
        P("When the light is presented alone, the model's prediction is the light's own learned
          value, about ", num(rw_tr$V_B_after[20], 4), ", because the absent tone contributes
          nothing. A learned value of ", num(rw_tr$V_B_after[20], 4), " does not by itself
          imply a 13% probability of a yes response. That requires a further assumption about
          how learned values relate to responses, which Step 4 examines."),
        div(class = "question", uiOutput("q1c")),
        conditionalPanel("output.done_q1c",
          settled("Under Rescorla–Wagner, the light gains little on each compound trial because the
                   shared error is small, and the tone gains exactly as much as the light. After ten
                   compound trials the light has about .1303, close to its limit of .1393. One
                   compound trial with both cues starting at .30, on which the two calculations
                   disagree, shows that the program applies the shared error.",
                  "Whether the light learned less because its companion had been trained
                   beforehand, and whether a different rule could produce the same pattern for a
                   different reason. Step 2 examines both."),
          aside("Section 2, Figure 2 and Box 1."))))
  )
}


rule_server <- function(input, output, answers, add_note) {
  n1 <- default_settings$n_first

  # The first trials, as the text works them out.
  output$first_trials <- renderUI({
    row <- function(t) {
      r <- rw_tr[t, ]
      gain <- if (r$light) paste0(num(r$gain_A, 4), " each") else num(r$gain_A, 4)
      after <- if (r$light) paste0("A ", num(r$V_A_after, 4), ", B ", num(r$V_B_after, 4))
               else paste0("A ", num(r$V_A_after, 4))
      vrow(t, if (r$light) "A and B" else "A", prob(r$prediction, 4), prob(r$error, 4), gain, after)
    }
    tagList(
      p(class = "table-note", "The first trials, each followed by the apple"),
      tags$table(class = "values",
        tags$thead(tags$tr(tags$th("Trial"), tags$th("Cues"), tags$th("Combined prediction"),
                           tags$th("Shared error"), tags$th("Gain"), tags$th("Values after"))),
        tags$tbody(row(1), row(2), row(n1), row(n1 + 1))),
      p(class = "table-note", "Values are calculated without rounding and shown to four decimals."))
  })

  # Box 1: the second compound trial. The expectation is saved first; the
  # result is shown after the check of the code.
  dir_choices <- c(more = "More than on its first compound trial.",
                   less = "Less than on its first compound trial.",
                   same = "The same amount.")
  why_choices <- c(
    error = "The tone and the light together now predict more of the apple, so the error they share is smaller.",
    finished = "The tone has finished learning about the apple, so little is left for the light.",
    rate = "The light's learning rate becomes smaller after its first compound trial.",
    left = "The light still has most of the apple left to learn.")
  q1a_items <- list(
    item("dir", paste0("The light gained about ", num(rw_tr$gain_B[n1 + 1], 4), " on its first
      compound trial. When the tone and the light are again followed by the apple, how much will
      the light gain?"), dir_choices),
    item("why", "What is your reason?", why_choices))
  ask_server("q1a", input, output, answers, q1a_items,
    reason = "Which quantities besides the light's value would you follow to check your reason? (optional)",
    button = "Save my expectation",
    feedback = function(a) p("Your expectation is saved in your notes."),
    on_submit = function(a) add_note("Step 1", "Your expectation for the second compound trial",
      q1a_items[[1]]$label,
      paste0(dir_choices[[a$dir]], " Reason: ", why_choices[[a$why]],
             if (nzchar(a$reason)) paste0(" Quantities to follow: ", a$reason)),
      settings = first_set(pear = FALSE)))

  # The check of the code (Section 2.3).
  test_choices <- c(
    single = "Train the tone alone for ten trials and check that it reaches .7215.",
    zero = "Run one compound trial with the tone and the light both starting at zero.",
    thirty = "Run one compound trial with the tone and the light both starting at .30.")
  q1b_items <- list(item("test", "Which test would catch a program that calculates each cue's
    own error instead of the shared error?", test_choices))
  check_text <- paste0("With one cue present, a cue's own error is identical to the shared
    error, so the single-cue check cannot catch the mistake. With both cues at zero, the shared
    error and each cue's own error are all 1, so the two calculations agree again. They agree
    whenever the other cues present predict nothing. With both cues at .30, the shared error
    is 1 &minus; (.30 + .30) = .40, so each cue must gain .12 &times; .40 = ",
    num(checks$thirty$specified[["B"]]), ". A program that used each cue's own error would give
    .12 &times; (1 &minus; .30) = ", num(checks$thirty$own[["B"]]), ". A program that updated the
    tone first, to .30 + .048 = .348, and then recalculated the error for the light would give
    the light .12 &times; [1 &minus; (.348 + .30)] = ", num(checks$thirty$one_at_a_time[["B"]], 5),
    ".")
  check_table <- function() {
    # Gains to at most five decimals, without trailing zeros: .048, .04224.
    short <- function(x) axis_num(round(x, 5))
    prog <- function(label, name) vrow(label, num(rw_tr$V_A_after[n1], 4),
      short(checks$zero[[name]][["B"]]), short(checks$thirty[[name]][["B"]]))
    tagList(
      p(class = "table-note", "What each program gives. The first column of numbers is the
        tone's learned value after ten trials alone. The last two columns are the light's gain on
        one compound trial followed by the apple."),
      div(class = "scroll-x", tabindex = "0", role = "region", `aria-label` = "Three programs",
        tags$table(class = "values",
          tags$thead(tags$tr(tags$th("Program"), tags$th("Tone after ten trials alone"),
                             tags$th("From 0 and 0"), tags$th("From .30 and .30"))),
          tags$tbody(prog("Shared error, as specified", "specified"),
                     prog("Each cue's own error", "own"),
                     prog("Tone updated first", "one_at_a_time")))))
  }
  ask_server("q1b", input, output, answers, q1b_items, button = "See what each program gives",
    feedback = function(a) tagList(
      if (a$test != "thirty") p(switch(a$test,
        single = "That test checks the tone's training, but it cannot catch this mistake.",
        zero = "That test catches a program that updates the tone first, but it cannot catch a
          program that uses each cue's own error.")),
      check_table(),
      P(check_text),
      p("These starting values were chosen for the check. They are not the result of the earlier
         training. Calculating a separate error for each cue is a legitimate modeling choice,
         and the second model, in Step 2, makes it. In a program for Rescorla–Wagner, however,
         it would be a mistake. Whether the model gives an adequate account of actual learning
         is a different question, usually called validation.")),
    on_submit = function(a) add_note("Step 1", "Checking the code", q1b_items[[1]]$label,
      test_choices[[a$test]],
      result = paste0("Light's gain from .30 and .30: ", num(checks$thirty$specified[["B"]]),
        " as specified, ", num(checks$thirty$own[["B"]]), " with each cue's own error, ",
        num(checks$thirty$one_at_a_time[["B"]], 5), " with the tone updated first."),
      settings = "One compound trial followed by the apple, learning rate .12 for both cues"))

  # The result of Box 1, compared with the saved expectation.
  second <- rw_tr[n1 + 2, ]
  result_text <- paste0("On the second compound trial, the tone has ", num(second$V_A, 4),
    " (its ", num(rw_tr$V_A_after[n1], 4), " plus ", num(rw_tr$gain_A[n1 + 1], 4), ") and the
    light ", num(second$V_B, 4), ". Together they predict ", num(second$prediction, 4), ", so
    the shared error is ", num(second$error, 4), ", and the light gains .12 &times; ",
    num(second$error, 4), " &asymp; ", num(second$gain_B, 4), ", less than on its first compound
    trial. On every compound trial both cues gain value, their combined prediction rises, and the
    shared error and the gains become smaller.")
  why_feedback <- c(
    error = "Your reason matches the calculation.",
    finished = paste0("The tone has not finished learning. It gains ", num(second$gain_A, 4),
      " on this trial, exactly as much as the light, because the two cues share the error and
      have equal learning rates."),
    rate = "Under Rescorla–Wagner the learning rate stays at .12. The gain falls because the
      shared error falls.",
    left = "The light does have most of the apple left to learn, but it does not learn from its
      own error. It learns from the error it shares with the tone, and that error is smaller.")
  output$q1a_result <- renderUI({
    req(answers$q1a, answers$q1b)
    a <- answers$q1a
    tagList(
      p(class = "answered-label", "Your expectation"),
      tags$blockquote(HTML(paste0(dir_choices[[a$dir]], " ", why_choices[[a$why]]))),
      P(result_text),
      P(if (a$dir == "less" && a$why == "error") "Your direction and your reason both match."
        else if (a$dir == "less") paste0("The direction is right, but the reason is not. ",
                                         why_feedback[[a$why]])
        else if (a$why == "error") paste0("Your reason is the right one, but it implies that the
          light gains less, not ", if (a$dir == "more") "more" else "the same", ".")
        else paste0("The light gains less, not ", if (a$dir == "more") "more" else "the same",
                    ". ", why_feedback[[a$why]])),
      p("If we followed only the light, we would see its gain fall but not why. We see why when
         we also follow the tone and the shared error."))
  })
  observeEvent(answers$q1b, {
    a <- answers$q1a
    if (is.null(a)) return()
    add_note("Step 1", "The second compound trial", "How much does the light gain on its second
      compound trial?", paste0(dir_choices[[a$dir]], " ", why_choices[[a$why]]),
      result = result_text, settings = first_set(pear = FALSE))
  }, ignoreInit = TRUE)

  output$p1_values <- renderPlot({ req(answers$q1b); plot_blocking_values(rw_tr) })
  output$p1_error <- renderPlot({ req(answers$q1b); plot_shared_error(rw_tr, mark = input$t1_trial) })
  output$t1_table <- renderUI({
    req(answers$q1b, input$t1_trial)
    r <- rw_tr[input$t1_trial, ]
    gain <- function(x) if (is.na(x)) "&mdash;" else paste0(".12 &times; ", prob(r$error, 4),
                                                             " = ", num(x, 4))
    # The combined prediction and the shared error belong to both cues, so
    # their cells span both columns.
    both <- function(label, x) tags$tr(tags$td(label), tags$td(colspan = 2, HTML(x)))
    tags$table(class = "values two",
      tags$thead(tags$tr(tags$th(paste("Trial", r$trial)), tags$th("Tone A"), tags$th(class = "cue-B", "Light B"))),
      tags$tbody(
        vrow("Present", "yes", if (r$light) "yes" else "no"),
        vrow("Value before the trial", prob(r$V_A, 4), prob(r$V_B, 4)),
        both("Combined prediction", if (r$light) paste0(prob(r$V_A, 4), " + ", prob(r$V_B, 4),
                                                        " = ", prob(r$prediction, 4))
             else paste0(prob(r$prediction, 4), ", from the tone alone")),
        both("Shared error", paste0("1 &minus; ", prob(r$prediction, 4), " = ", prob(r$error, 4))),
        vrow("Gain", gain(r$gain_A), gain(r$gain_B)),
        vrow("Value after the trial", num(r$V_A_after, 4), num(r$V_B_after, 4))))
  })

  # What the comparison with a cue trained alone shows.
  show_choices <- c(
    tone = "The trained tone made the light learn less, because it left little error for the light.",
    two = "The light's history differs from that cue's in two ways, so this comparison cannot show which one mattered.",
    any = "Any companion makes a light learn less, whatever the companion's own history.")
  q1c_items <- list(item("show", paste0("The light learned less than a new cue trained alone with
    the apple: about ", num(rw_tr$V_B_after[20], 4), " against ", num(rw_tr$V_A_after[n1], 4),
    " after ten trials. What does this comparison show?"), show_choices))
  q1c_text <- "The light had a companion, and the companion had been trained beforehand. Either
    difference could reduce what the light learns. In the learning literature, blocking is
    defined by a comparison in behavior: responding to a cue is weaker when its companion has
    been trained beforehand than when its companion is new. To find out whether the trained tone
    made the difference, the light has to be compared with a light that also has a companion,
    but a new one."
  ask_server("q1c", input, output, answers, q1c_items,
    feedback = function(a) tagList(
      if (a$show == "tone") p("The comparison changes two things at once, so it cannot single
        out the trained tone.")
      else if (a$show == "any") p("The comparison changes two things at once, so it cannot show
        that either."),
      p(q1c_text)),
    on_submit = function(a) add_note("Step 1", "What the comparison shows", q1c_items[[1]]$label,
      show_choices[[a$show]], result = q1c_text, settings = first_set(pear = FALSE),
      open = "Did the trained tone make the difference? Could another rule give the same result for a different reason?"))

  # Starting the inquiry again clears the answers but keeps the notes.
  list(reset = function() clear_answers(answers, c("q1a", "q1b", "q1c")))
}
