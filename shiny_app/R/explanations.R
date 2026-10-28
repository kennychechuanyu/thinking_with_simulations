# Step 2: telling two explanations apart (Section 3)
# The comparison light D with its new companion, the adaptive model and its
# first compound trials, the states that both models leave, the choice of a
# comparison, the pear trials (Box 2), and why the adaptive model's gap
# narrows.

explanations_ui <- function() {
  scene("scene-2", "2", "Telling two explanations apart", "output.done_q1c", "done_q2d",
    "Both models leave B below D. Only the adaptive model gives B a lower learning rate, and with the pear it predicts that B learns more slowly than D.",
    p("A light might learn less simply because it shares the apple with a companion, whether
       or not the companion was trained first. We therefore compare the light of Step 1 with a
       light whose companion is new. As with a control condition in an experiment, the two lights
       should then differ in one respect only."),
    h3("A light with a new companion"),
    p("We now need more letters, and they come in matching pairs: A and C are tones, and B and
       D are lights. Tone A is trained first and then appears with light B, as in Step 1. The
       comparison light D appears with the new tone C. A buzzer, X, is followed by the apple on
       ten earlier trials, so that both histories contain the same number of earlier apple
       trials. X never appears with C or D."),
    div(class = "training-design",
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "The two training histories", design_table()),
      div(class = "cue-key-box",
        h3(class = "sub", "Cue key"),
        tags$dl(class = "cue-key",
          tags$dt("A"), tags$dd("Tone, trained first, then with B."),
          tags$dt(class = "cue-B", "B"), tags$dd("Light that appears with the trained tone."),
          tags$dt("X"), tags$dd("Buzzer, trained first, never with C or D."),
          tags$dt("C"), tags$dd("New tone, the companion of D."),
          tags$dt(class = "cue-D", "D"), tags$dd("The comparison light.")))),
    p(class = "table-note", "A plus sign means that the apple follows. The counts are per
       history: ten A+ and ten X+ trials, then ten AB+ and ten CD+ trials."),
    p("The two histories share no cues, so in both models the trials with A and B leave C, D
       and X unchanged, and the reverse also holds. All cues are treated as equally noticeable.
       Under Rescorla–Wagner they share the learning rate .12, and in the second model below
       they share the same salience. This is a modeling assumption, which Step 3 examines."),
    P("Under ", RW, ", B gains about ", num(at(compound$B$rw_gain, 0), 4), " on its first
       compound trial, because A already predicts most of the apple. D gains ",
       num(at(compound$D$rw_gain, 0), 2), ", because neither D nor C predicts the apple yet. D
       and C then share the error, and each gains .12 of it, so together they remove 24% of it
       on each compound trial. After ten compound trials, each has half of 1 &minus;
       .76<sup>10</sup>, about ", num(trained$rw$V_D), ". This is less than the .7215 a cue
       reaches alone, because D shares the error with C from the start. This reduction is
       called overshadowing. B ends at about ", num(trained$rw$V_B), ", well below D. Both
       lights had companions, but the one that appeared with the trained tone acquired less
       value. This is the pattern that defines blocking, here in learned values rather than in
       responses."),
    h3("A different reason for learning less"),
    p("Under Rescorla–Wagner, B learns less because A reduces the error that B shares with it.
       There is another possibility. B might still make a large error of its own, yet change
       its learned value less after repeatedly appearing with a better predictor. Learning
       would then be reduced because B responds less to its error, rather than because little
       error remains."),
    p("Mackintosh (1975) proposed that attention shifts toward cues that predict outcomes
       better than the other cues present. In what the tutorial calls the adaptive model, each
       cue keeps a learned value, as in Rescorla–Wagner, but its learning rate has two parts: a
       fixed part, the cue's salience, and a part that changes with experience. Learning
       theorists call the changing part associability. A cue's learning rate is its salience
       times its associability, so lower associability means a smaller change from the same
       error. Associability is a quantity in the model, motivated by an account of
       attention."),
    P("Every cue has salience ", axis_num(default_settings$S), " and starts with associability ",
       axis_num(default_settings$alpha_init), ", so its learning rate begins at .3 &times; .5 =
       .15. Each cue learns from its own error. Its associability changes by &eta; = ",
       axis_num(default_settings$eta), " times the difference between the error of the other
       cues present and its own error. When tone A is trained alone, there is no other cue, so
       its error is compared with the error of predicting zero, which is 1 when the apple
       follows. This comparison is a choice in the specification."),
    P("On A's first trial, both errors are 1, so its associability stays at .5 and its learned
       value rises to ", num(at(tone$V, 1)), ". On the second trial, A's own error is .85,
       smaller than the error of predicting zero, so its associability rises by .05 &times;
       (1 &minus; .85) = .0075, to ", num(at(tone$alpha, 2), 4), ". After ten trials alone, A
       has a learned value of about ", num(at(tone$V, 10)), ", more than the 1 &minus;
       .85<sup>10</sup> &asymp; .803 that a fixed learning rate of .15 would give, because A's
       associability rises as A learns."),
    div(class = "question", uiOutput("q2a")),
    conditionalPanel("output.done_q2a",
      adaptive_formula_ui(),
      h3("After ten compound trials"),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Values and learning rates after ten compound trials",
          uiOutput("trained_table")),
      div(class = "question", uiOutput("q2b"))),
    conditionalPanel("output.done_q2b",
      h3("Learning about a different outcome"),
      p("How could you examine the difference between B's and D's learning rates under the
         adaptive model? Choose a comparison and say what you hope to learn from it. If it does
         not isolate that difference, you can try another."),
      div(class = "question", uiOutput("cmp"), uiOutput("cmp_msg", role = "status"))),
    conditionalPanel("output.done_q2d",
      h3("What the gap depends on"),
      p("The gap depends on three choices we made: associability carries over in full, learned
         values for the apple do not, and equal errors leave associability unchanged. The table
         lists these choices together with the rules that define the two models."),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "The choices behind the predictions", choices_table()),
      settled("Both models leave B below D after the compound trials. With the lights starting
               the pear trials at zero and their associabilities carried over in full, only the
               adaptive model predicts that B learns about the pear more slowly than D.",
              "Whether the gap remains under other choices: what carries over to the pear, how
               much training there is, and the rule for associability. Step 3 changes them."),
      aside("Section 3, Tables 1, 3 and 4, Figure 3 and Box 2."))
  )
}

# Table 4: the choices behind the two models' predictions, and the step in
# which each one is changed.
choices_table <- function() tags$table(class = "values two",
  tags$thead(tags$tr(tags$th("Choice"), tags$th(HTML(RW)), tags$th("Adaptive model"),
                     tags$th("Changed in"))),
  tags$tbody(
    vrow("Error from which a cue learns", "The shared error of all cues present",
         "The cue's own error", "&mdash;"),
    vrow("Learning rate", "Fixed", "Salience &times; associability, which changes", "&mdash;"),
    vrow("Associability when the two errors are equal", "&mdash;",
         "Unchanged, whereas Mackintosh proposed a fall", "Step 3"),
    vrow("Learned value for the apple at the start of the pear trials", "Not carried over",
         "Not carried over", "Step 3"),
    vrow("Associability at the start of the pear trials", "&mdash;", "Carried over in full",
         "Step 3")))


explanations_server <- function(input, output, answers, add_note) {
  n_base <- default_settings$n_compound
  B1 <- compound$B[1, ]; B2 <- compound$B[2, ]
  D1 <- compound$D[1, ]; D2 <- compound$D[2, ]

  # The first compound trials under the adaptive model.
  q2a_choices <- c(less = "Less than D, because A already predicts most of the apple.",
                   same = "The same as D.",
                   more = "More than D.")
  q2a_items <- list(item("first", "On the first AB+ trial, how much does B gain under the
    adaptive model, compared with what D gains on its first CD+ trial?", q2a_choices))
  q2a_text <- paste0("B still predicts nothing, so its own error is 1 and its learned value
    increases by .3 &times; .5 &times; 1 = ", num(B1$ad_gain), ". D also gains ",
    num(D1$ad_gain), " on its first CD+ trial. Unlike ", RW, ", the adaptive model does not give
    B a smaller gain at once. What changes on that first compound trial is B's associability.
    A's error is only about 1 &minus; ", num(at(tone$V, 10)), " = ", num(B1$ad_companion_error),
    ", whereas B's is 1, so B's associability falls by .05 &times; (1 &minus; ",
    num(B1$ad_companion_error), "), from ", num(B1$ad_alpha), " to about ",
    num(B1$ad_alpha_after), ". D and its new companion C make equally large errors, so D's
    associability stays at ", num(D1$ad_alpha_after), ". The lower associability affects B's
    next update, not the one that produced it. On the second compound trial, B and D both have
    the learned value ", num(B2$ad_V), " and the error ", num(B2$ad_error), ", but B gains about
    .3 &times; ", num(B2$ad_alpha), " &times; .85 &asymp; ", num(B2$ad_gain), " and D gains .3
    &times; ", num(D2$ad_alpha), " &times; .85 &asymp; ", num(D2$ad_gain), ". They now learn
    different amounts from the same error.")
  first_two_table <- function() {
    cols <- list(B1, B2, D1, D2)
    row <- function(label, f) do.call(vrow, c(list(label), lapply(cols, f)))
    tagList(
      p(class = "table-note", "The adaptive model on the first two compound trials"),
      div(class = "scroll-x", tabindex = "0", role = "region", `aria-label` = "The first two compound trials",
        tags$table(class = "values",
          tags$thead(tags$tr(tags$th(""), tags$th(class = "cue-B", "B, 1st"), tags$th(class = "cue-B", "B, 2nd"),
                             tags$th(class = "cue-D", "D, 1st"), tags$th(class = "cue-D", "D, 2nd"))),
          tags$tbody(
            row("Learned value before the trial", function(r) prob(r$ad_V)),
            row("Own error", function(r) prob(r$ad_error)),
            row("Companion's error", function(r) prob(r$ad_companion_error)),
            row("Learning rate", function(r) num(r$ad_rate)),
            row("Gain", function(r) num(r$ad_gain)),
            row("Associability after the trial", function(r) num(r$ad_alpha_after))))),
      p(class = "table-note", "Values are calculated without rounding. B's companion is A, and
        D's is C."))
  }
  ask_server("q2a", input, output, answers, q2a_items, button = "See the first compound trials",
    feedback = function(a) tagList(
      if (a$first == "less") P("That is what ", RW, " predicts. The adaptive model uses each
        cue's own error, and B's own error is still 1.")
      else if (a$first == "more") p("B and D start the compound trials in the same state: value
        zero and associability .5. Their first gains are equal."),
      P(q2a_text),
      first_two_table(),
      P("Mackintosh proposed that associability falls when the two errors are equal. This rule
        leaves it unchanged, which is why D's stays at ", num(D1$ad_alpha_after), ". Step 3
        returns to this choice.")),
    on_submit = function(a) add_note("Step 2", "The first compound trials", q2a_items[[1]]$label,
      q2a_choices[[a$first]], result = q2a_text, settings = first_set(pear = FALSE)))

  # Table 3, and what else differs between B and D.
  output$trained_table <- renderUI(tagList(
    p(class = "table-note", "Learned values for the apple after ten compound trials"),
    tags$table(class = "values",
      tags$thead(tags$tr(tags$th("Model"), tags$th(class = "cue-B", "B's value"),
                         tags$th(class = "cue-D", "D's value"))),
      tags$tbody(
        vrow(RW, num(trained$rw$V_B), num(trained$rw$V_D)),
        vrow("Adaptive model", num(trained$adaptive$V_B), num(trained$adaptive$V_D)))),
    p(class = "table-note", "Read across each row to compare B with D within one model.")))
  rates_table <- function() tagList(
    p(class = "table-note", "Learning rates after ten compound trials"),
    tags$table(class = "values",
      tags$thead(tags$tr(tags$th("Model"), tags$th(class = "cue-B", "B's learning rate"),
                         tags$th(class = "cue-D", "D's learning rate"))),
      tags$tbody(
        vrow(RW, num(trained$rw$rate_B), num(trained$rw$rate_D)),
        vrow("Adaptive model", num(trained$adaptive$rate_B), num(trained$adaptive$rate_D)))))

  q2b_choices <- c(
    nothing = "Nothing else: under both models, B and D differ only in their learned values for the apple.",
    adaptive = paste0("Under the adaptive model, B also has a lower learning rate than D. Under ",
                      RW, ", their learning rates stay equal."),
    both = "Under both models, B also ends with a lower learning rate, because it learned less about the apple.")
  q2b_items <- list(item("left", "Both models leave B below D. What else differs between B and D
    after the compound trials?", q2b_choices))
  q2b_text <- paste0("Under ", RW, ", B's and D's learning rates stay at ",
    num(trained$rw$rate_B, 2), ". Under the adaptive model, B's associability has fallen to about ",
    num(trained$adaptive$alpha_B), " while D's is still ", num(trained$adaptive$alpha_D),
    ", so B's learning rate is ", num(trained$adaptive$rate_B), " against D's ",
    num(trained$adaptive$rate_D), ". Both models leave B below D, so from the direction of the
    apple result alone we cannot tell which explanation is better. Under the adaptive model, B
    still reaches ", num(trained$adaptive$V_B), ": it learns less than D, not nothing.")
  ask_server("q2b", input, output, answers, q2b_items,
    feedback = function(a) tagList(
      if (a$left == "nothing") p("The adaptive model changes more than learned value: B also
        ends with lower associability.")
      else if (a$left == "both") P("Under ", RW, ", B's smaller gains come from a smaller
        shared error, not from a lower learning rate."),
      rates_table(),
      P(q2b_text),
      P("The two models' starting learning rates differ, .12 and .15, so comparing across rows
        would mix these settings with the rules. The adaptive model's result does not come from
        its larger starting learning rate. With salience .24, so that its learning rates also
        start at .12, B ends the compound trials with associability about ",
        num(low_S$adaptive$alpha_B), " and a learning rate of about ", num(low_S$adaptive$rate_B),
        ", against ", num(low_S$adaptive$rate_D), " for D.")),
    on_submit = function(a) add_note("Step 2", "What else differs after the compound trials",
      q2b_items[[1]]$label, q2b_choices[[a$left]], result = q2b_text,
      settings = first_set(pear = FALSE)))

  # The comparisons, tried one at a time. After more compound trials ("more"),
  # continued training with the apple ("cont") or the comparison across models
  # ("across"), the reader says whether it isolates the difference in learning
  # rates. The pear ("pear") leads to Box 2, after which no further comparison
  # is offered.
  cmp_label <- "Which comparison would you run?"
  cmp_choices <- c(
    more = "Give more compound trials, then compare B's and D's learned values for the apple.",
    cont = "Keep training B and D with the apple, each alone, and compare how quickly they learn.",
    across = "Compare B's learned value for the apple under the two models.",
    pear = "Show B and D alone with a new picture, a pear, starting from zero learned value for the pear, and compare how quickly each learns.")
  tells_label <- "Does this comparison isolate the difference in learning rates?"
  tells_choices <- c(yes = "Yes.", no = "No.")

  cmp_result <- list(
    more = tagList(
      p(class = "table-note", "Learned value for the apple at the end of the compound trials"),
      tags$table(class = "values", cue_head("Model", "Compound trials"),
        tags$tbody(
          vrow(RW, n_base, num(trained$rw$V_B), num(trained$rw$V_D)),
          vrow(RW, n_more, num(more$rw$V_B), num(more$rw$V_D)),
          vrow("Adaptive model", n_base, num(trained$adaptive$V_B), num(trained$adaptive$V_D)),
          vrow("Adaptive model", n_more, num(more$adaptive$V_B), num(more$adaptive$V_D))))),
    cont = tagList(
      p(class = "table-note", "Ten more trials with the apple, each light alone, starting from
        the values and associabilities that the compound trials left"),
      fig("p_cont_curves", "300px"),
      fig("p_cont_gap", "260px")),
    across = tagList(
      p(class = "table-note", "B at the end of the compound trials, and its learning rate on the
        first compound trial"),
      tags$table(class = "values",
        tags$thead(tags$tr(tags$th("Model"), tags$th("B's learned value"),
                           tags$th("First learning rate"))),
        tags$tbody(
          vrow(RW, num(trained$rw$V_B), num(default_settings$beta, 2)),
          vrow("Adaptive model", num(trained$adaptive$V_B),
               num(default_settings$S * default_settings$alpha_init, 2))))),
    pear = tagList(
      p("Suppose the task now shows a picture of a pear instead of an apple. The two lights
         appear separately, each followed by the pear on ten trials. For this comparison, each
         model keeps a separate learned value for each outcome. The update rules are unchanged,
         and only a light's learned value for the pear enters the pear trials. Associability,
         by contrast, belongs to the cue rather than to an outcome, so in the adaptive model the
         associability a light acquired with the apple can affect how it learns about the
         pear."),
      P("We start from the conditions most favorable to telling the models apart. B's and D's
         learned values for the pear start at zero. In the adaptive model, the associability
         each light acquired during the compound trials carries over in full, so B enters the
         pear trials with associability about ", num(trained$adaptive$alpha_B), " and D with ",
         num(trained$adaptive$alpha_D), ". B and D then differ only in their learning rates. A
         pear resembles an apple, however, so what was learned about the apple might well
         affect predictions about the pear. Step 3 changes these conditions one at a time."),
      design_table(pear = TRUE),
      P("We count each light's completed pear trials with <i>k</i>. The B&ndash;D gap is D's
         learned value for the pear minus B's after <i>k</i> pear trials. A positive gap means
         D has learned more."),
      div(class = "question inner", uiOutput("q2c")),
      conditionalPanel("output.done_q2c",
        fig("p2_curves", "300px"),
        fig("p2_gap", "280px"),
        P("Under ", RW, " the gap stays at zero. Under the adaptive model it rises to about ",
          num(at(base$adaptive$gap, peak_k)), " after ", pear_trials(peak_k), " and falls to
          about ", num(at(base$adaptive$gap, 10)), " after ten."),
        tags$details(class = "worked-example",
          tags$summary("Look inside one pear trial"),
          p("Choose a pear trial to check the calculation. Multiply the error by the learning
             rate to get the gain, then add it to the value before the trial."),
          div(class = "controls-inline",
            radioButtons("t_model", "Model", inline = TRUE,
                         choiceNames = list("Adaptive model", HTML(RW)),
                         choiceValues = c("adaptive", "rw")),
            sliderInput("t_k", "Pear trial", min = 1, max = 10, value = 1, step = 1,
                        ticks = FALSE, width = "260px")),
          uiOutput("trial_table"),
          uiOutput("trial_note")),
        div(class = "question inner", uiOutput("q2d")))))

  tells_text <- list(
    more = paste0("B remains below D after ", n_more, " compound trials in both models.
      Blocking remains with more training, but this comparison does not separate a difference in
      learning rates from the other differences between the models."),
    cont = "D stays ahead in both models, and the difference between B and D follows a
      different course under each model. But B and D start from different learned values for
      the apple. B has more left to learn, which tends to increase its gains, and under the
      adaptive model it also has a lower learning rate, which tends to reduce them. The
      difference you see mixes these two influences, so it does not isolate the difference in
      learning rates.",
    across = paste0("B ends at ", num(trained$rw$V_B), " under ", RW, " and ",
      num(trained$adaptive$V_B), " under the adaptive model. The models use different errors and
      different illustrative learning rates, so comparing across models mixes these settings
      with the rules."))
  tells_feedback <- function(ch, verdict) paste0(
    if (verdict == "yes") switch(ch,
      more = "B staying below D shows that blocking persists. Both models predict that. ",
      cont = "The same extra training does not give B and D the same starting conditions: they already have different values for the apple. ",
      across = "This comparison changes more than associability: the error a cue learns from and the learning rates also differ between the models. ")
    else "",
    tells_text[[ch]])
  cmp_note <- list(
    more = paste0(RW, ": B ", num(trained$rw$V_B), " and D ", num(trained$rw$V_D), " after ",
      n_base, " compound trials, B ", num(more$rw$V_B), " and D ",
      num(more$rw$V_D), " after ", n_more, ". Adaptive model: B ", num(trained$adaptive$V_B),
      " and D ", num(trained$adaptive$V_D), " after ", n_base, ", B ",
      num(more$adaptive$V_B), " and D ", num(more$adaptive$V_D), " after ", n_more,
      ". Both leave B below D."),
    cont = paste0("After ten more trials with the apple: ", RW, " B ", num(at(co$rw$B$V, 10)),
      ", D ", num(at(co$rw$D$V, 10)), ", difference ", num(at(co$rw$gap, 0)), " at the start and ",
      num(at(co$rw$gap, 10)), " at the end. Adaptive model B ", num(at(co$adaptive$B$V, 10)),
      ", D ", num(at(co$adaptive$D$V, 10)), ", difference ", num(at(co$adaptive$gap, 0)),
      " at the start and ", num(at(co$adaptive$gap, 10)), " at the end."),
    across = paste0("B's learned value ", num(trained$rw$V_B), " (", RW, ") and ",
      num(trained$adaptive$V_B), " (adaptive model). Learning rates on the first compound trial ",
      num(default_settings$beta, 2), " and ", num(default_settings$S * default_settings$alpha_init, 2), "."),
    pear = paste0("Learning rates at the end of the compound trials: ", RW, " B ",
      num(trained$rw$rate_B), " and D ", num(trained$rw$rate_D), ", adaptive model B ",
      num(trained$adaptive$rate_B), " and D ", num(trained$adaptive$rate_D), "."))
  cmp_settings <- list(
    more = paste0(first_set(pear = FALSE), "; also run with ", n_more, " AB+ and ", n_more, " CD+"),
    cont = paste0(first_set(pear = FALSE), "; then ten further trials with B alone and with D ",
                  "alone, followed by the apple, from the values and associabilities left by the compound trials"),
    across = paste0(first_set(pear = FALSE), "; compared at the end of the compound trials"),
    pear = first_set())

  cmp_tries <- reactiveVal(list())      # choice, reason and yes/no answer per comparison
  cmp_picking <- reactiveVal(TRUE)      # TRUE while the form for the next comparison is shown
  cmp_msg <- reactiveVal(NULL)

  cmp_try_ui <- function(attempt) {
    ch <- attempt$choice
    div(class = "try",
      div(class = "answered",
        p(span(class = "answered-label", "Comparison"),
          span(class = "answered-choice", cmp_choices[[ch]])),
        if (nzchar(attempt$reason)) p(span(class = "answered-label", "Your reason"),
                                      span(class = "answered-choice", attempt$reason))),
      div(class = "feedback",
        cmp_result[[ch]],
        if (ch == "pear") NULL
        else if (is.null(attempt$verdict)) tagList(
          radioButtons(paste0("cmp_tells_", ch), span(class = "item-label", tells_label), inline = TRUE,
                       choiceNames = unname(tells_choices), choiceValues = names(tells_choices),
                       selected = character(0)),
          actionButton(paste0("cmp_check_", ch), "Submit", class = "btn-submit"))
        else tagList(
          p(span(class = "answered-label", tells_label),
            span(class = "answered-choice", tells_choices[[attempt$verdict]])),
          P(tells_feedback(ch, attempt$verdict)))))
  }

  output$cmp <- renderUI({
    tries <- cmp_tries()
    tried <- vapply(tries, `[[`, "", "choice")
    left <- setdiff(names(cmp_choices), tried)
    last <- if (length(tries)) tries[[length(tries)]]
    last_done <- is.null(last) || !is.null(last$verdict)
    n <- length(tries) + 1
    tagList(
      lapply(tries, cmp_try_ui),
      if (cmp_picking() && length(left)) div(class = "try",
        div(class = "item",
          radioButtons(paste0("cmp_choice_", n),
                       span(class = "item-label",
                            if (n == 1) cmp_label else "Which comparison would you try next?"),
                       width = "100%", choiceNames = unname(cmp_choices[left]),
                       choiceValues = left, selected = character(0))),
        textAreaInput(paste0("cmp_reason_", n),
                      "What do you hope to learn from this comparison? A sentence is enough.",
                      rows = 2, width = "100%"),
        actionButton(paste0("cmp_go_", n), "Run it", class = "btn-submit"))
      else if (last_done && length(left) && !("pear" %in% tried))
        p(class = "another", actionLink("cmp_another", "Choose another comparison")))
  })
  output$cmp_msg <- renderUI(if (!is.null(cmp_msg())) p(class = "msg", cmp_msg()))

  # Running comparison n. A click counts only while the form for comparison n is
  # open, so a double click runs it once. The pear goes to the notes here; the
  # other comparisons go once the reader has said whether they isolate the
  # difference.
  for (i in seq_along(cmp_choices)) local({
    n <- i
    observeEvent(input[[paste0("cmp_go_", n)]], {
      tries <- cmp_tries()
      if (!cmp_picking() || length(tries) != n - 1) return()
      ch <- input[[paste0("cmp_choice_", n)]]
      if (is.null(ch) || !nzchar(ch)) { cmp_msg("Choose a comparison first."); return() }
      reason <- input[[paste0("cmp_reason_", n)]]
      reason <- trimws(if (is.null(reason)) "" else reason)
      if (!nzchar(reason)) {
        cmp_msg("Add a sentence about what you hope to learn before running the comparison.")
        return()
      }
      cmp_msg(NULL)
      cmp_tries(c(tries, list(list(choice = ch, reason = reason, verdict = NULL))))
      cmp_picking(FALSE)
      if (ch == "pear") add_note("Step 2", "Comparison chosen", cmp_label,
        paste0(cmp_choices[[ch]], " Reason: ", reason),
        result = cmp_note[[ch]], settings = cmp_settings[[ch]])
    })
  })
  # Answering whether the "more", "cont" or "across" comparison isolates the
  # difference.
  for (choice in c("more", "cont", "across")) local({
    ch <- choice
    observeEvent(input[[paste0("cmp_check_", ch)]], {
      tries <- cmp_tries()
      i <- which(vapply(tries, `[[`, "", "choice") == ch)
      if (!length(i) || !is.null(tries[[i]]$verdict)) return()
      verdict <- input[[paste0("cmp_tells_", ch)]]
      if (is.null(verdict) || !nzchar(verdict)) { cmp_msg("Choose yes or no first."); return() }
      cmp_msg(NULL)
      tries[[i]]$verdict <- verdict
      cmp_tries(tries)
      add_note("Step 2", "Comparison tried", paste0(cmp_choices[[ch]], " ", tells_label),
        paste0(tells_choices[[verdict]], " Reason for choosing it: ", tries[[i]]$reason),
        result = paste0(tells_feedback(ch, verdict), " ", cmp_note[[ch]]),
        settings = cmp_settings[[ch]])
    })
  })
  observeEvent(input$cmp_another, { cmp_msg(NULL); cmp_picking(TRUE) })

  output$p_cont_curves <- renderPlot(plot_curves(co, xlab = "Further trials with the apple",
                                                 ylab = "Learned value for the apple"))
  output$p_cont_gap <- renderPlot(plot_gap(co, xlab = "Further trials with the apple",
                                           title = "D minus B", ylim = c(-.02, .35)))

  # Box 2: what changes when the picture changes.
  first_choices <- c(equal = "Equal.", less = "B gains less than D.", more = "B gains more than D.")
  why_choices <- c(
    start = "B and D start the pear trials with different learned values.",
    rate = "B and D start with the same learned value and the same error, but B brings a lower learning rate into the pear trials.",
    A = "Tone A is still present and uses up part of the error.",
    error = "B and D make different errors on their first pear trial.")
  q2c_items <- list(
    item("rw", paste0("Under ", RW, ", how do B's and D's gains on their first pear trial compare?"),
         first_choices),
    item("ad", "Under the adaptive model?", first_choices),
    item("why", "What determines the adaptive model's answer?", why_choices))
  q2c_feedback <- function(a) c(
    rw = paste0(switch(a$rw,
      less = paste0("You expected B to gain less under ", RW, ". "),
      more = paste0("You expected B to gain more under ", RW, ". "),
      equal = ""),
      "Under ", RW, ", B and D begin with the same value for the pear and keep equal learning
      rates of ", num(trained$rw$rate_B, 2), ". Each gains ", num(at(base$rw$B$gain, 0), 2), " on
      its first pear trial, and their values stay equal on every later trial."),
    adaptive = paste0(switch(a$ad,
      equal = "You expected equal gains under the adaptive model. Equal starting values do not reset the lights' associabilities. ",
      more = "You expected B to gain more under the adaptive model. B starts with the lower associability, so it gains less from the same error. ",
      less = ""),
      "Under the adaptive model, the first errors are also 1, but the learning rates differ,
      because associability carried over. On its first pear trial, B gains .3 &times; ",
      num(trained$adaptive$alpha_B), " &asymp; ", num(at(ad_B$gain, 0)), ", whereas D gains .3
      &times; ", num(trained$adaptive$alpha_D), " = ", num(at(ad_D$gain, 0)), ". D is then ahead
      by about ", num(at(base$adaptive$gap, 1)), ". This is still a difference in learned values,
      not yet in responses."),
    why = paste0(switch(a$why,
      start = "Both lights start the pear trials at zero, so their starting values are equal. ",
      A = "A is absent during the pear trials, and the adaptive model uses each light's own error. ",
      error = "Both lights start at zero, so their first errors are equal, 1 for each. ",
      rate = ""),
      "The answer depends on the assumption that the associability acquired with the apple
      carries over to the pear trials, while the learned values for the apple do not."))
  ask_server("q2c", input, output, answers, q2c_items,
    reason = "On which assumption about earlier learning does your answer depend? (optional)",
    button = "See the pear trials",
    feedback = function(a) tagList(lapply(q2c_feedback(a), P)),
    on_submit = function(a) add_note("Step 2", "What changes when the picture changes",
      "B and D start the pear trials at zero. How do their first gains compare under each model?",
      paste0(RW, ": ", first_choices[[a$rw]], " Adaptive model: ", first_choices[[a$ad]],
             " Reason: ", why_choices[[a$why]],
             if (nzchar(a$reason)) paste0(" You added: ", a$reason)),
      result = paste(q2c_feedback(a), collapse = " "), settings = first_set()))

  output$p2_curves <- renderPlot({ req(answers$q2c); plot_curves(base) })
  output$p2_gap <- renderPlot({ req(answers$q2c); plot_gap(base, ylim = c(-.02, .32)) })

  # One row per step of the calculation, so that the table reads top to bottom.
  output$trial_table <- renderUI({
    req(answers$q2c, input$t_model, input$t_k)
    m <- input$t_model; j <- input$t_k
    B <- base[[m]]$B; D <- base[[m]]$D
    row <- function(label, f) tags$tr(tags$td(label), tags$td(f(B)), tags$td(f(D)))
    tagList(tags$table(class = "values",
      tags$thead(tags$tr(tags$th(paste("Pear trial", j)), tags$th(class = "cue-B", "B"),
                         tags$th(class = "cue-D", "D"))),
      tags$tbody(
        row("Value before", function(d) prob(at(d$V, j - 1))),
        row("Error", function(d) prob(at(d$error, j - 1))),
        row("Associability", function(d) if (m == "rw") "—" else num(at(d$alpha, j - 1))),
        row("Learning rate", function(d) num(at(d$rate, j - 1))),
        row("Gain", function(d) num(at(d$gain, j - 1))),
        row("Value after", function(d) num(at(d$V, j))))),
      p(class = "table-note", "Gains, values and gaps are calculated from unrounded values,
        so a hand calculation can differ in the last digit."))
  })
  output$trial_note <- renderUI({
    req(answers$q2c, input$t_model, input$t_k)
    m <- input$t_model; j <- input$t_k
    gain_B <- at(base[[m]]$B$gain, j - 1); gain_D <- at(base[[m]]$D$gain, j - 1)
    p(class = "muted", paste0("Gap after pear trial ", j, ": ", prob(at(base[[m]]$gap, j)), ". ",
      if (m == "rw") "B and D gain the same."
      else if (gain_B > gain_D) "On this trial B gains more than D, so the gap narrows."
      else "On this trial D gains more than B, so the gap grows."))
  })

  # Why the adaptive model's gap narrows after its peak.
  q2d_choices <- c(
    stop = "D stops learning once its learned value is high enough, so only B keeps gaining.",
    converge = "B's associability catches up with D's, so B again learns as readily as D.",
    error = "B has more of the pear left to learn, and its larger error comes to outweigh its lower learning rate, so B starts to gain more per trial than D.",
    overtake = "B's learned value overtakes D's after the eighth pear trial.")
  q2d_items <- list(item("n", paste0("The adaptive model's gap is largest after ",
    pear_trials(peak_k), " and then narrows. Why?"), q2d_choices))
  alpha_gap <- ad_D$alpha - ad_B$alpha
  narrow_text <- paste0("On the ", ordinal(peak_k + 1), " pear trial, B gains about ",
    num(at(ad_B$gain, peak_k)), " and D about ", num(at(ad_D$gain, peak_k)), ", so the gap begins
    to narrow, although D still has the higher value. At first D gains more because its learning
    rate is higher. As B falls behind, however, it has more of the pear left to learn, and so a
    larger error. Its associability is still the lower one: D's exceeds B's by ",
    num(at(alpha_gap, peak_k)), ", up from ", num(at(alpha_gap, 0)), " at the start of the pear
    trials. B can narrow the gap without overtaking D.")
  ask_server("q2d", input, output, answers, q2d_items,
    feedback = function(a) P(switch(a$n,
      stop = "D keeps learning on every pear trial. ",
      converge = "B's associability does rise during the pear trials, but D's rises faster.
                  When a light is trained alone, a smaller error gives a larger increase in
                  associability, and D predicts the pear more accurately than B. ",
      error = "",
      overtake = "B never overtakes D under these assumptions. The gap stays positive after
                  every pear trial. What changes is how much each light gains per trial. "),
      narrow_text),
    on_submit = function(a) add_note("Step 2", "Why the gap narrows", q2d_items[[1]]$label,
      q2d_choices[[a$n]], result = narrow_text, settings = first_set(),
      open = "Does the gap remain under other choices about carryover, training and the rule for associability?"))

  # Starting the inquiry again clears the answers and the comparisons tried but
  # keeps the notes.
  list(reset = function() {
    clear_answers(answers, c("q2a", "q2b", "q2c", "q2d"))
    cmp_tries(list()); cmp_picking(TRUE); cmp_msg(NULL)
  })
}
