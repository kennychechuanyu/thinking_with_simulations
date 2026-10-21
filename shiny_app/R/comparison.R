# Step 2: where the two models would disagree (Sections 3.3 and 3.4)
# The four comparisons the reader can try, one at a time. Training on a new
# outcome, O2, leads to the prediction for O2, its learning curves, one update
# in detail, and why the gap narrows.

comparison_ui <- function() {
  scene("scene-2", "2", "Where would the models disagree?", "output.done_q1", "done_q2",
    "With equal starting values for a new outcome, the adaptive model predicts slower learning for B than for D; Rescorla–Wagner predicts no difference.",
    p("B finishes training with a lower value than D. The adaptive model also gives
       B a lower associability. How could you examine that difference in how readily
       the cues learn? Choose a comparison and say what you hope to learn from it.
       If it does not isolate that difference, you can try another."),
    div(class = "question", uiOutput("cmp"), uiOutput("cmp_msg", role = "status")))
}


comparison_server <- function(input, output, answers, add_note) {

  # The comparisons, tried one at a time. After more training ("more"),
  # continued training with O1 ("cont") or the comparison across models
  # ("across"), the reader says whether it isolates the difference in how readily
  # B and D learn. The new outcome ("new") leads to the prediction for O2, after
  # which no further comparison is offered.
  cmp_label <- "Which comparison would you run?"
  cmp_choices <- c(
    more = "Give more compound training and compare B's and D's final learned values.",
    cont = "Keep training B and D with the same outcome, each alone, and compare their learning.",
    across = "Compare the two models' learned values for B after training.",
    new = "Train B and D on a new outcome, from the same starting value, and compare how quickly each learns.")
  tells_label <- "Does this comparison isolate the difference in how readily B and D learn?"
  tells_choices <- c(yes = "Yes.", no = "No.")

  cmp_result <- list(
    more = tagList(
      p(class = "table-note", "Learned value for O1 at the end of compound training"),
      tags$table(class = "values", cue_head("Compound trials"),
        tags$tbody(
          vrow(RW, n_base, num(trained$rw$V_B), num(trained$rw$V_D)),
          vrow(RW, n_more, num(more$rw$V_B), num(more$rw$V_D)),
          vrow("Adaptive model", n_base, num(trained$adaptive$V_B), num(trained$adaptive$V_D)),
          vrow("Adaptive model", n_more, num(more$adaptive$V_B), num(more$adaptive$V_D))))),
    cont = tagList(
      p(class = "table-note", "Ten more trials with O1, each cue alone, starting from the values
                               that compound training left"),
      fig("p_cont_curves", "300px"),
      fig("p_cont_gap", "260px")),
    across = tagList(
      p(class = "table-note", "B at the end of compound training, and the multiplier on the
                               first compound trial"),
      tags$table(class = "values",
        tags$thead(tags$tr(tags$th("Model"), tags$th("B's learned value"),
                           tags$th("First multiplier"))),
        tags$tbody(
          vrow(RW, num(trained$rw$V_B), num(trained$rw$mult_first)),
          vrow("Adaptive model", num(trained$adaptive$V_B), num(trained$adaptive$mult_first))))),
    new = tagList(
      p("Give B and D ten trials each on their own, now followed by a new outcome,
         O2 (for example, a different kind of food). Start both cues at value 0 for O2.
         In the adaptive model, keep the associabilities left by O1 training."),
      design_table(third = TRUE),
      p("The B–D gap is D's value minus B's.
         A positive gap means D has learned more. What do you expect each model to predict?"),
      div(class = "question inner", uiOutput("q2")),
      conditionalPanel("output.done_q2",
        fig("p2_curves", "300px"),
        fig("p2_gap", "280px"),
        tags$details(
          tags$summary("Look inside one update"),
          p("Choose an update to check the calculation. Multiply the error by the
             multiplier to get the gain, then add it to the value before the update."),
          div(class = "controls-inline",
            radioButtons("t_model", "Model", inline = TRUE,
                         choiceNames = list("Adaptive model", HTML(RW)),
                         choiceValues = c("adaptive", "rw")),
            sliderInput("t_k", "Update number", min = 1, max = 10, value = 1, step = 1,
                        ticks = FALSE, width = "260px")),
          uiOutput("trial_table"),
          uiOutput("trial_note"),
          div(class = "question inner", uiOutput("q2b"))),
        aside("Sections 3.3 and 3.4."))))

  tells_text <- list(
    more = paste0("B remains below D after ", n_more, " compound trials in both models.
      More training has preserved blocking. The numerical predictions differ, but they
      depend on both the error rule and the chosen learning rates. You still cannot
      separate a change in how readily B learns from those other differences."),
    cont = paste0("D stays ahead in both models. By the tenth trial B has closed part of
      the gap, although in the adaptive model the gap first widens a little. The numerical
      predictions differ, but B starts with more left to learn, and the adaptive model
      also makes it learn less readily. Those influences pull in opposite directions.
      Continuing O1 training mixes the starting-value difference with the difference
      in associability."),
    across = paste0("B ends at ", num(trained$rw$V_B), " in ", RW, " and ",
      num(trained$adaptive$V_B), " in the adaptive model. The models use different
      errors and different learning rates, chosen here to illustrate their behavior.
      B shares the small error left by A in ", RW, "; it learns from its own error in
      the adaptive model. You could compare quantitative predictions with data, but
      these two values alone do not isolate a change in how readily B learns."))
  tells_feedback <- function(ch, verdict) paste0(
    if (verdict == "yes") switch(ch,
      more = "B staying below D would show that blocking persists. It would not distinguish Rescorla–Wagner from the adaptive model. ",
      cont = "Giving B and D the same extra training does not give them the same starting conditions: they already have different values for O1. ",
      across = "This comparison changes more than associability: the error rule and the learning rates also differ between the models. ")
    else "",
    tells_text[[ch]])
  cmp_note <- list(
    more = paste0(RW, ": B ", num(trained$rw$V_B), " and D ", num(trained$rw$V_D), " after ",
      n_base, " compound trials, B ", num(more$rw$V_B), " and D ", num(more$rw$V_D), " after ",
      n_more, ". Adaptive model: B ", num(trained$adaptive$V_B), " and D ",
      num(trained$adaptive$V_D), " after ", n_base, ", B ", num(more$adaptive$V_B), " and D ",
      num(more$adaptive$V_D), " after ", n_more, ". Both leave B below D."),
    cont = paste0("After ten more trials with O1: ", RW, " B ", num(at(co$rw$B$V, 10)), ", D ",
      num(at(co$rw$D$V, 10)), ", gap ", num(at(co$rw$gap, 0)), " at the start and ",
      num(at(co$rw$gap, 10)), " at the end. Adaptive model B ", num(at(co$adaptive$B$V, 10)),
      ", D ", num(at(co$adaptive$D$V, 10)), ", gap ", num(at(co$adaptive$gap, 0)),
      " at the start and ", num(at(co$adaptive$gap, 10)), " at the end."),
    across = paste0("B's learned value ", num(trained$rw$V_B), " (", RW, ") and ",
      num(trained$adaptive$V_B), " (adaptive model). Multipliers on the first compound trial ",
      num(trained$rw$mult_first), " and ", num(trained$adaptive$mult_first), "."),
    new = paste0("Multipliers at the end of compound training: ", RW, " B ",
      num(trained$rw$mult_B), " and D ", num(trained$rw$mult_D), ", adaptive model B ",
      num(trained$adaptive$mult_B), " and D ", num(trained$adaptive$mult_D), "."))
  cmp_settings <- list(
    more = paste0(first_set(phase3 = FALSE), "; also run with ", n_more, " AB+ and ",
                  n_more, " DE+ in Phase 2"),
    cont = paste0(first_set(phase3 = FALSE), "; then ten further B+ and D+ trials with O1,",
                  " starting from the values and associabilities left by Phase 2"),
    across = paste0(first_set(phase3 = FALSE), "; compared at the end of Phase 2"),
    new = first_set())

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
        if (ch == "new") NULL
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
      else if (last_done && length(left) && !("new" %in% tried))
        p(class = "another", actionLink("cmp_another", "Choose another comparison")))
  })
  output$cmp_msg <- renderUI(if (!is.null(cmp_msg())) p(class = "msg", cmp_msg()))

  # Running comparison n. A click counts only while the form for comparison n is
  # open, so a double click runs it once. The new outcome goes to the notes here;
  # the other comparisons go once the reader has said whether they isolate the
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
      if (ch == "new") add_note("Step 2", "Comparison chosen", cmp_label,
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

  output$p_cont_curves <- renderPlot(plot_curves(co, xlab = "Further trials with O1",
                                                 ylab = "Learned value for O1"))
  output$p_cont_gap <- renderPlot(plot_gap(co, xlab = "Further trials with O1",
                                           ylim = c(-.02, .35)))

  # The prediction for the new outcome, then the result.
  q2_items <- list(
    item("rw", paste0("What gap do you expect under ", RW, "?"), gap_choices),
    item("ad", "What gap do you expect under the adaptive model?", gap_choices),
    item("why", "What would explain a gap under the adaptive model?", c(
      start = "B and D start the new task with different learned values for O2.",
      error = "B and D face different prediction errors on their first O2 trial.",
      assoc = "B and D start with the same value and the same error, but B learns less from that error because its associability is lower.",
      A = "A is still present and uses up part of the prediction error.")))
  first_gains <- paste0("On the first O2 trial both errors are 1, but B gains .3 × ",
    num(at(ad_B$alpha, 0)), " × 1 = ", num(at(ad_B$gain, 0)), " and D gains .3 × ",
    num(at(ad_D$alpha, 0)), " × 1 = ", num(at(ad_D$gain, 0)), ".")
  why_text <- list(
    start = "You attributed the adaptive gap to different starting values. Both cues start at 0 for O2, so the gap starts at zero.",
    error = "You attributed the adaptive gap to different first-trial errors. Both cues start at value 0, so their first errors are equal. Later, B has the larger error, which helps it catch up.",
    assoc = "Equal starting values do give the cues equal errors at first. Their associabilities differ, so they turn that same error into different gains.",
    A = "You attributed the adaptive gap to A's prediction. A is absent during O2 training, and the adaptive model uses each cue's own prediction error.")
  q2_feedback <- function(a) c(
    rw = paste0(switch(a$rw,
      pos = paste0("You expected D to pull ahead under ", RW, ". "),
      neg = paste0("You expected B to pull ahead under ", RW, ". "),
      zero = ""),
      "Under ", RW, ", the gap is zero after every update. Both cues start at 0 for O2
      and train alone with the same fixed update multiplier, so their errors and gains
      stay identical."),
    adaptive = paste0(switch(a$ad,
      zero = "You expected equal learning under the adaptive model. Equal starting values do not reset the cues' associabilities. ",
      neg = "You expected B to pull ahead under the adaptive model. But B starts with lower associability, so it initially gains less from the same error. ",
      pos = ""),
      "D learns faster at first under the adaptive model because its associability
      starts higher than B's.
      The gap grows to ", num(at(base$adaptive$gap, peak_k)), " after ", updates(peak_k),
      " and then narrows to ", num(at(base$adaptive$gap, 10)), " after ten."),
    why = paste0(why_text[[a$why]],
      if (a$why != "assoc") " D's initial advantage comes from associability.",
      " ", first_gains))

  ask_server("q2", input, output, answers, q2_items,
    reason = "Add a thought, if you like", button = "See the learning curves",
    feedback = function(a) {
      tagList(
        lapply(q2_feedback(a), P),
        p("Both models produced blocking, but they predict different learning for O2.
           What if B and D brought some of their old learned values into this task?"))
    },
    on_submit = function(a) add_note("Step 2", "Prediction for the new outcome",
      "B and D learn a new outcome from value 0. What happens to the B–D gap?",
      paste0(RW, ": ", gap_choices[[a$rw]], " Adaptive model: ", gap_choices[[a$ad]],
             " Reason: ", q2_items[[3]]$choices[[a$why]],
             if (nzchar(a$reason)) paste0(" You added: ", a$reason)),
      result = paste(q2_feedback(a), collapse = " "),
      settings = first_set()))

  output$p2_curves <- renderPlot({ req(answers$q2); plot_curves(base) })
  output$p2_gap <- renderPlot({ req(answers$q2); plot_gap(base, ylim = c(-.02, .32)) })

  # One row per step of the calculation, so that the table reads top to bottom.
  output$trial_table <- renderUI({
    req(answers$q2)
    m <- input$t_model; j <- input$t_k
    B <- base[[m]]$B; D <- base[[m]]$D
    row <- function(label, f) tags$tr(tags$td(label), tags$td(f(B)), tags$td(f(D)))
    tagList(tags$table(class = "values",
      tags$thead(tags$tr(tags$th(paste("Update", j)), tags$th(class = "cue-B", "B"),
                         tags$th(class = "cue-D", "D"))),
      tags$tbody(
        row("Value before", function(d) prob(at(d$V, j - 1))),
        row("Error", function(d) prob(at(d$error, j - 1))),
        row("Associability", function(d) if (m == "rw") "—" else num(at(d$alpha, j - 1))),
        row("Multiplier", function(d) num(at(d$mult, j - 1))),
        row("Gain", function(d) num(at(d$gain, j - 1))),
        row("Value after", function(d) num(at(d$V, j))))),
      p(class = "table-note", "Gains, values and gaps are calculated from unrounded values,
        so a hand calculation can differ in the last digit."))
  })
  output$trial_note <- renderUI({
    req(answers$q2)
    m <- input$t_model; j <- input$t_k
    gain_B <- at(base[[m]]$B$gain, j - 1); gain_D <- at(base[[m]]$D$gain, j - 1)
    p(class = "muted", paste0("Gap after update ", j, ": ", prob(at(base[[m]]$gap, j)), ". ",
      if (m == "rw") "B and D gain the same."
      else if (gain_B > gain_D) "On this update B gains more than D, so the gap narrows."
      else "On this update D gains more than B, so the gap grows."))
  })

  q2b_items <- list(item("n", paste0("The adaptive model's gap is largest after ",
      updates(peak_k), " and then narrows. Why?"), c(
    stop = "D stops learning once its value is high enough.",
    converge = "B's associability catches up with D's, so B again learns as readily as D.",
    error = "B has more left to learn, and its larger error comes to outweigh its lower associability, so B starts to gain more per trial than D.",
    overtake = "B's learned value overtakes D's.")))
  alpha_gap <- ad_D$alpha - ad_B$alpha
  narrow_text <- paste0("On update ", peak_k + 1, ", B gains ", num(at(ad_B$gain, peak_k)),
    " and D gains ", num(at(ad_D$gain, peak_k)), ". B is still behind in value but gains
    more on this trial, because it has more left to learn. Its associability is still the
    lower one: D's exceeds B's by ", num(at(alpha_gap, peak_k)), ", up from ",
    num(at(alpha_gap, 0)), " at the start of the new task. B can catch up without overtaking D.")
  ask_server("q2b", input, output, answers, q2b_items,
    feedback = function(a) P(switch(a$n,
      stop = "D keeps learning on every trial. ",
      converge = "B's associability does rise during the new training, but D's rises faster.
                  When a cue is trained alone, a smaller prediction error gives a larger
                  increase in associability. D predicts O2 more accurately than B. ",
      error = "",
      overtake = "B never overtakes D under these assumptions; the gap stays positive
                  after every update. What changes is how much each cue gains per trial. "),
      narrow_text),
    on_submit = function(a) add_note("Step 2", "Why the gap narrows",
      "Why does the adaptive model's gap narrow after its peak?",
      q2b_items[[1]]$choices[[a$n]], result = narrow_text, settings = first_set()))

  # Starting the inquiry again clears the answers and the comparisons tried but
  # keeps the notes.
  list(reset = function() {
    clear_answers(answers, c("q2", "q2b"))
    cmp_tries(list()); cmp_picking(TRUE); cmp_msg(NULL)
  })
}
