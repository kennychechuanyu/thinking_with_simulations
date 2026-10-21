# Step 3: what the prediction depends on (Sections 4.3 and 4.5)
# The reader's conclusion so far, then the three assumptions behind the curves
# of Step 2. Old-value carryover comes first, after which the reader may change
# the conclusion; associability carryover and the exactly matched control are
# further explorations.

assumptions_ui <- function() {
  scene("scene-3", "3", "What does the prediction depend on?", "output.done_q2", "done_q3c",
    "Old-value carryover gives Rescorla–Wagner a B–D gap too. When the gap appears may help distinguish the explanations.",
    P("The two models gave different learning curves for O2. Those curves used
       three starting assumptions:"),
    tags$ul(
      tags$li(HTML(paste0("Both cues start with no learned value for O2 (no old-value
        carryover, <span class='nowrap'>", gV, " = 0</span>). Their values for O1 do not set their starting values for O2."))),
      tags$li(HTML(paste0("In the adaptive model, each cue keeps the associability it
        reached during O1 training (full associability carryover, <span class='nowrap'>", gA, " = 1</span>)."))),
      tags$li(HTML(paste0("The control cues D and E are exactly equally salient, so
        that D's associability stays at ", num(trained$adaptive$alpha_D), ".")))),
    p("The adaptive prediction also depends on this model's particular rule for updating
       associability. After revising your conclusion, you can test a slight mismatch
       between D and E in “Explore further: control-cue salience” below."),
    carryover_formulas_ui(),
    p("If D learned more than B about O2, what would you conclude? Save your answer,
       then try allowing some old value to carry over. These settings show what
       follows from an assumption; data would still be needed to judge what real
       learners carry over."),
    div(class = "question", uiOutput("q3a")),
    conditionalPanel("output.done_q3a",
      # Old-value carryover
      tagList(
        h3("Allow some old value to carry over"),
        P("Suppose part of what the learner expects from O1 also affects its
           expectation of O2. Set each cue's O2 starting value to a fraction ", gV,
           " of its final O1 value: .5 keeps half, and 1 keeps all. At the end of
           compound training, ", RW, " left B at ", num(trained$rw$V_B),
           " and D at ", num(trained$rw$V_D), "."),
        div(class = "question", uiOutput("q3b")),
        conditionalPanel("output.done_q3b",
          P("Try a few values of ", gV, ". This changes the starting values in both
             models; associability still carries over in full. Look at the gap before
             any O2 feedback, as well as the gap during learning."),
          sliderInput("gv", HTML(paste0("Old-value carryover, ", gV)),
                      min = 0, max = 1, value = .5, step = .05, ticks = FALSE, width = "320px"),
          uiOutput("gv_text"),
          fig("p3_gap", "280px"),
          fig("p3_grid", "280px"),
          p(class = "muted", "The open circle is the adaptive model at the Step 2 setting, with no
             old-value carryover. As more old value carries over, the adaptive model's gap after
             eight updates becomes smaller, because both cues begin with higher predictions
             about O2 and leave less error for their different associabilities to act on."),
          aside("Section 4.3."),
          div(class = "question", uiOutput("q3c_claim"), uiOutput("q3c")))),

      # Associability carryover
      conditionalPanel("output.done_q3c",
        tags$details(class = "further", tags$summary("Explore further: associability carryover"),
          h3("Only part of the associability change carries over"),
          P("Now vary how much of the associability change carries over. With ", gA,
             " = 1, B enters O2 at ", num(trained$adaptive$alpha_B), " and D at ",
             num(trained$adaptive$alpha_D), ". With ", gA, " = 0, both return to the
             original baseline of .5. A setting of .5 keeps half of the change from
             baseline, rather than half of the associability itself. Here, old-value
             carryover is 0, so both cues start O2 at value 0."),
          div(class = "question", uiOutput("q3d")),
          conditionalPanel("output.done_q3d",
            sliderInput("ga", HTML(paste0("Associability carryover, ", gA)),
                        min = 0, max = 1, value = .5, step = .05, ticks = FALSE, width = "320px"),
            uiOutput("ga_text"),
            fig("p3_ga", "280px"),
            aside("Section 4.3."))),

        # The exactly matched control
        tags$details(id = "control-cue-salience", class = "further",
          tags$summary("Explore further: control-cue salience"),
          h3("How closely must the control cues match?"),
          P("D's associability stayed at ", num(trained$adaptive$alpha_D), " because D and E
             made exactly the same error on every compound trial."),
          div(class = "question", uiOutput("q6"))))))
}


assumptions_server <- function(input, output, answers, add_note) {

  # The conclusion so far
  claim_choices <- c(
    support = "If an experiment finds D ahead of B on the new outcome, that supports the adaptive model over Rescorla–Wagner.",
    depends = "The models disagree under these assumptions. Whether a B–D difference supports the adaptive model depends on what carries over to the new task.",
    none = "The two explanations cannot be told apart.")
  q3a_items <- list(item("claim", "What would you conclude so far?", claim_choices))
  ask_server("q3a", input, output, answers, q3a_items, button = "Save my conclusion",
    feedback = function(a) p("Your notes now keep this conclusion. See whether you would
      keep it after allowing some old value to carry over."),
    on_submit = function(a) add_note("Step 3", "Initial conclusion",
      "Which conclusion would you draw after Step 2?", claim_choices[[a$claim]],
      settings = first_set()))

  # Old-value carryover
  when_choices <- c(before = "Already before the first O2 trial.",
                    after = "Only after some O2 trials.",
                    none = "There is no gap.")
  q3b_items <- list(
    item("dir", paste0("If some old value carries over, what will ", RW,
                       " predict for the B–D gap?"), gap_choices),
    item("when", "If there is a gap, when does it first appear?", when_choices))
  ask_server("q3b", input, output, answers, q3b_items,
    reason = "What makes you think so? (optional)", button = "See what changes",
    feedback = function(a) tagList(
      if (a$dir == "zero")
        p("Equal starting values gave no gap in Step 2. Here, the same positive
          fraction of two unequal O1 values is carried over, so their O2 starting
          values are unequal too.")
      else if (a$dir == "neg")
        p("B finished O1 training below D. Keeping the same fraction of each old
          value preserves that order, so it cannot put B ahead at the start of O2."),
      P(RW, " now puts D ahead of B. B ended compound training at ", num(trained$rw$V_B),
        " and D at ", num(trained$rw$V_D), ", so with any carryover D starts the new
        task higher."),
      if (a$when != "before")
        p("Carryover sets the values before learning the new outcome. The first
          response can therefore show a gap before any O2 feedback has occurred."),
      P("The gap is there before any O2 feedback. After that it shrinks: both cues use the
        same multiplier, and B, which is farther from 1, gains more on each trial.")),
    on_submit = function(a) add_note("Step 3", "Old-value carryover",
      paste0("With old-value carryover, what does ", RW, " predict?"),
      paste0(gap_choices[[a$dir]], " ", when_choices[[a$when]],
             if (nzchar(a$reason)) paste0(" Reason: ", a$reason)),
      result = paste0("D ahead from the start. At ", gV, " = 1 the gap after eight updates is ",
        num(at(gv1$rw$gap, 8)), " (", RW, ") and ", num(at(gv1$adaptive$gap, 8)),
        " (adaptive model). The two are equal near ", gV, " = ", num(gv_cross), "."),
      settings = paste0(first_set(), "; vary g_V from 0 to 1")))

  gv_run <- reactive(run_inquiry(settings_with(g_V = input$gv)))
  output$gv_text <- renderUI({
    req(answers$q3b)
    r <- gv_run(); g <- input$gv
    P("With ", gV, " = ", axis_num(g), ", ",
      if (g == 0) "B and D both start at 0 for O2, so neither model has a gap before any
        O2 feedback. "
      else paste0("the gap before any O2 feedback is already ", num(at(r$rw$gap, 0)),
        " under ", RW, " (B starts at ", num(at(r$rw$B$V, 0)), ", D at ",
        num(at(r$rw$D$V, 0)), ") and ", num(at(r$adaptive$gap, 0)),
        " under the adaptive model. "),
      "After eight updates the gaps are ", prob(at(r$rw$gap, 8)), " (", RW, ") and ",
      num(at(r$adaptive$gap, 8)), " (adaptive model). Above about ", gV, " = ",
      num(gv_cross), ", ", RW, " predicts the larger gap after eight updates.",
      if (g > 0) " Gaps are calculated from unrounded values.")
  })
  output$p3_gap <- renderPlot({
    req(answers$q3b)
    plot_gap(gv_run(), ylim = c(-.02, .35),
             title = paste0("The B–D gap with old-value carryover of ", axis_num(input$gv)))
  })
  output$p3_grid <- renderPlot({
    req(answers$q3b)
    plot_gap_by_gV(gv_grid, input$gv, cross = gv_cross)
  })

  output$q3c_claim <- renderUI({
    req(answers$q3a)
    if (!is.null(answers$q3c)) return(NULL)
    tagList(p(class = "item-label", "Earlier you chose this conclusion:"),
            tags$blockquote(HTML(claim_choices[[answers$q3a$claim]])))
  })
  revise_choices <- c(
    keep = "Keep it as it is.",
    qualify = "Under the first assumptions the explanations predict different B–D gaps. Once old learned values may carry over, a positive gap alone no longer tells them apart. When the gap first appears may still help.",
    useless = "The comparison is useless, because both explanations can produce the gap.")
  q3c_items <- list(item("rev", "Would you keep your earlier conclusion or change it?",
                         revise_choices))
  ask_server("q3c", input, output, answers, q3c_items,
    reason = "You can also put the conclusion in your own words",
    button = "Save my conclusion",
    feedback = function(a) {
      earlier <- answers$q3a$claim
      tagList(
        p(class = "answered-label", "Your earlier conclusion"),
        tags$blockquote(HTML(claim_choices[[earlier]])),
        P(switch(a$rev,
          keep = switch(earlier,
            support = paste0("With old-value carryover, ", RW, " also puts D ahead of B,
              and above about ", gV, " = ", num(gv_cross), " its gap is the larger one
              after eight updates. A B–D difference alone no longer favors the adaptive
              model."),
            depends = paste0("Your earlier conclusion already allowed for this. The
              simulation shows one way it matters: old-value carryover gives ", RW,
              " a gap too."),
            none = "Under the first assumptions the explanations disagree. With old-value
              carryover both start with a gap, so the size of a difference alone does not
              tell them apart. A gap from old values is there before any O2 feedback. A gap
              from associability, with equal starting values, appears only after O2
              feedback. So when the gap first appears may still help."),
          qualify = "A gap from old values is present before O2 feedback; a gap from
            associability develops during O2 learning. Recording the first response
            lets you check for this distinction. Whether the responses show it also
            depends on how learned values become answers.",
          useless = paste0("You can still use the timing of the gap. With old-value carryover, ", RW,
            "'s gap is largest before any O2 feedback and then shrinks. With no old-value
            carryover, the adaptive model's gap starts at zero and grows over the first
            updates. To examine that distinction, you need records from the first
            presentation and a response measure that can express it."))))
    },
    on_submit = function(a) {
      g <- input$gv; r <- gv_run()
      add_note("Step 3", "Revised conclusion", "Would you keep or change your conclusion?",
        paste0("Before: ", claim_choices[[answers$q3a$claim]], " After: ",
               revise_choices[[a$rev]],
               if (nzchar(a$reason)) paste0(" Your own version: ", a$reason)),
        result = paste0("With ", gV, " = 0: ", RW, " gap ", prob(at(base$rw$gap, 8)),
          " and adaptive gap ", num(at(base$adaptive$gap, 8)), " after eight updates. With ",
          gV, " = ", axis_num(g), ", the value you were looking at: ", RW, " gap ",
          prob(at(r$rw$gap, 0)), " before any O2 feedback and ", prob(at(r$rw$gap, 8)),
          " after eight updates, adaptive gap ", num(at(r$adaptive$gap, 8)),
          " after eight updates."),
        settings = paste0(first_set(settings_with(g_V = g)), "; compared with g_V = 0"),
        open = "Real learners may carry over old values, associability, both, or neither.")
    })

  # Associability carryover
  q3d_items <- list(item("h", paste0("If only half of the associability change is kept (",
    gA, " = .5), what happens to the adaptive model's gap on the first update?"), c(
    half = "It is half as large.",
    gone = "It disappears.",
    same = "It is unchanged.")))
  ask_server("q3d", input, output, answers, q3d_items, button = "See what changes",
    feedback = function(a) tagList(
      if (a$h == "gone")
        p("Keeping half of the change moves B's associability halfway back to
          baseline; it does not reset it completely. D is already at baseline,
          so the two still begin O2 with different associabilities.")
      else if (a$h == "same")
        p("The first O2 update uses the associability carried into the task. Moving
          B halfway back to baseline reduces its difference from D, so the first
          gap changes even though both cues still start at value 0."),
      P("The first-update gap halves: from ", num(at(base$adaptive$gap, 1)), " to ",
        num(at(ga_half$adaptive$gap, 1)), ". B now enters the new task at ",
        num(ga_half$start$alpha_B), " instead of ", num(trained$adaptive$alpha_B), ", and the first
        gap is the fixed salience, .3, times the difference in associability. With no carryover at
        all, B and D enter at the same associability and the gap is zero throughout. The
        learning rule does not determine how much carries over to O2. That fraction
        has to be specified separately.")),
    on_submit = function(a) add_note("Step 3", "Associability carryover",
      q3d_items[[1]]$label, q3d_items[[1]]$choices[[a$h]],
      result = paste0("First gap ", num(at(base$adaptive$gap, 1)), " with full carryover, ",
        num(at(ga_half$adaptive$gap, 1)), " with half, 0 with none."),
      settings = paste0(first_set(), "; compare g_alpha = 1, .5 and 0")))
  ga_run <- reactive(run_inquiry(settings_with(g_alpha = input$ga)))
  output$ga_text <- renderUI({
    req(answers$q3d)
    r <- ga_run(); g <- input$ga
    P("With ", gA, " = ", axis_num(g), ", B enters the new task at ", num(r$start$alpha_B),
      " and D at ", num(r$start$alpha_D), ". The gap is ", prob(at(r$adaptive$gap, 1)),
      " after the first update and ", prob(at(r$adaptive$gap, 8)), " after eight.")
  })
  output$p3_ga <- renderPlot({
    req(answers$q3d)
    plot_gap(ga_run(), ylim = c(-.02, .32), ref = base, ref_label = "Full carryover",
             title = paste0("B–D gap, associability carryover of ", axis_num(input$ga)))
  })

  # The exactly matched control
  gap8 <- function(r) at(r$adaptive$gap, 8)
  # How the gap after eight updates compares with the exactly matched control's:
  # below half counts as much smaller, above double as much larger.
  e_change <- function(r) {
    ratio <- gap8(r) / gap8(base)
    if (ratio < .5) "smaller" else if (ratio > 2) "larger" else "same"
  }
  e_choices <- c(same = "About the same.", smaller = "Much smaller.", larger = "Much larger.")
  q6_items <- list(item("c", "If E is one part in a million more salient than D, what happens
    to the B–D gap after eight updates?", e_choices))
  e_row <- function(label, r) tags$tr(tags$td(label),
    tags$td(num(r$trained$adaptive$alpha_D)), tags$td(num(gap8(r))))
  ask_server("q6", input, output, answers, q6_items,
    feedback = function(a) {
      tagList(
        if (a$c != e_change(e_up))
          p("A tiny salience difference makes D's error differ from E's by a tiny
            amount. This rule changes associability in proportion to the difference
            between the errors, so D's associability changes by a similarly tiny
            amount."),
        P("The gap stays about the same. With E one part in a million more salient,
          D's associability after compound training and the gap after eight updates
          each change by less than one millionth."),
        p(class = "table-note", "Adaptive model, no old-value carryover"),
        tags$table(class = "values",
          tags$thead(tags$tr(tags$th("Salience of E"), tags$th("D's associability"),
                             tags$th("Gap after eight updates"))),
          tags$tbody(e_row("Same as D", base),
                     e_row("One part in a million higher", e_up),
                     e_row("One part in a million lower", e_down))),
        p("A rule that uses only which cue predicts better could give a large effect
           even for a tiny mismatch. Here, the size of the error difference controls
           the size of the associability change. The matching assumption therefore
           needs to be read together with the update rule."),
        p(actionLink("q6_explore", "Open Explore with E one part in a million more salient")),
        aside("Section 4.5 and Supplement S3."))
    },
    on_submit = function(a) add_note("Step 3", "The matched control",
      q6_items[[1]]$label, e_choices[[a$c]],
      result = paste0("D's associability after compound training ",
        num(base$trained$adaptive$alpha_D), " with equal salience, ",
        num(e_up$trained$adaptive$alpha_D), " with E more salient, ",
        num(e_down$trained$adaptive$alpha_D), " with E less salient. Gap after eight updates ",
        num(gap8(base)), ", ", num(gap8(e_up)), " and ", num(gap8(e_down)), "."),
      settings = paste0(first_set(), "; compare E salience offsets +1e-6 and -1e-6")))

  # Starting the inquiry again clears the answers but keeps the notes.
  list(reset = function() clear_answers(answers, c("q3a", "q3b", "q3c", "q3d", "q6")))
}
