# Explore: change the settings freely
# The controls cover the changes to the adaptive model (Section 4.1), training
# and the associability-change rate (4.2), carryover to O2 (4.3), the response
# rule (4.4) and the salience of the control cue E (4.5). Every curve is computed
# live with engine.R.

explore_ui <- function() div(class = "part wide",
  p(class = "lead",
    "Try changing the training, what carries over to O2, or the response function.
     The curves update as you go. You start with the settings used in the inquiry
     and the tutorial."),
  p(class = "muted", "B is trained together with A, which already predicts the
     training outcome O1; D is trained together with the new cue E and serves as B's
     control. O2 is the new outcome. A positive B–D gap means D's learned value is
     higher than B's."),
  p(class = "muted", "The saved cases in Step 5 stay the same, and Explore settings
     are not added to your inquiry notes."),
  div(class = "explore",
    div(class = "explore-controls",
      div(class = "control-primary",
        h3("Change the adaptive model"),
        p(class = "muted", "In the adaptive model, associability controls how much a cue
           learns from its prediction error: the outcome minus the cue's value.
           The update multiplier is salience (a fixed cue weight) × associability.
           Multiply the error by this multiplier to get the value update."),
        radioButtons("x_change", span(class = "visually-hidden", "Change the adaptive model"), choices = c(
          "No change: keep the adaptive model" = "none",
          "Prevent: keep associability fixed during compound training" = "prevent",
          "Remove: reset associability before O2" = "remove",
          "Supply: use fixed update multipliers during O2" = "supply")),
        uiOutput("x_change_hint"),
        p(class = "small-screen-note", tags$a(href = "#x_change_explanation", "Read about this change")),
        aside("Section 4.1 compares these changes with no old-value carryover (Figure 5).")),
      tags$details(class = "control-group", open = NA, `data-mobile-collapse` = "true",
        tags$summary("Training"),
        p(class = "muted", "Set how many times AB and DE are each presented together,
           followed by O1. Earlier A and C training stays at ten trials each.
           The change rate controls how strongly associability responds to the
           difference between prediction errors throughout O1 and O2, wherever the
           selected model allows it to change."),
        sliderInput("x_n2", "Compound trials per pair", 1, 30, 10, step = 1, ticks = FALSE),
        sliderInput("x_eta", HTML("Associability-change rate, &eta;"), .01, .10, .05,
                    step = .01, ticks = FALSE)),
      tags$details(class = "control-group", open = NA, `data-mobile-collapse` = "true",
        tags$summary("Carryover to O2"),
        p(class = "muted", "For old value, 0 starts both cues at zero; 1 keeps each
           cue's final O1 value. For associability, 0 resets each cue to .5;
           1 keeps its final O1 associability. Half carryover keeps half the old value,
           or half the associability change from .5; it does not halve associability itself."),
        sliderInput("x_gv", HTML(paste0("Old value, ", gV)), 0, 1, 0, step = .05,
                    ticks = FALSE),
        sliderInput("x_ga", HTML(paste0("Associability, ", gA)), 0, 1, 1, step = .05,
                    ticks = FALSE)),
      tags$details(class = "control-group", id = "control-e",
        tags$summary("Control cue E"),
        p(class = "muted", "Salience is a cue's fixed weight in learning. These settings
           change E's weight slightly while keeping D's fixed."),
        p(class = "muted", "Under this rule, a tiny error difference produces a tiny
           associability change, so the curves and rounded table values may look the
           same. Step 3's control-cue exercise and the tutorial's Section 4.5 explain
           this result."),
        radioButtons("x_e", span(class = "visually-hidden", "Control cue E: salience relative to D"), choices = c(
          "Same salience as D" = "0",
          "One part in a million more salient than D" = "1e-6",
          "One part in a million less salient than D" = "-1e-6"))),
      tags$details(class = "control-group",
        tags$summary("Response function"),
        p(class = "muted", "These settings affect only the response plot. The sigmoid
           rises smoothly; its midpoint gives a yes probability of .5, and a
           larger slope makes the rise steeper. The threshold rule gives yes only
           above the threshold. Both cues use the same rule."),
        radioButtons("x_rule", span(class = "visually-hidden", "Response function"), inline = TRUE,
                     choices = c("Sigmoid" = "sigmoid", "Threshold" = "threshold")),
        conditionalPanel("input.x_rule == 'sigmoid'",
          sliderInput("x_gamma", HTML("Slope, &gamma;"), 1, 16, 8, step = .5, ticks = FALSE)),
        sliderInput("x_theta", HTML("Midpoint or threshold, &theta;"), 0, 1, .5, step = .05,
                    ticks = FALSE)),
      actionLink("x_reset", "Reset these settings")),
    div(class = "explore-output",
      uiOutput("x_change_explanation"),
      tags$details(class = "starting-states", id = "x_starting_states",
        tags$summary("Check the values and update multipliers"),
        uiOutput("x_summary")),
      fig("x_curves", "300px"),
      fig("x_gap", "280px"),
      fig("x_resp", "280px"),
      h3("Update by update"),
      uiOutput("x_table_note"),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Values after each O2 update", uiOutput("x_table"))))
)


explore_server <- function(input, output, session) {
  x_run <- reactive({
    req(input$x_n2, input$x_eta, !is.null(input$x_gv), !is.null(input$x_ga), input$x_change)
    run_inquiry(settings_with(
      n_compound = input$x_n2, eta = input$x_eta, g_V = input$x_gv,
      g_alpha = input$x_ga, change = input$x_change,
      e_offset = if (is.null(input$x_e)) 0 else as.numeric(input$x_e)))
  })
  x_label <- reactive(if (input$x_change == "supply") "Fixed-rate learner" else "Adaptive model")
  x_gamma <- reactive(if (is.null(input$x_gamma)) 8 else input$x_gamma)

  output$x_change_hint <- renderUI({
    req(input$x_change)
    p(class = "muted", switch(input$x_change,
      none = "Associability can change during O1 and O2 learning.",
      prevent = "Associability is held fixed during compound training only.",
      remove = "Both associabilities restart at .5 when O2 begins.",
      supply = "Each cue keeps its own starting update multiplier throughout O2. Its learned value still changes."))
  })
  output$x_change_explanation <- renderUI({
    req(input$x_change)
    div(class = "model-change-explanation",
      switch(input$x_change,
        none = tagList(
          h3("The unchanged adaptive model"),
          p("During compound training, each cue's associability rises when it predicts
             the outcome better than the cue presented with it and falls when it predicts
             worse, by an amount that grows with the difference between their errors. The
             carryover sliders set the values and associabilities that each cue brings to O2,
             where associability can change again."),
          p("Use these curves as a reference when you try one of the changes.
             For a cue learning alone, its error is compared with the error of
             predicting zero; associability can rise as the cue learns.")),
        prevent = tagList(
          h3("Prevent: keep associability fixed during compound training"),
          p("Keep each cue's associability at the value it had just before AB and DE
             training. B and D both start that phase at .5, so B cannot acquire a lower
             associability than D during those trials. Their values for O1 still change.
             Associability can change again once O2 training begins."),
          p("Does B's later disadvantage depend on an associability difference
             developing during compound training?"),
          p(class = "muted", "This also changes what B learns about O1. If you allow
             old-value carryover, the difference in O1 learning affects its starting
             value for O2. The associability-carryover slider makes no difference to
             B and D here, because both leave compound training at baseline.")),
        remove = tagList(
          h3("Remove: reset associability when O2 begins"),
          p("Run O1 training as usual, then reset B's and D's associabilities to .5
             just before their first O2 trial. Their learned O1 values stay intact,
             and the old-value carryover slider still sets how much of those values
             enters O2. Associability can change again during O2 learning."),
          p("Does B's later disadvantage depend on retaining the associability
             difference from O1? Here you remove that difference after it has formed
             and keep the earlier value learning."),
          p(class = "muted", "While Remove is selected, the associability-carryover
             slider has no effect: both cues restart at .5.
             Prevent changes O1 learning; Remove changes associability only when O2
             begins. With old-value carryover, B therefore starts O2 at a different value
             under the two changes.")),
        supply = {
          r <- x_run()
          tagList(
            h3("Supply: start a fixed-rate learner where the adaptive model starts"),
            p("Give each cue the O2 starting value and update multiplier it would
               have in the unchanged adaptive model, using your current carryover
               settings. Keep each multiplier fixed throughout O2. The learner still
               updates its values from prediction error on every trial."),
            P("B's fixed multiplier is ", num(at(r$adaptive$B$mult, 0)),
              "; D's is ", num(at(r$adaptive$D$mult, 0)), ". These are taken from
               the adaptive model's salience &times; associability at the start of O2.
               The separate ", RW, " curve keeps its own starting values and rate."),
            p("How much of the B–D gap comes from the starting values and multipliers,
               and how much depends on associability continuing to change during O2?"))
        }),
      if (input$x_change != "none") p(class = "muted", "In the gap plot, the pale line
        labeled “No change” shows the unchanged adaptive model with the same slider settings."))
  })

  output$x_summary <- renderUI({
    r <- x_run(); end_o1 <- r$trained$adaptive
    row <- function(label, B, D) tags$tr(tags$td(label), tags$td(prob(B)), tags$td(prob(D)))
    tagList(
      p("The O1 rows show the adaptive model at the end of training. The O2 rows
         show the starting states used for the second panel of curves."),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Starting states", tags$table(class = "values",
        tags$thead(tags$tr(tags$th(""), tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D"))),
        tags$tbody(
          row("Value at the end of O1", end_o1$V_B, end_o1$V_D),
          row("Associability at the end of O1", end_o1$alpha_B, end_o1$alpha_D),
          row("Value at the start of O2", at(r$adaptive$B$V, 0), at(r$adaptive$D$V, 0)),
          if (input$x_change != "supply") row("Associability at the start of O2", r$start$alpha_B, r$start$alpha_D),
          row(if (input$x_change == "supply") "Fixed multiplier during O2" else "Multiplier on the first O2 update",
              at(r$adaptive$B$mult, 0), at(r$adaptive$D$mult, 0))))),
      P(RW, " starts O2 at values ", prob(at(r$rw$B$V, 0)), " (B) and ",
        prob(at(r$rw$D$V, 0)), " (D), with multiplier ", num(at(r$rw$B$mult, 0)),
        " for both cues."))
  })
  # Drawn even while its details are closed, because the Explore link of Step 3
  # waits for it before scrolling to the results (app.R).
  outputOptions(output, "x_summary", suspendWhenHidden = FALSE)
  output$x_curves <- renderPlot(plot_curves(x_run(),
    labels = c(rw = "Rescorla–Wagner", adaptive = x_label())))
  # The unchanged adaptive model, drawn for reference when a change is selected.
  x_ref <- reactive(if (input$x_change == "none") NULL else
    run_inquiry(modifyList(x_run()$settings, list(change = "none"))))
  # The axis always spans at least -.02 to .25, so a small gap looks small.
  output$x_gap <- renderPlot({
    r <- x_run()
    plot_gap(r, second = x_label(), ref = x_ref(),
             ylim = range(-.02, .25, r$rw$gap, r$adaptive$gap, x_ref()$adaptive$gap))
  })
  output$x_resp <- renderPlot(plot_response_models(x_run(), input$x_rule, x_gamma(),
                                                   input$x_theta, second = x_label()))
  output$x_table_note <- renderUI(p(class = "muted",
    if (input$x_change == "supply") "The last two columns show the fixed update
      multiplier for each cue. These multipliers stay constant while the learned
      values change."
    else "Follow each cue's O2 value and associability after every update."))
  # Model names head pairs of B and D columns, so the table fits a narrow screen.
  # The third pair gives associability, or under Supply the fixed multiplier.
  output$x_table <- renderUI({
    r <- x_run()
    supply <- input$x_change == "supply"
    third <- if (supply) "mult" else "alpha"
    cues <- function() list(tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D"))
    tags$table(class = "values grouped",
      tags$thead(
        tags$tr(tags$th(""), tags$th(colspan = 2, span(HTML(RW))),
                tags$th(colspan = 2, span(x_label())),
                tags$th(colspan = 2, span(if (supply) "Fixed multiplier" else "Associability"))),
        tags$tr(tags$th("Update"), cues(), cues(), cues())),
      tags$tbody(lapply(1:10, function(k) tags$tr(tags$td(k),
        tags$td(num(at(r$rw$B$V, k))), tags$td(num(at(r$rw$D$V, k))),
        tags$td(num(at(r$adaptive$B$V, k))), tags$td(num(at(r$adaptive$D$V, k))),
        tags$td(num(at(r$adaptive$B[[third]], k))), tags$td(num(at(r$adaptive$D[[third]], k)))))))
  })

  explore_defaults <- function() {
    updateSliderInput(session, "x_n2", value = 10)
    updateSliderInput(session, "x_eta", value = .05)
    updateSliderInput(session, "x_gv", value = 0)
    updateSliderInput(session, "x_ga", value = 1)
    updateRadioButtons(session, "x_change", selected = "none")
    updateRadioButtons(session, "x_e", selected = "0")
    updateRadioButtons(session, "x_rule", selected = "sigmoid")
    updateSliderInput(session, "x_gamma", value = 8)
    updateSliderInput(session, "x_theta", value = .5)
  }
  observeEvent(input$x_reset, explore_defaults())
  list(defaults = explore_defaults)
}
