# Explore: change the settings freely
# The controls cover the changes to the adaptive model (Section 4.1), the
# carryover to the pear trials (4.2), the amount of training and the rate eta
# (4.3), the salience of the new tone C (4.4) and the response function
# (Section 5). Every curve is computed live with engine.R.

explore_ui <- function() div(class = "part wide",
  p(class = "lead",
    "Try changing the training, what carries over to the pear trials, or the response function.
     The curves update as you go. You start with the settings used in the inquiry and the
     tutorial."),
  p(class = "muted", HTML("B is the light that appeared with the trained tone A, and D is the
     comparison light, which appeared with the new tone C. Both are trained with the apple and
     then shown alone with the pear. A positive B&ndash;D gap means that D's learned value for
     the pear is higher than B's.")),
  p(class = "muted", "The saved cases of Steps 5 and 6 stay the same, and Explore settings are
     not added to your notes."),
  div(class = "explore",
    div(class = "explore-controls",
      div(class = "control-primary",
        h3("Change the adaptive model"),
        p(class = "muted", "In the adaptive model, a cue's learning rate is its salience times its
           associability. Salience is a fixed weight, and associability changes with experience.
           The gain on a trial is the learning rate times the cue's own error."),
        radioButtons("x_change", span(class = "visually-hidden", "Change the adaptive model"), choices = c(
          "Unchanged: the adaptive model as specified" = "none",
          "Prevent: hold associability fixed during the compound trials" = "prevent",
          "Remove: reset associability to .5 before the pear trials" = "remove",
          "Supply: fixed learning rates during the pear trials" = "supply")),
        uiOutput("x_change_hint"),
        p(class = "small-screen-note", tags$a(href = "#x_change_explanation", "Read about this change")),
        aside("Section 4.1 compares these interventions with no carryover of learned value (Figure 4).")),
      tags$details(class = "control-group", open = NA, `data-mobile-collapse` = "true",
        tags$summary("Training"),
        p(class = "muted", "Set how many compound trials each pair, AB and CD, has with the apple.
           The earlier A+ and X+ trials stay at ten each. The rate of change controls how
           strongly associability responds to the difference between the errors, wherever the
           chosen model lets it change."),
        sliderInput("x_n2", "Compound trials per pair", 1, 30, 10, step = 1, ticks = FALSE),
        sliderInput("x_eta", HTML("Rate of change of associability, &eta;"), .01, .10, .05,
                    step = .01, ticks = FALSE)),
      tags$details(class = "control-group", open = NA, `data-mobile-collapse` = "true",
        tags$summary("Carryover to the pear trials"),
        p(class = "muted", "For learned value, 0 starts both lights at zero, and 1 keeps each
           light's full value for the apple. For associability, 0 resets both lights to .5, and
           1 keeps the associability reached at the end of the compound trials. Half carryover
           keeps half of the value, or half of the change in associability from .5. It does not
           halve associability itself."),
        sliderInput("x_gv", HTML(paste0("Learned value, ", gV)), 0, 1, 0, step = .05,
                    ticks = FALSE),
        sliderInput("x_ga", HTML(paste0("Associability, ", gA)), 0, 1, 1, step = .05,
                    ticks = FALSE)),
      tags$details(class = "control-group", id = "control-c",
        tags$summary("Salience of the new tone C"),
        p(class = "muted", "These settings change C's salience while keeping D's fixed. Explore
           uses the rule of Equation 3, under which a tiny difference in the errors gives a tiny
           change in associability, so the curves and rounded values may look the same. Step 3
           compares this rule with two rank-based rules."),
        radioButtons("x_c", span(class = "visually-hidden", "Salience of C relative to D"), choices = c(
          "Equal to D's" = "0",
          "One part in a million higher than D's" = "1e-6",
          "1% higher than D's" = "0.01",
          "1% lower than D's" = "-0.01"))),
      tags$details(class = "control-group",
        tags$summary("Response function"),
        p(class = "muted", "These settings affect only the plot of the difference in yes
           probability. The sigmoid rises gradually. Its midpoint gives a yes probability of .5,
           and a larger slope makes the rise steeper. The threshold rule gives a yes only above
           the threshold. Both lights use the same function."),
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
        tags$summary("Check the values and learning rates"),
        uiOutput("x_summary")),
      fig("x_curves", "300px"),
      fig("x_gap", "280px"),
      fig("x_resp", "280px"),
      h3("Pear trial by pear trial"),
      uiOutput("x_table_note"),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Values after each pear trial", uiOutput("x_table"))))
)


explore_server <- function(input, output, session) {
  x_run <- reactive({
    req(input$x_n2, input$x_eta, !is.null(input$x_gv), !is.null(input$x_ga), input$x_change)
    run_inquiry(settings_with(
      n_compound = input$x_n2, eta = input$x_eta, g_V = input$x_gv,
      g_alpha = input$x_ga, change = input$x_change,
      c_offset = if (is.null(input$x_c)) 0 else as.numeric(input$x_c)))
  })
  x_label <- reactive(if (input$x_change == "supply") "Supply" else "Adaptive model")
  x_gamma <- reactive(if (is.null(input$x_gamma)) 8 else input$x_gamma)

  output$x_change_hint <- renderUI({
    req(input$x_change)
    p(class = "muted", switch(input$x_change,
      none = "Associability can change during the compound trials and the pear trials.",
      prevent = "Associability is held fixed during the compound trials only.",
      remove = "Both associabilities restart at .5 when the pear trials begin.",
      supply = "Each light keeps its starting learning rate throughout the pear trials. Its learned value still changes."))
  })
  output$x_change_explanation <- renderUI({
    req(input$x_change)
    div(class = "model-change-explanation",
      switch(input$x_change,
        none = tagList(
          h3("The adaptive model, unchanged"),
          p("During the compound trials, each cue's associability rises when it predicts the
             apple better than the cue presented with it and falls when it predicts worse, by an
             amount that grows with the difference between their errors. The carryover sliders
             set the values and associabilities that each light brings to the pear trials, where
             associability can change again."),
          p("Use these curves as a reference when you try one of the interventions. For a light
             trained alone, its error is compared with the error of predicting zero, so its
             associability can rise as it learns.")),
        prevent = tagList(
          h3("Prevent: hold associability fixed during the compound trials"),
          p("Each cue keeps the associability it had just before the AB+ and CD+ trials. B and D
             both start those trials at .5, so B cannot acquire a lower associability than D
             there. Their values for the apple still change. Associability can change again once
             the pear trials begin."),
          p("Does B's later disadvantage depend on an associability difference that develops
             during the compound trials?"),
          p(class = "muted", "This also changes what B learns about the apple. If you let
             learned value carry over, that difference enters the pear trials. The associability
             slider makes no difference to B and D here, because both leave the compound trials
             at .5.")),
        remove = tagList(
          h3("Remove: reset associability before the pear trials"),
          p("The compound trials run as usual, then B's and D's associabilities are reset to .5
             just before their first pear trial. Their learned values for the apple stay intact,
             and the learned-value slider still sets how much of them enters the pear trials.
             Associability can change again during the pear trials."),
          p("Does B's later disadvantage depend on keeping the associability difference from
             the compound trials? Here the difference is removed after it has formed, and the
             earlier learning about the apple is kept."),
          p(class = "muted", "While Remove is chosen, the associability slider has no effect:
             both lights restart at .5. Prevent changes the learning about the apple, and Remove
             changes associability only when the pear trials begin. With carried-over learned
             value, B therefore starts the pear trials at a different value under the two
             interventions.")),
        supply = {
          r <- x_run()
          tagList(
            h3(HTML(paste0("Supply: a ", RW, " learner with the adaptive model's learning rates"))),
            p("Each light starts the pear trials with the learned value and the learning rate it
               would have in the unchanged adaptive model, under your carryover settings. The
               learning rate then stays fixed throughout the pear trials. The learner still
               updates its learned value from its error on every trial."),
            P("B's fixed learning rate is ", num(at(r$adaptive$B$rate, 0)), ", and D's is ",
              num(at(r$adaptive$D$rate, 0)), ". Each is the light's salience times its
              associability in the adaptive model at the start of the pear trials. The separate ",
              RW, " curves keep their own starting values and learning rate."),
            p("How much of the B–D gap comes from the starting values and learning rates, and how
               much depends on associability continuing to change during the pear trials?"))
        }),
      if (input$x_change != "none") p(class = "muted", "In the gap plot, the pale line labeled
        “Unchanged” shows the unchanged adaptive model with the same slider settings."))
  })

  output$x_summary <- renderUI({
    r <- x_run(); end_apple <- r$trained$adaptive
    row <- function(label, B, D) tags$tr(tags$td(label), tags$td(prob(B)), tags$td(prob(D)))
    tagList(
      p("The first two rows show the adaptive model at the end of the compound trials. The
         others show the starting states for the pear trials."),
      div(class = "scroll-x", tabindex = "0", role = "region",
          `aria-label` = "Starting states", tags$table(class = "values",
        tags$thead(tags$tr(tags$th(""), tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D"))),
        tags$tbody(
          row("Learned value for the apple", end_apple$V_B, end_apple$V_D),
          row("Associability at the end of the compound trials", end_apple$alpha_B, end_apple$alpha_D),
          row("Learned value for the pear at the start", at(r$adaptive$B$V, 0), at(r$adaptive$D$V, 0)),
          if (input$x_change != "supply") row("Associability at the start of the pear trials",
                                              r$start$alpha_B, r$start$alpha_D),
          row(if (input$x_change == "supply") "Fixed learning rate during the pear trials"
              else "Learning rate on the first pear trial",
              at(r$adaptive$B$rate, 0), at(r$adaptive$D$rate, 0))))),
      P("Under ", RW, ", B starts the pear trials at ", prob(at(r$rw$B$V, 0)), " and D at ",
        prob(at(r$rw$D$V, 0)), ", with learning rate ", num(at(r$rw$B$rate, 0), 2),
        " for both lights."))
  })
  output$x_curves <- renderPlot(plot_curves(x_run(),
    labels = c(rw = "Rescorla–Wagner", adaptive = x_label())))
  # The unchanged adaptive model, drawn for reference when a change is chosen.
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
    if (input$x_change == "supply") "The last two columns show each light's fixed learning rate.
      The learning rates stay constant while the learned values change."
    else "Follow each light's learned value for the pear and its associability after every pear
      trial."))
  # Model names head pairs of B and D columns, so the table fits a narrow screen.
  # The third pair gives associability, or under Supply the fixed learning rate.
  output$x_table <- renderUI({
    r <- x_run()
    supply <- input$x_change == "supply"
    third <- if (supply) "rate" else "alpha"
    cues <- function() list(tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D"))
    tags$table(class = "values grouped",
      tags$thead(
        tags$tr(tags$th(""), tags$th(colspan = 2, span(HTML(RW))),
                tags$th(colspan = 2, span(x_label())),
                tags$th(colspan = 2, span(if (supply) "Fixed learning rate" else "Associability"))),
        tags$tr(tags$th("After pear trial"), cues(), cues(), cues())),
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
    updateRadioButtons(session, "x_c", selected = "0")
    updateRadioButtons(session, "x_rule", selected = "sigmoid")
    updateSliderInput(session, "x_gamma", value = 8)
    updateSliderInput(session, "x_theta", value = .5)
  }
  observeEvent(input$x_reset, explore_defaults())
  list(defaults = explore_defaults)
}
