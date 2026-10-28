# Step 3: what the disagreement depends on (Section 4)
# The interventions Prevent, Remove and Supply; the carryover of associability
# and of learned value, with the reader's conclusion before and after (Box 3);
# the amount of training and the rate eta; and the rule for associability.

assumptions_ui <- function() {
  scene("scene-3", "3", "What the disagreement depends on", "output.done_q2d", "done_q3e",
    "With no carried-over learned value, removing the associability difference removes the gap. Carried-over learned value gives Rescorla–Wagner a gap too, largest before the first pear trial. Under a rank-based rule, a tiny salience difference removes most of the gap, and if ties also lower associability there is no gap at all.",
    P("Under ", RW, ", ", B_gloss, " and ", D_gloss, " learned identically about the pear,
       whereas under the adaptive model D pulled ahead, because B brought a lower learning rate
       into the pear trials. This step changes the choices behind that result. In the terms of
       the opening, what carries over, the amount of training and the rate &eta; at which
       associability changes are settings. The rule for associability is one of the model's
       rules."),
    h3("Checking the explanation by changing the model"),
    p("We attributed the adaptive model's gap to B's lower associability. To check that
       explanation, we change the choice we believe produced the result and see what happens.
       In an experiment we cannot set a participant's associability, but in a simulation we
       can. The tutorial calls these three interventions Prevent, Remove and Supply. In all
       three, learned values for the apple do not carry over to the pear trials."),
    tags$ul(
      tags$li("Prevent: hold every cue's associability fixed during the compound trials, while
               learned values change as usual. B and D both keep their starting associability of
               .5. From the first pear trial on, associability changes as usual again."),
      tags$li(HTML(paste0("Remove: let associability change as usual during the compound
               trials, so B's falls to about ", num(trained$adaptive$alpha_B), ", then reset B's
               and D's associabilities to .5 before the pear trials."))),
      tags$li(HTML(paste0("Supply: give a ", RW, " learner the learning rates with which the
               adaptive model starts the pear trials, about ", num(trained$adaptive$rate_B),
               " for B and ", num(trained$adaptive$rate_D), " for D, and keep them fixed. Each
               light is trained alone with the pear, so its shared error is its own error, as in
               the adaptive model. Supply and the adaptive model differ only in whether the
               learning rates change.")))),
    div(class = "question", uiOutput("q3a")),
    conditionalPanel("output.done_q3a",
      fig("p3_runs", "300px"),
      h3("What carries over to the new outcome"),
      p("So far the lights' learned values for the apple did not carry over to the pear trials,
         whereas the associability they acquired carried over in full. Earlier training could
         affect the pear trials in two ways. It could change how much the lights learn from a
         given error, or how much they already predict when the pear trials begin. The two can
         occur together, so each has its own fraction."),
      P("First consider associability. Suppose that only a fraction ", gA, " of the change in
         associability during the compound trials carries over. With ", gA, " = 1, as in Step 2,
         B begins the pear trials with associability about ", num(trained$adaptive$alpha_B),
         " and D with ", num(trained$adaptive$alpha_D), ". With half carryover, B begins with
         about ", num(ga_half$start$alpha_B), ", and with none, both begin with .5. The fraction
         applies to the change in associability, not to associability itself. Both lights start
         the pear trials at zero and share the salience .3, so each gains .3 times its starting
         associability on its first pear trial. Half carryover therefore halves the gap after
         that trial, from about ", num(at(base$adaptive$gap, 1)), " to ",
         num(at(ga_half$adaptive$gap, 1)), ". How much associability people carry over is an
         empirical question."),
      tags$details(class = "worked-example",
        tags$summary(HTML(paste0("Try other values of ", gA))),
        sliderInput("ga", HTML(paste0("Carryover of associability, ", gA)),
                    min = 0, max = 1, value = .5, step = .05, ticks = FALSE, width = "320px"),
        uiOutput("ga_text"),
        fig("p3_ga", "280px")),
      P("Now suppose that some of what each light learned about the apple also enters its first
         prediction about the pear. A second fraction, ", gV, ", sets each light's starting value
         for the pear to ", gV, " times its learned value for the apple. With ", gV, " = 0, both
         lights start the pear trials at zero, as in Step 2. With ", gV, " = 1, each starts with
         its full value for the apple. Within each model, the same fraction applies to both
         lights."),
      carryover_formulas_ui(),
      div(class = "question", uiOutput("q3b"))),
    conditionalPanel("output.done_q3b",
      div(class = "question", uiOutput("q3c"))),
    conditionalPanel("output.done_q3c",
      P("Try a few values of ", gV, ". Both models use the same value, and the adaptive model
         keeps all of its change in associability. Look at the gap before the first pear trial
         as well as the gap during the pear trials."),
      sliderInput("gv", HTML(paste0("Carryover of learned value, ", gV)),
                  min = 0, max = 1, value = .5, step = .05, ticks = FALSE, width = "320px"),
      uiOutput("gv_text"),
      fig("p3_gap", "280px"),
      fig("p3_grid", "280px"),
      P("As more of the value for the apple carries over, the ", RW, " gap grows. The adaptive
         gap shrinks, because both lights start closer to 1 and leave less error for their
         different learning rates to act on. The two gaps after eight pear trials are equal at
         about ", gV, " = ", num(gv_cross), "."),
      P("With ", gV, " = .90 the two gaps are nearly equal after eight pear trials, about ",
        num(at(gv90$rw$gap, 8)), " under ", RW, " and ", num(at(gv90$adaptive$gap, 8)), " under
        the adaptive model, but they follow different courses. The ", RW, " gap is largest,
        about ", num(at(gv90$rw$gap, 0)), ", before the first pear trial and shrinks from then
        on, whereas the adaptive gap first grows and then shrinks. A gap produced by
        carried-over learned values is present before the first pear trial. When the lights
        start with equal learned values, a gap produced by unequal associabilities develops only
        during the pear trials."),
      div(class = "question", uiOutput("q3d_claim"), uiOutput("q3d"))),
    conditionalPanel("output.done_q3d",
      h3("How much training, and when to measure"),
      P("In the examples so far, B and D had ten compound trials, and associability changed at
         the rate &eta; = .05. Both settings determine how far B's associability falls before the
         pear trials, and &eta; also applies during the pear trials. Change them and see how the
         gap changes over fifteen pear trials."),
      div(class = "controls-inline",
        sliderInput("n3_compound", "Compound trials", min = 1, max = 20, value = 10, step = 1,
                    ticks = FALSE, width = "260px"),
        sliderInput("n3_eta", HTML("Rate of change of associability, &eta;"), min = .01,
                    max = .05, value = .05, step = .01, ticks = FALSE, width = "260px")),
      uiOutput("n3_text"),
      fig("p3_training", "280px"),
      P("The tutorial varied the number of compound trials from 1 to 20 and &eta; from .01 to
         .05, 100 combinations in all. In every combination, the gap was positive after every
         pear trial, and it was largest between the sixth and the tenth pear trial. The gap stays
         positive more generally, as long as B enters the pear trials with lower associability
         than D, the saliences are equal, the values for the pear start at zero, and the learning
         rates are below 1. D's associability then stays at least as high as B's, so B can narrow
         the gap but cannot overtake D."),
      P("The size and timing of the gap, however, changed with both settings. With ten
         compound trials, lowering &eta; from .05 to .01 reduced the largest gap from about ",
         num(max(base$adaptive$gap)), " to ", num(max(eta_low$adaptive$gap)), " and moved it
         from the ", ordinal(peak_k), " to the ", ordinal(which.max(eta_low$adaptive$gap) - 1),
         " pear trial. We can choose the amount of training in an experiment, but we do not know
         a participant's &eta; in advance, so we do not know on which pear trial the gap will be
         largest. A test only at the end of the pear trials would show whether a difference
         remains, but not when it appeared or how it developed."),
      h3("Which conclusions depend on the rule"),
      P("Under the rule used so far, Equation 3, associability changes in proportion to the
         difference between the two errors and does not change when the errors are equal. This
         matters for
         the comparison light. D and its companion, the new tone C, start with equal values and
         equal saliences, so they make equal errors, and D keeps its associability of .500.
         Equal salience is a modeling assumption, however, and a tone and a light may well
         differ in how noticeable they are. D is a fair comparison light only if it differs from
         B in nothing but its companion's earlier training."),
      P("Mackintosh's proposal can also be written as other rules. Under a rank-based rule,
         associability moves a fixed proportion of the way toward 1 whenever a cue makes the
         smaller error, and toward 0 whenever it makes the larger error. How much smaller or
         larger does not matter. The proportion is set so that B again ends the compound trials
         with associability about ", num(trained$adaptive$alpha_B), "."),
      div(class = "question", uiOutput("q3e"))),
    conditionalPanel("output.done_q3e",
      tags$details(class = "worked-example",
        tags$summary("Compare the rules yourself"),
        div(class = "controls-inline",
          radioButtons("r3_rule", "Rule for associability", choices = setNames(names(rule_names),
                       rule_names)),
          radioButtons("r3_c", "Salience of C", choices = c(
            "Equal to D's" = "0", "One part in a million higher" = "1e-6", "1% higher" = "0.01"))),
        uiOutput("r3_text"),
        fig("p3_rule", "280px")),
      settled("When no learned value for the apple carries over, the difference in
               associability is the only difference between B and D that enters the pear trials,
               so preventing or removing it removes the gap, as it must. Carried-over learned
               value gives Rescorla–Wagner a gap too, but one that is largest before the first
               pear trial. Whether D is a fair comparison light depends on the rule: among the
               three rules, only under Equation 3.",
              "These are all differences in learned values, which we cannot observe. Step 4 asks
               how they would appear in a participant's responses."),
      aside("Section 4, Equations 4 and 5, Figures 4 and 5, Table 5 and Box 3; Supplements S3 and S4.1."))
  )
}


assumptions_server <- function(input, output, answers, add_note) {

  # Prevent, Remove and Supply (Section 4.1).
  pr_choices <- c(gone = "It disappears.", smaller = "It becomes smaller but stays positive.",
                  same = "It stays about the same.")
  supply_choices <- c(same = "The same on every pear trial, because Supply starts with the same learning rates.",
                      close = "Close over the ten pear trials, but different over more pear trials.",
                      smaller = "Much smaller from the first pear trial, because Supply's learning rates cannot change.")
  q3a_items <- list(
    item("pr", "Under Prevent and under Remove, what happens to the B–D gap?", pr_choices),
    item("supply", "How does the gap under Supply compare with the adaptive model's?", supply_choices))
  gap_runs <- function(r) r$adaptive$gap
  unchanged_minus_supply <- gap_runs(runs$none) - gap_runs(runs$supply)
  # How much a light's learned value rises from the tenth to the last pear trial.
  later_rise <- function(r, light) at(r$adaptive[[light]]$V, n_long) - at(r$adaptive[[light]]$V, 10)
  pr_text <- paste0("Under both interventions, B and D start the pear trials with equal learned
    values and equal associabilities, so they learn identically, and the gap stays at zero.
    Under these starting conditions the result is guaranteed, because associability is the only
    difference between B and D that enters the pear trials. Here, Prevent and Remove mainly
    check that the program does what the specification says. They become informative when
    learned values for the apple also carry over.")
  prevent_text <- paste0("Prevent also changes what B learns about the apple. With its
    associability held at .5, B reaches ", num(runs$prevent$trained$adaptive$V_B), ", the same
    value as D, instead of ", num(trained$adaptive$V_B), ". In the adaptive model, B learns less
    about the apple than D only because its associability falls. If the learned values for the
    apple carried over in full, Prevent would still give no gap, whereas Remove would leave B
    behind D by about ", num(at(remove_carried$adaptive$gap, 0)), " before the first pear trial,
    because the two interventions alter the model at different times.")
  supply_text <- paste0("On the first pear trial, a light's own error and the error of
    predicting zero are both 1, so its associability does not change, and the second pear trial
    uses the same learning rate as the first. Supply and the adaptive model therefore give the
    same gaps after one and after two pear trials, about ", num(at(gap_runs(runs$supply), 1)),
    " after the first. From the second pear trial on, the adaptive model's associabilities rise,
    D's faster than B's during the ten pear trials of the task, while Supply's learning rates stay
    fixed. Over these ten pear trials the two gaps never differ by more than about ",
    num(max(abs(unchanged_minus_supply[1:11]))), ". Over more pear trials, B's learning rate
    keeps rising under the adaptive model, while Supply keeps it at ",
    num(trained$adaptive$rate_B), ". B still has much of the pear left to learn, so over the next
    ten pear trials its value rises by ", num(later_rise(runs$none, "B")), " under the adaptive
    model and by ", num(later_rise(runs$supply, "B")), " under Supply. D's value is already
    closer to 1 and rises by about the same amount under both, by ",
    num(later_rise(runs$none, "D")), " and by ", num(later_rise(runs$supply, "D")), ". After
    twenty pear trials the gap is therefore ",
    num(at(gap_runs(runs$supply), n_long)), " under Supply and ", num(at(gap_runs(runs$none), n_long)),
    " under the adaptive model. Because Supply assigns the learning rates directly, it cannot
    show how they arose.")
  ask_server("q3a", input, output, answers, q3a_items, button = "See the four runs",
    feedback = function(a) tagList(
      if (a$pr != "gone") p("Under Prevent and Remove, B and D enter the pear trials in the same
        state, with no difference left for the gap to come from."),
      P(pr_text),
      P(prevent_text),
      if (a$supply == "same") p("The two runs agree only at first.")
      else if (a$supply == "smaller") p("Supply starts with the same learning rates as the
        adaptive model, so its first gaps are the same."),
      P(supply_text)),
    on_submit = function(a) add_note("Step 3", "Changing the model",
      "What happens to the gap under Prevent, Remove and Supply?",
      paste0("Prevent and Remove: ", pr_choices[[a$pr]], " Supply: ", supply_choices[[a$supply]]),
      result = paste0("Prevent and Remove give no gap. Supply gives ", num(at(gap_runs(runs$supply), 1)),
        " after one pear trial, as the adaptive model does. Largest difference over ten pear trials: ",
        num(max(abs(unchanged_minus_supply[1:11]))), ". After twenty pear trials: ",
        num(at(gap_runs(runs$supply), n_long)), " against ", num(at(gap_runs(runs$none), n_long)), "."),
      settings = paste0(first_set(), "; Prevent, Remove and Supply over ", n_long, " pear trials")))
  output$p3_runs <- renderPlot({
    req(answers$q3a)
    plot_gap_runs(list(Unchanged = gap_runs(runs$none), Supply = gap_runs(runs$supply),
                       "Prevent, Remove" = gap_runs(runs$prevent)),
                  title = "The B–D gap under the adaptive model and the three interventions",
                  subtitle = "Unchanged: the adaptive model of Step 2. Shaded: pear trials beyond the ten of the task",
                  shade_after = default_settings$n_pear, ylim = c(-.02, .32))
  })

  # The carryover of associability.
  ga_run <- reactive(run_inquiry(settings_with(g_alpha = input$ga)))
  output$ga_text <- renderUI({
    req(input$ga)
    r <- ga_run(); g <- input$ga
    P("With ", gA, " = ", axis_num(g), ", B enters the pear trials with associability ",
      num(r$start$alpha_B), " and D with ", num(r$start$alpha_D), ". The gap is ",
      prob(at(r$adaptive$gap, 1)), " after the first pear trial and ", prob(at(r$adaptive$gap, 8)),
      " after eight.")
  })
  output$p3_ga <- renderPlot({
    req(input$ga)
    plot_gap(ga_run(), ylim = c(-.02, .32), ref = base, ref_label = "Full carryover",
             title = paste0("The B–D gap with associability carryover of ", axis_num(input$ga)))
  })

  # The conclusion so far.
  claim_choices <- c(
    support = "If an experiment finds D ahead of B on the pear trials, that supports the adaptive model over Rescorla–Wagner.",
    depends = "The models disagree under these assumptions. Whether a B–D difference supports the adaptive model depends on what carries over to the pear trials.",
    none = "The two explanations cannot be told apart.")
  q3b_items <- list(item("claim", "Suppose an experiment found that D learned more than B about
    the pear. What would you conclude so far?", claim_choices))
  ask_server("q3b", input, output, answers, q3b_items, button = "Save my conclusion",
    feedback = function(a) p("Your notes now keep this conclusion. See whether you would keep
      it once learned value may carry over."),
    on_submit = function(a) add_note("Step 3", "Your conclusion so far", q3b_items[[1]]$label,
      claim_choices[[a$claim]], settings = first_set()))

  # Box 3: Rescorla-Wagner with full value carryover.
  ahead_choices <- c(D = "D.", B = "B.", neither = "Neither: they start equal.")
  gains_choices <- c(B = "B.", D = "D.", equal = "They gain the same.")
  after_choices <- c(D = "D.", B = "B.", equal = "Neither: their values are equal.")
  q3c_items <- list(
    item("ahead", paste0("Under ", RW, ", B ended the compound trials with a learned value for
      the apple of about ", num(trained$rw$V_B), ", and D with about ", num(trained$rw$V_D),
      ". Suppose these values carry over in full (", gV, " = 1). Which light starts the pear
      trials ahead?"), ahead_choices),
    item("gains", "Which light gains more on its first pear trial?", gains_choices),
    item("after", "Which light has the higher learned value after the first pear trial?",
         after_choices))
  q3c_text <- paste0("D starts ahead, at about ", num(at(gv1$rw$D$V, 0)), " against ",
    num(at(gv1$rw$B$V, 0)), ". Both have the learning rate .12, so the light with more of the
    pear left to learn gains more. B gains about .12 &times; (1 &minus; ", num(at(gv1$rw$B$V, 0)),
    ") &asymp; ", num(at(gv1$rw$B$gain, 0)), " on the first pear trial, and D only about .12
    &times; (1 &minus; ", num(at(gv1$rw$D$V, 0)), ") &asymp; ", num(at(gv1$rw$D$gain, 0)), ". D
    nevertheless stays ahead, because it started higher. Each light's remaining error shrinks by
    12% on every pear trial, and so does the difference between their values. The gap is
    therefore largest before the first pear trial.")
  ask_server("q3c", input, output, answers, q3c_items,
    reason = "What makes you think so? (optional)", button = paste0("See what Rescorla–Wagner predicts"),
    feedback = function(a) tagList(
      if (a$ahead != "D") p("Carryover keeps the same fraction of each light's value for the
        apple, so it keeps their order: D, with the higher value, starts ahead."),
      if (a$gains != "B") p("B gains more, because it starts lower."),
      if (a$after != "D") p("D still has the higher value: it gains less, but it started much
        higher."),
      P(q3c_text)),
    on_submit = function(a) add_note("Step 3", "Rescorla–Wagner with full value carryover",
      "With g_V = 1, which light starts ahead, which gains more on the first pear trial, and which is ahead after it?",
      paste0("Starts ahead: ", ahead_choices[[a$ahead]], " Gains more: ", gains_choices[[a$gains]],
             " Ahead after the first pear trial: ", after_choices[[a$after]],
             if (nzchar(a$reason)) paste0(" Reason: ", a$reason)),
      result = q3c_text, settings = paste0(first_set(settings_with(g_V = 1)))))

  # The carryover of learned value.
  gv_run <- reactive(run_inquiry(settings_with(g_V = input$gv)))
  output$gv_text <- renderUI({
    req(answers$q3c, input$gv)
    r <- gv_run(); g <- input$gv
    P("With ", gV, " = ", axis_num(g), ", ",
      if (g == 0) "B and D both start the pear trials at zero, so neither model has a gap before
        the first pear trial. "
      else paste0("the gap before the first pear trial is already ", num(at(r$rw$gap, 0)),
        " under ", RW, " (B starts at ", num(at(r$rw$B$V, 0)), ", D at ", num(at(r$rw$D$V, 0)),
        ") and ", num(at(r$adaptive$gap, 0)), " under the adaptive model. "),
      "After eight pear trials the gaps are ", prob(at(r$rw$gap, 8)), " (", RW, ") and ",
      num(at(r$adaptive$gap, 8)), " (adaptive model). Above about ", gV, " = ", num(gv_cross),
      ", ", RW, " gives the larger gap after eight pear trials.")
  })
  output$p3_gap <- renderPlot({
    req(answers$q3c, input$gv)
    plot_gap(gv_run(), ylim = c(-.02, .35),
             title = paste0("The B–D gap with value carryover of ", axis_num(input$gv)))
  })
  output$p3_grid <- renderPlot({
    req(answers$q3c, input$gv)
    plot_gap_by_gV(gv_grid, input$gv, cross = gv_cross)
  })

  # Keeping or changing the earlier conclusion.
  output$q3d_claim <- renderUI({
    req(answers$q3b)
    if (!is.null(answers$q3d)) return(NULL)
    tagList(p(class = "item-label", "Earlier you chose this conclusion:"),
            tags$blockquote(HTML(claim_choices[[answers$q3b$claim]])))
  })
  change_choices <- c(
    keep = "Keep it as it is.",
    qualify = "Under the first assumptions the models predict different B–D gaps. Once learned values for the apple may carry over, both models can predict a positive gap, but they may still differ in when the gap first appears.",
    useless = "The comparison is useless, because both explanations can produce a positive gap, so no B–D result can tell them apart.")
  q3d_items <- list(item("rev", "Would you keep your earlier conclusion or change it?",
                         change_choices))
  ask_server("q3d", input, output, answers, q3d_items,
    reason = "You can also put the conclusion in your own words",
    button = "Save my conclusion",
    feedback = function(a) {
      earlier <- answers$q3b$claim
      tagList(
        p(class = "answered-label", "Your earlier conclusion"),
        tags$blockquote(HTML(claim_choices[[earlier]])),
        if (earlier == "support" && a$rev != "keep") P("With carried-over learned value, ", RW,
          " also puts D ahead of B, so a B&ndash;D difference alone no longer favors the adaptive
          model."),
        P(switch(a$rev,
          keep = switch(earlier,
            support = paste0("With carried-over learned value, ", RW, " also puts D ahead of B,
              and above about ", gV, " = ", num(gv_cross), " its gap after eight pear trials is
              the larger one. A B&ndash;D difference alone no longer favors the adaptive model."),
            depends = paste0("Your earlier conclusion already allowed for this. The calculations
              show one way it matters: carried-over learned value gives ", RW, " a gap too."),
            none = "Under the first assumptions the explanations disagree. With carried-over
              value both can give a gap, so the size of a difference alone does not tell them
              apart. A gap from carried-over values is there before the first pear trial. A gap
              from associability, with equal starting values, appears only during the pear
              trials. We can still ask when the gap first appears."),
          qualify = "Recording the first response lets you check whether a difference is already
            present before any pear trial. Whether the responses would show it depends on how
            learned values become responses, which Step 4 examines.",
          useless = paste0("You can still use the timing of the gap. With carried-over value, the ",
            RW, " gap is largest before the first pear trial and then shrinks. With no
            carried-over value, the adaptive gap starts at zero and grows over the first pear
            trials. To examine that difference, you need responses from the first pear trial and
            a response measure that can express it."))))
    },
    on_submit = function(a) {
      g <- input$gv; r <- gv_run()
      add_note("Step 3", "Keeping or changing your conclusion", "Would you keep or change your conclusion?",
        paste0("Before: ", claim_choices[[answers$q3b$claim]], " After: ", change_choices[[a$rev]],
               if (nzchar(a$reason)) paste0(" Your own version: ", a$reason)),
        result = paste0("With g_V = 0: ", RW, " gap ", prob(at(base$rw$gap, 8)),
          " and adaptive gap ", num(at(base$adaptive$gap, 8)), " after eight pear trials. With g_V = ",
          axis_num(g), ", the value you were looking at: ", RW, " gap ", prob(at(r$rw$gap, 0)),
          " before the first pear trial and ", prob(at(r$rw$gap, 8)),
          " after eight, adaptive gap ", num(at(r$adaptive$gap, 8)), " after eight."),
        settings = paste0(first_set(settings_with(g_V = g)), "; compared with g_V = 0"),
        open = "Real learners may carry over learned value, associability, both, or neither.")
    })

  # The amount of training and the rate eta (Section 4.3).
  training_run <- reactive({
    req(input$n3_compound, input$n3_eta)
    run_inquiry(settings_with(n_compound = input$n3_compound, eta = input$n3_eta), n = 15)
  })
  output$n3_text <- renderUI({
    r <- training_run(); g <- r$adaptive$gap
    P("With ", input$n3_compound, " compound ", if (input$n3_compound == 1) "trial" else "trials",
      " and &eta; = ", axis_num(input$n3_eta), ", B enters the pear trials with associability ",
      num(r$start$alpha_B), " and D with ", num(r$start$alpha_D), ". The gap is largest after ",
      pear_trials(which.max(g) - 1), ", at about ", num(max(g)), ".")
  })
  output$p3_training <- renderPlot({
    r <- training_run()
    plot_gap_runs(list("Your setting" = r$adaptive$gap, "10 compound trials, η = .05" = base15$adaptive$gap),
                  title = "The adaptive model's gap over fifteen pear trials",
                  ylim = c(-.02, .32), mark_peak = TRUE)
  })

  # The rule for associability (Section 4.4).
  same_choices <- c(same = "About the same.", smaller = "Much smaller.", larger = "Much larger.")
  q3e_items <- list(
    item("prop", "Under the rule used so far, Equation 3, if C is one part in a million more
      salient than D, what happens to the largest B–D gap?", same_choices),
    item("rank", "And under the rank-based rule?", same_choices))
  rule_row <- function(rule) {
    r <- rule_runs[[rule]]
    largest <- function(g) if (max(g) < 1e-12) "no gap" else
      paste0(num(max(g)), " (", which.max(g) - 1, ")")
    vrow(rule_names[[rule]], num(r$matched$alpha_D), num(r$mismatched$alpha_D),
         largest(r$matched$gap), largest(r$mismatched$gap))
  }
  rules_table <- function() tagList(
    p(class = "table-note", "D's associability after the compound trials, and the largest B–D
      gap during fifteen pear trials. The number in parentheses is the pear trial after which
      the gap is largest."),
    div(class = "scroll-x", tabindex = "0", role = "region", `aria-label` = "Three rules",
      tags$table(class = "values",
        tags$thead(
          tags$tr(tags$th("Rule for associability"), tags$th("D's associability, matched"),
                  tags$th("D's associability, C more salient"), tags$th("Largest gap, matched"),
                  tags$th("Largest gap, C more salient"))),
        tags$tbody(rule_row("proportional"), rule_row("rank"), rule_row("rank_ties")))),
    p(class = "table-note", "Matched: C and D have equal saliences. C more salient: by one part in
      a million. Under every rule, B ends the compound trials with associability about .235."))
  rank_r <- rule_runs$rank
  rule_text <- paste0("Under Equation 3, a tiny salience difference makes D's error differ from
    C's by a tiny amount, so D's associability changes by a similarly tiny amount. Even if C is 1%
    more salient than D, D's associability ends the compound trials at about ",
    num(one_percent$alpha_D), " instead of .500, and the gap after eight pear trials changes by
    about ", num(abs(at(one_percent$gap, 8) - at(base$adaptive$gap, 8))), ".")
  rank_text <- paste0("Under the rank-based rule, with C and D matched exactly, the gap is largest
    after ", pear_trials(which.max(rank_r$matched$gap) - 1), ", at about ",
    num(max(rank_r$matched$gap)), ". This differs from Equation 3 although the lights start the
    pear trials with the same associabilities. Under the rank-based rule, a light's associability
    rises by the same fraction of its remaining distance to 1 on every pear trial after the first,
    so the difference between D's and B's associabilities shrinks. Under Equation 3, a light's
    associability rises by &eta; times its learned value, so during the ten pear trials the
    difference grows. If C is more salient than D, whether by one part in a million or by 1%, D
    makes the larger error on every compound trial after the first, so its associability falls
    on each of them, to about ", num(rank_r$mismatched$alpha_D), ". The
    largest gap is then only about ", num(max(rank_r$mismatched$gap)), ". If the rule also lowered
    associability when the errors are equal, as Mackintosh proposed, D would lose as much
    associability during the CD+ trials as B does during the AB+ trials, and there would be no gap
    at all. Among these rules, D is a fair comparison light only under Equation 3.")
  ask_server("q3e", input, output, answers, q3e_items, button = "See the three rules",
    feedback = function(a) tagList(
      if (a$prop != "same") p("Under Equation 3, the size of the change in
        associability follows the size of the difference between the errors."),
      if (a$rank != "smaller") p("Under the rank-based rule, any difference between the errors,
        however small, changes associability by the full step, so D's associability falls on
        almost every compound trial and the gap shrinks."),
      rules_table(),
      P(rule_text),
      P(rank_text)),
    on_submit = function(a) add_note("Step 3", "The rule for associability",
      "If C is one part in a million more salient than D, what happens to the largest gap under each rule?",
      paste0("Equation 3: ", same_choices[[a$prop]], " Rank-based rule: ",
             same_choices[[a$rank]]),
      result = paste(rule_text, rank_text), settings = paste0(first_set(), "; fifteen pear trials; rank-based step ",
                                            num(rank_step(), 4))))

  # Comparing the rules freely, after the question.
  rule_run <- reactive({
    req(input$r3_rule, input$r3_c)
    rule_comparison(input$r3_rule, as.numeric(input$r3_c))
  })
  output$r3_text <- renderUI({
    req(answers$q3e)
    r <- rule_run(); g <- r$gap
    P("D's associability after the compound trials is ", num(r$alpha_D), ", and B's ",
      num(r$alpha_B), ". ", if (max(g) < 1e-12) "There is no gap." else paste0("The gap is
      largest after ", pear_trials(which.max(g) - 1), ", at about ", num(max(g)), "."))
  })
  output$p3_rule <- renderPlot({
    req(answers$q3e)
    r <- rule_run()
    plot_gap_runs(list("Chosen rule" = r$gap,
                       "Equation 3, matched" = rule_runs$proportional$matched$gap),
                  title = "The B–D gap over fifteen pear trials", ylim = c(-.02, .32))
  })

  # Starting the inquiry again clears the answers but keeps the notes.
  list(reset = function() clear_answers(answers, c("q3a", "q3b", "q3c", "q3d", "q3e")))
}
