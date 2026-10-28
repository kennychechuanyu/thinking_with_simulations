# Step 7: when a different measurement is needed (Section 8)
# Two settings of the drift diffusion model with the same accuracy, the
# question of Box 7, their decision times, and a closing question about
# response times.

# The settings line of the Step 7 notes.
ddm_settings_text <- paste0("drift diffusion model, drift rate 0.30 with boundary separation 2.0 ",
  "and drift rate 0.60 with boundary separation 1.0; noise standard deviation 1, start halfway, ",
  "no deadline; parameters fixed across independent trials")

measurement_ui <- function() scene("scene-7", "7", "When a different measurement is needed",
  "output.done_f5", "done_x3",
  "Two settings of the drift diffusion model predict the same accuracy but different decision times. Recording decision times changes the observation bridge.",
  p("In the learning example, the alternative candidates could imitate the difference on pear
     trials 8–10 closely but not exactly (Step 5), so with much larger experiments we might, in
     principle, still tell the explanations apart. With a model of simple decisions, we can
     examine a case in which two parameter settings predict exactly the same probability of a
     correct response."),
  h3("Two settings with the same accuracy"),
  p("Suppose participants judge whether a patch of moving dots is drifting to the left or to
     the right, and we record only whether each response is correct. The drift diffusion model
     describes such a decision as the accumulation of noisy evidence until the evidence reaches
     one of two boundaries. The boundary it reaches gives the response, and the time taken is
     the decision time. Because the evidence fluctuates from moment to moment, either boundary
     can be reached on a given trial."),
  P("The drift rate, <i>v</i>, is how quickly the evidence moves, on average, toward the
     boundary for the correct response. The boundary separation, <i>a</i>, is the distance
     between the two boundaries. The farther apart they are, the more evidence must accumulate
     before a response. Evidence starts halfway between them."),
  P("Consider two settings. In the first, evidence accumulates slowly, with <i>v</i> = 0.30,
     and the boundaries are far apart, with <i>a</i> = 2.0. In the second, evidence accumulates
     twice as fast, with <i>v</i> = 0.60, but the boundaries are half as far apart, with
     <i>a</i> = 1.0. There is no response deadline, and the parameters stay the same across
     independent trials."),
  div(class = "scroll-x", tabindex = "0", role = "region",
      `aria-label` = "The two settings", uiOutput("ddm_table")),
  p(class = "table-note", "Accuracy is rounded to three decimal places."),
  ddm_formulas_ui(),
  p("You have recorded whether each response was correct, without recording its timing."),
  div(class = "question", uiOutput("x1")),
  conditionalPanel("output.done_x1",
    div(class = "question", uiOutput("x2"))),
  conditionalPanel("output.done_x2",
    fig("p_ddm", "300px"),
    p(class = "table-note", "Both curves are calculated from the model, not simulated. Correct
       and incorrect responses are counted together, and the time needed to perceive the
       stimulus and to execute the response is not included."),
    uiOutput("ddm_text"),
    div(class = "question", uiOutput("x3_prompt"), uiOutput("x3"))),
  conditionalPanel("output.done_x3",
    div(class = "end",
      settled("Two settings can predict exactly the same accuracy, so that no number of
               accuracy-only trials distinguishes them, and yet predict different decision
               times. Recording decision times changed the observation bridge, not the rules or
               the settings.",
              "How precisely the two parameters could be estimated from a limited experiment. As
               in Step 5, we could answer this by fitting the model to responses and response
               times simulated from known parameter values."),
      aside("Section 8, Equation 9, Figure 10 and Box 7; Supplement S1."),
      p("Your notes keep your expectations, the results you saw and your conclusions. Download a
         copy to look back over your reasoning. Reopen an earlier step to review a result, or
         start again to try different answers."),
      p(actionLink("restart", "Start the inquiry again"), span(class = "muted", " (your notes are kept)")))),
  last = TRUE
)


measurement_server <- function(input, output, answers, add_note) {
  # The two settings, with their predicted accuracy.
  output$ddm_table <- renderUI(tags$table(class = "values",
    tags$thead(tags$tr(tags$th(""), tags$th(HTML("Drift rate <i>v</i>")),
                       tags$th(HTML("Boundary separation <i>a</i>")),
                       tags$th("Predicted accuracy"))),
    tags$tbody(
      tags$tr(tags$td("First setting"), tags$td(num0(dd$v[1], 2)), tags$td(num0(dd$a[1], 1)),
              tags$td(num(dd$acc[1]))),
      tags$tr(tags$td("Second setting"), tags$td(num0(dd$v[2], 2)), tags$td(num0(dd$a[2], 1)),
              tags$td(num(dd$acc[2]))))))
  acc_text <- paste0("A correct response is more likely when evidence accumulates faster or
    when the boundaries are farther apart. Accuracy depends on <i>v</i> and <i>a</i> only
    through their product, and both settings have <i>va</i> = 0.60, so both predict the
    probability ", num(dd$acc[1]), " of a correct response. The equality is exact. Because the
    probability is the same on every trial and the trials are independent, any sequence of
    correct and incorrect responses is equally likely under the two settings. More trials would
    estimate that probability more precisely, but they could not show which combination of
    <i>v</i> and <i>a</i> produced it.")

  x1_items <- list(item("more", "Could many more correct-or-incorrect responses tell the two
    settings apart?", c(
    yes = "Yes. A larger sample would reveal the effect of the different drift rates.",
    no = "No. Their predicted accuracies are identical.")))
  ask_server("x1", input, output, answers, x1_items,
    feedback = function(a) tagList(
      if (a$more == "yes") p("Here both settings predict the same accuracy."),
      P(acc_text)),
    on_submit = function(a) add_note("Step 7", "More accuracy data",
      x1_items[[1]]$label, x1_items[[1]]$choices[[a$more]], result = acc_text,
      settings = ddm_settings_text))

  # Box 7: what would distinguish the settings?
  x2_items <- list(
    item("rec", "What else could you record on each trial that might tell the settings apart?", c(
      rt = "The time from the onset of the dots to the response.",
      conf = "How confident the participant feels.",
      side = "Which response was given, left or right.",
      none = "Nothing else: accuracy is enough.")),
    item("longer", "Which setting should give the longer decision times?", c(
      first = "The first: slower drift, boundaries farther apart.",
      second = "The second: faster drift, boundaries closer together.",
      same = "The same for both.")),
    item("part", "Which part of a model's prediction would recording decision times change?", c(
      bridge = "The observation bridge, which links model states to what is recorded.",
      rules = "The rules of the model.",
      settings = "The settings: the parameter values.")))
  rt_text <- paste0("Decisions take longer under the first setting. The boundaries are farther
    from the starting point, and evidence moves toward the correct one more slowly on average.
    The mean decision time is ", num0(dd$mean[1], 2), " in the model's time units under the first
    setting and ", num0(dd$mean[2], 2), " under the second, one quarter as long. Because the
    evidence fluctuates within each trial, decision times also vary from trial to trial. By time
    0.5, about ", round(100 * dd$by_half[1]), "% of the decisions have finished under the first
    setting and about ", round(100 * dd$by_half[2]), "% under the second.")
  ask_server("x2", input, output, answers, x2_items, button = "See the predicted decision times",
    feedback = function(a) tagList(
      if (a$longer == "second") p("Faster drift and closer boundaries both shorten the
        decisions, so the second setting gives the shorter times.")
      else if (a$longer == "same") p("Equal accuracy does not require equal decision times. The
        settings differ in both drift rate and boundary separation."),
      P(switch(a$rec,
        rt = "",
        conf = "Confidence would need an additional rule linking it to the model before its predictions could be compared. This model already predicts decision times. ",
        side = "For a given stimulus, both settings predict the same proportions of left and right responses, so recording the side would leave them indistinguishable. Their decision times differ. ",
        none = "With correctness alone the settings cannot be told apart. Their decision times differ. "),
        rt_text),
      P(if (a$part != "bridge") "The rules and the settings stay as they were. " else "",
        "To predict response times, we change only the observation bridge, which now links the
        same model states, the evidence paths, to response times as well as to correct and
        incorrect responses. Alternatively, we could keep recording accuracy alone and look for
        conditions under which the two settings predict different accuracy.")),
    on_submit = function(a) add_note("Step 7", "What would distinguish the settings",
      "What else could you record, which setting gives the longer decision times, and which part of the prediction changes?",
      paste0(x2_items[[1]]$choices[[a$rec]], " ", x2_items[[2]]$choices[[a$longer]], " ",
             x2_items[[3]]$choices[[a$part]]),
      result = paste0("Mean decision time ", num0(dd$mean[1], 2), " under the first setting and ",
                      num0(dd$mean[2], 2), " under the second. Accuracy ", num(dd$acc[1]),
                      " under both. Recording decision times changes the observation bridge."),
      settings = ddm_settings_text))

  output$p_ddm <- renderPlot({ req(answers$x2); plot_ddm_finished(dd) })
  output$ddm_text <- renderUI({
    req(answers$x2)
    tagList(
      p("A recorded response time also includes the time needed to perceive the stimulus and to
         execute the response, called nondecision time. If it is the same under both settings,
         it shifts both predicted response-time distributions by the same amount and keeps the
         difference between them."),
      P("As with the learning rule in Step 1, we can use these calculated values to check a
         simulation of the model. A program that checks the evidence only at each time step can
         miss a boundary crossing between two steps, so its simulated decision times come out
         slightly too long. They come closer to ", num0(dd$mean[1], 2), " and ",
         num0(dd$mean[2], 2), " as the step is made smaller."))
  })

  challenge <- "Now suppose nondecision time also differs between the settings. If you recorded
    longer response times under one setting, could you conclude that it had the lower drift
    rate? What would you want to know before making that claim?"
  reflection <- "A longer recorded response time need not mean slower accumulation of evidence.
    Boundaries farther apart, or a longer nondecision time, could also lengthen it. To draw a
    conclusion about the drift rate, you would need to account for those components and ask
    whether the combined predictions for responses and response times distinguish the
    explanations you are considering. The two curves in this step are predictions from two
    specified settings. A drift rate cannot be read directly from an observed response time."
  output$x3_prompt <- renderUI(tagList(p(class = "item-label", challenge),
    p(class = "muted", "Write what you would consider, then save it to add it to your notes and
       see the discussion.")))
  ask_server("x3", input, output, answers, list(), reason = "Your reasoning",
    reason_required = TRUE, button = "Save my answer",
    feedback = function(a) tagList(h3("Discussion"), p(reflection)),
    on_submit = function(a) add_note("Step 7", "Interpreting response times", challenge,
      a$reason, result = reflection,
      settings = paste0(ddm_settings_text, "; decision times exclude nondecision time; ",
                        "the question allows different nondecision times")))

  # Trying another design or case, or starting the inquiry again, clears the
  # answers but keeps the notes.
  list(reset = function() clear_answers(answers, c("x1", "x2", "x3")))
}
