# Step 6: a different model, the same question (Section 6)

# The settings line of the Step 6 notes.
ddm_settings_text <- paste0("diffusion model, drift 0.30 with separation 2.0 and drift 0.60 with ",
                            "separation 1.0; noise 1, start midway, no deadline; fixed parameters across independent trials")

decision_ui <- function() conditionalPanel("output.done_e6", id = "scene-6",
  step_head("6", "A different model, the same question"),
  p("Now try the same reasoning within a decision model. Participants judge whether
     a patch of moving dots drifts left or right. In a diffusion model, noisy evidence
     accumulates until it reaches one of two boundaries. The boundary reached
     determines the answer; the time taken to reach it is the decision time."),
  P("You are considering two settings of this model. Drift rate <i>v</i> describes
     how quickly evidence moves, on average, toward the correct answer. Boundary
     separation <i>a</i> is the distance between the two boundaries. Evidence starts
     halfway between them, so wider boundaries require more evidence before an
     answer. Both settings use the same noise level and have no deadline.
     The parameters stay fixed across independent trials."),
  div(class = "scroll-x", tabindex = "0", role = "region",
      `aria-label` = "The two settings", uiOutput("ddm_table")),
  ddm_formulas_ui(),
  p("You have recorded whether each answer was correct, without recording its timing."),
  div(class = "question", uiOutput("x1")),
  conditionalPanel("output.done_x1",
    div(class = "question", uiOutput("x2"))),
  conditionalPanel("output.done_x2",
    fig("p_ddm", "300px"),
    uiOutput("ddm_text"),
    aside("Section 6."),
    div(class = "question", uiOutput("x3_prompt"), uiOutput("x3"))),
  conditionalPanel("output.done_x3",
    div(class = "end",
      p("Your notes keep your predictions, the results you saw and your revised
         conclusions. Download a copy to look back over your reasoning. Reopen an
         earlier step to review a result, or start again to try different answers."),
      p(actionLink("restart", "Start the inquiry again"), span(class = "muted", " (your notes are kept)"))))
)


decision_server <- function(input, output, answers, add_note) {
  # The two settings, with their predicted accuracy and mean decision time.
  dd <- ddm_summary()
  output$ddm_table <- renderUI(tags$table(class = "values",
    tags$thead(tags$tr(tags$th(""), tags$th(HTML("Drift rate <i>v</i>")),
                       tags$th(HTML("Boundary separation <i>a</i>")),
                       tags$th("Predicted accuracy"))),
    tags$tbody(
      tags$tr(tags$td("First setting"), tags$td(num0(dd$v[1], 2)), tags$td(num0(dd$a[1], 1)),
              tags$td(num(dd$acc[1]))),
      tags$tr(tags$td("Second setting"), tags$td(num0(dd$v[2], 2)), tags$td(num0(dd$a[2], 1)),
              tags$td(num(dd$acc[2]))))))
  acc_text <- paste0("Accuracy depends on the product of drift rate and boundary
    separation in this example. That product is 0.60 in both settings, so both predict accuracy ", num(dd$acc[1]),
    ". Any sequence of correct and incorrect answers is equally likely under the two
    settings. More trials would estimate accuracy more precisely but would not show
    which setting produced it.")

  x1_items <- list(item("more", "Could many more correct-or-incorrect answers tell the two
    settings apart?", c(
    yes = "Yes. A larger sample could reveal the effect of the different drift rates.",
    no = "No. Their predicted accuracies are identical.")))
  ask_server("x1", input, output, answers, x1_items,
    feedback = function(a) tagList(
      if (a$more == "yes") p("You expected more trials to reveal the different drift
        rates. They cannot, for this reason:"),
      P(acc_text)),
    on_submit = function(a) add_note("Step 6", "More accuracy data",
      x1_items[[1]]$label, x1_items[[1]]$choices[[a$more]], result = acc_text,
      settings = ddm_settings_text))

  x2_items <- list(
    item("rec", "What else would you record on each trial?", c(
      rt = "The time from stimulus onset to the participant's response.",
      conf = "How confident the participant feels.",
      side = "Which answer was given, left or right.",
      none = "Nothing else; accuracy is enough.")),
    item("longer", "Which setting should give the longer decision times?", c(
      first = "The first (slower drift, wider boundaries).",
      second = "The second (faster drift, narrower boundaries).",
      same = "The same for both.")))
  rt_text <- paste0("Decisions take longer in the first setting. The evidence starts
    farther from either boundary and moves toward the correct one more slowly. Mean
    decision time is ", num0(dd$mean[1], 2), ", compared with ", num0(dd$mean[2], 2),
    " in the second setting, in the model's time units. Noise makes the distributions
    overlap, but they still differ. Your recorded response times would also include
    perception and motor execution, called nondecision time. If that extra time is shared between settings,
    the difference remains. How precisely you could estimate the parameters would
    still depend on the data available.")
  ask_server("x2", input, output, answers, x2_items, button = "See the predicted times",
    feedback = function(a) tagList(
      if (a$longer == "second") p("You expected longer decisions in the second setting.
        Its drift is faster and its boundaries are closer; both changes shorten the
        predicted decision times.")
      else if (a$longer == "same") p("Equal accuracy does not require equal decision
        times. The settings differ in both drift and boundary separation."),
      P(switch(a$rec,
        rt = "",
        conf = "You could record confidence, but you would need an additional rule connecting it to the model before comparing predictions. This model already predicts decision times. ",
        side = "For a given stimulus, both settings predict the same proportions of left and right choices. Recording the chosen side would therefore leave these settings indistinguishable. Their decision times differ. ",
        none = "With correctness alone you cannot tell these settings apart: both predict the same accuracy, but their decision times differ. "),
        rt_text)),
    on_submit = function(a) add_note("Step 6", "What else to record",
      "What else would you record, and which setting gives the longer decision times?",
      paste0(x2_items[[1]]$choices[[a$rec]], " ", x2_items[[2]]$choices[[a$longer]]),
      result = paste0("Mean decision time ", num0(dd$mean[1], 2), " in the first setting and ",
                      num0(dd$mean[2], 2), " in the second; accuracy ", num(dd$acc[1]), " in both."),
      settings = ddm_settings_text))

  output$p_ddm <- renderPlot({ req(answers$x2); plot_ddm(dd) })
  output$ddm_text <- renderUI({
    req(answers$x2)
    p(class = "muted", "Both curves are calculated from the model, not simulated. Correct and
       incorrect responses are pooled, and nondecision time is not included.")
  })

  challenge <- "Now suppose the time needed to perceive the stimulus and make a
    motor response also differs between settings. If you recorded longer response
    times under one setting, could you conclude that it had the lower drift rate?
    What would you want to know before making that claim?"
  reflection <- "A longer recorded response time need not mean slower evidence
    accumulation. Wider boundaries or a longer nondecision time could
    also lengthen it. To draw a conclusion about drift rate, you would need to account for those
    components and ask whether the combined predictions for choices and response
    times distinguish your proposed explanations. The two decision-time curves are
    predictions from two specified settings. A drift rate cannot be read directly
    from an observed response time."
  output$x3_prompt <- renderUI(tagList(p(class = "item-label", challenge),
    p(class = "muted", "Write what you would consider. Save your thoughts to add them
       to your notes and read the discussion.")))
  ask_server("x3", input, output, answers, list(), reason = "Your reasoning",
    reason_required = TRUE, button = "Save my thoughts",
    feedback = function(a) tagList(h3("Thinking it through"), p(reflection)),
    on_submit = function(a) add_note("Step 6", "Interpreting response times", challenge,
      a$reason, result = reflection,
      settings = paste0(ddm_settings_text, "; decision-time curves exclude nondecision time; ",
                        "challenge allows different nondecision times")))

  # Trying another design or case, or starting the inquiry again, clears the
  # answers but keeps the notes.
  list(reset = function() clear_answers(answers, c("x1", "x2", "x3")))
}
