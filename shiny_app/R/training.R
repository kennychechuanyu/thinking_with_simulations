# Step 1: what training leaves in each model (Sections 2, 3.1 and 3.2)
# The training design, the first compound trial in both models, the values and
# multipliers after compound training, and the question of what else differs
# between B and D.

training_ui <- function() {
  scene("scene-1", "1", "What changed during training?", "true", "done_q1",
    "Both models leave B with less learned value than D. The adaptive model also lowers B's associability.",
    p("Compare two training histories. In one, a new cue is trained alongside a cue
       that already predicts food. In the other, two new cues are trained together.
       Both histories have the same number of trials with two cues together,
       called compound trials."),
    p("Follow the two new cues, B and D. B is the light from the opening
       example; D is the control cue. The cue key below lists all five cues. O1 is
       the food outcome."),
    div(class = "training-design",
      div(class = "scroll-x", tabindex = "0", role = "region",
        `aria-label` = "The two training histories", design_table()),
      div(class = "cue-key-box",
        h3(class = "sub", "Cue key"),
        tags$dl(class = "cue-key",
          tags$dt("A"), tags$dd("The tone, trained before B is added."),
          tags$dt(class = "cue-B", "B"), tags$dd("The light, added alongside A."),
          tags$dt("C"), tags$dd("Trained like A, then set aside before D and E appear."),
          tags$dt(class = "cue-D", "D"), tags$dd("The new cue compared with B."),
          tags$dt("E"), tags$dd("D's companion, also new.")))),
    p(class = "table-note", "A+ means A is presented and O1 follows; AB+ means A and B
       appear together and O1 follows. Training C first gives the control history
       the same number of earlier food trials; C is absent during DE training."),
    h3("How the models learn"),
    p("Each model assigns a learned value to a cue: 0 means no expectation of the
       outcome, and 1 means the full outcome is expected. All cues start at 0 before
       any training."),
    p("Both models multiply a prediction error by an update multiplier to get the
       change in value. The adaptive model can change that multiplier by changing
       associability: how readily the cue learns from a given error."),
    learning_formulas_ui(),
    h3("After ten compound trials"),
    div(class = "scroll-x", tabindex = "0", role = "region",
        `aria-label` = "Values after ten compound trials", uiOutput("trained_table")),
    tags$details(class = "worked-example",
      tags$summary("Work through the first compound trial"),
      p("Return to the first time A and B appear together. How much does B's value
         increase, and what changes before its next trial? Compare this with D's first trial
         alongside E."),
      div(class = "scroll-x", tabindex = "0", role = "region",
        `aria-label` = "The first compound trial", uiOutput("first_trial"))),
    div(class = "question", uiOutput("q1")),
    aside("Sections 2, 3.1 and 3.2."))
}


training_server <- function(input, output, answers, add_note) {

  # The first compound trial, and what ten compound trials leave behind
  rw_first <- trace$rw[1, ]
  ad_first <- trace$adaptive[1, ]
  output$first_trial <- renderUI(tagList(
    tags$table(class = "values two",
      tags$thead(tags$tr(tags$th(""), tags$th(HTML(RW)), tags$th("Adaptive model"))),
      tags$tbody(
        vrow("A's value before the trial", num(rw_first$V_A), num(ad_first$V_A)),
        vrow("The error B learns from",
             paste0(num(rw_first$error),
                    "<br><span class='muted'>what A and B together leave unexpected</span>"),
             paste0(prob(ad_first$error_B),
                    "<br><span class='muted'>B's own error: it predicts nothing yet</span>")),
        vrow("B's gain", num(rw_first$gain_B), num(ad_first$gain_B)),
        vrow("D's gain on its first compound trial", num(rw_first$gain_D), num(ad_first$gain_D)),
        vrow("B's associability after the trial", "&mdash;",
             paste0(num(ad_first$alpha_B), " &rarr; ", num(ad_first$alpha_B_after))))),
    p(class = "table-note", "The dash marks a quantity that Rescorla–Wagner does not have:
       it keeps a fixed update multiplier, with no separate associability update."),
    P("In ", RW, ", A already predicts most of the food, so B has little error to
       learn from on this first trial. In the adaptive model, B learns as much as D
       at first. Its own error is still ", prob(ad_first$error_B), ", compared with A's ",
       num(ad_first$error_A), ". That poorer prediction lowers B's associability, so it
       will learn less from a given error on later trials.")))

  output$trained_table <- renderUI(tagList(
    p(class = "table-note", "Learned value for O1, and the multiplier each cue would now
                             apply to an error"),
    tags$table(class = "values",
      tags$thead(tags$tr(tags$th("Model"), tags$th(class = "cue-B", "B"), tags$th(class = "cue-D", "D"),
                         tags$th(class = "cue-B", "B's multiplier"), tags$th(class = "cue-D", "D's multiplier"))),
      tags$tbody(
        vrow(RW, num(trained$rw$V_B), num(trained$rw$V_D),
             num(trained$rw$mult_B), num(trained$rw$mult_D)),
        vrow("Adaptive model", num(trained$adaptive$V_B), num(trained$adaptive$V_D),
             num(trained$adaptive$mult_B), num(trained$adaptive$mult_D)))),
    p(class = "table-note", "Read across each row to compare B with D within that model.
       The models use different error rules and illustrative learning rates. A larger
       value in one model does not by itself isolate the effect of associability.")))

  q1_items <- list(item("left", "Both models leave B with less learned value than D.
    What else differs between B and D after training?", c(
    nothing = "Nothing else: in both models, only their learned values differ.",
    adaptive = paste0("The adaptive model also leaves B less ready to learn from the next error. ",
                     RW, " gives B and D the same fixed multiplier."),
    both = "Both models leave B less ready to learn from the next error.")))
  q1_text <- paste0("Here, ", RW, " keeps B's and D's multipliers at ",
    num(trained$rw$mult_B), ". In the adaptive model B's associability has fallen to ",
    num(trained$adaptive$alpha_B), " while D's is still ", num(trained$adaptive$alpha_D),
    ", so B's multiplier is ", num(trained$adaptive$mult_B), " compared with D's ",
    num(trained$adaptive$mult_D), ". B has learned less than D in both models, but only
    the adaptive model leaves it less ready to learn from the next error.")
  q1_feedback <- function(a) paste0(switch(a$left,
    nothing = "The adaptive model changes more than learned value: B also ends with lower associability. ",
    both = paste0("In ", RW, ", B's smaller gain comes from a smaller error, not a lower update multiplier. "),
    adaptive = ""), q1_text)
  ask_server("q1", input, output, answers, q1_items,
    feedback = function(a) P(q1_feedback(a)),
    on_submit = function(a) add_note("Step 1", "What differs after training",
      q1_items[[1]]$label, q1_items[[1]]$choices[[a$left]], result = q1_feedback(a),
      settings = first_set(phase3 = FALSE)))

  # Starting the inquiry again clears the answer but keeps the notes.
  list(reset = function() clear_answers(answers, "q1"))
}
