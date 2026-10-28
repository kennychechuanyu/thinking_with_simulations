# The opening: the task, how each step works, and the buttons for the seven
# steps, which follow Sections 2 to 8 of the paper.

step_names <- c("One rule", "Two explanations", "Assumptions", "Behavior", "Experiment",
                "Shared prediction", "Measurement")

opening_ui <- function() tagList(
  h2(class = "opening", "A question about a tone and a light"),
  p("Imagine taking part in a computer task. A tone sounds, and a picture of an apple
     appears. This happens ten times. On the next ten trials, a light comes on together with
     the tone, and the apple appears again. You then see the light on its own and are asked,
     before any picture appears, whether you expect the apple."),
  p("The apple has followed the light every time the light appeared, which seems a good
     reason to answer yes. But the tone already predicted the apple before the light was
     introduced. If learning depends on the difference between what happens and what was
     expected, the light might come to predict little on its own."),
  p("This page works through one investigation of this task with two learning models, in six
     steps, and a seventh step with a model of simple decisions. The models are worked examples,
     not accounts of how people learn. Each step follows the same pattern:"),
  tags$ol(class = "pattern",
    tags$li("A question about what a model will do."),
    tags$li("Your expectation and your reason, recorded before you see the result."),
    tags$li("The result, worked out from the model's equations or by simulation."),
    tags$li("A comparison of the result with your expectation and, where you gave one, your
             reason."),
    tags$li("What the result settles, and what it leaves open for the next step.")),
  p("A model's prediction depends on several choices. The rules express an idea as equations,
     and the code applies the rules. The settings are the trial sequence, the starting values and
     the parameter values under which the code runs. The observation bridge links the model's
     states to the responses an experiment records. Each step changes one or more of these
     choices and asks what the result shows."),
  p(class = "muted", "You can work through the steps without reading the tutorial. After each
     result, use Continue to move on. The step buttons let you return to completed steps. Your
     notes keep your expectations and the results you see. Download a copy before closing or
     reloading the page."),
  tags$nav(class = "inquiry-progress", `aria-label` = "Inquiry steps",
    tags$ol(Map(function(i, name) tags$li(
      tags$button(type = "button", `data-step-target` = i,
        `aria-label` = paste0("Step ", i, ": ", name),
        disabled = if (i > 1) NA else NULL,
        span(class = "progress-n", i),
        span(name))),
      seq_along(step_names), step_names)))
)
