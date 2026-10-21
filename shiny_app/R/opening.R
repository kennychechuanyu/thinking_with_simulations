# The opening: why did participants learn so little about the light?
# opening_ui() introduces blocking and the two explanations that the inquiry
# compares, and lays out the buttons for the six steps.

opening_ui <- function() tagList(
  h2(class = "opening", "Why did participants learn so little about the light?"),
  p("Participants have learned that a tone means food is coming. You now add a light:
     tone and light appear together, followed by food. After several trials, you present
     the light on its own and ask whether they expect food. Few say yes, even though
     the light has accompanied food every time. Had the tone been new as well, many
     more would say yes to the light. This is called blocking."),
  p(class = "muted", "The tone and light are cues; the food is the outcome.
     Prediction error is the difference between the outcome and what was expected."),
  p("Perhaps the tone already predicts the food so well that there is little
     prediction error left for the light. Or perhaps training changes how readily the
     participants learn about the light. Both explanations can account for blocking.
     What could you try next to find out where their predictions differ?"),
  tags$details(tags$summary("The two learning models"),
    tags$ul(
      tags$li(HTML(paste0("The ", RW, " model updates each cue's learned value using
        the prediction error shared by all cues present. The well-trained tone leaves
        little error for the new light to learn from."))),
      tags$li(HTML("The adaptive-associability model changes how readily participants
        learn about each cue. It lowers the light's associability when the light
        predicts the food less accurately than the tone does. Lower associability
        means a smaller value update for the same prediction error. Associability
        represents attention in this explanation; attention is not measured directly.")))),
  p("Start with what the two models say happened during training. Then choose a
     follow-up comparison and see whether its results change your view."),
  p(class = "muted", "You can work through the six steps without reading the tutorial.
     After each result, use Continue to move on; the step buttons let you return to
     completed sections. Your notes keep your predictions and the results you see.
     Download a copy before closing or reloading the page."),
  tags$nav(class = "inquiry-progress", `aria-label` = "Inquiry steps",
    tags$ol(Map(function(i, name) tags$li(
      tags$button(type = "button", `data-step-target` = i,
        `aria-label` = paste0("Step ", i, ": ", name),
        disabled = if (i > 1) NA else NULL,
        span(class = "progress-n", i),
        span(name))),
      1:6, c("Training", "Comparison", "Assumptions", "Responses", "Experiment", "Decisions"))))
)
