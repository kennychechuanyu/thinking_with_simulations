# Thinking with Models: companion app
# One inquiry in six steps. The reader finds where two explanations of blocking
# disagree, examines what the disagreement depends on, designs and runs a
# simulated experiment, decides what its records support, and meets the same
# question in a different model. Explore lets the reader change the settings
# freely.
#
# Files in R/ are loaded by Shiny before this one:
#   models.R      the learning models (a copy of R/models.R; the tests require
#                 the two to be identical)
#   engine.R      the model calculations, except the experiments of Step 5;
#                 checked by tests/verify_app.R
#   formulas.R    displayed model equations and symbol definitions
#   helpers.R     shared text, number formats and small building blocks
#   ask.R         questions, locked answers, and notes
#   plots.R       figures
#   opening.R     the opening and the step buttons
#   training.R    Step 1
#   comparison.R  Step 2
#   assumptions.R Step 3
#   responses.R   Step 4
#   experiment.R  Step 5 (records from data/inference_cases.rds,
#                 written by shiny_app/make_inference_cases.R)
#   decision.R    Step 6
#   explore.R     the Explore tab
#   values.R      the model results the page shows, computed once at startup
# tests/verify_app_flow.R walks through the six steps of the inquiry.
#
# Requirements: shiny, bslib, ggplot2
# Launch from the repository root: shiny::runApp("shiny_app")

library(shiny)
library(bslib)
library(ggplot2)


# UI

# The steps with the notes beside them. On narrow screens the notes come after
# the steps, and a small fixed link jumps there.
with_notes <- function(content) div(class = "layout",
  div(class = "main part", content),
  div(class = "notes", id = "notes_box",
    tags$details(id = "notes_details", open = NA,
      tags$summary(h2("Your notes")),
      downloadLink("notes_download", "Download notes (.txt)"),
      uiOutput("notes"))),
  tags$a(class = "notes-jump", href = "#notes_box", "Notes"))

about <- div(class = "part",
  h2(class = "about", "About this page"),
  p(HTML("You can work through this page without reading the accompanying tutorial,
          <i>Thinking with Models through Simulation to Guide Scientific Inquiry</i> by
          Kenny Yu (KU Leuven). Both use the same blocking
          example to compare the Rescorla&ndash;Wagner and adaptive-associability models.")),
  p("For the derivations, code checks and full simulation study, follow the references
     marked “In the tutorial” or inspect the R scripts in the repository linked below."),
  h3("Where the numbers come from"),
  p("Steps 1 to 4 and Explore calculate the model predictions as you use the page.
     Step 6 calculates accuracy and decision times from the diffusion model.
     The numerical checks compare these results with the tutorial."),
  p("Step 5 uses saved simulated experiments from the tutorial's study. Changing
     the sample size or the recorded presentations selects the corresponding data
     and analysis; no models are fitted while you use the page."),
  p("The individual cases use one generating parameter set. The percentages shown
     at the end summarize 400 experiments per design: 100 under each of four
     parameter sets."),
  tags$details(class = "formula-details",
    tags$summary("Code and numerical checks"),
    P("The learning calculations use <code>shiny_app/R/models.R</code>, an identical
       copy of <code>R/models.R</code> used in the tutorial. Step 6 uses
       <code>shiny_app/R/engine.R</code>. <code>tests/verify_app.R</code> checks both
       against the reported values, including B's learned value of ",
       num(trained$rw$V_B, 4), " under ", RW, " after compound training, and the
       adaptive model's B&ndash;D gap of ", num(at(base$adaptive$gap, 8)),
       " after eight updates."),
    P("For Step 5, <code>shiny_app/make_inference_cases.R</code> regenerates one
       experiment of each size per generating process from saved random seeds.
       It analyzes every design with the study's code and stops if a result
       disagrees with the corresponding saved study analysis. These cases use the
       first parameter set.")),
  p(HTML(paste0("Code and materials: ",
                "<a href='https://github.com/kennychechuanyu/thinking_with_simulations' target='_blank'>github.com/kennychechuanyu/thinking_with_simulations</a>.")))
)

ui <- page_fixed(
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = "#252525", primary = "#252525",
                   base_font = font_collection("Charter", "Georgia", "Cambria", "serif"),
                   heading_font = font_collection("Charter", "Georgia", "serif")),
  title = "Thinking with Models",
  tags$head(tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(src = "scenes.js"),
    # The link in Step 3's matched-control question opens Explore at its results.
    # It waits until the tab is shown and the starting states have been drawn.
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        Shiny.addCustomMessageHandler('show_explore', function (m) {
          document.getElementById('control-e').open = true;
          var tries = 0;
          (function scrollWhenDrawn() {
            var results = document.querySelector('.explore-output');
            var states = document.getElementById('x_summary');
            if (results && results.offsetParent !== null && states && states.textContent.trim()) results.scrollIntoView();
            else if (++tries < 100) setTimeout(scrollWhenDrawn, 50);
          })();
        });
        Shiny.addCustomMessageHandler('scroll_top', function (m) {
          var heading = document.querySelector('.opening');
          if (heading) {
            heading.setAttribute('tabindex', '-1');
            heading.focus({ preventScroll: true });
          }
          window.scrollTo(0, 0);
        });
      });"))),
  div(class = "masthead",
    h1("Thinking with Models"),
    p("Two learning models explain blocking. What would you try next?")),
  navset_underline(id = "tabs",
    nav_panel("The inquiry", with_notes(tagList(opening_ui(), training_ui(), comparison_ui(),
      assumptions_ui(), responses_ui(), experiment_ui(), decision_ui()))),
    nav_panel("Explore", explore_ui()),
    nav_panel("About", about))
)


# Server

server <- function(input, output, session) {
  # The saved answer to each question, and the entries of the notes.
  answers <- reactiveValues()
  entries <- reactiveVal(list())
  add_note <- function(...) entries(c(entries(), list(note(...))))

  # A logical output read by conditionalPanel. The "..." lets both Shiny and
  # shiny::testServer call it. The flags that complete Steps 1 to 5 also go
  # to scenes.js, which marks the step complete.
  flag <- function(name, condition) {
    output[[name]] <- function(...) condition()
    outputOptions(output, name, suspendWhenHidden = FALSE)
    if (name %in% c("done_q1", "done_q2", "done_q3c", "done_q4b", "done_e6"))
      observe(session$sendCustomMessage("scene_done", list(name = name, done = isTRUE(condition()))))
  }
  # One flag per question, done_q1 and so on: TRUE once it has been answered.
  for (id in c("q1", "q2", "q2b", "q3a", "q3b", "q3c", "q3d", "q6", "q4a", "q4b",
               "e_design", "e1", "e2", "e3", "e4", "e5", "e6", "x1", "x2", "x3"))
    local({ i <- id; flag(paste0("done_", i), function() !is.null(answers[[i]])) })

  output$notes <- renderUI(notes_ui(entries()))
  output$notes_download <- downloadHandler(
    filename = function() paste0("thinking-with-models-notes-", Sys.Date(), ".txt"),
    content = function(file) writeLines(notes_text(entries()), file))

  training <- training_server(input, output, answers, add_note)
  comparison <- comparison_server(input, output, answers, add_note)
  assumptions <- assumptions_server(input, output, answers, add_note)
  responses <- responses_server(input, output, answers, add_note)
  experiment <- experiment_server(input, output, answers, add_note, flag)
  decision <- decision_server(input, output, answers, add_note)
  explore <- explore_server(input, output, session)

  # The link in Step 3's matched-control question opens Explore at the default
  # settings, with E one part in a million more salient than D.
  observeEvent(input$q6_explore, {
    explore$defaults()
    updateRadioButtons(session, "x_e", selected = "1e-6")
    nav_select("tabs", "Explore", session = session)
    session$sendCustomMessage("show_explore", TRUE)
  })
  # Trying another design or case clears the answers of Steps 5 and 6, since
  # Step 6 opens only after Step 5. The notes are kept.
  observeEvent(input$e_again, { experiment$reset(); decision$reset() })
  # Starting again clears the answers of every step and keeps the notes.
  observeEvent(input$restart, {
    training$reset(); comparison$reset(); assumptions$reset(); responses$reset()
    experiment$reset(); decision$reset()
    session$sendCustomMessage("scroll_top", TRUE)
  })
}

shinyApp(ui, server)
