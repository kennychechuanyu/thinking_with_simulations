# Thinking with Computational Models: companion app
# One investigation in seven steps, following Sections 2 to 8 of the paper. The
# reader works out what one learning rule implies, finds a comparison in which
# two explanations of blocking disagree, examines what the disagreement depends
# on, asks how it would appear in responses, analyzes a simulated experiment,
# checks a prediction that all its candidates share, and sees two settings of a
# model of simple decisions that only a different measurement can tell apart.
# Explore lets the reader change the settings freely.
#
# Files in R/ are loaded by Shiny before this one:
#   models.R        the learning models (a copy of R/models.R; the tests require
#                   the two to be identical)
#   engine.R        the model calculations, except the experiments of Steps 5
#                   and 6; checked by tests/verify_app.R
#   formulas.R      displayed model equations and their symbols
#   helpers.R       shared text, number formats and small building blocks
#   ask.R           questions, saved answers, and notes
#   plots.R         figures
#   opening.R       the opening and the step buttons
#   rule.R          Step 1, Section 2
#   explanations.R  Step 2, Section 3
#   assumptions.R   Step 3, Section 4
#   behavior.R      Step 4, Section 5
#   experiment.R    Step 5, Section 6 (responses from data/inference_cases.rds,
#                   written by shiny_app/make_inference_cases.R)
#   shared.R        Step 6, Section 7
#   measurement.R   Step 7, Section 8
#   explore.R       the Explore tab
#   values.R        the model results the page shows, computed once at startup
# tests/verify_app_flow.R walks through the seven steps, and
# tests/verify_app_feedback.R checks the feedback on every combination of answers.
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
          <i>Thinking with Computational Models to Guide Scientific Inquiry</i> by Kenny Yu
          (KU Leuven). Both use the same learning task and the same two learning models,
          Rescorla&ndash;Wagner and the adaptive model, followed by a model of simple decisions.
          The seven steps follow Sections 2 to 8 of the tutorial.")),
  p("For the derivations, the code checks and the full simulation study, follow the references
     marked “In the tutorial” or inspect the R scripts in the repository linked below."),
  h3("Where the numbers come from"),
  p("Steps 1 to 4 and Explore calculate the model predictions as you use the page. Step 7
     calculates accuracy and decision times from the drift diffusion model. The numerical checks
     compare these results with the tutorial."),
  p("Steps 5 and 6 use saved simulated experiments from the tutorial's study. Changing the
     sample size or the analyzed pear trials selects the corresponding responses and analysis.
     The page does not fit any models while you use it."),
  p("The saved cases use the base parameter set. The percentages shown with them summarize 400
     experiments for each process and design, 100 under each of four parameter sets."),
  tags$details(class = "formula-details",
    tags$summary("Code and numerical checks"),
    P("The learning calculations use <code>shiny_app/R/models.R</code>, an identical copy of
       <code>R/models.R</code> used in the tutorial. The engine of the page is
       <code>shiny_app/R/engine.R</code>. <code>tests/verify_app.R</code> checks both against the
       reported values, including the light's learned value of ", num(rw_tr$V_B_after[20], 4),
       " after ten compound trials under ", RW, " and the adaptive model's B&ndash;D gap of ",
       num(at(base$adaptive$gap, 8)), " after eight pear trials."),
    P("For Steps 5 and 6, <code>shiny_app/make_inference_cases.R</code> regenerates one
       experiment of each size per generating process from saved random seeds. It analyzes every
       design with the study's code and stops if a result disagrees with the corresponding saved
       analysis of the study.")),
  p(HTML(paste0("Code and materials: ",
                "<a href='https://github.com/kennychechuanyu/thinking_with_simulations' target='_blank'>github.com/kennychechuanyu/thinking_with_simulations</a>.")))
)

ui <- page_fixed(
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = "#252525", primary = "#252525",
                   base_font = font_collection("Charter", "Georgia", "Cambria", "serif"),
                   heading_font = font_collection("Charter", "Georgia", "serif")),
  title = "Thinking with Computational Models",
  tags$head(tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(src = "scenes.js"),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
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
    h1("Thinking with Computational Models"),
    p("One investigation, worked through in seven steps.")),
  navset_underline(id = "tabs",
    nav_panel("The inquiry", with_notes(tagList(opening_ui(), rule_ui(), explanations_ui(),
      assumptions_ui(), behavior_ui(), experiment_ui(), shared_ui(), measurement_ui()))),
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
  # shiny::testServer call it. The flags that complete Steps 1 to 7 also go
  # to scenes.js, which marks the step complete.
  step_flags <- c("done_q1c", "done_q2d", "done_q3e", "done_q4c", "done_e4", "done_f5", "done_x3")
  flag <- function(name, condition) {
    output[[name]] <- function(...) condition()
    outputOptions(output, name, suspendWhenHidden = FALSE)
    if (name %in% step_flags)
      observe(session$sendCustomMessage("scene_done", list(name = name, done = isTRUE(condition()))))
  }
  # One flag per question, done_q1a and so on: TRUE once it has been answered.
  for (id in c("q1a", "q1b", "q1c", "q2a", "q2b", "q2c", "q2d", "q3a", "q3b", "q3c", "q3d",
               "q3e", "q4a", "q4b", "q4c", "e_design", "e1", "e2", "e3", "e4", "f1", "f2", "f3",
               "f4", "f5", "x1", "x2", "x3"))
    local({ i <- id; flag(paste0("done_", i), function() !is.null(answers[[i]])) })

  output$notes <- renderUI(notes_ui(entries()))
  output$notes_download <- downloadHandler(
    filename = function() paste0("thinking-with-models-notes-", Sys.Date(), ".txt"),
    content = function(file) writeLines(notes_text(entries()), file))

  rule <- rule_server(input, output, answers, add_note)
  explanations <- explanations_server(input, output, answers, add_note)
  assumptions <- assumptions_server(input, output, answers, add_note)
  behavior <- behavior_server(input, output, answers, add_note)
  experiment <- experiment_server(input, output, answers, add_note, flag)
  shared <- shared_server(input, output, answers, add_note, flag, experiment)
  measurement <- measurement_server(input, output, answers, add_note)
  explore <- explore_server(input, output, session)

  # Trying another case or design clears the answers of Steps 5 to 7, since
  # Step 6 opens only after Step 5 and Step 7 only after Step 6. The notes are
  # kept.
  observeEvent(input$e_again, { experiment$reset(); shared$reset(); measurement$reset() })
  # Starting again clears the answers of every step and keeps the notes.
  observeEvent(input$restart, {
    rule$reset(); explanations$reset(); assumptions$reset(); behavior$reset()
    experiment$reset(); shared$reset(); measurement$reset()
    session$sendCustomMessage("scroll_top", TRUE)
  })
}

shinyApp(ui, server)
