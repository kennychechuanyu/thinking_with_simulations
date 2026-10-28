# Questions and notes
#
# A question has multiple-choice items, a free-text reason, or both. Until it
# is submitted, the reader sees the form. Afterward the answers are fixed: the
# form is replaced by the answers and the feedback, so a prediction cannot be
# changed after the result is shown.
#
# Labels may contain simple HTML (<i>, <sub>, <b>, entities). Notes are kept
# as plain text, so they can be downloaded.

# One multiple-choice item: its input name, its label and the named choices.
item <- function(name, label, choices) list(name = name, label = label, choices = choices)

# Text for the notes, without the markup the page uses.
plain_text <- function(x) {
  if (is.null(x)) return(NULL)
  x <- gsub("<sub>(.*?)</sub>", "_\\1", x, perl = TRUE)
  # Remove the markup used by labels without swallowing mathematical inequalities.
  x <- gsub("</?(?:i|b|sub|sup|br|em|strong)\\s*/?>", "", x, perl = TRUE)
  # Greek letters are spelled out, so the notes read the same in any font.
  x <- gsub("&alpha;", "alpha", x, fixed = TRUE)
  x <- gsub("&beta;", "beta", x, fixed = TRUE)
  x <- gsub("&eta;", "eta", x, fixed = TRUE)
  x <- gsub("&gamma;", "gamma", x, fixed = TRUE)
  x <- gsub("&theta;", "theta", x, fixed = TRUE)
  x <- gsub("&lambda;", "lambda", x, fixed = TRUE)
  x <- gsub("&ndash;", "–", x, fixed = TRUE)
  x <- gsub("&times;", "×", x, fixed = TRUE)
  x <- gsub("&rarr;", "→", x, fixed = TRUE)
  x <- gsub("&asymp;", "≈", x, fixed = TRUE)
  x <- gsub("&mdash;", "—", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&minus;", "−", x, fixed = TRUE)
  x <- gsub("&plusmn;", "±", x, fixed = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  gsub("\\s+", " ", trimws(x))
}

# Draws the question with this id and keeps the submitted answer in
# answers[[id]]: the choice for each item, named by the item, and "reason", the
# text the reader wrote ("" if none). The fixed answer is followed by
# feedback(a); on_submit(a) runs once, when the answer is saved.
ask_server <- function(id, input, output, answers, items, reason = NULL,
                       button = "Save my answer", feedback = function(a) NULL,
                       on_submit = function(a) NULL, reason_required = FALSE) {
  # The message has its own output, so showing it keeps the choices already
  # made. It is kept with the answers, so starting a part again clears it.
  msg_id <- paste0(id, "_msg")
  output[[msg_id]] <- renderUI(
    if (!is.null(answers[[msg_id]])) p(class = "msg", answers[[msg_id]]))

  output[[id]] <- renderUI({
    a <- answers[[id]]
    if (is.null(a)) {
      tagList(
        lapply(items, function(it) div(class = "item",
          radioButtons(paste0(id, "_", it$name),
                       span(class = "item-label", HTML(it$label)), width = "100%",
                       choiceNames = lapply(unname(it$choices), HTML),
                       choiceValues = names(it$choices), selected = character(0)))),
        if (!is.null(reason))
          div(class = "reasoning",
            textAreaInput(paste0(id, "_reason"), reason, rows = 2, width = "100%")),
        actionButton(paste0(id, "_go"), button, class = "btn-submit"),
        uiOutput(msg_id, role = "status")
      )
    } else {
      tagList(
        div(class = "answered",
          lapply(items, function(it) p(
            if (!is.null(it$label)) span(class = "answered-label", HTML(it$label)),
            span(class = "answered-choice", HTML(it$choices[[a[[it$name]]]])))),
          if (nzchar(a$reason)) p(span(class = "answered-label", "You wrote"),
                                  span(class = "answered-choice", a$reason))),
        div(class = "feedback", feedback(a))
      )
    }
  })

  observeEvent(input[[paste0(id, "_go")]], {
    if (!is.null(answers[[id]])) return()     # already answered, e.g., a double click
    chosen <- lapply(items, function(it) input[[paste0(id, "_", it$name)]])
    if (any(vapply(chosen, function(v) is.null(v) || !nzchar(v), logical(1)))) {
      answers[[msg_id]] <- "Choose an answer to each question first."
      return()
    }
    a <- setNames(chosen, vapply(items, `[[`, "", "name"))
    written <- input[[paste0(id, "_reason")]]
    a$reason <- trimws(if (is.null(written)) "" else written)
    if (reason_required && !nzchar(a$reason)) {
      answers[[msg_id]] <- "Write a sentence about your reasoning before continuing."
      return()
    }
    answers[[msg_id]] <- NULL
    answers[[id]] <- a
    on_submit(a)
  })
}

# Clears the answers to the questions with these ids, and their messages, so
# that each question shows its form again. The notes are kept.
clear_answers <- function(answers, ids) {
  for (id in ids) {
    answers[[id]] <- NULL
    answers[[paste0(id, "_msg")]] <- NULL
  }
}


# Notes
# One entry per saved answer or further result viewed: its step and topic,
# what was asked, what the reader said, the result shown, the settings that
# produced it, and what is still open.

note <- function(step, topic, question, answer, result = NULL, open = NULL,
                 settings = NULL) {
  list(step = step, topic = topic, question = plain_text(question),
       answer = plain_text(answer), result = plain_text(result),
       settings = plain_text(settings), open = plain_text(open),
       time = format(Sys.time(), "%H:%M %Z"))
}

# Newest first; only the newest entry starts open.
notes_ui <- function(entries) {
  if (!length(entries))
    return(p(class = "muted", "Your answers will appear here as you go."))
  entries <- rev(entries)
  tagList(lapply(seq_along(entries), function(i) {
    e <- entries[[i]]
    tags$details(class = "note", open = if (i == 1) NA else NULL,
      tags$summary(
        span(class = "note-step", paste0(e$step, ", ", e$topic)),
        span(class = "note-q", e$question)),
      p(span(class = "note-label", "You: "), e$answer),
      if (!is.null(e$result)) p(span(class = "note-label", "Result: "), e$result),
      if (!is.null(e$settings)) tags$details(class = "note-settings",
                                  tags$summary("Settings"), p(e$settings)),
      if (!is.null(e$open)) p(span(class = "note-label", "Still open: "), e$open))
  }))
}

notes_text <- function(entries) {
  if (!length(entries)) return("The notes are empty.")
  lines <- c("Notes from the companion app to 'Thinking with Computational Models to Guide",
             "Scientific Inquiry'", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "")
  for (e in entries) {
    lines <- c(lines, paste0(e$step, ", ", e$topic, " (", e$time, ")"),
               paste0("  Question:   ", e$question),
               paste0("  You:        ", e$answer),
               if (!is.null(e$result)) paste0("  Result:     ", e$result),
               if (!is.null(e$settings)) paste0("  Settings:   ", e$settings),
               if (!is.null(e$open)) paste0("  Still open: ", e$open), "")
  }
  lines
}
