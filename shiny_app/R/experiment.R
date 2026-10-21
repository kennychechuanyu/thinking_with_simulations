# Step 5: design the experiment and analyze its records (Section 5)
# The records and every analysis come from data/inference_cases.rds, written by
# shiny_app/make_inference_cases.R from the paper's simulation study. The
# reader's design (the number of participants and which presentations are
# recorded) selects the analysis shown. Nothing is fitted here.

experiment_ui <- function() scene("scene-5", "5", "What could the experiment tell you?",
  "output.done_q4b", "done_e6",
  "The records, model comparison and first-answer check answer different questions. Your notes keep the results for this design.",
  p("Suppose you could run the O2 experiment. How many participants would you recruit,
     and when would you record their responses? Here you can try those choices with
     simulated participants whose learning rates vary."),
  p("The simulated participants use the learning rules you have examined, with
     different parameter values. The two pictures used as cues can also be learned
     at different rates. A sigmoid turns each learned value into a yes probability,
     then a yes-or-no answer is drawn using that probability. These are Bernoulli
     responses."),
  experiment_formula_ui(),
  h3("Choose a case to examine"),
  p("Each case contains a saved set of simulated responses. Choose any case; you
     will learn how it was generated after interpreting its data."),
  uiOutput("e_case"),
  h3("Choose what to record"),
  p("Choose your sample size and the responses to keep. Everyone completes ten
     presentations of each cue, in two blocks, with O2 feedback each time. Keeping only
     some responses changes the available data, while training remains the same."),
  p(class = "muted", "The 40-person and 160-person samples are separate simulated
     experiments. Within a case and sample size, changing the recorded presentations
     keeps the same participants and their training; it changes which answers you see."),
  div(class = "question", uiOutput("e_design")),

  conditionalPanel("output.done_e_design",
    h3("Your data"),
    uiOutput("e_design_line"),
    fig("e_records", "330px"),
    p(class = "table-note", "The grid shows the first 24 participants. A row follows
       the same person in both panels. Colored cells are yes, gray cells are no;
       blank cells were not recorded. The analyses use everyone in your sample."),
    fig("e_contrast", "260px"),
    p(class = "table-note", "The response contrast is D's yes proportion minus B's:
       zero means equal proportions, and .10 means ten percentage points more yes
       responses to D. Bars show 95% intervals for the estimated difference at each
       presentation. They describe uncertainty in the estimate, rather than
       differences between people."),
    uiOutput("e_effect"),
    div(class = "question", uiOutput("e1")),

    conditionalPanel("output.done_e1",
      h3("Compare explanations"),
      p("How well can these three candidates account for the responses?"),
      tags$ul(
        tags$li("The carryover candidate is the adaptive model; the fraction of the earlier
                 associability change that is kept for O2 is estimated from the data."),
        tags$li("The reset candidate is the same adaptive model, but associability returns
                 to its starting value when O2 begins."),
        tags$li(HTML(paste0("The fixed-rate candidate is ", RW, ".")))),
      p("To fit a candidate, its parameters are chosen using half the participants.
         It then predicts the responses of the other half, whose data were not used
         for that fit. The halves swap roles. The held-out log score rewards assigning
         higher probabilities to the responses that actually occurred; a higher
         total is better."),
      p(class = "muted", "A standard error (SE) describes uncertainty in an estimated
         score difference. Each bar below extends two SE in either direction.
         A positive difference means the carryover candidate predicted better."),
      p("The decision rule was set before the analysis. Favor associability carryover
         if it beats both alternatives by more than two standard errors. Favor a model
         without associability carryover if either alternative beats carryover by that
         margin. Otherwise, leave the comparison unresolved. This is a practical rule
         for the exercise, not a significance test or the probability that the favored
         mechanism is true."),
      div(class = "question", uiOutput("e2")),
      conditionalPanel("output.done_e2",
        uiOutput("e2_result"),
        uiOutput("e2_other_link"),
        conditionalPanel("output.e_seen_zero",
          div(class = "question", uiOutput("e3"))))),

    conditionalPanel("output.done_e3",
      h3("What all the candidates share"),
      P("Look again at the models with ", gV, " fixed at 0. Each cue's first response
         comes before the participant has received any O2 feedback for that cue."),
      div(class = "question", uiOutput("e4")),
      conditionalPanel("output.done_e4",
        uiOutput("e_first"),
        uiOutput("e5_box"))),

    conditionalPanel("output.e_revealed",
      h3("What produced these responses?"),
      uiOutput("e_truth"),
      fig("e_contrast_truth", "260px"),
      uiOutput("e_rates"),
      aside("Sections 5.2, 5.5 and 5.6."),
      div(class = "question", uiOutput("e6")),
      conditionalPanel("output.done_e6",
        p(actionLink("e_again", "Try another design or case"),
          span(class = "muted", " (your notes are kept)")))))
)


experiment_server <- function(input, output, answers, add_note, flag) {

  # The design
  output$e_case <- renderUI({
    a <- answers$e_design
    if (is.null(a)) radioButtons("e_design_set", span(class = "visually-hidden", "Case to examine"),
      inline = TRUE, choiceNames = unname(set_labels), choiceValues = names(set_labels),
      selected = "value")
    else p(span(class = "answered-label", "Case examined"), set_labels[[a$set]])
  })
  design_items <- list(
    item("n", "How many participants?", c(`40` = "40", `160` = "160")),
    item("rec", "Which answers are recorded?", c(
      late = "Responses on presentations 8–10 of each cue, before feedback.",
      early = "Responses on presentations 1–6 of each cue, before feedback.",
      full = "Responses on presentations 1–10 of each cue, before feedback.")))
  ask_server("e_design", input, output, answers, design_items,
    reason = "Why keep these responses? Say what you hope they will tell you.",
    reason_required = TRUE, button = "Run the experiment",
    on_submit = function(a) {
      a$set <- input$e_design_set
      if (is.null(a$set) || !(a$set %in% names(set_labels))) a$set <- "value"
      answers$e_design <- a
      add_note("Step 5", "Design",
        "How many participants, and which answers are recorded?",
        paste0(set_labels[[a$set]], ", ", a$n, " participants, ", view_labels[[a$rec]], " recorded.",
               if (nzchar(a$reason)) paste0(" Reason: ", a$reason)),
        settings = design_set(a))
    })
  design_set <- function(a = answers$e_design) {
    cs <- cases[[a$set]][[a$n]]
    paste0(set_labels[[a$set]], ", ", a$n, " participants, ", view_labels[[a$rec]],
      " recorded before feedback; dataset ID = ", cs$id, "; seed = ", cs$seed,
      "; parameter set = ", cs$config,
      "; Phase 1: 10 A+, 10 C+ with O1; Phase 2: 10 AB+, 10 DE+ with O1; Phase 3: 10 B+ and 10 D+ with O2, block order counterbalanced",
      "; cue pictures counterbalanced equally; sigmoid/Bernoulli observations",
      "; p(yes) = logistic(b + gamma * V + b_hist * I(cue = B))",
      "; b and gamma estimated in every candidate; b_hist fixed at 0; candidate learning rules: fixed-rate shared error, adaptive individual error with associability reset or carryover",
      "; two-fold held-out participant log score; carryover favored if both score differences > 2 SE, no associability carryover favored if either score difference < -2 SE, otherwise unresolved",
      "; aggregate contrast: allocation-stratified standard error, Welch–Satterthwaite degrees of freedom, 95% t interval; presentation contrasts: Agresti–Min interval for paired proportions")
  }
  generating_set <- function() {
    g <- case()$generating
    # The symbols of Supplement S2; g_V, g_alpha and eta are printed without a
    # leading zero.
    symbols <- c(mu = "mu", spread = "tau", cue = "d", intercept = "b",
                 log_slope = "log gamma", g_value = "g_V", eta = "eta", g_alpha = "g_alpha",
                 history_intercept = "b_hist")
    printed <- vapply(names(g), function(nm) {
      x <- format(g[[nm]], digits = 15, trim = TRUE)
      if (nm %in% c("g_value", "g_alpha", "eta")) sub("^0[.]", ".", x) else x
    }, "")
    paste0("Generating parameters: ", paste(symbols[names(g)], printed, sep = " = ", collapse = "; "),
      "; per-cue rate r = .5 * logistic(mu + tau * z + x * d)",
      "; z = -1 or +1, sampled with equal probability; x = allocation for B, -allocation for D, 0 otherwise; allocation = -1 or +1, counterbalanced equally",
      "; fixed-rate alpha = r, beta = 1; adaptive S = 2r, alpha_init = .5",
      "; initial O1 values = 0; initial O2 values = g_V * final O1 values; initial O2 associabilities = .5 + g_alpha * (final O1 associabilities - .5)",
      "; all outcome magnitudes = 1; first-answer check: exact one-sided paired binomial test, p < .05",
      "; repeated-experiment percentages average four parameter sets, 100 experiments each per design")
  }

  case <- reactive({ a <- answers$e_design; req(a); cases[[a$set]][[a$n]] })
  design <- reactive({ a <- answers$e_design; req(a); case()$designs[[a$rec]] })
  # The question about the ranking opens once the reader has seen the comparison
  # with g_V fixed at 0, whether chosen first or viewed as the other choice.
  seen_other <- reactiveVal(FALSE)
  e_seen_zero <- reactive(!is.null(answers$e2) && (answers$e2$policy == "zero" || seen_other()))
  flag("e_seen_zero", e_seen_zero)
  # The generating process is revealed after the first-answer check, or after the
  # question about the shared prediction when the records lack the first presentation.
  flag("e_revealed", function() !is.null(answers$e5) ||
         (!is.null(answers$e4) && is.null(design()$first)))

  output$e_design_line <- renderUI({
    a <- answers$e_design; req(a)
    P("<b>", set_labels[[a$set]], "</b>, ", a$n, " participants, ", view_labels[[a$rec]],
      " recorded. Each answer was given before that presentation's feedback. Each cue's ten
      presentations formed one block, and half of the participants had the B block first.
      Each of the two pictures used as cues served as B for half of the participants and
      as D for the other half.")
  })
  output$e_records <- renderPlot(plot_records(case(), cols = design()$cols))
  output$e_contrast <- renderPlot(plot_contrast(case(), design()))
  effect_text <- function() {
    e <- design()$effect
    paste0("For your ", case()$n, " participants and the presentations you kept, the
      proportion of yes responses to D minus the proportion to B is ", num(e[["estimate"]]),
      " (95% interval ", num(e[["lower"]]), " to ", num(e[["upper"]]), ").",
      if (e[["detected"]] == 1) " The interval lies above zero."
      else if (e[["upper"]] < 0) " The interval lies below zero; it does not support a positive D-minus-B contrast."
      else " The interval includes zero; it provides no clear evidence of a positive D-minus-B contrast.")
  }
  output$e_effect <- renderUI(p(effect_text()))

  # What the records show
  e1_items <- list(item("e", "What do the records show?", c(
    more = "The estimated D-minus-B response contrast is positive, with a 95% interval above zero.",
    none = "The 95% interval does not lie above zero, so it provides no clear evidence of a positive D-minus-B contrast.",
    assoc = "Earlier training reduced B's associability.",
    value = "Learned values for O1 carried over to O2.")))
  ask_server("e1", input, output, answers, e1_items,
    feedback = function(a) {
      e <- design()$effect
      tagList(
        p(effect_text()),
        p(if (a$e %in% c("assoc", "value"))
          "More than one process can produce a response contrast, so the difference alone does not say which process is at work. The records measure responses; associability and learned value are explanations to compare."
        else if (a$e == "more" && e[["detected"]] != 1)
          "You selected evidence of a positive contrast. That claim needs the whole interval to lie above zero; this interval does not."
        else if (a$e == "none" && e[["detected"]] == 1)
          "You selected no clear evidence of a positive contrast, but the whole interval lies above zero. That supports a response difference but does not identify its source."
        else "What could have produced this pattern? Compare the candidates below."))
    },
    on_submit = function(a) add_note("Step 5", "The records",
      "What do the records show?", e1_items[[1]]$choices[[a$e]],
      result = effect_text(), settings = design_set()))

  # The comparison
  policy_choices <- c(
    zero = paste0("Fix ", gV, " at 0. Start both cues with no learned value for O2."),
    estimated = paste0("Estimate ", gV, ". Allow some old value to carry over."))
  e2_items <- list(item("policy", paste0("How would you handle old-value carryover? ",
    gV, " is the fraction of O1 value brought to O2. This choice applies to all three candidates."), policy_choices))
  # Score differences and their standard errors to one decimal; where both are
  # below .05, to two significant digits, so that the decision can be checked
  # from the printed numbers.
  score_num <- function(x, difference, se) {
    if (abs(difference) >= .05 || se >= .05) num0(x, 1)
    else if (x == 0) "0" else formatC(x, digits = 2, format = "fg", flag = "#")
  }
  score_sentence <- function(policy) {
    d <- design(); scores <- d$scores[[policy]]
    f <- scores[scores$against == "fixed", ]; r <- scores[scores$against == "reset", ]
    paste0("Carryover minus fixed-rate: ", score_num(f$difference, f$difference, f$se),
           " (SE ", score_num(f$se, f$difference, f$se), "). Carryover minus reset: ",
           score_num(r$difference, r$difference, r$se), " (SE ", score_num(r$se, r$difference, r$se),
           "). Under the stated rule, the comparison ", decision_text[[d$decision[[policy]]]], ".",
           if (near_reset_scores(scores)) " The carryover and reset candidates have
             nearly identical held-out scores. A difference this small can still meet the
             rule; it does not show how precisely carryover was estimated.")
  }
  ask_server("e2", input, output, answers, e2_items, reason = "Why? (optional)",
    button = "Run the comparison",
    on_submit = function(a) add_note("Step 5", "Comparison",
      "Should the candidates estimate old-value carryover?",
      paste0(policy_choices[[a$policy]], if (nzchar(a$reason)) paste0(" Reason: ", a$reason)),
      result = score_sentence(a$policy),
      settings = paste0(design_set(), "; ", policy_label[[a$policy]])))

  other_policy <- function() setdiff(c("zero", "estimated"), answers$e2$policy)
  output$e2_result <- renderUI({
    req(answers$e2)
    first <- answers$e2$policy
    shown <- if (seen_other()) c(first, other_policy()) else first
    a <- answers$e_design
    # The study ran the comparison with g_V fixed at 0 only for value generalization
    # and carried associability, with 160 participants and all ten presentations.
    in_study <- a$n == "160" && a$rec == "full" && a$set != "reset"
    tagList(lapply(seq_along(shown), function(i) div(
      h3(class = "sub", HTML(paste0("Result with ", policy_label[[shown[i]]]))),
      fig(paste0("e2_plot_", shown[i]), "170px"),
      if (i == 1) p(class = "muted", "Bars show ±2 SE. Carryover is favored when both bars
        lie entirely to the right of 0, and an explanation without associability carryover when a
        bar lies entirely to the left of 0. Otherwise the comparison is unresolved."),
      p(score_sentence(shown[i])),
      if (shown[i] == "zero" && !in_study)
        p(class = "muted", "The study did not run the comparison with old-value carryover
          fixed at 0 for this case and design. It was run for this page with the study's code."))))
  })
  for (pol in c("zero", "estimated")) local({
    pp <- pol
    output[[paste0("e2_plot_", pp)]] <- renderPlot(plot_scores(design()$scores[[pp]]))
  })
  output$e2_other_link <- renderUI({
    req(answers$e2)
    if (seen_other()) return(NULL)
    other <- other_policy()
    tagList(
      if (other == "zero") P("Would the result change if you started both cues at
        zero for O2? Check the comparison with ", gV, " fixed at 0 before continuing.")
      else P("You can also see what happens when the models estimate ", gV, "."),
      p(actionLink("e2_other", HTML(paste0("Show the result with ", policy_label[[other]])))))
  })
  observeEvent(input$e2_other, {
    req(answers$e2)
    if (seen_other()) return()
    seen_other(TRUE)
    other <- other_policy()
    add_note("Step 5", "Comparison, other choice", paste0("Result with ", policy_label[[other]]),
             "Viewed.", result = score_sentence(other),
             settings = paste0(design_set(), "; ", policy_label[[other]]))
  })

  e3_items <- list(item("which", paste0("Does the result with ", gV,
    " fixed at 0 tell you which process generated the records?"), c(
    yes = "Yes: the candidate that predicted held-out participants better is the process at work.",
    not = "Not by itself: it ranks only the candidates that were compared.")))
  ask_server("e3", input, output, answers, e3_items,
    feedback = function(a) tagList(
      if (a$which == "yes") p("You treated a better prediction among these candidates as
        evidence that its learning process was the one at work. A candidate can win
        this comparison while sharing an incorrect assumption with every alternative."),
      p("The scores measure how well these models predict participants whose data
        were held out. Whether one captures the process that produced the responses
        is a separate question. Look at their first-answer predictions next.")),
    on_submit = function(a) add_note("Step 5", "What the ranking shows",
      e3_items[[1]]$label, e3_items[[1]]$choices[[a$which]], result = score_sentence("zero"),
      settings = paste0(design_set(), "; ", policy_label[["zero"]])))

  # The shared prediction
  e4_items <- list(item("f", paste0("With ", gV, " fixed at 0, what do all three candidates
    predict about a participant's first answer to B and first answer to D?"),
    c(equal = "The same probability of a yes for B and D.",
      carry = "A higher probability for D, but only in the carryover candidate.",
      depends = "It depends on the fitted parameters.")))
  equal_text <- "Both cues start at 0 for O2 and share one response function. Associability
    changes how much a cue learns from feedback, but neither cue has had O2 feedback of
    its own before its first answer, and in these models feedback about one cue does
    not change the other cue's learned state. So every candidate, the carryover
    candidate included, predicts the same probability of a yes for the first answer
    to B and the first answer to D. Because all three candidates predict this
    equality, their ranking cannot test it. You need the recorded first answers."
  ask_server("e4", input, output, answers, e4_items,
    feedback = function(a) p(switch(a$f, equal = "",
      carry = "The carryover candidate predicts equal first-answer probabilities too. Carried associability changes learning after feedback; it does not give a cue a larger starting value. ",
      depends = "Fitting different parameters cannot remove this equality while old-value carryover is fixed at 0 and the response function is shared. "), equal_text),
    on_submit = function(a) add_note("Step 5", "The shared prediction",
      "With old-value carryover fixed at 0, what do all candidates predict about the first answers?",
      e4_items[[1]]$choices[[a$f]], result = "Equal first-answer probabilities for B and D.",
      settings = paste0(design_set(), "; ", policy_label[["zero"]])))

  output$e_first <- renderUI({
    req(answers$e4)
    f <- design()$first
    if (is.null(f)) return(tagList(
      P("Your records begin at presentation ", min(design()$cols), ". You need each
         participant's first response to B and first response to D for this check.
         Those responses were not kept, so the check cannot be made with your data.
         A design that includes presentation 1 would keep that information."),
      p(class = "muted", "You can still see how this case was generated below.")))
    tagList(
      p("Look at each participant's first answer to B and first answer to D:"),
      tags$table(class = "values",
        tags$tbody(
          tags$tr(tags$td("Yes to D, no to B"), tags$td(f$d_only)),
          tags$tr(tags$td("Yes to B, no to D"), tags$td(f$b_only)),
          tags$tr(tags$td("Same answer to both"), tags$td(f$n - f$d_only - f$b_only)))),
      p(paste0("If the two probabilities were equal, either direction would be equally
        likely among participants who answered the cues differently. This exact
        one-sided test asks how often equality would produce at least this many
        D-only answers among those participants. That probability is the p-value: ", p_text(f$p),
        ". The cutoff for evidence against equality in this direction is p < .05.")))
  })
  e5_items <- list(item("h", "What does this check tell you?", c(
    disagree = "The records disagree with a prediction that all the candidates share.",
    consistent = "The check found no clear evidence against the shared prediction.",
    value = "Old learned values carried over to O2.")))
  output$e5_box <- renderUI({
    req(answers$e4)
    if (is.null(design()$first)) return(NULL)
    div(class = "question", uiOutput("e5"))
  })
  ask_server("e5", input, output, answers, e5_items,
    feedback = function(a) {
      rejects <- design()$first$p < .05
      tagList(
        p(if (rejects) "At the .05 level, the one-sided test rejects equal
          first-answer probabilities in the direction of more yes responses to D.
          That is evidence against a prediction all three models share."
          else "The one-sided test finds no clear evidence of more yes responses to D
          on the first presentation at the .05 level. That does not show that the two
          first-answer probabilities are equal."),
        if (a$h == "consistent" && rejects) p("You chose no clear evidence against the
          shared prediction, but this test rejects it. A model ranking cannot reveal
          that mismatch because all three candidates predict the same equality."),
        if (a$h == "disagree" && !rejects) p("You chose a disagreement with the shared
          prediction, but this check did not find clear evidence in that direction.
          The observed counts can differ without meeting the stated test criterion."),
        if (a$h == "value") p("The check cannot show that old values carried over.
          It tests the shared prediction, without identifying the source of a difference."),
        p("If the check rejects equality, you still have several possibilities to
           consider. B and D could differ in how readily they elicit a yes at the same
           learned value. Old-value carryover is another possible source of the
           difference."))
    },
    on_submit = function(a) {
      f <- design()$first
      add_note("Step 5", "The first-answer check",
        "What does the first-answer check show?", e5_items[[1]]$choices[[a$h]],
        result = paste0(f$d_only, " participants favored D and ", f$b_only, " favored B (",
                        p_text(f$p), ")."),
        settings = paste0(design_set(), "; first answers to B and D, exact one-sided paired test at the .05 level"),
        open = "If equality is rejected, which shared assumption failed? It could be the starting values or the response function.")
    })

  # The reveal
  # How the two comparisons came out, in one clause when they agree.
  both_decisions <- function(d) if (d$decision$zero == d$decision$estimated)
    paste0("With ", gV, " fixed at 0 and with ", gV, " estimated, the comparison ",
           decision_text[[d$decision$zero]])
  else paste0("With ", gV, " fixed at 0 the comparison ", decision_text[[d$decision$zero]],
              "; with ", gV, " estimated it ", decision_text[[d$decision$estimated]])
  output$e_truth <- renderUI({
    cs <- case(); d <- design(); g <- cs$generating; a <- answers$e_design
    # With 160 participants and all ten presentations recorded, the comparison
    # with g_V fixed at 0 favored carryover in the value-generalization case; the
    # note on how that case was chosen applies to this design only.
    chosen <- a$rec == "full" && a$n == "160"
    switch(cs$source,
      value = tagList(
        P("<b>Value generalization under fixed-rate learning.</b> The records were generated
           by ", RW, " learning, with ", round(100 * g$g_value), "% of each cue's O1 value
           carried to O2 and no change in associability. With ", gV, " fixed at 0, none of
           the three candidates can represent this process, and the comparison ",
           decision_text[[d$decision$zero]],
           if (d$decision$zero == "carry") ", because the carryover candidate is the only
             one that can produce a systematic gap at all" else "",
           ". With ", gV, " estimated, the comparison ", decision_text[[d$decision$estimated]], "."),
        if (chosen) tagAppendAttributes(P("This case was chosen from the experiments in which
           the comparison with ", gV, " fixed at 0 favored carryover with this design. The
           text below the table says how often that happened."), class = "muted")),
      associability = tagList(
        P("<b>Carried associability.</b> The records were generated by the adaptive model,
           with ", round(100 * g$g_alpha), "% of the earlier associability change kept for O2
           and no old-value carryover. ", both_decisions(d), ".")),
      reset = tagList(
        P("<b>Transfer-only adaptation.</b> The records were generated by the adaptive model
           with no carryover of either kind. B and D entered the new task in the same state,
           so the two cues had the same response probabilities apart from the counterbalanced
           difference between the two pictures. ", both_decisions(d),
           ". The generating process contains no systematic effect of earlier learning
           history. Across the study's repeated experiments with this process, all
           analyzed with ", gV, " estimated, most comparisons remained unresolved. A
           formal preference can still occur in a finite sample, in either direction; it
           does not show which process generated the records.")))
  })
  output$e_contrast_truth <- renderPlot(plot_contrast(case(), design(), truth = TRUE))

  output$e_rates <- renderUI({
    cs <- case(); a <- answers$e_design
    r <- rates[rates$scenario == cs$source, ]
    r <- r[order(r$n, match(r$view, c("late", "early", "full"))), ]
    rec_short <- c(late = "8–10", early = "1–6", full = "1–10")
    row <- function(i) {
      mine <- r$n[i] == as.integer(a$n) && r$view[i] == a$rec
      tags$tr(class = if (mine) "mine",
        tags$td(r$n[i]),
        tags$td(class = "nowrap", paste0(rec_short[[r$view[i]]], if (mine) " (your design)")),
        tags$td(pct(r$detected[i])), tags$td(pct(r$carry[i])), tags$td(pct(r$no_carry[i])),
        tags$td(pct(r$unresolved[i])))
    }
    z <- zero_rates[zero_rates$scenario == cs$source, ]
    fr <- first_rates[first_rates$scenario == cs$source, ]
    tagList(
      h3(class = "sub", "Would another experiment give the same result?"),
      P("Each row below summarizes 400 experiments using the same learning process:
         100 experiments under each of four generating parameter sets. All were
         analyzed with ", gV, " estimated. Your case uses the first parameter set. The
         percentages average over all four sets, so they describe a broader range of
         conditions than the case you just examined."),
      p(class = "table-note", "The first percentage counts experiments whose response
         contrast had a 95% interval above zero. The last three percentages show
         which decision the model comparison reached. A detected response difference
         can accompany any of those decisions."),
      p(class = "small-screen-note table-note", "Scroll sideways to see all columns."),
      div(class = "scroll-x", tabindex = "0", role = "region", `aria-label` = "Repeated-experiment results",
        tags$table(class = "values rates",
        tags$thead(tags$tr(tags$th("Participants"), tags$th("Recorded"),
                           tags$th("Positive contrast detected"), tags$th("Associability carryover favored"),
                           tags$th("No associability carryover favored"), tags$th("Unresolved"))),
        tags$tbody(lapply(seq_len(nrow(r)), row)))),
      if (nrow(z)) P("The table does not show a second analysis of the same experiments,
        with ", gV, " fixed at 0. With 160 participants and all ten presentations, that
        analysis ", if (cs$source == "value") "wrongly " else "",
        "favored carryover in ", pct(z$carry), " of the experiments",
        ". The first-answer check rejected equal first answers in ",
        pct(fr$initial_rejection), " of them",
        if (cs$source == "value") paste0(", and in ", pct(fr$rejection_given_carry),
          " of those in which the comparison had wrongly favored carryover") else "", "."))
  })

  # What can be concluded
  statements <- reactive({
    d <- design()
    policy <- if (!is.null(answers$e2)) answers$e2$policy else "estimated"
    decision <- d$decision[[policy]]
    list(
      list(text = "The estimated D-minus-B response contrast is positive, with a 95% interval above zero.",
           ok = d$effect[["detected"]] == 1, why = effect_text()),
      list(text = "The comparison gave a preference for or against associability carryover under the stated rule.",
           ok = decision != "unresolved",
           why = paste0("With ", policy_label[[policy]], ", the comparison ", decision_text[[decision]],
                        ". This uses the rule set before the analysis.")),
      list(text = "The model comparison showed which process generated the records.",
           ok = FALSE, why = "Held-out scores compare the supplied candidates' predictions. They cannot test assumptions shared by every candidate, and the generating process may be absent."),
      list(text = "The first-answer check tested a prediction that all three candidates share.",
           ok = !is.null(d$first),
           why = if (is.null(d$first)) "Your records did not include the first presentation, so the check could not be made."
                 else "Every candidate predicts equal first-answer probabilities when old-value carryover is fixed at 0. You can check that prediction against the first responses you kept."),
      {
        r <- rates[rates$scenario == case()$source, ]
        preferences <- r$carry + r$no_carry
        list(text = "The proportion of experiments reaching a model preference varied across the six designs.",
          ok = diff(range(preferences)) > 0,
          why = paste0("The decision rule reached a preference in ", pct(min(preferences)),
            " to ", pct(max(preferences)), " of the experiments per design. These are
            observed frequencies across the two sample sizes and the three sets of
            recorded presentations, averaged over four parameter sets.",
            if (case()$source == "reset") " Even with no systematic effect of earlier
              learning history, finite random samples can yield a preference.
              Unresolved was the most common outcome in all six designs." else
              " The table describes these simulated conditions; it does not show that
              a larger sample always identifies the generating process."))
      })
  })
  output$e6 <- renderUI({
    st <- statements()
    if (is.null(answers$e6)) {
      tagList(
        checkboxGroupInput("e6_pick", span(class = "item-label", "Which statements would you include in a report of this experiment? Select all that your results support."), width = "100%",
          choiceNames = lapply(seq_along(st), function(i) HTML(paste0(i, ". ", st[[i]]$text))),
          choiceValues = as.character(seq_along(st))),
        actionButton("e6_go", "Compare with the results", class = "btn-submit"))
    } else {
      picked <- answers$e6$picked
      tagList(lapply(seq_along(st), function(i) {
        s <- st[[i]]; mine <- as.character(i) %in% picked
        div(class = "statement",
          P("<b>", i, ". ", s$text, "</b>"),
          p(class = "answered-label", paste0(if (mine) "You selected this. " else "You left this out. ",
            if (s$ok) "Your results support it." else "Your results do not support it.")),
          tagAppendAttributes(
            P(s$why),
            class = if (mine == s$ok) "agree" else "disagree"))
      }))
    }
  })
  observeEvent(input$e6_go, {
    if (!is.null(answers$e6)) return()
    st <- statements()
    picked <- if (is.null(input$e6_pick)) character(0) else input$e6_pick
    answers$e6 <- list(picked = picked)
    judged <- vapply(seq_along(st), function(i) paste0(i, ". ", st[[i]]$text, " ",
      if (st[[i]]$ok) "Supported. " else "Not supported. ", st[[i]]$why), "")
    process <- c(value = "value generalization", associability = "carried associability",
                 reset = "transfer-only adaptation")[[case()$source]]
    add_note("Step 5", "Conclusions", "What can you conclude from your experiment?",
      paste0("Selected ", if (!length(picked)) "none" else
               paste(if (length(picked) == 1) "statement" else "statements",
                     paste(picked, collapse = ", ")),
             "."),
      result = paste0(paste(judged, collapse = " "), " Generating process: ", process, "."),
      settings = paste0(design_set(), "; ", generating_set()))
  })

  # Trying another design or case, or starting the inquiry again, clears the
  # answers but keeps the notes.
  list(reset = function() {
    clear_answers(answers, c("e_design", "e1", "e2", "e3", "e4", "e5", "e6"))
    seen_other(FALSE)
  })
}
