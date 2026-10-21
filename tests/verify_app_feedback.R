# Checks of the feedback the companion app gives in Steps 1-5: every combination
# of answers to the Step 1 question, the Step 2 prediction for O2, the Step 3
# question on old-value carryover and the two Step 4 questions, and the readings
# of Step 5 for every case, sample size and set of recorded presentations. A
# correction must
# appear for a wrong answer and only for a wrong answer. Stops at the first
# failure.
#   Rscript tests/verify_app_feedback.R

local({
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  suppressPackageStartupMessages({ library(shiny); library(bslib); library(ggplot2) })

  n_checks <- 0L
  check <- function(label, ok) {
    if (!isTRUE(ok)) stop(sprintf("FAIL  %s", label), call. = FALSE)
    n_checks <<- n_checks + 1L
  }
  # The text as a reader sees it: runs of spaces and line breaks count as one space.
  shown <- function(x) gsub("\\s+", " ", paste(as.character(x$html), collapse = " "))
  contains <- function(s, p) grepl(p, s, fixed = TRUE)
  # Presses an action button in a testServer session.
  clicker <- function(session) function(id) {
    now <- isolate(session$input[[id]])
    do.call(session$setInputs, setNames(list(if (is.null(now)) 1 else now + 1), id))
  }

  cat("Checks of the app's feedback\n")

  # Step 1 and Step 2
  testServer("shiny_app", {
    click <- clicker(session)
    leads <- c(nothing = "changes more than learned value", both = "comes from a smaller error")
    for (q1 in c("nothing", "both", "adaptive")) {
      training$reset()
      session$setInputs(q1_left = q1)
      click("q1_go")
      s <- shown(output$q1)
      check(paste("Step 1,", q1, "answer: the feedback addresses this answer only"),
            all(vapply(names(leads), function(k) contains(s, leads[[k]]) == (k == q1), logical(1))) &&
              contains(s, "only the adaptive model leaves it less ready"))
    }

    # The new-outcome comparison records a note without a yes/no verdict.
    comparison$reset()
    n_before <- length(entries())
    session$setInputs(cmp_choice_1 = "new", cmp_reason_1 = " ")
    click("cmp_go_1")
    check("Step 2: a comparison needs a reason before it is recorded",
          contains(shown(output$cmp_msg), "Add a sentence") && length(entries()) == n_before)
    session$setInputs(cmp_reason_1 = "Hold starting values equal and compare learning.")
    click("cmp_go_1")
    check("Step 2: the new-outcome comparison opens",
          contains(shown(output$cmp), "What do you expect each model to predict?"))
    e <- entries()
    check("Step 2: the comparison adds one note, with the state at the end of training",
          length(e) == n_before + 1L && e[[length(e)]]$topic == "Comparison chosen" &&
            contains(e[[length(e)]]$result, "Multipliers at the end of compound training"))
    click("cmp_go_1")
    check("Step 2: a repeated click does not add a second note", length(entries()) == n_before + 1L)

    why_text <- c(start = "You attributed the adaptive gap to different starting values",
                  error = "You attributed the adaptive gap to different first-trial errors",
                  assoc = "Their associabilities differ",
                  A = "A is absent during O2 training")
    for (rw in c("zero", "pos", "neg")) for (ad in c("zero", "pos", "neg")) {
      for (why in names(why_text)) {
        comparison$reset()
        session$setInputs(q2_rw = rw, q2_ad = ad, q2_why = why)
        click("q2_go")
        s <- shown(output$q2)
        label <- paste("Step 2,", rw, ad, why)
        check(paste(label, "answers: both models' predictions"),
              contains(s, "gap is zero after every update") && contains(s, "D learns faster at first"))
        if (rw != "zero") {
          check(paste(label, "answers: the Rescorla-Wagner direction is corrected"),
                contains(s, paste("You expected", if (rw == "pos") "D" else "B",
                                  "to pull ahead under Rescorla")))
        }
        if (ad != "pos") {
          check(paste(label, "answers: the adaptive direction is corrected"),
                contains(s, if (ad == "zero") "Equal starting values do not reset" else
                  "B starts with lower associability"))
        }
        check(paste(label, "answers: a correction only for a wrong prediction"),
              contains(s, "You expected") == (rw != "zero" || ad != "pos"))
        check(paste(label, "answers: a correction of the reason only for a wrong reason"),
              contains(s, "You attributed") == (why != "assoc") &&
                contains(s, "initial advantage comes from associability") == (why != "assoc"))
        check(paste(label, "answers: the reason is answered and saved in the notes"),
              contains(s, why_text[[why]]) &&
                contains(entries()[[length(entries())]]$result, why_text[[why]]))
      }
    }
  })

  # Step 4 and the old-value question of Step 3
  testServer("shiny_app", {
    session$setInputs(
      gv = .5, ga = .5, t_model = "adaptive", t_k = 1,
      q4_try_rule = "sigmoid", q4_try_time = "1", x_n2 = 10, x_eta = .05,
      x_gv = 0, x_ga = 1, x_change = "none", x_e = "0", x_rule = "sigmoid",
      x_gamma = 8, x_theta = .5)
    click <- clicker(session)
    timing <- c(`1` = "You expected the largest difference at the first answer.",
                `10` = "You expected the largest difference at the last answer.",
                same = "You expected the same response difference at all three presentations.")
    for (rule in c("sigmoid", "threshold")) for (time in c("1", "6", "10")) {
      # Which cue has the higher probability of a yes at this presentation; under
      # both rules the difference is largest at the sixth.
      truth <- if (time == "1" || (rule == "threshold" && time == "10")) "same" else "D"
      for (dir in c("B", "same", "D")) for (peak in c("1", "6", "10", "same")) {
        responses$reset()
        session$setInputs(q4a_rule = rule, q4a_time = time, q4a_dir = dir, q4a_peak = peak)
        click("q4a_go")
        text <- shown(output$q4a)
        label <- paste("Step 4,", rule, "rule, presentation", time, "answers", dir, peak)
        if (dir != truth) {
          check(paste0(label, ": the direction is corrected"),
                contains(text, if (dir == "same") "You expected equal yes probabilities." else
                  paste0("You expected a higher probability of a yes for ", dir, ".")))
        }
        if (peak != "6") check(paste0(label, ": the timing is corrected"), contains(text, timing[[peak]]))
        check(paste0(label, ": the difference is largest at the sixth"),
              contains(text, "largest at the sixth"))
        check(paste0(label, ": a correction only for a wrong prediction"),
              contains(text, "You expected") == (dir != truth || peak != "6"))
      }
      for (more in c("yes", "no")) {
        answers$q4b <- NULL
        answers$q4b_msg <- NULL
        session$setInputs(q4b_more = more)
        click("q4b_go")
        text <- shown(output$q4b)
        label <- paste("Step 4,", rule, "rule, presentation", time, "more participants:", more)
        check(paste0(label, ": the feedback says what the responses at this presentation show"),
              contains(text, if (time == "1") "same learned value" else
                if (truth == "same") "same probability of a yes" else
                  if (rule == "threshold") "every participant answers" else
                    "The two probabilities differ by"))
        if (truth == "same" && more == "yes") {
          check(paste0(label, ": more participants cannot create a difference"),
                contains(text, "It cannot change the fact that this rule gives both cues the same response probability"))
        }
        if (truth != "same" && more == "no") {
          check(paste0(label, ": the rule keeps a response difference"),
                contains(text, "does preserve a response difference"))
        }
        check(paste0(label, ": a correction only for a wrong answer"),
              contains(text, "It cannot change the fact that this rule gives both cues the same response probability") ==
                (truth == "same" && more == "yes") &&
                contains(text, "does preserve a response difference") ==
                (truth != "same" && more == "no"))
      }
    }
    for (dir in c("zero", "pos", "neg")) for (when in c("before", "after", "none")) {
      assumptions$reset()
      session$setInputs(q3b_dir = dir, q3b_when = when)
      click("q3b_go")
      text <- shown(output$q3b)
      label <- paste("Step 3, old-value carryover,", dir, when, "answers")
      check(paste0(label, ": the gap is there before any O2 feedback"),
            contains(text, "The gap is there before any O2 feedback"))
      if (dir == "zero") check(paste0(label, ": unequal O1 values carry over"), contains(text, "two unequal O1 values"))
      if (dir == "neg") check(paste0(label, ": carryover keeps the order"), contains(text, "preserves that order"))
      if (when != "before") {
        check(paste0(label, ": carryover sets the values before learning"),
              contains(text, "Carryover sets the values before learning"))
      }
      check(paste0(label, ": a correction only for a wrong prediction"),
            contains(text, "two unequal O1 values") == (dir == "zero") &&
              contains(text, "preserves that order") == (dir == "neg") &&
              contains(text, "Carryover sets the values before learning") == (when != "before"))
    }
  })

  # Step 5, for every case, sample size and recording window
  app_data <- readRDS("shiny_app/data/inference_cases.rds")
  statement_text <- c(
    "The estimated D-minus-B response contrast is positive, with a 95% interval above zero.",
    "The comparison gave a preference for or against associability carryover under the stated rule.",
    "The model comparison showed which process generated the records.",
    "The first-answer check tested a prediction that all three candidates share.",
    "The proportion of experiments reaching a model preference varied across the six designs.")
  testServer("shiny_app", {
    click <- clicker(session)
    for (process in c("value", "associability", "reset")) for (n in c("40", "160")) {
      for (view in c("early", "full", "late")) {
        experiment$reset()
        session$setInputs(e_design_set = process, e_design_n = n, e_design_rec = view,
                          e_design_reason = "Keep the responses needed for this comparison.")
        click("e_design_go")
        design <- app_data$cases[[process]][[n]]$designs[[view]]
        detected <- design$effect[["detected"]] == 1
        label <- paste("Step 5,", process, n, view)
        for (choice in c("more", "none", "assoc", "value")) {
          answers$e1 <- NULL
          session$setInputs(e1_e = choice)
          click("e1_go")
          text <- shown(output$e1)
          if (choice %in% c("assoc", "value")) {
            check(paste0(label, " ", choice, ": a response contrast does not identify the process"),
                  contains(text, "does not say which process"))
          } else if (choice == "more" && !detected) {
            check(paste0(label, " ", choice, ": a positive contrast needs the whole interval above zero"),
                  contains(text, "whole interval to lie above zero"))
          } else if (choice == "none" && detected) {
            check(paste0(label, " ", choice, ": an interval above zero shows a positive contrast"),
                  contains(text, "whole interval lies above zero"))
          } else {
            check(paste0(label, " ", choice, ": a correct reading leads to the candidates"),
                  contains(text, "What could have produced"))
          }
          check(paste0(label, " ", choice, ": a correction only for a wrong reading"),
                contains(text, "You selected") ==
                  ((choice == "more" && !detected) || (choice == "none" && detected)))
        }
        # The comparison under each choice of g_V. The near-tie note appears only
        # when the carryover and reset candidates' held-out scores differ by less
        # than .01 with a standard error below .01.
        for (policy in c("zero", "estimated")) {
          answers$e2 <- NULL
          session$setInputs(e2_policy = policy, e2_reason = "")
          click("e2_go")
          text <- shown(output$e2_result)
          r <- design$scores[[policy]][design$scores[[policy]]$against == "reset", ]
          tie <- abs(r$difference) < .01 && r$se < .01
          check(paste0(label, " ", policy, ": the near-tie note appears only for a near tie"),
                contains(text, "nearly identical held-out scores") == tie)
        }
        for (choice in c("yes", "not")) {
          answers$e3 <- NULL
          session$setInputs(e3_which = choice)
          click("e3_go")
          check(paste0(label, " ", choice, ": a correction only for reading the ranking as the process"),
                contains(shown(output$e3), "sharing an incorrect assumption") == (choice == "yes"))
        }
        for (choice in c("equal", "carry", "depends")) {
          answers$e4 <- NULL
          session$setInputs(e4_f = choice)
          click("e4_go")
          text <- shown(output$e4)
          check(paste0(label, " ", choice, ": a correction only for a wrong answer about the first answers"),
                contains(text, "predicts equal first-answer probabilities too") == (choice == "carry") &&
                  contains(text, "response function is shared") == (choice == "depends") &&
                  contains(text, "their ranking cannot test it"))
        }
        if (!is.null(design$first)) {
          rejects <- design$first$p < .05
          for (choice in c("consistent", "disagree", "value")) {
            answers$e5 <- NULL
            session$setInputs(e5_h = choice)
            click("e5_go")
            text <- shown(output$e5)
            if (choice == "consistent" && rejects) {
              check(paste0(label, " ", choice, ": a rejected prediction is corrected"),
                    contains(text, "but this test rejects it"))
            } else if (choice == "disagree" && !rejects) {
              check(paste0(label, " ", choice, ": failing to reject is not a disagreement"),
                    contains(text, "did not find clear evidence"))
            } else if (choice == "value") {
              check(paste0(label, " ", choice, ": the check does not identify old-value carryover"),
                    contains(text, "cannot show that old values carried over"))
            }
            check(paste0(label, " ", choice, ": a correction only for a wrong reading"),
                  contains(text, "You chose") ==
                    ((choice == "consistent" && rejects) || (choice == "disagree" && !rejects)))
          }
        }
        # The conclusions, judged with g_V estimated, the last choice above.
        session$setInputs(e6_pick = "5")
        click("e6_go")
        result <- entries()[[length(entries())]]$result
        supported <- c(detected, design$decision$estimated != "unresolved", FALSE,
                       !is.null(design$first), TRUE)
        for (i in 1:5) {
          check(paste0(label, ": statement ", i, if (supported[i]) " is supported" else " is not supported"),
                contains(result, paste0(i, ". ", statement_text[i],
                                        if (supported[i]) " Supported." else " Not supported.")))
        }
      }
    }
  })
  cat(sprintf("All %d feedback checks passed.\n", n_checks))
})
