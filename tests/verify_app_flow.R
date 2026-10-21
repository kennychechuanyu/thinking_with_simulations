# A walk through the companion app (shiny_app/) with shiny::testServer. The
# checks answer the questions as a reader would and confirm what the app shows
# next: the four comparisons of Step 2, with the new outcome chosen last and,
# after starting again, first; the old-value carryover comparison and the two
# further explorations of Step 3; the response functions of Step 4; the experiment
# under several designs (both sizes, all three sets of recorded presentations, both orders of
# the old-value carryover choice); Step 6; starting again; the axis ranges of
# the contrast and score plots; and the notes with the settings behind each
# result. Warnings count as failures. Stops at the first failure.
#   Rscript tests/verify_app_flow.R

local({
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  suppressPackageStartupMessages({ library(shiny); library(bslib); library(ggplot2) })

  n_checks <- 0L
  check <- function(label, ok) {
    if (!isTRUE(ok)) stop(sprintf("FAIL  %s", label), call. = FALSE)
    n_checks <<- n_checks + 1L
    cat(sprintf("  PASS  %s\n", label))
  }
  # The text as a reader sees it: runs of spaces and line breaks count as one space.
  shown <- function(x) gsub("\\s+", " ", paste(as.character(x$html), collapse = "\n"))
  has <- function(x, ...) all(vapply(c(...), grepl, logical(1), x = shown(x), fixed = TRUE))
  is_png <- function(x) grepl("^data:image/png", x$src)

  cat("Checks of the companion app flow\n")
  testServer("shiny_app", {
    # The app's own values and helpers, for comparing what it shows.
    app <- parent.env(session$env)
    for (nm in c("num", "num0", "pct", "prob", "at", "rates", "base", "trained", "more", "co",
                 "ad_B", "ad_D",
                 "run_inquiry", "settings_with", "response_prob", "cases", "ddm_summary",
                 "decision_text", "near_reset_scores", "plot_contrast", "plot_scores",
                 "plain_text"))
      assign(nm, get(nm, envir = app))

    # Inputs that the browser sends when the page loads.
    session$setInputs(t_model = "adaptive", t_k = 1, gv = .5, ga = .5,
                      q4_try_rule = "sigmoid", q4_try_time = "1",
                      x_n2 = 10, x_eta = .05, x_gv = 0, x_ga = 1, x_change = "none",
                      x_e = "0", x_rule = "sigmoid", x_gamma = 8, x_theta = .5)
    click <- function(id) {
      now <- isolate(input[[id]])
      do.call(session$setInputs, setNames(list(if (is.null(now)) 1 else now + 1), id))
    }
    answer <- function(id, ...) { session$setInputs(...); click(paste0(id, "_go")) }
    done <- function(id) isTRUE(output[[paste0("done_", id)]])
    last_note <- function() { e <- entries(); e[[length(e)]] }

    # Step 1
    check("Step 1: the first compound trial shows both models' numbers",
          has(output$first_trial, num(.2785), num(.0334), ".150", ".500 &rarr; .458"))
    check("Step 1: exact errors print without decimals",
          has(output$first_trial, "<td>1<br>") && !has(output$first_trial, "1.000"))
    check("Step 1: the table after training gives the multipliers",
          has(output$trained_table, num(trained$adaptive$mult_B), num(trained$adaptive$mult_D)))
    click("q1_go")
    check("Step 1: an unanswered question gets a message", has(output$q1_msg, "Choose an answer"))
    answer("q1", q1_left = "nothing")
    check("Step 1: the feedback gives the associability difference and Step 2 opens",
          done("q1") && has(output$q1, num(trained$adaptive$alpha_B),
                            num(trained$adaptive$mult_B), num(trained$adaptive$mult_D)))

    # Step 2: the four comparisons
    click("cmp_go_1")
    check("Step 2: a comparison must be chosen first",
          has(output$cmp_msg, "Choose a comparison first."))
    session$setInputs(cmp_choice_1 = "cont", cmp_reason_1 = "   "); click("cmp_go_1")
    check("Step 2: a reason is required before the result",
          has(output$cmp_msg, "Add a sentence") && !has(output$cmp, "Ten more trials with O1"))
    session$setInputs(cmp_choice_1 = "cont", cmp_reason_1 = "B and D already differ.")
    click("cmp_go_1")
    check("Step 2: continuing with O1 draws both plots",
          is_png(output$p_cont_curves) && is_png(output$p_cont_gap) &&
            has(output$cmp, "Ten more trials with O1", "Does this comparison isolate"))
    check("Step 2: Step 3 stays closed after an uninformative comparison", !done("q2"))
    session$setInputs(cmp_tells_cont = "yes"); click("cmp_check_cont")
    check("Step 2: the numerical differences are noted without isolating the history effect",
          
          has(output$cmp, "numerical predictions differ", "mixes the starting-value difference",
              "Choose another comparison"))
    check("Step 2: the note records the comparison and its result",
          grepl("Keep training B and D", last_note()$question) &&
            grepl(num(at(co$adaptive$gap, 10)), last_note()$result, fixed = TRUE))
    click("cmp_another")
    check("Step 2: only the untried comparisons are offered",
          has(output$cmp, 'value="more"', 'value="across"', 'value="new"') && !has(output$cmp,
                                                                                   'value="cont"'))
    session$setInputs(cmp_choice_2 = "more",
                      cmp_reason_2 = "Perhaps the extra trials separate the predictions.")
    click("cmp_go_2")
    check("Step 2: more training shows 10 and 30 trials for both models",
          has(output$cmp, num(more$rw$V_B), num(more$adaptive$V_D)))
    session$setInputs(cmp_tells_more = "no"); click("cmp_check_more")
    check("Step 2: blocking persists without isolating the effect",
          has(output$cmp, "preserved blocking", "cannot separate"))
    click("cmp_another")
    session$setInputs(cmp_choice_3 = "across", cmp_reason_3 = "Compare the numerical predictions.")
    click("cmp_go_3")
    session$setInputs(cmp_tells_across = "no"); click("cmp_check_across")
    check("Step 2: the cross-model comparison is traced to the error each model uses",
          has(output$cmp, "models use different errors and different learning rates"))
    click("cmp_another")
    session$setInputs(cmp_choice_4 = "new",
                      cmp_reason_4 = "Equal starting values might isolate learning rate.")
    click("cmp_go_4")
    check("Step 2: the new outcome opens the prediction and offers no further comparison",
          has(output$cmp, "What do you expect each model to predict?") &&
            !has(output$cmp, "Choose another comparison"))
    answer("q2", q2_rw = "pos", q2_ad = "pos", q2_why = "start")
    check("Step 2: both predictions judged, the reason corrected",
          done("q2") && has(output$q2, "gap is zero after every update", "D learns faster at first",
                                      "Both cues start at 0 for O2",
                                          "advantage comes from associability"))
    check("Step 2: the plots render", is_png(output$p2_curves) && is_png(output$p2_gap))
    session$setInputs(t_model = "adaptive", t_k = 9)
    check("Step 2: the ninth update shows B gaining more than D",
          grepl("B gains more than D", shown(output$trial_note)) &&
            has(output$trial_table, "Update 9", num(at(ad_B$gain, 8)), num(at(ad_D$gain, 8))))
    session$setInputs(t_model = "rw")
    check("Step 2: the RW table has no separately updated associability",
          has(output$trial_table, "<td>—</td>"))
    session$setInputs(t_model = "adaptive")
    answer("q2b", q2b_n = "stop")
    check("Step 2: the narrowing question is answered",
          done("q2b") && has(output$q2b, "D keeps learning"))

    # Step 3: core comparison, then the two further explorations
    answer("q3a", q3a_claim = "none")
    check("Step 3: the conclusion is saved before the core carryover comparison",
          done("q3a") && !done("q3b") && !done("q3c"))
    answer("q3b", q3b_dir = "zero", q3b_when = "after", q3b_reason = "D starts higher.")
    check("Step 3: the old-value prediction is corrected",
          has(output$q3b, "two unequal O1 values", "Carryover sets the values before learning",
              "The gap is there before any O2 feedback"))
    session$setInputs(gv = .5)
    r5 <- run_inquiry(settings_with(g_V = .5))
    check("Step 3: the slider text gives both models' gaps",
          has(output$gv_text, paste0("already ", num(at(r5$rw$gap, 0)), " under"),
              paste0("and ", num(at(r5$adaptive$gap, 0)), " under the adaptive model"),
              paste0("the gaps are ", prob(at(r5$rw$gap, 8)), " ("),
              paste0("and ", num(at(r5$adaptive$gap, 8)), " (adaptive model)")) &&
            is_png(output$p3_gap) && is_png(output$p3_grid))
    check("Step 3: the gaps are equal at g_V = .895, as in Section 4.3",
          has(output$gv_text, "Above about", ".895,"))
    check("Step 4 stays closed before the revised conclusion", !done("q3c"))
    answer("q3c", q3c_rev = "keep", q3c_reason = "My version.")
    check("Step 3: keeping 'cannot be told apart' explains the two sources of a gap",
          done("q3c") && has(output$q3c, "both start with a gap", "when the gap first appears"))
    n3 <- last_note()
    check("Step 3: the revised-conclusion note records the g_V examined and both sets of gaps",
          grepl("g_V = .5", n3$result, fixed = TRUE) &&
            grepl(num(at(base$adaptive$gap, 8)), n3$result, fixed = TRUE) &&
            grepl("Your own version: My version.", n3$answer, fixed = TRUE))
    half <- run_inquiry(settings_with(g_alpha = .5))
    answer("q3d", q3d_h = "gone")
    check("Step 3: half carryover halves the first gap",
          has(output$q3d, num(at(half$adaptive$gap, 1)), num(half$start$alpha_B),
              "specified separately"))
    session$setInputs(ga = .25)
    quarter <- run_inquiry(settings_with(g_alpha = .25))
    check("Step 3: the associability slider text follows the engine",
          has(output$ga_text, num(quarter$start$alpha_B), prob(at(quarter$adaptive$gap, 8))) &&
            is_png(output$p3_ga))
    up <- run_inquiry(settings_with(e_offset = 1e-6))
    down <- run_inquiry(settings_with(e_offset = -1e-6))
    salience_row <- function(label, r) {
      paste0("<td>", label, "</td> <td>", num(r$trained$adaptive$alpha_D), "</td> <td>",
             num(at(r$adaptive$gap, 8)), "</td>")
    }
    answer("q6", q6_c = "smaller")
    check("Step 3: the matched-control test corrects the answer and gives a row for each salience",
          has(output$q6, "in proportion to the difference", "The gap stays about the same",
              salience_row("Same as D", base), salience_row("One part in a million higher", up),
              salience_row("One part in a million lower", down)))
    # testServer does not apply input updates, so record what the Explore link sends.
    # The stubs go in the environment where the files in R/ were sourced, which app.R also sees.
    shared <- parent.env(app)
    sent <- list()
    assign("updateRadioButtons", function(session, inputId, ..., selected = NULL)
      sent[[inputId]] <<- selected, envir = shared)
    assign("nav_select", function(id, selected = NULL, session) sent[[id]] <<- selected,
           envir = shared)
    click("q6_explore")
    rm("updateRadioButtons", "nav_select", envir = shared)
    check("Step 3: the Explore link selects E more salient and opens Explore",
          identical(sent$x_e, "1e-6") && identical(sent$x_change, "none") && identical(sent$tabs,
                                                                                       "Explore"))

    # Step 4: the response functions
    sig <- function(k) response_prob(at(ad_D$V, k), "sigmoid") - response_prob(at(ad_B$V, k),
                                                                               "sigmoid")
    answer("q4a", q4a_rule = "sigmoid", q4a_time = "1", q4a_dir = "same", q4a_peak = "6")
    check("Step 4: the first answer has equal probabilities; the sixth the largest difference",
          has(output$q4a, "the same probability of a yes for both cues",
              prob(sig(5)), "largest at the sixth"))
    check("Step 4: the note carries rule, slope, midpoint and presentation",
          grepl("sigmoid, gamma = 8, theta = .5; presentation 1", last_note()$settings,
                fixed = TRUE))
    answer("q4b", q4b_more = "yes")
    check("Step 4: equal learned values leave nothing for more participants to reveal",
          has(output$q4b, "no difference for more participants to reveal"))
    n_before <- length(entries())
    session$setInputs(q4_try_rule = "threshold", q4_try_time = "6"); click("q4_try_go")
    session$setInputs(q4_try_rule = "threshold", q4_try_time = "10"); click("q4_try_go")
    check("Step 4: extra tries show their results and add notes",
          has(output$q4_tries, "Threshold", "unrounded values", "<td>1</td>", "<td>0</td>") &&
            length(entries()) == n_before + 2)

    # Step 5: the experiment, late records first
    design <- function(set, n, rec) cases[[set]][[n]]$designs[[rec]]
    answer("e_design", e_design_set = "value", e_design_n = "160", e_design_rec = "late",
           e_design_reason = "   ")
    check("Experiment: the recorded presentations need a reason before results appear",
          
          !done("e_design") && has(output$e_design_msg, "Write a sentence"))
    answer("e_design", e_design_set = "value", e_design_n = "160", e_design_rec = "late",
           e_design_reason = "Late responses show how much learning remains near the end.")
    check("Experiment: the design line and the records appear",
          done("e_design") && has(output$e_design_line, "Case 1", "160 participants", "8–10") &&
            is_png(output$e_records) && is_png(output$e_contrast))
    effect <- design("value", "160", "late")$effect
    check("Experiment: the effect is the saved estimate for this design",
          has(output$e_effect, num(effect[["estimate"]]), num(effect[["lower"]]),
              "lies above zero"))
    answer("e1", e1_e = "assoc")
    check("Experiment: over-reading the records is corrected",
          has(output$e1, "does not say which process"))
    answer("e2", e2_policy = "estimated", e2_reason = "")
    scores <- design("value", "160", "late")$scores$estimated
    check("Experiment: the scores and decision for the late design, g_V estimated",
          has(output$e2_result, num0(scores$difference[scores$against == "fixed"], 1),
              paste0("Under the stated rule, the comparison ",
                     decision_text[[design("value", "160", "late")$decision$estimated]])) &&
            is_png(output$e2_plot_estimated) && !isTRUE(output$e_seen_zero))
    check("Experiment: the other choice is offered before the next question",
          has(output$e2_other_link, "fixed at 0") && !done("e3"))
    click("e2_other")
    check("Experiment: the g_V = 0 result is shown, run for this page",
          isTRUE(output$e_seen_zero) && has(output$e2_result,
                                            "run for this page with the study's code"))
    answer("e3", e3_which = "yes")
    check("Experiment: the ranking question does not reveal the source",
          done("e3") && has(output$e3, "separate question") &&
            !grepl("generalization|Carried associability|generated by", shown(output$e3)))
    answer("e4", e4_f = "carry")
    check("Experiment: the shared prediction is explained",
          has(output$e4, "predicts equal first-answer probabilities too"))
    check("Experiment: late records cannot make the first-answer check; the reveal opens",
          
          has(output$e_first, "begin at presentation 8",
              "cannot be made") && is.null(output$e5_box$html) &&
            isTRUE(output$e_revealed))
    check("Experiment: the reveal names the process and the late-design rates",
          has(output$e_truth, "Value generalization") && is_png(output$e_contrast_truth) &&
            has(output$e_rates, "8–10 (your design)", "74.0%", "the first parameter set",
                "four generating parameter sets", "100 experiments"))
    session$setInputs(e6_pick = c("1", "4")); click("e6_go")
    check("Experiment: the conclusions are judged against this design, and Step 6 opens",
          done("e6") && has(output$e6, "1. The estimated D-minus-B", "You selected this",
                            num(effect[["estimate"]])) &&
            has(output$e6, "did not include the first presentation"))
    check("Experiment: the completed notes preserve generating parameters and the seed",
          grepl("Generating parameters: mu =", last_note()$settings, fixed = TRUE) &&
            grepl("seed = 610005", last_note()$settings, fixed = TRUE))

    # The experiment again: full records, g_V fixed at 0 first, all three cases
    for (set in c("value", "associability", "reset")) {
      click("e_again")
      check(paste0("Again (", set, "): starts with no answers"),
            !done("e_design") && !done("e1") && !done("e6") && !isTRUE(output$e_seen_zero))
      answer("e_design", e_design_set = set, e_design_n = "160", e_design_rec = "full",
             e_design_reason = "Keep initial expectations and the full learning curve.")
      full <- design(set, "160", "full")
      answer("e1", e1_e = if (full$effect[["detected"]] == 1) "more" else "none")
      check(paste0("Again (", set, "): reading of the records matches detection"),
            has(output$e1, num(full$effect[["estimate"]]),
                if (full$effect[["detected"]] == 1) "The interval lies above zero."
                else if (full$effect[["upper"]] < 0) "The interval lies below zero"
                else "The interval includes zero"))
      answer("e2", e2_policy = "zero", e2_reason = "")
      check(paste0("Again (", set, "): the g_V = 0 result gives the saved decision"),
            isTRUE(output$e_seen_zero) &&
              has(output$e2_result, paste0("Under the stated rule, the comparison ",
                                           decision_text[[full$decision$zero]])) &&
              has(output$e2_result, "run for this page") == (set == "reset"))
      if (set == "reset") check("Again: a near tie is explained only when there is one",
            has(output$e2_result, "nearly identical held-out scores") ==
              near_reset_scores(full$scores$zero))
      answer("e3", e3_which = "not")
      answer("e4", e4_f = "equal")
      check(paste0("Again (", set, "): the first-answer check is shown"),
            has(output$e_first, "Yes to D, no to B") && has(output$e5_box, 'id="e5"') &&
              has(output$e5, "What does this check tell you?") && !isTRUE(output$e_revealed))
      rejected <- full$first$p < .05
      answer("e5", e5_h = "consistent")
      check(paste0("Again (", set, "): the check is judged"),
            isTRUE(output$e_revealed) &&
              has(output$e5, if (rejected) "rejects equal" else "finds no clear evidence"))
      check(paste0("Again (", set, "): the reveal names the process"),
            has(output$e_truth, c(value = "Value generalization",
                                  associability = "Carried associability",
                                  reset = "Transfer-only adaptation")[[set]]))
      session$setInputs(e6_pick = character(0)); click("e6_go")
      check(paste0("Again (", set, "): conclusions explained with none selected"),
            done("e6") && has(output$e6, "You left this out"))
      check(paste0("Again (", set, "): statement 5 is marked supported"),
            grepl("The proportion of experiments reaching a model preference varied across the six designs. Supported.",
                  last_note()$result, fixed = TRUE))
      if (set == "reset") {
        reset_rates <- rates[rates$scenario == "reset", ]
        preferences <- reset_rates$carry + reset_rates$no_carry
        check("Again: the feedback says a preference can arise by chance",
              has(output$e6, pct(min(preferences)), pct(max(preferences)), "finite random samples",
                  "most common outcome"))
      }
    }
    click("e_again")
    answer("e_design", e_design_set = "associability", e_design_n = "40", e_design_rec = "early",
           e_design_reason = "The first six preserve the onset of learning.")
    answer("e1", e1_e = "more"); answer("e2", e2_policy = "estimated", e2_reason = "")
    check("Again: 40 participants with the first six leave the comparison unresolved",
          has(output$e2_result, "Under the stated rule, the comparison is unresolved"))
    click("e2_other"); answer("e3", e3_which = "not"); answer("e4", e4_f = "equal")
    check("Again: early records include the first answer", has(output$e_first, "Yes to D, no to B"))
    answer("e5", e5_h = "value")
    check("Again: 'old values carried over' is corrected", has(output$e5, "cannot show that"))
    check("Again: the rates table marks the early design",
          has(output$e_rates, "1–6 (your design)"))
    session$setInputs(e6_pick = c("4", "5")); click("e6_go")
    check("Again: the repeated-experiment statement is supported",
          grepl("The proportion of experiments reaching a model preference varied across the six designs. Supported.",
                last_note()$result, fixed = TRUE))

    # Step 6
    answer("x1", x1_more = "yes")
    check("Step 6: more accuracy data cannot separate the settings",
          has(output$x1, "Accuracy depends"))
    answer("x2", x2_rec = "conf", x2_longer = "second")
    check("Step 6: the decision times are revealed with both means",
          has(output$x2, "first setting", num(ddm_summary()$mean[1], 2), num(ddm_summary()$mean[2],
                                                                             2)) &&
            is_png(output$p_ddm))
    answer("x3", x3_reason = "  ")
    check("Step 6: the open challenge requires the reader's own reasoning",
          !done("x3") && has(output$x3_msg, "Write a sentence"))
    answer("x3", x3_reason = "Perception and movement could take longer even with the same drift.")
    check("Step 6: the challenge saves the reader's words before revealing a discussion",
          done("x3") && grepl("Perception and movement", last_note()$answer, fixed = TRUE) &&
            has(output$x3, "nondecision time", "Wider boundaries") &&
            has(output$x3_prompt, "lower drift rate"))
    click("e_again")
    check("Experiment restart: the Step 6 answers are cleared, and the notes remain",
          !done("e6") && !done("x1") && !done("x2") && !done("x3") &&
            grepl("Perception and movement", last_note()$answer, fixed = TRUE))

    # Starting again
    n_notes <- length(entries())
    click("restart")
    check("Start again: everything is cleared and the notes are kept",
          !done("q1") && !done("q2") && !done("q3c") && !done("e_design") && !done("x3") &&
            has(output$cmp, "Which comparison would you run?") &&
            length(entries()) == n_notes)
    answer("q1", q1_left = "adaptive")
    session$setInputs(cmp_choice_1 = "new", cmp_reason_1 = "Compare learning from equal values.")
    click("cmp_go_1")
    check("Start again: the new outcome can be chosen first",
          has(output$cmp, "What do you expect each model to predict?"))
    answer("q2", q2_rw = "zero", q2_ad = "pos", q2_why = "assoc")
    answer("q3a", q3a_claim = "support")
    answer("q3b", q3b_dir = "pos", q3b_when = "before")
    session$setInputs(gv = 1)
    answer("q3c", q3c_rev = "qualify")
    check("Start again: the qualified conclusion gets the scope feedback",
          has(output$q3c, "Recording the first response",
              "lets you check for this distinction") && done("q3c"))
    answer("q4a", q4a_rule = "threshold", q4a_time = "10", q4a_dir = "D", q4a_peak = "10")
    check("Step 4, threshold at the last answer: the same answer for both; largest at the sixth",
          has(output$q4a, "the same answer for both cues", "largest at the sixth"))
    answer("q4b", q4b_more = "no")
    check("Step 4, threshold at the last answer: more participants cannot help",
          has(output$q4b, "same probability", "does not express"))
    answer("e_design", e_design_set = "value", e_design_n = "160", e_design_rec = "full",
           e_design_reason = "Keep the first response and the whole curve.")
    check("Step 5 after a threshold demonstration: records still use the sigmoid experiment",
          has(output$e_design_line, "Case 1") &&
            grepl("sigmoid/Bernoulli observations", last_note()$settings, fixed = TRUE) &&
            has(output$e_effect, num(design("value", "160", "full")$effect[["estimate"]])))
    session$setInputs(e_design_set = "reset", e_design_n = "40", e_design_rec = "late")
    check("Experiment: changing stale inputs cannot alter the submitted case and design",
          answers$e_design$set == "value" && answers$e_design$n == "160" &&
            has(output$e_design_line, "Case 1", "160 participants", "1–10"))

    # Step 5: the near-tie explanation and the axes of the contrast and score plots
    near_tie <- data.frame(against = c("fixed", "reset"), difference = c(.001, 2), se = c(.001, 1))
    check("Near-tie note: a near tie with the fixed-rate candidate does not trigger it",
          !near_reset_scores(near_tie))
    near_tie$difference <- c(2, .001); near_tie$se <- c(1, .001)
    check("Near-tie note: a near tie with the reset candidate triggers it",
          near_reset_scores(near_tie))
    wide <- design("value", "160", "full")
    wide$by_presentation$lower[1] <- -.6
    wide$by_presentation$upper[1] <- .7
    built <- ggplot_build(plot_contrast(cases$value[["160"]], wide))
    yrange <- built$layout$panel_params[[1]]$y.range
    check("Contrast plot: the axis holds the widened interval",
          built$data[[2]]$ymin[1] == -.6 && built$data[[2]]$ymax[1] == .7 &&
            yrange[1] <= -.6 && yrange[2] >= .7)
    wide_scores <- data.frame(against = c("fixed", "reset"),
                              difference = c(-260, 380), se = c(55, 90))
    built <- ggplot_build(plot_scores(wide_scores))
    xrange <- built$layout$panel_params[[1]]$x.range
    check("Score plot: estimates and complete two-SE intervals fit on both sides of zero",
          xrange[1] <= -370 && xrange[2] >= 560 &&
            all(is.finite(built$data[[2]]$xmin)) && all(is.finite(built$data[[2]]$xmax)))
    saved_scores <- unlist(lapply(cases, function(by_size) lapply(by_size, function(cas)
      lapply(cas$designs, function(des) lapply(des$scores, function(scores) {
        xrange <- ggplot_build(plot_scores(scores))$layout$panel_params[[1]]$x.range
        xrange[1] <= min(0, scores$difference - 2 * scores$se) &&
          xrange[2] >= max(0, scores$difference + 2 * scores$se)
      })))), use.names = FALSE)
    check("Score plot: every saved case, design and carryover policy shows its full intervals",
          length(saved_scores) == 36L && all(saved_scores))

    # Notes
    check("Notes: mathematical inequalities survive HTML-to-text conversion",
          plain_text("contrast < -2 SE; <i>g</i><sub>V</sub> > 0") == "contrast < -2 SE; g_V > 0")
    notes <- entries()
    check("Notes: every entry carries its settings",
          all(vapply(notes, function(entry) !is.null(entry$settings), logical(1))))
    txt <- readLines(output$notes_download, encoding = "UTF-8")
    check("Notes: the download includes the settings and the designs",
          any(grepl("Case 1, 160 participants, presentations 8–10 recorded", txt, fixed = TRUE)) &&
            any(grepl("sigmoid, gamma = 8, theta = .5; presentation 1", txt, fixed = TRUE)) &&
            any(grepl("alpha_init = .5, eta = .05", txt, fixed = TRUE)) &&
            any(grepl("either score difference < -2 SE, otherwise unresolved", txt,
                      fixed = TRUE)) &&
            any(grepl("^  Result:     ", txt)))
  })

  cat(sprintf("\nAll %d app flow checks passed.\n\n", n_checks))
})
