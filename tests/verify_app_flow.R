# A walk through the companion app (shiny_app/) with shiny::testServer. The
# checks answer the questions as a reader would and confirm what the app shows
# next: the result of Box 1, held back until the code has been checked; the four
# comparisons of Step 2; the changes to the model, the two carryover fractions,
# the training settings and the rules of Step 3; the response functions of
# Step 4; the experiment of Steps 5 and 6 under several designs, with and
# without the first pear trial; Step 7; trying another design; starting again;
# Explore; the notes with the settings behind each result; and character
# entities shown literally on the page. Warnings count as failures. Stops at the
# first failure.
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
  # An output that waits for an earlier answer is not drawn yet.
  waits <- function(expr) tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  # A character entity such as &eta; in text not marked as HTML reaches the
  # page with its ampersand escaped and is shown literally.
  escaped <- "&amp;([a-zA-Z]+|#[0-9]+);"
  # The ids of the outputs whose HTML the server draws.
  output_ids <- function(html) unlist(regmatches(html,
    gregexpr('(?<=id=")[^"]+(?="[^>]*class="[^"]*shiny-html-output)', html, perl = TRUE)))

  cat("Checks of the companion app flow\n")
  testServer("shiny_app", {
    # The app's own values and helpers, for comparing what it shows.
    app <- parent.env(session$env)
    for (nm in c("num", "num0", "pct", "prob", "at", "rates", "zero_rates", "first_rates",
                 "response_rates", "base", "trained", "rw_tr", "checks", "compound", "low_S",
                 "runs", "ga_half", "gv90", "gv_cross", "rule_runs", "one_percent", "responses",
                 "cases", "dd", "plain_text", "notes_text", "plot_scores"))
      assign(nm, get(nm, envir = app))

    # Inputs that the browser sends when the page loads.
    session$setInputs(t1_trial = 12, t_model = "adaptive", t_k = 1, ga = .5, gv = .5,
                      n3_compound = 10, n3_eta = .05, r3_rule = "proportional", r3_c = "0",
                      q4_try_rule = "sigmoid", q4_try_q = 1,
                      x_n2 = 10, x_eta = .05, x_gv = 0, x_ga = 1, x_change = "none",
                      x_c = "0", x_rule = "sigmoid", x_gamma = 8, x_theta = .5)
    click <- function(id) {
      now <- isolate(input[[id]])
      do.call(session$setInputs, setNames(list(if (is.null(now)) 1 else now + 1), id))
    }
    answer <- function(id, ...) { session$setInputs(...); click(paste0(id, "_go")) }
    done <- function(id) isTRUE(output[[paste0("done_", id)]])
    last_note <- function() { e <- entries(); e[[length(e)]] }

    page <- as.character(htmltools::renderTags(get("ui", envir = app))$html)
    check("The page shows no character entity with an escaped ampersand", !grepl(escaped, page))

    # Step 1
    check("Step 1: the first trials are worked out, with exact values printed as such",
          has(output$first_trials, ".7215", ".2785", ".0334 each", "A .7549, B .0334") &&
            grepl("<td>1</td>", shown(output$first_trials), fixed = TRUE))
    click("q1a_go")
    check("Step 1: an unanswered question gets a message", has(output$q1a_msg, "Choose an answer"))
    answer("q1a", q1a_dir = "less", q1a_why = "finished", q1a_reason = "the tone's value")
    check("Step 1: the expectation is saved and the check of the code opens",
          done("q1a") && has(output$q1a, "Your expectation is saved") && !done("q1b"))
    check("Step 1: the note keeps the expectation without the result",
          last_note()$topic == "Your expectation for the second compound trial" &&
            is.null(last_note()$result) && grepl("the tone's value", last_note()$answer, fixed = TRUE))
    check("Step 1: the result waits for the check of the code", waits(output$q1a_result))
    answer("q1b", q1b_test = "zero")
    check("Step 1: the check shows all three programs, with .048, .084 and .04224",
          has(output$q1b, "That test catches a program that updates the tone first",
              ".048", ".084", ".04224", ".1056"))
    check("Step 1: the result follows, comparing the reason as well as the direction",
          has(output$q1a_result, "Your expectation", ".7549", ".7883", ".2117", ".0254",
              "The direction is right, but the reason is not.", "exactly as much as the light"))
    e <- entries()
    check("Step 1: the notes add the check and then the result",
          e[[length(e) - 1]]$topic == "Checking the code" &&
            last_note()$topic == "The second compound trial" &&
            grepl(".2117", last_note()$result, fixed = TRUE))
    check("Step 1: both plots are drawn", is_png(output$p1_values) && is_png(output$p1_error))
    session$setInputs(t1_trial = 11)
    check("Step 1: the trial table works out the first compound trial",
          has(output$t1_table, "Trial 11", ".7215 + 0 = .7215", "1 &minus; .7215 = .2785",
              ".12 &times; .2785 = .0334"))
    session$setInputs(t1_trial = 3)
    check("Step 1: on a tone-only trial the light is absent",
          has(output$t1_table, "from the tone alone", "&mdash;"))
    answer("q1c", q1c_show = "tone")
    check("Step 1 is complete, and the comparison is corrected",
          done("q1c") && has(output$q1c, "changes two things at once", "blocking is defined"))

    # Step 2
    answer("q2a", q2a_first = "less")
    check("Step 2: the first compound trials, with .458, .117 and .128",
          done("q2a") && has(output$q2a, "That is what Rescorla", ".458", ".117", ".128",
                             "Mackintosh proposed that associability falls"))
    check("Step 2: the table after the compound trials gives the learned values only",
          has(output$trained_table, num(trained$adaptive$V_B), num(trained$rw$V_D)) &&
            !has(output$trained_table, "learning rate"))
    answer("q2b", q2b_left = "nothing")
    check("Step 2: the feedback gives the learning rates and the check with salience .24",
          done("q2b") && has(output$q2b, "changes more than learned value",
                             num(trained$adaptive$rate_B), num(trained$adaptive$rate_D),
                             num(low_S$adaptive$alpha_B), num(low_S$adaptive$rate_B)))
    click("cmp_go_1")
    check("Step 2: a comparison must be chosen first", has(output$cmp_msg, "Choose a comparison first."))
    session$setInputs(cmp_choice_1 = "cont", cmp_reason_1 = "   "); click("cmp_go_1")
    check("Step 2: a reason is required before the result",
          has(output$cmp_msg, "Add a sentence") && !has(output$cmp, "Ten more trials with the apple"))
    session$setInputs(cmp_choice_1 = "cont", cmp_reason_1 = "B and D already differ.")
    click("cmp_go_1")
    check("Step 2: continuing with the apple draws both plots",
          is_png(output$p_cont_curves) && is_png(output$p_cont_gap) &&
            has(output$cmp, "Ten more trials with the apple", "Does this comparison isolate"))
    session$setInputs(cmp_tells_cont = "yes"); click("cmp_check_cont")
    check("Step 2: the comparison is explained and another can be chosen",
          has(output$cmp, "mixes these two influences", "Choose another comparison") &&
            last_note()$topic == "Comparison tried")
    click("cmp_another")
    check("Step 2: only the untried comparisons are offered",
          has(output$cmp, 'value="more"', 'value="across"', 'value="pear"') &&
            !has(output$cmp, 'value="cont"'))
    session$setInputs(cmp_choice_2 = "pear", cmp_reason_2 = "Equal starting values."); click("cmp_go_2")
    check("Step 2: the pear opens Box 2 and no further comparison is offered",
          has(output$cmp, "most favorable to telling the models apart", "10 B alone") &&
            !has(output$cmp, "Choose another comparison") && last_note()$topic == "Comparison chosen")
    answer("q2c", q2c_rw = "less", q2c_ad = "equal", q2c_why = "start")
    check("Step 2: Box 2 corrects each wrong answer and gives .071, .150 and .079",
          done("q2c") && has(output$q2c, "You expected B to gain less under",
                              "You expected equal gains under the adaptive model",
                              "Both lights start the pear trials at zero", ".071", ".150", ".079"))
    check("Step 2: the pear plots are drawn", is_png(output$p2_curves) && is_png(output$p2_gap))
    session$setInputs(t_model = "adaptive", t_k = 9)
    check("Step 2: inside the ninth pear trial, B gains more and the gap narrows",
          has(output$trial_table, "Pear trial 9", num(at(base$adaptive$B$gain, 8))) &&
            has(output$trial_note, "B gains more than D, so the gap narrows"))
    session$setInputs(t_model = "rw")
    check("Step 2: under Rescorla-Wagner the lights gain the same",
          has(output$trial_note, "B and D gain the same"))
    answer("q2d", q2d_n = "overtake")
    check("Step 2 is complete; the gap stays positive and the choices are listed",
          done("q2d") && has(output$q2d, "B never overtakes D", ".051", ".048", ".337", ".265"))

    # Step 3
    answer("q3a", q3a_pr = "gone", q3a_supply = "same")
    check("Step 3: the interventions, with .803, .122, .012, .192 and .068",
          done("q3a") && has(output$q3a, ".803", ".122", ".012", ".192", ".068",
                             "The two runs agree only at first."))
    check("Step 3: the four runs are drawn", is_png(output$p3_runs))
    session$setInputs(ga = 0)
    check("Step 3: with no associability carryover the gap is zero",
          has(output$ga_text, "associability .500 and D with .500", "The gap is 0 after the first"))
    session$setInputs(ga = .5)
    check("Step 3: half associability carryover gives .368 and a first gap of .040",
          has(output$ga_text, ".368", ".040"))
    answer("q3b", q3b_claim = "support")
    check("Step 3: the conclusion is saved and Box 3 follows",
          done("q3b") && last_note()$topic == "Your conclusion so far")
    answer("q3c", q3c_ahead = "B", q3c_gains = "D", q3c_after = "B")
    check("Step 3: Box 3 corrects all three answers and gives .104 and .064",
          done("q3c") && has(output$q3c, "keeps their order", "B gains more, because it starts lower.",
                             "D still has the higher value", ".104", ".064",
                             "largest before the first pear trial"))
    session$setInputs(gv = 0)
    check("Step 3: with g_V = 0 neither model has a gap before the first pear trial",
          has(output$gv_text, "neither model has a gap before the first pear trial"))
    session$setInputs(gv = .9)
    check("Step 3: with g_V = .9, .304 before the first pear trial and .109 and .108 after eight",
          has(output$gv_text, ".304", ".109", ".108", num(gv_cross)) && is_png(output$p3_grid) &&
            is_png(output$p3_gap))
    check("Step 3: the earlier conclusion is shown before it is kept or changed",
          has(output$q3d_claim, "Earlier you chose this conclusion", "supports the adaptive model"))
    answer("q3d", q3d_rev = "keep")
    check("Step 3: keeping the conclusion is answered with the crossing at .895",
          done("q3d") && has(output$q3d, "Your earlier conclusion", num(gv_cross),
                             "no longer favors the adaptive model") &&
            grepl("g_V = .9", last_note()$result, fixed = TRUE))
    session$setInputs(n3_compound = 10, n3_eta = .01)
    check("Step 3: with eta = .01 the gap is largest after six pear trials, at .037",
          has(output$n3_text, "largest after six pear trials, at about .037") &&
            is_png(output$p3_training))
    session$setInputs(n3_compound = 1, n3_eta = .05)
    check("Step 3: one compound trial is named in the singular",
          has(output$n3_text, "With 1 compound trial and"))
    answer("q3e", q3e_prop = "smaller", q3e_rank = "same")
    check("Step 3: the rules table gives .254, .213 (5), .017 (6) and none",
          done("q3e") && has(output$q3e, ".254", ".213 (5)", ".017 (6)", "no gap", ".498",
                             "follows the size of the difference", "changes associability by the full step"))
    session$setInputs(r3_rule = "rank", r3_c = "1e-6")
    check("Step 3: comparing the rules freely, the rank-based rule with a tiny mismatch",
          has(output$r3_text, "is .254", ".017") && is_png(output$p3_rule))
    session$setInputs(r3_rule = "rank_ties", r3_c = "0")
    check("Step 3: ties that lower associability leave no gap", has(output$r3_text, "There is no gap."))

    # Step 4
    answer("q4a", q4a_likely = "value", q4a_must = "yes")
    check("Step 4: Box 4 corrects both answers and gives .184 and .631",
          done("q4a") && has(output$q4a, "A learned value is not a probability",
                             "Only a deterministic rule, such as a threshold at .5", ".184", ".631", ".253", ".447"))
    check("Step 4: before the next question, Table 6 shows the learned values only",
          has(output$resp_table, ".314 / .567") && !has(output$resp_table, ".184 / .631") &&
            is_png(output$p4_functions))
    check("Step 4: Table 6 rounds D's exact .2775 up, as the paper does",
          has(output$resp_table, ".136 / .278"))
    answer("q4b", q4b_rule = "threshold", q4b_peak = "6")
    check("Step 4: the threshold rule's difference is largest on pear trial 6",
          done("q4b") && has(output$q4b, "is 0 on pear trial 1, 1 on pear trial 6 and 0 on pear trial 10",
                              "only on pear trials 6 to 9") &&
            is_png(output$p4_differences))
    check("Step 4: Table 6 then shows the yes probabilities and the threshold responses",
          has(output$resp_table, ".184 / .631", "no / yes", "yes / yes"))
    answer("q4c", q4c_sig = "no", q4c_thr = "yes")
    check("Step 4 is complete, with both answers corrected",
          done("q4c") && has(output$q4c, "estimate that difference more precisely",
                              "add only more yes responses", ".553", ".921"))
    session$setInputs(q4_try_rule = "threshold", q4_try_q = 7); click("q4_try_go")
    check("Step 4: a further try is added to the table and the notes",
          has(output$q4_tries, "Threshold", "<td>7</td>") && last_note()$topic == "Another function or response")

    # Step 5, with the first case, 160 participants and all ten pear trials
    session$setInputs(e_design_set = "value")
    answer("e_design", e_design_n = "160", e_design_rec = "full", e_design_reason = "")
    check("Step 5: the design needs a reason", !done("e_design") &&
            has(output$e_design_msg, "Write a sentence"))
    answer("e_design", e_design_n = "160", e_design_rec = "full",
           e_design_reason = "To see when the difference appears.")
    cs <- cases$value[["160"]]; des <- cs$designs$full
    check("Step 5: the design is run and noted with its seed",
          done("e_design") && has(output$e_case, "Case 1") &&
            grepl(paste0("seed = ", cs$seed), last_note()$settings, fixed = TRUE))
    check("Step 5: the responses and each participant's difference are drawn",
          is_png(output$e_records) && is_png(output$e_participants))
    check("Step 5: the mean difference and its interval are reported",
          has(output$e_effect, num(des$effect[["estimate"]]), num(des$effect[["lower"]]),
              "is detected in this sample"))
    answer("e1", e1_e = "assoc")
    check("Step 5: an explanation read off the responses is corrected",
          done("e1") && has(output$e1, "explanations to compare"))
    answer("e2", e2_est = "yes", e2_look = "last")
    check("Step 5: Box 5 corrects both answers", done("e2") &&
            has(output$e2, "does not show where it came from", "By the last pear trials"))
    check("Step 5: the differences by pear trial, with the first pear trial reported",
          is_png(output$e_contrast) && has(output$e_contrast_text, "On the first pear trial",
                                           num(des$by_trial$estimate[1])))
    answer("e3", e3_pref = "process")
    check("Step 5: a preference is not read as the process at work",
          done("e3") && has(output$e3, "may be missing from the candidates") &&
            has(output$e_scores_text, "Under the decision rule, the comparison is unresolved"))
    answer("e4", e4_which = "identify")
    check("Step 5: the rates for this design, under all three processes",
          has(output$e_rates, "carried associability", "carried learned value",
              "reset associability", pct(rates$carry[rates$scenario == "associability" &
                                                       rates$n == 160 & rates$view == "full"]),
              "(yours)", "Fixed-rate or reset favored"))
    check("Step 5 is complete, with 59.5%, 0.5% and 21.2%",
          done("e4") && has(output$e4, "Detecting a difference was the easier task",
                            "59.5%", "0.5%", "21.2%", "800 responses"))

    # Step 6
    answer("f1", f1_which = "all")
    check("Step 6: the restricted comparison of this case favors associability carryover",
          done("f1") && has(output$f1, "no way to make B and D differ") &&
            has(output$f1_result, "favors associability carryover", "With <i>g</i><sub>V</sub> estimated"))
    check("Step 6: the study's rates for the restricted comparison",
          has(output$f1_rates, "96.8%", "59.5%", "95.0%", "0.0%") && is_png(output$f_scores_zero))
    answer("f2", f2_first = "carry", f2_rank = "yes")
    check("Step 6: Box 6 corrects both answers", done("f2") &&
            has(output$f2, "predicts equal first responses too", "cannot favor any one of them"))
    check("Step 6: the first responses are counted and tested",
          has(output$f_first, "Yes to D, no to B", "McNemar", "p &lt; .001"))
    check("Step 6: the process is revealed only after the check", !isTRUE(output$f_revealed))
    answer("f3", f3_h = "consistent")
    check("Step 6: missing a rejection is corrected", done("f3") &&
            has(output$f3, "rejects the shared prediction") && isTRUE(output$f_revealed))
    check("Step 6: the process is revealed with the case's own results",
          has(output$f_truth, "Carried learned value.", "65%", "the first experiment of this kind in the base parameter set",
              "light 1 more salient") && is_png(output$f_contrast_truth))
    check("Step 6: the first-response check across the study",
          has(output$f_check_rates, "1.5%", "2.2%", "94.8%", "95.3%", "380"))
    answer("f4", f4_explain = "responding")
    check("Step 6: a difference from responding is attributed to learning",
          done("f4") && has(output$f4, "100.0%", "68.0%", "7.2%", "90.0%", "10.5%", "82.0%"))
    session$setInputs(f5_pick = c("1", "4")); click("f5_go")
    check("Step 6 is complete; each statement is judged",
          done("f5") && has(output$f5, "Your results support it.", "Your results do not support it.",
                            "A decision is not the same as a correct decision"))
    check("Step 6: the note names the generating process",
          grepl("Generating process: carried learned value", last_note()$result, fixed = TRUE))

    # Step 7
    answer("x1", x1_more = "yes")
    check("Step 7: equal accuracy, .646", done("x1") && has(output$x1, ".646", "The equality is exact."))
    answer("x2", x2_rec = "side", x2_longer = "second", x2_part = "rules")
    check("Step 7: Box 7, with 0.97, 0.24, 32% and 90%",
          done("x2") && has(output$x2, "0.97", "0.24", "32%", "90%", "same proportions of left and right",
                            "closer boundaries both shorten", "The rules and the settings stay"))
    check("Step 7: the decision times are drawn", is_png(output$p_ddm) && has(output$ddm_text, "nondecision time"))
    answer("x3", x3_reason = "")
    check("Step 7: the closing question needs a sentence", !done("x3") && has(output$x3_msg, "Write a sentence"))
    answer("x3", x3_reason = "Nondecision time could differ.")
    check("Step 7 is complete", done("x3") && has(output$x3, "Discussion"))

    # Another design: the second case, 40 participants, pear trials 8-10
    n_before <- length(entries())
    click("e_again")
    check("Trying another design clears Steps 5 to 7 and keeps the notes",
          !done("e_design") && !done("f1") && !done("x1") && length(entries()) == n_before)
    session$setInputs(e_design_set = "associability")
    answer("e_design", e_design_n = "40", e_design_rec = "late", e_design_reason = "A final test.")
    check("Step 5: pear trials 8-10 cannot show the first pear trial",
          done("e_design") && { answer("e1", e1_e = "more"); answer("e2", e2_est = "no", e2_look = "first");
                                has(output$e_contrast_text, "cannot show whether the difference was present") })
    answer("e3", e3_pref = "best"); answer("e4", e4_which = "detect")
    answer("f1", f1_which = "carry"); answer("f2", f2_first = "equal", f2_rank = "no")
    check("Step 6: without the first pear trial the check cannot be made, and the process is revealed",
          has(output$f_first, "cannot be made with the responses you kept") &&
            isTRUE(output$f_revealed) && has(output$f_truth, "Carried associability."))
    check("Step 6: the restricted comparison of other designs was run for the page",
          has(output$f1_result, "were run with the study's code when this page was built"))

    # Every output drawn after the seven steps, including outputs placed inside
    # other outputs.
    queue <- output_ids(page); drawn <- character(0); escaping <- character(0)
    while (length(queue)) {
      id <- queue[1]; queue <- queue[-1]
      if (id %in% drawn) next
      drawn <- c(drawn, id)
      html <- tryCatch(paste(as.character(output[[id]]$html), collapse = "\n"),
                       error = function(e) "")
      if (grepl(escaped, html)) escaping <- c(escaping, id)
      queue <- c(queue, output_ids(html))
    }
    check(paste0("No output shows a character entity with an escaped ampersand (",
                 length(drawn), " outputs)",
                 if (length(escaping)) paste0(": ", paste(escaping, collapse = ", "))),
          length(drawn) > 50 && !length(escaping))

    # Starting again
    n_before <- length(entries())
    click("restart")
    check("Starting again clears every step and keeps the notes",
          !done("q1a") && !done("q2d") && !done("q3e") && !done("q4c") && !done("e_design") &&
            !done("f5") && !done("x3") && length(entries()) == n_before)
    check("Starting again offers the comparisons of Step 2 afresh",
          has(output$cmp, 'value="cont"', 'value="pear"'))

    # Explore
    check("Explore: the starting states at the default settings",
          has(output$x_summary, num(trained$adaptive$alpha_B), "learning rate .12"))
    session$setInputs(x_change = "supply")
    check("Explore: Supply names its fixed learning rates",
          has(output$x_change_explanation, num(trained$adaptive$rate_B), num(trained$adaptive$rate_D)) &&
            is_png(output$x_gap) && has(output$x_table, "Fixed learning rate"))
    session$setInputs(x_change = "none", x_c = "0.01")
    check("Explore: C 1% more salient leaves D at .498", has(output$x_summary, ".498"))
    session$setInputs(x_c = "0", x_rule = "threshold", x_theta = .5)
    check("Explore: the response plot is drawn under the threshold rule", is_png(output$x_resp))

    # The notes
    text <- notes_text(entries())
    check("The notes name the paper and keep every step",
          grepl("Thinking with Computational Models to Guide", text[1], fixed = TRUE) &&
            all(vapply(paste("Step", 1:7), function(s) any(grepl(s, text, fixed = TRUE)), logical(1))))
    check("The notes are plain text, without markup",
          !any(grepl("<[a-z/]", text)) && !any(grepl("&[a-z]+;", text)))
    check("Every note of Steps 1 to 4 records its settings",
          all(vapply(Filter(function(e) e$step %in% paste("Step", 1:4), entries()),
                     function(e) !is.null(e$settings), logical(1))))

    # The score plot keeps a common scale for the saved cases.
    p <- plot_scores(cases$associability[["160"]]$designs$full$scores$estimated)
    x_range <- ggplot_build(p)$layout$panel_params[[1]]$x.range
    check("The score plot spans at least -70 to 130", x_range[1] <= -70 && x_range[2] >= 130)
  })

  cat(sprintf("\nAll %d app flow checks passed.\n\n", n_checks))
})
