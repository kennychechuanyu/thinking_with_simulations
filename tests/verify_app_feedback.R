# Checks of the feedback the companion app gives: every combination of answers
# to the questions of Steps 1 to 4 and 7, and to those of Steps 5 and 6 for
# every saved case, sample size and set of analyzed pear trials. A correction
# must appear for a wrong answer and only for a wrong answer, and no feedback
# may show a character entity literally. Stops at the first failure.
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
  # A character entity such as &eta; in text not marked as HTML reaches the
  # page with its ampersand escaped and is shown literally.
  escaped <- "&amp;([a-zA-Z]+|#[0-9]+);"
  escaping <- character(0)
  # The text as a reader sees it: runs of spaces and line breaks count as one space.
  shown <- function(x) {
    s <- gsub("\\s+", " ", paste(as.character(x$html), collapse = " "))
    if (grepl(escaped, s)) escaping <<- c(escaping, s)
    s
  }
  contains <- function(s, p) grepl(p, s, fixed = TRUE)

  cat("Checks of the app's feedback\n")
  testServer("shiny_app", {
    app <- parent.env(session$env)
    cases <- get("cases", envir = app)
    clear_answers <- get("clear_answers", envir = app)
    click <- function(id) {
      now <- isolate(input[[id]])
      do.call(session$setInputs, setNames(list(if (is.null(now)) 1 else now + 1), id))
    }
    answer <- function(id, a) {
      clear_answers(answers, id)
      do.call(session$setInputs, setNames(a, paste0(id, "_", names(a))))
      click(paste0(id, "_go"))
    }
    # Answers question id with every row of grid and checks that each phrase
    # appears exactly when expect() names it. "out" is the output that shows
    # the feedback, and "before" runs ahead of each answer.
    combinations <- function(id, grid, expect, phrases, out = id, before = function(a) NULL,
                             label = id) {
      for (i in seq_len(nrow(grid))) {
        a <- lapply(as.list(grid[i, , drop = FALSE]), as.character)
        before(a)
        answer(id, a)
        s <- shown(output[[out]])
        want <- expect(a)
        ok <- all(vapply(names(phrases), function(k) contains(s, phrases[[k]]) == (k %in% want),
                         logical(1)))
        check(paste0(label, ", ", paste(unlist(a), collapse = "/"),
                     ": the feedback addresses this answer only"), ok)
      }
    }
    grid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
    session$setInputs(t1_trial = 12, t_model = "adaptive", t_k = 1, ga = .5, gv = .5,
                      n3_compound = 10, n3_eta = .05, r3_rule = "proportional", r3_c = "0")

    # Step 1. The result of Box 1 is shown once the code has been checked.
    answer("q1b", list(test = "thirty"))
    combinations("q1a", grid(dir = c("more", "less", "same"),
                             why = c("error", "finished", "rate", "left")),
      expect = function(a) c(
        if (a$dir == "less" && a$why == "error") "both",
        if (a$dir == "less" && a$why != "error") c("right_dir", a$why),
        if (a$dir != "less" && a$why == "error") "right_reason",
        if (a$dir != "less" && a$why != "error") c(paste0("not_", a$dir), a$why)),
      phrases = c(both = "Your direction and your reason both match.",
                  right_dir = "The direction is right, but the reason is not.",
                  right_reason = "Your reason is the right one",
                  not_more = "The light gains less, not more.",
                  not_same = "The light gains less, not the same.",
                  finished = "The tone has not finished learning.",
                  rate = "the learning rate stays at .12",
                  left = "it does not learn from its own error"),
      out = "q1a_result")
    combinations("q1b", grid(test = c("single", "zero", "thirty")),
      expect = function(a) if (a$test != "thirty") a$test,
      phrases = c(single = "That test checks the tone's training",
                  zero = "That test catches a program that updates the tone first"))
    combinations("q1c", grid(show = c("tone", "two", "any")),
      expect = function(a) if (a$show != "two") a$show,
      phrases = c(tone = "cannot single out the trained tone", any = "so it cannot show that either"))

    # Step 2
    combinations("q2a", grid(first = c("less", "same", "more")),
      expect = function(a) if (a$first != "same") a$first,
      phrases = c(less = "That is what Rescorla", more = "start the compound trials in the same state"))
    combinations("q2b", grid(left = c("nothing", "adaptive", "both")),
      expect = function(a) if (a$left != "adaptive") a$left,
      phrases = c(nothing = "changes more than learned value",
                  both = "come from a smaller shared error"))
    tells <- c(more = "B staying below D shows that blocking persists",
               cont = "The same extra training does not give B and D the same starting conditions",
               across = "This comparison changes more than associability")
    for (ch in names(tells)) for (verdict in c("yes", "no")) {
      explanations$reset()
      session$setInputs(cmp_choice_1 = ch, cmp_reason_1 = "A reason.")
      click("cmp_go_1")
      do.call(session$setInputs, setNames(list(verdict), paste0("cmp_tells_", ch)))
      click(paste0("cmp_check_", ch))
      s <- shown(output$cmp)
      check(paste0("Step 2 comparison ", ch, ", ", verdict, ": the feedback addresses this answer only"),
            all(vapply(names(tells), function(k) contains(s, tells[[k]]) == (k == ch && verdict == "yes"),
                       logical(1))))
    }
    combinations("q2c", grid(rw = c("equal", "less", "more"), ad = c("equal", "less", "more"),
                             why = c("start", "rate", "A", "error")),
      expect = function(a) c(if (a$rw != "equal") paste0("rw_", a$rw),
                             if (a$ad != "less") paste0("ad_", a$ad),
                             if (a$why != "rate") paste0("why_", a$why)),
      phrases = c(rw_less = "You expected B to gain less under Rescorla",
                  rw_more = "You expected B to gain more under Rescorla",
                  ad_equal = "You expected equal gains under the adaptive model",
                  ad_more = "You expected B to gain more under the adaptive model",
                  why_start = "so their starting values are equal",
                  why_A = "A is absent during the pear trials",
                  why_error = "so their first errors are equal"))
    combinations("q2d", grid(n = c("stop", "converge", "error", "overtake")),
      expect = function(a) if (a$n != "error") a$n,
      phrases = c(stop = "D keeps learning on every pear trial",
                  converge = "B's associability does rise during the pear trials",
                  overtake = "B never overtakes D"))

    # Step 3
    combinations("q3a", grid(pr = c("gone", "smaller", "same"),
                             supply = c("same", "close", "smaller")),
      expect = function(a) c(if (a$pr != "gone") "pr", if (a$supply != "close") paste0("supply_", a$supply)),
      phrases = c(pr = "with no difference left for the gap to come from",
                  supply_same = "The two runs agree only at first.",
                  supply_smaller = "so its first gaps are the same"))
    combinations("q3c", grid(ahead = c("D", "B", "neither"), gains = c("B", "D", "equal"),
                             after = c("D", "B", "equal")),
      expect = function(a) c(if (a$ahead != "D") "ahead", if (a$gains != "B") "gains",
                             if (a$after != "D") "after"),
      phrases = c(ahead = "keeps their order", gains = "B gains more, because it starts lower.",
                  after = "D still has the higher value"))
    # A first conclusion that a positive gap supports the adaptive model is
    # answered whether the reader keeps or changes it.
    q3d_phrases <- c(support = "no longer favors the adaptive model",
                     depends = "already allowed for this",
                     none = "We can still ask when the gap first appears.",
                     qualify = "Recording the first response lets you check whether",
                     useless = "You can still use the timing of the gap.")
    combinations("q3d", grid(claim = c("support", "depends", "none"),
                             rev = c("keep", "qualify", "useless")),
      expect = function(a) c(if (a$claim == "support") "support",
                             if (a$rev == "keep" && a$claim != "support") a$claim,
                             if (a$rev != "keep") a$rev),
      phrases = q3d_phrases,
      before = function(a) answer("q3b", list(claim = a$claim)))
    combinations("q3e", grid(prop = c("same", "smaller", "larger"), rank = c("same", "smaller", "larger")),
      expect = function(a) c(if (a$prop != "same") "prop", if (a$rank != "smaller") "rank"),
      phrases = c(prop = "follows the size of the difference between the errors",
                  rank = "changes associability by the full step"))

    # Step 4
    combinations("q4a", grid(likely = c("depends", "value", "same"), must = c("yes", "only", "never")),
      expect = function(a) c(if (a$likely != "depends") paste0("likely_", a$likely),
                             if (a$must != "only") paste0("must_", a$must)),
      phrases = c(likely_value = "A learned value is not a probability of a yes",
                  likely_same = "The lights started equal",
                  must_yes = "Only a deterministic rule, such as a threshold at .5, makes these",
                  must_never = "A threshold rule does make them certain."))
    combinations("q4b", grid(rule = c("sigmoid", "threshold"), peak = c("1", "6", "10", "same")),
      expect = function(a) if (a$peak != "6") paste0(a$peak, if (a$peak == "10") paste0("_", a$rule)),
      phrases = c(`1` = "It does not give either light a value for the pear before the first pear trial.",
                  `10_sigmoid` = "D's yes probability is near the top of the sigmoid",
                  `10_threshold` = "the threshold rule gives a yes to both lights",
                  same = "so the difference changes too"))
    combinations("q4c", grid(sig = c("yes", "no"), thr = c("yes", "no")),
      expect = function(a) c(if (a$sig == "no") "sig", if (a$thr == "yes") "thr"),
      phrases = c(sig = "so more responses estimate that difference more precisely",
                  thr = "add only more yes responses"))

    # Steps 5 and 6, for every saved case, sample size and set of analyzed pear trials
    for (set in c("value", "associability", "reset")) for (n in c("40", "160"))
    for (rec in c("late", "early", "full")) {
      des <- cases[[set]][[n]]$designs[[rec]]
      detected <- des$effect[["detected"]] == 1
      label <- paste(set, n, rec)
      experiment$reset(); shared$reset()
      session$setInputs(e_design_set = set)
      answer("e_design", list(n = n, rec = rec, reason = "A reason."))
      check(paste(label, ": the design is run"), isTRUE(output$done_e_design))

      combinations("e1", grid(e = c("more", "none", "assoc", "value")),
        expect = function(a) {
          if (a$e %in% c("assoc", "value")) "explain"
          else if (a$e == "more" && !detected) "not_detected"
          else if (a$e == "none" && detected) "detected"
          else "right"
        },
        phrases = c(explain = "explanations to compare",
                    not_detected = "A positive difference needs the whole interval",
                    detected = "The whole interval lies above zero",
                    right = "Your reading matches the interval."), label = paste(label, "e1"))
      combinations("e2", grid(est = c("yes", "no"), look = c("first", "last", "overall")),
        expect = function(a) c(if (a$est == "yes") "est", if (a$look != "first") a$look),
        phrases = c(est = "does not show where it came from", last = "By the last pear trials",
                    overall = "mixes all the pear trials"), label = paste(label, "e2"))
      combinations("e3", grid(pref = c("process", "best")),
        expect = function(a) if (a$pref == "process") "process",
        phrases = c(process = "may be missing from the candidates"), label = paste(label, "e3"))
      combinations("e4", grid(which = c("detect", "identify", "same")),
        expect = function(a) if (a$which != "detect") "wrong",
        phrases = c(wrong = "Detecting a difference was the easier task"), label = paste(label, "e4"))

      combinations("f1", grid(which = c("carry", "all", "none")),
        expect = function(a) if (a$which != "carry") a$which,
        phrases = c(all = "no way to make B and D differ systematically",
                    none = "can still explain a difference."), label = paste(label, "f1"))
      combinations("f2", grid(first = c("equal", "carry", "depends"), rank = c("no", "yes")),
        expect = function(a) c(if (a$first != "equal") a$first, if (a$rank == "yes") "rank"),
        phrases = c(carry = "predicts equal first responses too",
                    depends = "Every choice of parameters keeps this equality",
                    rank = "cannot favor any one of them"), label = paste(label, "f2"))
      check(paste(label, ": the process is revealed exactly when the first pear trial is missing, before the check"),
            isTRUE(output$f_revealed) == is.null(des$first))
      if (!is.null(des$first)) {
        rejects <- des$first$p < .05
        combinations("f3", grid(h = c("disagree", "consistent", "value")),
          expect = function(a) c(
            if (a$h == "consistent" && rejects) "missed",
            if (a$h == "disagree" && !rejects) "claimed",
            if (a$h == "value") "value",
            if (rejects) "rejects" else "no_rejection"),
          phrases = c(missed = "You chose no clear evidence",
                      claimed = "You chose a disagreement",
                      value = "The check cannot show that learned values carried over",
                      rejects = "the one-sided test rejects equal first-response probabilities",
                      no_rejection = "finds no clear evidence of more yes responses to D"),
          label = paste(label, "f3"))
        check(paste(label, ": the process is revealed after the check"), isTRUE(output$f_revealed))
      }
      combinations("f4", grid(explain = c("learning", "responding", "unfit")),
        expect = function(a) if (a$explain != "learning") a$explain,
        phrases = c(responding = "attribute the difference to responding only if",
                    unfit = "They fit the responses"), label = paste(label, "f4"))

      # The statements: the app's judgment of each follows from the case.
      clear_answers(answers, "f5")
      session$setInputs(f5_pick = c("1", "2", "3", "4", "5"))
      click("f5_go")
      s <- shown(output$f5)
      supported <- c(detected, des$decision$estimated != "unresolved", FALSE, FALSE,
                     !is.null(des$first) && des$first$p < .05)
      judgments <- regmatches(s, gregexpr("Your results (do not )?support it", s))[[1]]
      check(paste(label, ": each statement is judged from this case's results"),
            length(judgments) == 5 && identical(!grepl("do not", judgments), supported))
    }

    # Step 7
    combinations("x1", grid(more = c("yes", "no")),
      expect = function(a) if (a$more == "yes") "more",
      phrases = c(more = "Here both settings predict the same accuracy."))
    combinations("x2", grid(rec = c("rt", "conf", "side", "none"), longer = c("first", "second", "same"),
                            part = c("bridge", "rules", "settings")),
      expect = function(a) c(if (a$rec != "rt") a$rec, if (a$longer != "first") a$longer,
                             if (a$part != "bridge") "part"),
      phrases = c(conf = "Confidence would need an additional rule",
                  side = "same proportions of left and right responses",
                  none = "With correctness alone the settings cannot be told apart",
                  second = "Faster drift and closer boundaries both shorten",
                  same = "Equal accuracy does not require equal decision times",
                  part = "The rules and the settings stay as they were."))
  })
  check(paste0("No feedback shows a character entity with an escaped ampersand",
               if (length(escaping)) paste0(": ...", regmatches(escaping[1],
                 regexpr(paste0(".{0,80}", escaped), escaping[1])))),
        !length(escaping))

  cat(sprintf("All %d feedback checks passed.\n\n", n_checks))
})
