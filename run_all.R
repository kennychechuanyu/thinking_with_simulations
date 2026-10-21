# Runs the implementation checks and then every analysis of the paper and its
# supplement in the order of the paper, each script in its own R process, and
# last the checks of the companion app. The worked example of Section 5 is
# simulated and fitted again, but the figures and numbers of the Section 5
# study come from its saved results in results/section5; only the test of the
# first answers regenerates the records from their seeds. With --full-study,
# the study's 3,600 simulated experiments, the check of its starting vectors,
# the refits with wider bounds and the app's simulated experiments are run
# again, which takes about 5 CPU hours (45 minutes with --workers=8).
#   Rscript run_all.R
#   Rscript run_all.R --full-study --workers=8

if (!file.exists("R/models.R")) stop("Run this script from the repository root.")
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("Run R in a UTF-8 locale; the Greek letters in the figures and the app need it.")
}

arguments <- commandArgs(trailingOnly = TRUE)
full_study <- "--full-study" %in% arguments
workers_option <- grep("^--workers=[1-9][0-9]*$", arguments, value = TRUE)
unknown <- setdiff(arguments, c("--full-study", workers_option))
if (length(unknown)) {
  stop("Unrecognized argument ", unknown[1], "; the options are --full-study and --workers=N.")
}

needed <- c("ggplot2", "patchwork", "ragg", "Rcpp")
not_installed <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(not_installed)) stop("Install the R packages ", paste(not_installed, collapse = ", "), ".")

run <- function(script, ...) {
  cat("\n", script, "\n", sep = "")
  if (system2(file.path(R.home("bin"), "Rscript"), c(script, ...)) != 0) {
    stop(script, " did not finish; its message is above.", call. = FALSE)
  }
}

# Implementation checks: Section 2.2 and Supplement S2
run("tests/verify_models.R")
run("tests/verify_inference.R")

# Sections 2-4
run("analysis/section2_blocking.R")
run("analysis/section3_new_outcome.R")
run("analysis/section4_1_changing_associability.R")
run("analysis/section4_2_training_and_rate.R")
run("analysis/section4_3_carryover.R")
run("analysis/section4_4_predicted_responses.R")
run("analysis/section4_5_matched_control.R")

# Section 5 and Supplement S2
if (full_study) run("analysis/section5_study.R", workers_option)
if (full_study) run("analysis/section5_starting_values_check.R", workers_option)
run("analysis/section5_worked_example.R")
run("analysis/section5_first_response_and_bounds.R", workers_option, if (full_study) "--refit")
run("analysis/section5_report.R")
run("analysis/section5_fig09_one_experiment.R")
run("analysis/section5_fig10_detection_and_identification.R")
run("analysis/section5_fig11_shared_prediction.R")
run("analysis/section5_figS1_mimicry_and_profile.R")
run("analysis/section5_figS2_fit_by_cue.R")

# Section 6 and Supplement S1
run("analysis/section6_decision_model.R")

# Supplement S3
run("analysis/supplementS3_update_rule.R")

# Companion app: the simulated experiments of Step 5, rebuilt from the study
# with --full-study, then the checks of the app. The walk through the app and the
# feedback checks need shiny and bslib.
if (full_study) run("shiny_app/make_inference_cases.R")
run("tests/verify_app.R")
if (all(vapply(c("shiny", "bslib"), requireNamespace, logical(1), quietly = TRUE))) {
  run("tests/verify_app_flow.R")
  run("tests/verify_app_feedback.R")
} else {
  cat("\nSkipped the app flow and feedback checks, which need shiny and bslib\n")
}

# The versions of R and of the packages the scripts use
packages <- c("ggplot2", "patchwork", "ragg", "systemfonts", "textshaping", "Rcpp",
              "shiny", "bslib")
versions <- vapply(packages, function(package) {
  if (nzchar(system.file(package = package))) format(packageVersion(package)) else "not installed"
}, character(1))
writeLines(c(R.version.string,
             paste("platform:", R.version$platform),
             paste("operating system:", utils::osVersion),
             paste0(packages, ": ", versions)),
           "results/session_info.txt")
cat("\nSaved results/session_info.txt\n")
