`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ensure_core_ingest_loaded <- function() {
  if (!exists("ingest_rdm_file", mode = "function")) {
    source("R/rdm_ingest.R")
  }
}

contains_any <- function(text, needles) {
  tx <- tolower(as.character(text %||% ""))
  if (length(tx) == 0 || is.na(tx)) return(FALSE)
  any(vapply(needles, function(n) grepl(n, tx, fixed = TRUE), logical(1)))
}

has_survey_payload <- function(survey_responses_json) {
  if (is.null(survey_responses_json) || length(survey_responses_json) == 0) return(FALSE)
  x <- trimws(tolower(as.character(survey_responses_json)))
  if (length(x) == 0 || is.na(x)) return(FALSE)
  !(x %in% c("", "na", "null", "{}", "[]"))
}

is_rdm_row <- function(task_type, plugin_type, trial_type) {
  contains_any(task_type, c("rdm", "random-dot-motion", "random_dot_motion")) ||
    contains_any(plugin_type, c("rdm")) ||
    contains_any(trial_type, c("rdm"))
}

is_gabor_row <- function(task_type, plugin_type, trial_type) {
  contains_any(task_type, c("gabor", "gabor-patch", "gabor_patch", "contrast-detection", "orientation")) ||
    contains_any(plugin_type, c("gabor", "gabor-patch", "gabor_patch")) ||
    contains_any(trial_type, c("gabor", "gabor-patch", "gabor_patch"))
}

is_drt_row <- function(task_type, plugin_type, trial_type) {
  contains_any(task_type, c("drt", "dual-response", "dual_response")) ||
    contains_any(plugin_type, c("drt", "dual-response", "dual_response")) ||
    contains_any(trial_type, c("drt", "dual-response", "dual_response"))
}

is_mw_probe_row <- function(task_type, plugin_type, trial_type) {
  contains_any(task_type, c("mw-probe", "mw_probe", "mind-probe", "mind_probe", "mind-wandering", "mind_wandering")) ||
    contains_any(plugin_type, c("mw-probe", "mw_probe", "mind-probe", "mind_probe")) ||
    contains_any(trial_type, c("mw-probe", "mw_probe", "mind-probe", "mind_probe"))
}

is_survey_row <- function(task_type, plugin_type, trial_type, survey_responses_json) {
  contains_any(task_type, c("survey", "questionnaire")) ||
    contains_any(plugin_type, c("survey", "questionnaire")) ||
    contains_any(trial_type, c("survey", "questionnaire")) ||
    has_survey_payload(survey_responses_json)
}

split_task_types <- function(all_trials) {
  if (nrow(all_trials) == 0) {
    return(list(
      rdm_trials = all_trials,
      gabor_trials = all_trials,
      drt_trials = all_trials,
      mw_probe_trials = all_trials,
      survey_trials = all_trials,
      other_trials = all_trials
    ))
  }

  is_rdm <- mapply(
    is_rdm_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  is_drt <- mapply(
    is_drt_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  is_gabor <- mapply(
    is_gabor_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  is_mw <- mapply(
    is_mw_probe_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  survey_col <- if ("survey_responses_json" %in% names(all_trials)) all_trials$survey_responses_json else rep(NA, nrow(all_trials))
  is_survey <- mapply(
    is_survey_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    survey_col,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  matched <- is_rdm | is_gabor | is_drt | is_mw | is_survey

  list(
    rdm_trials = all_trials[which(is_rdm), , drop = FALSE],
    gabor_trials = all_trials[which(is_gabor), , drop = FALSE],
    drt_trials = all_trials[which(is_drt), , drop = FALSE],
    mw_probe_trials = all_trials[which(is_mw), , drop = FALSE],
    survey_trials = all_trials[which(is_survey), , drop = FALSE],
    other_trials = all_trials[which(!matched), , drop = FALSE]
  )
}

ingest_mixed_task_file <- function(path) {
  ensure_core_ingest_loaded()
  base <- ingest_rdm_file(path)
  all_trials <- base$all_trials
  split <- split_task_types(all_trials)

  list(
    file_path = path,
    source_format = base$source_format,
    all_trials = all_trials,
    rdm_trials = split$rdm_trials,
    gabor_trials = split$gabor_trials,
    drt_trials = split$drt_trials,
    mw_probe_trials = split$mw_probe_trials,
    survey_trials = split$survey_trials,
    other_trials = split$other_trials
  )
}

ingest_mixed_task_paths <- function(files) {
  ensure_core_ingest_loaded()
  if (length(files) == 0) stop("No files provided")

  per_file <- lapply(files, ingest_mixed_task_file)

  list(
    files_scanned = files,
    per_file = per_file,
    all_trials = bind_df_list(lapply(per_file, function(x) x$all_trials)),
    rdm_trials = bind_df_list(lapply(per_file, function(x) x$rdm_trials)),
    gabor_trials = bind_df_list(lapply(per_file, function(x) x$gabor_trials)),
    drt_trials = bind_df_list(lapply(per_file, function(x) x$drt_trials)),
    mw_probe_trials = bind_df_list(lapply(per_file, function(x) x$mw_probe_trials)),
    survey_trials = bind_df_list(lapply(per_file, function(x) x$survey_trials)),
    other_trials = bind_df_list(lapply(per_file, function(x) x$other_trials))
  )
}

ingest_mixed_task_dir <- function(dir, pattern = "\\.(json|csv)$", recursive = TRUE) {
  ensure_core_ingest_loaded()
  files <- list.files(path = dir, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (length(files) == 0) stop(sprintf("No files matched pattern '%s' in %s", pattern, dir))
  ingest_mixed_task_paths(files)
}

summarize_mixed_task_counts <- function(all_trials) {
  if (nrow(all_trials) == 0) return(data.frame(stringsAsFactors = FALSE))

  split <- split_task_types(all_trials)
  data.frame(
    task_type = c("rdm", "gabor", "drt", "mw_probe", "survey", "other"),
    rows = c(
      nrow(split$rdm_trials),
      nrow(split$gabor_trials),
      nrow(split$drt_trials),
      nrow(split$mw_probe_trials),
      nrow(split$survey_trials),
      nrow(split$other_trials)
    ),
    stringsAsFactors = FALSE
  )
}
