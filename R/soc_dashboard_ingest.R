`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ensure_core_ingest_loaded <- function() {
  if (!exists("ingest_rdm_file", mode = "function")) {
    source("R/rdm_ingest.R")
  }
}

is_soc_row <- function(task_type, plugin_type, trial_type) {
  t <- tolower(as.character(task_type %||% ""))
  p <- tolower(as.character(plugin_type %||% ""))
  tt <- tolower(as.character(trial_type %||% ""))

  t %in% c("soc", "soc_dashboard", "soc-dashboard") ||
    grepl("soc-dashboard", p, fixed = TRUE) ||
    grepl("soc_dashboard", p, fixed = TRUE) ||
    grepl("soc", tt, fixed = TRUE)
}

normalize_numeric_col <- function(df, candidates) {
  for (name in candidates) {
    if (name %in% names(df)) {
      return(suppressWarnings(as.numeric(df[[name]])))
    }
  }
  rep(NA_real_, nrow(df))
}

ingest_soc_dashboard_file <- function(path) {
  ensure_core_ingest_loaded()
  base <- ingest_rdm_file(path)
  all_trials <- base$all_trials

  if (nrow(all_trials) == 0) {
    soc_trials <- all_trials
  } else {
    keep <- mapply(
      is_soc_row,
      all_trials$task_type,
      all_trials$plugin_type,
      all_trials$trial_type,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    soc_trials <- all_trials[which(keep), , drop = FALSE]
  }

  list(
    file_path = path,
    source_format = base$source_format,
    all_trials = all_trials,
    soc_dashboard_trials = soc_trials
  )
}

ingest_soc_dashboard_paths <- function(files) {
  ensure_core_ingest_loaded()
  if (length(files) == 0) stop("No files provided")

  per_file <- lapply(files, ingest_soc_dashboard_file)
  all_trials <- bind_df_list(lapply(per_file, function(x) x$all_trials))
  soc_trials <- bind_df_list(lapply(per_file, function(x) x$soc_dashboard_trials))

  list(
    files_scanned = files,
    per_file = per_file,
    all_trials = all_trials,
    soc_dashboard_trials = soc_trials
  )
}

ingest_soc_dashboard_dir <- function(dir, pattern = "\\.(json|csv)$", recursive = TRUE) {
  ensure_core_ingest_loaded()
  files <- list.files(path = dir, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (length(files) == 0) stop(sprintf("No files matched pattern '%s' in %s", pattern, dir))
  ingest_soc_dashboard_paths(files)
}

summarize_soc_dashboard_trials <- function(soc_df) {
  if (nrow(soc_df) == 0) return(data.frame(stringsAsFactors = FALSE))

  rt <- normalize_numeric_col(soc_df, c("rt_ms", "trial_rt_ms", "trial_rt", "rt"))
  score <- normalize_numeric_col(soc_df, c("score", "trial_score", "points", "trial_points"))
  correct <- normalize_numeric_col(soc_df, c("correct", "trial_correct", "accuracy", "trial_accuracy"))

  tmp <- data.frame(
    file_path = soc_df$file_path,
    config_id = soc_df$config_id,
    rt = rt,
    score = score,
    correct = correct,
    stringsAsFactors = FALSE
  )

  aggregate(
    cbind(rt, score, correct) ~ file_path + config_id,
    data = tmp,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
}
