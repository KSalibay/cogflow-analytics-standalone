`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ensure_core_ingest_loaded <- function() {
  if (!exists("ingest_rdm_file", mode = "function")) {
    source("R/rdm_ingest.R")
  }
}

is_gabor_row <- function(task_type, plugin_type, trial_type) {
  t <- tolower(as.character(task_type %||% ""))
  p <- tolower(as.character(plugin_type %||% ""))
  tt <- tolower(as.character(trial_type %||% ""))

  t %in% c("gabor", "gabor-patch", "gabor_patch") ||
    grepl("gabor", p, fixed = TRUE) ||
    grepl("gabor", tt, fixed = TRUE)
}

normalize_numeric_col <- function(df, candidates) {
  for (name in candidates) {
    if (name %in% names(df)) {
      return(suppressWarnings(as.numeric(df[[name]])))
    }
  }
  rep(NA_real_, nrow(df))
}

ingest_gabor_file <- function(path) {
  ensure_core_ingest_loaded()
  base <- ingest_rdm_file(path)
  all_trials <- base$all_trials

  if (nrow(all_trials) == 0) {
    gabor_trials <- all_trials
  } else {
    keep <- mapply(
      is_gabor_row,
      all_trials$task_type,
      all_trials$plugin_type,
      all_trials$trial_type,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    gabor_trials <- all_trials[which(keep), , drop = FALSE]
  }

  list(
    file_path = path,
    source_format = base$source_format,
    all_trials = all_trials,
    gabor_trials = gabor_trials
  )
}

ingest_gabor_paths <- function(files) {
  ensure_core_ingest_loaded()
  if (length(files) == 0) stop("No files provided")

  per_file <- lapply(files, ingest_gabor_file)
  all_trials <- bind_df_list(lapply(per_file, function(x) x$all_trials))
  gabor_trials <- bind_df_list(lapply(per_file, function(x) x$gabor_trials))

  list(
    files_scanned = files,
    per_file = per_file,
    all_trials = all_trials,
    gabor_trials = gabor_trials
  )
}

ingest_gabor_dir <- function(dir, pattern = "\\.(json|csv)$", recursive = TRUE) {
  ensure_core_ingest_loaded()
  files <- list.files(path = dir, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (length(files) == 0) stop(sprintf("No files matched pattern '%s' in %s", pattern, dir))
  ingest_gabor_paths(files)
}

summarize_gabor_trials <- function(gabor_df) {
  if (nrow(gabor_df) == 0) return(data.frame(stringsAsFactors = FALSE))

  rt <- normalize_numeric_col(gabor_df, c("rt_ms", "trial_rt_ms", "trial_rt", "rt"))
  accuracy <- normalize_numeric_col(gabor_df, c("accuracy", "trial_accuracy", "correct", "trial_correct"))
  contrast <- normalize_numeric_col(gabor_df, c("contrast", "trial_contrast", "stimulus_contrast", "threshold"))
  orientation <- normalize_numeric_col(gabor_df, c("orientation", "trial_orientation", "orientation_deg", "tilt"))

  tmp <- data.frame(
    file_path = gabor_df$file_path,
    config_id = gabor_df$config_id,
    rt = rt,
    accuracy = accuracy,
    contrast = contrast,
    orientation = orientation,
    stringsAsFactors = FALSE
  )

  aggregate(
    cbind(rt, accuracy, contrast, orientation) ~ file_path + config_id,
    data = tmp,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
}
