`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

ensure_core_ingest_loaded <- function() {
  if (!exists("ingest_rdm_file", mode = "function")) {
    source("R/rdm_ingest.R")
  }
}

is_sart_row <- function(task_type, plugin_type, trial_type) {
  t <- tolower(as.character(task_type %||% ""))
  p <- tolower(as.character(plugin_type %||% ""))
  tt <- tolower(as.character(trial_type %||% ""))
  t %in% c("sart", "go-nogo", "go_nogo") || grepl("sart", p, fixed = TRUE) || grepl("sart", tt, fixed = TRUE)
}

normalize_numeric_col <- function(df, candidates) {
  for (name in candidates) {
    if (name %in% names(df)) {
      return(suppressWarnings(as.numeric(df[[name]])))
    }
  }
  rep(NA_real_, nrow(df))
}

ingest_sart_file <- function(path) {
  ensure_core_ingest_loaded()
  base <- ingest_rdm_file(path)
  all_trials <- base$all_trials

  if (nrow(all_trials) == 0) {
    sart_trials <- all_trials
  } else {
    keep <- mapply(
      is_sart_row,
      all_trials$task_type,
      all_trials$plugin_type,
      all_trials$trial_type,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    sart_trials <- all_trials[which(keep), , drop = FALSE]
  }

  list(
    file_path = path,
    source_format = base$source_format,
    all_trials = all_trials,
    sart_trials = sart_trials
  )
}

ingest_sart_paths <- function(files) {
  ensure_core_ingest_loaded()
  if (length(files) == 0) stop("No files provided")

  per_file <- lapply(files, ingest_sart_file)
  all_trials <- bind_df_list(lapply(per_file, function(x) x$all_trials))
  sart_trials <- bind_df_list(lapply(per_file, function(x) x$sart_trials))

  list(
    files_scanned = files,
    per_file = per_file,
    all_trials = all_trials,
    sart_trials = sart_trials
  )
}

ingest_sart_dir <- function(dir, pattern = "\\.(json|csv)$", recursive = TRUE) {
  ensure_core_ingest_loaded()
  files <- list.files(path = dir, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (length(files) == 0) stop(sprintf("No files matched pattern '%s' in %s", pattern, dir))
  ingest_sart_paths(files)
}

summarize_sart_trials <- function(sart_df) {
  if (nrow(sart_df) == 0) return(data.frame(stringsAsFactors = FALSE))

  rt <- normalize_numeric_col(sart_df, c("rt_ms", "trial_rt_ms", "trial_rt", "rt"))
  accuracy <- normalize_numeric_col(sart_df, c("accuracy", "trial_accuracy", "correct", "trial_correct"))
  commission <- normalize_numeric_col(sart_df, c("commission_error", "trial_commission_error"))
  omission <- normalize_numeric_col(sart_df, c("omission_error", "trial_omission_error"))

  tmp <- data.frame(
    file_path = sart_df$file_path,
    config_id = sart_df$config_id,
    rt = rt,
    accuracy = accuracy,
    commission_error = commission,
    omission_error = omission,
    stringsAsFactors = FALSE
  )

  aggregate(
    cbind(rt, accuracy, commission_error, omission_error) ~ file_path + config_id,
    data = tmp,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
}
