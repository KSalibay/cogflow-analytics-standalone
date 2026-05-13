`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

file_ext <- function(path) {
  tolower(tools::file_ext(path))
}

is_scalar <- function(x) {
  length(x) == 1 && !is.list(x)
}

normalize_scalar <- function(x) {
  if (is.null(x)) return(NA)
  if (length(x) == 0) return(NA)
  if (is.logical(x)) return(ifelse(is.na(x), NA, x))
  if (is.numeric(x)) return(ifelse(is.na(x), NA, x))
  if (is.character(x)) return(ifelse(length(x) == 0, NA, x))
  if (length(x) > 1) return(paste(as.character(x), collapse = " | "))
  as.character(x)
}

normalize_cell <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)

  # Keep complex nested fields analysable without breaking row binding.
  if (is.list(x)) {
    return(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  }

  normalize_scalar(x)
}

pluck <- function(x, path, default = NA) {
  cur <- x
  for (p in path) {
    if (is.null(cur) || is.null(cur[[p]])) return(default)
    cur <- cur[[p]]
  }
  normalize_scalar(cur)
}

flatten_named <- function(x, prefix = "") {
  out <- list()
  if (is.null(x)) return(out)

  nm <- names(x)
  if (is.null(nm)) return(out)

  for (k in nm) {
    key <- if (prefix == "") k else paste0(prefix, k)
    v <- x[[k]]

    if (is.null(v)) {
      out[[key]] <- NA
    } else if (is_scalar(v)) {
      out[[key]] <- normalize_scalar(v)
    } else if (is.list(v) && !is.null(names(v))) {
      nested <- flatten_named(v, paste0(key, "_"))
      out <- c(out, nested)
    }
  }

  out
}

rows_to_df <- function(rows) {
  if (length(rows) == 0) return(data.frame(stringsAsFactors = FALSE))

  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  normalized <- lapply(rows, function(r) {
    missing <- setdiff(all_names, names(r))
    if (length(missing) > 0) {
      for (m in missing) r[[m]] <- NA
    }

    for (nm in names(r)) {
      r[[nm]] <- normalize_cell(r[[nm]])
    }

    r[all_names]
  })

  parts <- lapply(normalized, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE)
  })

  df <- do.call(rbind, parts)
  rownames(df) <- NULL
  df
}

df_to_rows <- function(df) {
  if (nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

bind_df_list <- function(df_list) {
  row_list <- unlist(lapply(df_list, df_to_rows), recursive = FALSE)
  rows_to_df(row_list)
}

read_cogflow_result <- function(path) {
  ext <- file_ext(path)
  if (ext == "json") {
    return(list(kind = "json", data = jsonlite::fromJSON(path, simplifyVector = FALSE)))
  }
  if (ext == "csv") {
    return(list(kind = "csv", data = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)))
  }
  stop(sprintf("Unsupported file extension for %s (expected .json or .csv)", path))
}

extract_trial_list_json <- function(root) {
  # Portal-export wrapper: {run_session_id, ..., result_payload:{trials:[...]}}
  if (is.list(root) && is.list(root$result_payload) && is.list(root$result_payload$trials)) {
    env <- root$result_payload
    env$run_session_id <- root$run_session_id %||% NA
    env$study_slug <- root$study_slug %||% NA
    env$run_status <- root$status %||% NA
    env$participant_key_preview <- root$participant_key_preview %||% NA
    env$started_at <- root$started_at %||% NA
    env$completed_at <- root$completed_at %||% NA
    return(list(
      trials = root$result_payload$trials,
      envelope = env,
      source = "cogflow-run-json-wrapper"
    ))
  }

  # Direct jatos envelope: {trials:[...]}.
  if (is.list(root) && !is.null(root$trials) && is.list(root$trials)) {
    return(list(
      trials = root$trials,
      envelope = root,
      source = "cogflow-jatos-result-v1"
    ))
  }

  if (is.list(root) && (is.null(names(root)) || all(names(root) == ""))) {
    return(list(
      trials = root,
      envelope = list(),
      source = "jspsych-array"
    ))
  }

  stop("Unsupported JSON structure: expected {trials:[...]} or top-level array of trial objects")
}

extract_trial_list_csv <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(trials = list(), envelope = list(), source = "cogflow-run-csv"))
  }

  first <- as.list(df[1, , drop = FALSE])
  envelope <- list(
    run_session_id = normalize_scalar(first$run_session_id %||% NA),
    study_slug = normalize_scalar(first$study_slug %||% NA),
    run_status = normalize_scalar(first$status %||% NA),
    participant_key_preview = normalize_scalar(first$participant_key_preview %||% NA),
    started_at = normalize_scalar(first$started_at %||% NA),
    completed_at = normalize_scalar(first$completed_at %||% NA),
    task_type = normalize_scalar(first$trial_task_type %||% NA),
    experiment_type = normalize_scalar(first$trial_experiment_type %||% NA),
    config_id = normalize_scalar(first$trial_config_id %||% NA)
  )

  rows <- split(df, seq_len(nrow(df)))
  trials <- lapply(rows, function(r) {
    row <- as.list(r[1, , drop = FALSE])
    list(
      plugin_type = row$trial_plugin_type %||% NA,
      rt = row$trial_rt %||% NA,
      stimulus = row$trial_stimulus %||% NA,
      response = row$trial_response %||% NA,
      trial_type = row$trial_trial_type %||% NA,
      trial_index = row$trial_trial_index %||% NA,
      plugin_version = row$trial_plugin_version %||% NA,
      time_elapsed = row$trial_time_elapsed %||% NA,
      task_type = row$trial_task_type %||% NA,
      experiment_type = row$trial_experiment_type %||% NA,
      config_id = row$trial_config_id %||% NA,
      trial_row_index = row$trial_row_index %||% NA
    )
  })

  list(trials = trials, envelope = envelope, source = "cogflow-run-csv")
}

extract_trial_list <- function(root_obj) {
  if (root_obj$kind == "json") return(extract_trial_list_json(root_obj$data))
  if (root_obj$kind == "csv") return(extract_trial_list_csv(root_obj$data))
  stop("Unsupported payload kind")
}

base_context <- function(file_path, source_format, tr, envelope) {
  list(
    file_path = file_path,
    source_format = source_format,
    run_session_id = envelope$run_session_id %||% NA,
    study_slug = envelope$study_slug %||% NA,
    run_status = envelope$run_status %||% NA,
    participant_key_preview = envelope$participant_key_preview %||% NA,
    started_at = envelope$started_at %||% NA,
    completed_at = envelope$completed_at %||% NA,
    task_type = tr$task_type %||% envelope$task_type %||% NA,
    experiment_type = tr$experiment_type %||% envelope$experiment_type %||% NA,
    config_id = tr$config_id %||% envelope$config_id %||% NA,
    created_at = envelope$created_at %||% NA
  )
}

extract_all_trials <- function(trials, file_path, envelope, source_format) {
  out <- list()
  for (tr in trials) {
    row <- c(
      base_context(file_path, source_format, tr, envelope),
      list(
        trial_index = tr$trial_index %||% NA,
        plugin_type = tr$plugin_type %||% NA,
        trial_type = tr$trial_type %||% NA,
        rt_ms = tr$rt_ms %||% tr$rt %||% NA,
        response = tr$response %||% NA,
        stimulus = tr$stimulus %||% NA,
        plugin_version = tr$plugin_version %||% NA,
        time_elapsed = tr$time_elapsed %||% NA,
        correct_side = tr$correct_side %||% NA,
        response_side = tr$response_side %||% NA,
        response_key = tr$response_key %||% NA,
        response_angle_deg = tr$response_angle_deg %||% NA,
        response_segment_index = tr$response_segment_index %||% NA,
        accuracy = tr$accuracy %||% tr$correctness %||% NA,
        end_reason = tr$end_reason %||% tr$ended_reason %||% NA,
        survey_responses_json = if (!is.null(tr$responses)) jsonlite::toJSON(tr$responses, auto_unbox = TRUE, null = "null") else NA
      ),
      # Keep plugin-specific scalar fields (e.g., drt_* metrics) for downstream analysis.
      flatten_named(tr, "trial_")
    )
    out[[length(out) + 1]] <- row
  }
  rows_to_df(out)
}

extract_rdm_trial_based <- function(trials, file_path, envelope, source_format) {
  out <- list()

  for (tr in trials) {
    plugin_type <- tolower(as.character(tr$plugin_type %||% ""))
    trial_type <- tolower(as.character(tr$trial_type %||% ""))

    is_rdm_trial <- plugin_type %in% c("rdm-trial", "rdm") || trial_type == "rdm"
    if (!is_rdm_trial) next

    row <- list(
      run_context = base_context(file_path, source_format, tr, envelope),
      trial_index = tr$trial_index %||% NA,
      trial_row_index = tr$trial_row_index %||% NA,
      block_index = tr$`_block_index` %||% NA,
      plugin_type = tr$plugin_type %||% NA,
      trial_type = tr$trial_type %||% NA,
      correct_side = tr$correct_side %||% NA,
      response_side = tr$response_side %||% NA,
      response_key = tr$response_key %||% NA,
      response_angle_deg = tr$response_angle_deg %||% NA,
      response_segment_index = tr$response_segment_index %||% NA,
      rt_ms = tr$rt_ms %||% tr$rt %||% NA,
      accuracy = tr$accuracy %||% tr$correctness %||% NA,
      end_reason = tr$end_reason %||% tr$ended_reason %||% NA,
      time_elapsed = tr$time_elapsed %||% NA
    )

    row <- c(row$run_context, row[names(row) != "run_context"])

    row <- c(
      row,
      flatten_named(tr$rdm_parameters %||% list(), "rdm_"),
      flatten_named(tr$response_parameters %||% list(), "response_params_"),
      flatten_named(tr$timing_parameters %||% list(), "timing_")
    )

    out[[length(out) + 1]] <- row
  }

  rows_to_df(out)
}

extract_rdm_continuous <- function(trials, file_path, envelope, source_format) {
  event_rows <- list()
  frame_rows <- list()

  for (tr in trials) {
    plugin_type <- tolower(as.character(tr$plugin_type %||% ""))
    trial_type <- tolower(as.character(tr$trial_type %||% ""))

    is_continuous <- plugin_type == "rdm-continuous" || trial_type == "rdm-continuous"
    if (!is_continuous) next

    records <- tr$records %||% list()
    for (rec in records) {
      base <- list(
        run_context = base_context(file_path, source_format, tr, envelope),
        trial_index = tr$trial_index %||% NA,
        frames_count = tr$frames_count %||% NA,
        trial_end_reason = tr$ended_reason %||% tr$end_reason %||% NA,
        event = rec$event %||% NA,
        frame_index = rec$frame_index %||% NA,
        t_ms = rec$t_ms %||% NA,
        rt_ms = rec$rt_ms %||% NA,
        event_end_reason = rec$ended_reason %||% NA,
        correct_side = rec$correct_side %||% NA,
        response_side = rec$response_side %||% NA,
        response_key = rec$response_key %||% NA,
        response_angle_deg = rec$response_angle_deg %||% NA,
        response_segment_index = rec$response_segment_index %||% NA,
        accuracy = rec$accuracy %||% rec$correctness %||% NA
      )

      base <- c(base$run_context, base[names(base) != "run_context"])

      event_row <- c(
        base,
        flatten_named(rec$rdm %||% list(), "rdm_"),
        flatten_named(rec$response %||% list(), "response_")
      )

      event_rows[[length(event_rows) + 1]] <- event_row

      if (tolower(as.character(rec$event %||% "")) == "frame_end") {
        frame_rows[[length(frame_rows) + 1]] <- event_row
      }
    }
  }

  list(
    events = rows_to_df(event_rows),
    frame_summary = rows_to_df(frame_rows)
  )
}

ingest_rdm_file <- function(path) {
  root_obj <- read_cogflow_result(path)
  extracted <- extract_trial_list(root_obj)

  all_trials <- extract_all_trials(
    trials = extracted$trials,
    file_path = path,
    envelope = extracted$envelope,
    source_format = extracted$source
  )

  trial_based <- extract_rdm_trial_based(
    trials = extracted$trials,
    file_path = path,
    envelope = extracted$envelope,
    source_format = extracted$source
  )

  continuous <- extract_rdm_continuous(
    trials = extracted$trials,
    file_path = path,
    envelope = extracted$envelope,
    source_format = extracted$source
  )

  list(
    file_path = path,
    source_format = extracted$source,
    all_trials = all_trials,
    trial_based = trial_based,
    continuous_events = continuous$events,
    continuous_frame_summary = continuous$frame_summary
  )
}

ingest_rdm_paths <- function(files) {
  if (length(files) == 0) {
    stop("No files provided")
  }

  per_file <- lapply(files, ingest_rdm_file)

  all_trials <- bind_df_list(lapply(per_file, function(x) x$all_trials))
  trial_based <- bind_df_list(lapply(per_file, function(x) x$trial_based))
  continuous_events <- bind_df_list(lapply(per_file, function(x) x$continuous_events))
  continuous_frame_summary <- bind_df_list(lapply(per_file, function(x) x$continuous_frame_summary))

  list(
    files_scanned = files,
    per_file = per_file,
    all_trials = all_trials,
    trial_based = trial_based,
    continuous_events = continuous_events,
    continuous_frame_summary = continuous_frame_summary
  )
}

ingest_rdm_dir <- function(dir, pattern = "\\.(json|csv)$", recursive = TRUE) {
  files <- list.files(
    path = dir,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )

  if (length(files) == 0) {
    stop(sprintf("No files matched pattern '%s' in %s", pattern, dir))
  }

  ingest_rdm_paths(files)
}

summarize_rdm_trial_based <- function(trial_based_df) {
  if (nrow(trial_based_df) == 0) return(data.frame(stringsAsFactors = FALSE))

  trial_based_df$accuracy_num <- suppressWarnings(as.numeric(trial_based_df$accuracy))
  trial_based_df$responded <- !is.na(trial_based_df$response_side)

  aggregate(
    cbind(accuracy_num, responded) ~ file_path + config_id,
    data = trial_based_df,
    FUN = function(x) mean(x, na.rm = TRUE)
  )
}

summarize_rdm_continuous_frames <- function(frame_df) {
  if (nrow(frame_df) == 0) return(data.frame(stringsAsFactors = FALSE))

  frame_df$accuracy_num <- suppressWarnings(as.numeric(frame_df$accuracy))
  reg_col <- if ("response_response_registered" %in% names(frame_df)) frame_df$response_response_registered else rep(NA, nrow(frame_df))
  reg_raw <- tolower(as.character(reg_col))
  frame_df$responded <- !is.na(frame_df$response_side) | reg_raw %in% c("true", "1")

  aggregate(
    cbind(accuracy_num, responded) ~ file_path + config_id,
    data = frame_df,
    FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
  )
}
