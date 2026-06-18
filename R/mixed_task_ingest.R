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
    rdm_continuous_events = base$continuous_events,
    rdm_continuous_frame_summary = base$continuous_frame_summary,
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
    rdm_continuous_events = bind_df_list(lapply(per_file, function(x) x$rdm_continuous_events)),
    rdm_continuous_frame_summary = bind_df_list(lapply(per_file, function(x) x$rdm_continuous_frame_summary)),
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

num_or_na <- function(x) {
  suppressWarnings(as.numeric(x))
}

first_existing_col <- function(df, candidates, default = NA) {
  if (!is.data.frame(df) || nrow(df) == 0) return(default)
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  rep(default, nrow(df))
}

char_col <- function(df, name, default = NA_character_) {
  as.character(first_existing_col(df, c(name), default))
}

extract_probe_answer <- function(json_text, key) {
  txt <- as.character(json_text %||% "")
  if (length(txt) == 0) return(character(0))
  pat <- sprintf('"%s"\\s*:\\s*"?([^",}]*)"?', key)
  out <- rep(NA_character_, length(txt))
  for (i in seq_along(txt)) {
    m <- regexec(pat, txt[[i]], perl = TRUE)
    got <- regmatches(txt[[i]], m)[[1]]
    if (length(got) >= 2) out[[i]] <- got[[2]]
  }
  out
}

empty_event_log_df <- function() {
  data.frame(
    file_path = character(),
    run_session_id = character(),
    task_type = character(),
    config_id = character(),
    stream = character(),
    event_type = character(),
    event_time_ms = numeric(),
    source_trial_index = numeric(),
    segment_index = numeric(),
    frame_index = numeric(),
    rdm_coherence = numeric(),
    rdm_direction = numeric(),
    rt_ms = numeric(),
    response = character(),
    response_angle_deg = numeric(),
    response_angle_error_deg = numeric(),
    accuracy = character(),
    drt_segment_id = character(),
    drt_trial_number = numeric(),
    drt_responded = character(),
    probe_response_q1 = character(),
    probe_response_q2 = character(),
    probe_response_q3 = character(),
    raw_event = character(),
    stringsAsFactors = FALSE
  )
}

event_priority <- function(event_type) {
  v <- tolower(as.character(event_type %||% ""))
  out <- rep(9L, length(v))
  out[v == "crdm_update"] <- 1L
  out[v == "drt_onset"] <- 2L
  out[v == "crdm_response"] <- 3L
  out[v == "drt_response"] <- 4L
  out[v == "thought_probe"] <- 5L
  out
}

fill_down_by_session <- function(event_log) {
  if (!is.data.frame(event_log) || nrow(event_log) == 0) return(event_log)

  run_ids <- as.character(event_log$run_session_id)
  run_ids[is.na(run_ids)] <- ""
  idx <- split(seq_len(nrow(event_log)), run_ids)

  for (ids in idx) {
    last_coh <- NA_real_
    last_dir <- NA_real_
    for (i in ids) {
      cur_coh <- suppressWarnings(as.numeric(event_log$rdm_coherence[[i]]))
      cur_dir <- suppressWarnings(as.numeric(event_log$rdm_direction[[i]]))

      if (!is.na(cur_coh)) {
        last_coh <- cur_coh
      } else if (!is.na(last_coh)) {
        event_log$rdm_coherence[[i]] <- last_coh
      }

      if (!is.na(cur_dir)) {
        last_dir <- cur_dir
      } else if (!is.na(last_dir)) {
        event_log$rdm_direction[[i]] <- last_dir
      }
    }
  }

  event_log
}

compute_continuous_abs_time <- function(continuous_df, all_trials_df) {
  rel_time <- num_or_na(first_existing_col(continuous_df, c("event_time_ms", "t_ms")))
  if (!is.data.frame(continuous_df) || nrow(continuous_df) == 0) return(rel_time)

  event_trial_idx <- as.character(first_existing_col(continuous_df, c("trial_index"), ""))
  if (!is.data.frame(all_trials_df) || nrow(all_trials_df) == 0) return(rel_time)

  plugin <- tolower(as.character(first_existing_col(all_trials_df, c("plugin_type"), "")))
  trial_rows <- all_trials_df[plugin == "rdm-continuous", , drop = FALSE]
  if (!is.data.frame(trial_rows) || nrow(trial_rows) == 0) return(rel_time)

  trial_idx <- as.character(first_existing_col(trial_rows, c("trial_index"), ""))
  trial_end <- num_or_na(first_existing_col(trial_rows, c("trial_time_elapsed", "time_elapsed")))

  rel_max <- tapply(rel_time, event_trial_idx, function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    if (all(is.na(x_num))) return(NA_real_)
    suppressWarnings(max(x_num, na.rm = TRUE))
  })

  end_by_trial <- tapply(trial_end, trial_idx, function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    if (all(is.na(x_num))) return(NA_real_)
    suppressWarnings(max(x_num, na.rm = TRUE))
  })

  trial_start <- setNames(rep(NA_real_, length(rel_max)), names(rel_max))
  common <- intersect(names(rel_max), names(end_by_trial))
  if (length(common) > 0) {
    trial_start[common] <- end_by_trial[common] - rel_max[common]
  }

  abs_time <- rel_time + unname(trial_start[event_trial_idx])
  abs_time[is.na(abs_time)] <- rel_time[is.na(abs_time)]
  abs_time
}

build_mixed_event_log <- function(ingested) {
  # CRDM updates are the frame_end rows from continuous RDM.
  frames <- ingested$rdm_continuous_frame_summary %||% data.frame(stringsAsFactors = FALSE)
  updates <- empty_event_log_df()
  if (is.data.frame(frames) && nrow(frames) > 0) {
    frame_time <- compute_continuous_abs_time(frames, ingested$all_trials %||% data.frame(stringsAsFactors = FALSE))
    coherence <- num_or_na(first_existing_col(frames, c("rdm_coherence")))
    direction <- num_or_na(first_existing_col(frames, c("rdm_direction", "response_target_direction_deg")))

    updates <- data.frame(
      file_path = char_col(frames, "file_path"),
      run_session_id = char_col(frames, "run_session_id"),
      task_type = char_col(frames, "task_type"),
      config_id = char_col(frames, "config_id"),
      stream = rep("crdm", nrow(frames)),
      event_type = rep("crdm_update", nrow(frames)),
      event_time_ms = frame_time,
      source_trial_index = num_or_na(first_existing_col(frames, c("trial_index"))),
      segment_index = num_or_na(first_existing_col(frames, c("segment_index"))),
      frame_index = num_or_na(first_existing_col(frames, c("frame_index"))),
      rdm_coherence = coherence,
      rdm_direction = direction,
      rt_ms = NA_real_,
      response = NA_character_,
      response_angle_deg = NA_real_,
      response_angle_error_deg = NA_real_,
      accuracy = NA_character_,
      drt_segment_id = NA_character_,
      drt_trial_number = NA_real_,
      drt_responded = NA_character_,
      probe_response_q1 = NA_character_,
      probe_response_q2 = NA_character_,
      probe_response_q3 = NA_character_,
      raw_event = char_col(frames, "event"),
      stringsAsFactors = FALSE
    )
  }

  # CRDM responses are response-tagged rows from continuous event stream.
  events <- ingested$rdm_continuous_events %||% data.frame(stringsAsFactors = FALSE)
  responses <- empty_event_log_df()
  if (is.data.frame(events) && nrow(events) > 0) {
    ev <- tolower(as.character(events$event %||% ""))
    keep <- which(ev == "response")
    if (length(keep) > 0) {
      evt <- events[keep, , drop = FALSE]
      evt_time <- compute_continuous_abs_time(evt, ingested$all_trials %||% data.frame(stringsAsFactors = FALSE))
      responses <- data.frame(
        file_path = char_col(evt, "file_path"),
        run_session_id = char_col(evt, "run_session_id"),
        task_type = char_col(evt, "task_type"),
        config_id = char_col(evt, "config_id"),
        stream = rep("crdm", nrow(evt)),
        event_type = rep("crdm_response", nrow(evt)),
        event_time_ms = evt_time,
        source_trial_index = num_or_na(first_existing_col(evt, c("trial_index"))),
        segment_index = num_or_na(first_existing_col(evt, c("segment_index"))),
        frame_index = num_or_na(first_existing_col(evt, c("frame_index"))),
        rdm_coherence = num_or_na(first_existing_col(evt, c("rdm_coherence"))),
        rdm_direction = num_or_na(first_existing_col(evt, c("response_target_direction_deg", "rdm_direction"))),
        rt_ms = num_or_na(first_existing_col(evt, c("rt_ms"))),
        response = char_col(evt, "response_side"),
        response_angle_deg = num_or_na(first_existing_col(evt, c("response_angle_deg"))),
        response_angle_error_deg = num_or_na(first_existing_col(evt, c("response_angle_error_deg"))),
        accuracy = char_col(evt, "accuracy"),
        drt_segment_id = NA_character_,
        drt_trial_number = NA_real_,
        drt_responded = NA_character_,
        probe_response_q1 = NA_character_,
        probe_response_q2 = NA_character_,
        probe_response_q3 = NA_character_,
        raw_event = char_col(evt, "event"),
        stringsAsFactors = FALSE
      )
    }
  }

  # DRT source rows may appear as dedicated drt rows and/or flattened trial_drt_* fields.
  drt_source <- ingested$all_trials %||% data.frame(stringsAsFactors = FALSE)
  drt_onsets <- empty_event_log_df()
  drt_responses <- empty_event_log_df()
  if (is.data.frame(drt_source) && nrow(drt_source) > 0) {
    drt_onset_ms <- num_or_na(first_existing_col(drt_source, c("trial_drt_onset_ms", "drt_onset_ms")))
    drt_rt_ms <- num_or_na(first_existing_col(drt_source, c("trial_drt_rt_ms", "drt_rt_ms", "rt_ms")))
    drt_segment_id <- as.character(first_existing_col(drt_source, c("trial_drt_segment_id", "drt_segment_id")))
    drt_trial_number <- num_or_na(first_existing_col(drt_source, c("trial_drt_trial_number", "drt_trial_number")))
    drt_responded <- as.character(first_existing_col(drt_source, c("trial_drt_responded", "drt_responded")))
    drt_key <- as.character(first_existing_col(drt_source, c("trial_drt_key", "drt_key", "response")))
    drt_accuracy <- as.character(first_existing_col(drt_source, c("trial_drt_correct", "drt_correct", "accuracy")))

    valid_onset <- which(!is.na(drt_onset_ms))
    if (length(valid_onset) > 0) {
      src <- drt_source[valid_onset, , drop = FALSE]
      drt_onsets <- data.frame(
        file_path = char_col(src, "file_path"),
        run_session_id = char_col(src, "run_session_id"),
        task_type = char_col(src, "task_type"),
        config_id = char_col(src, "config_id"),
        stream = rep("drt", nrow(src)),
        event_type = rep("drt_onset", nrow(src)),
        event_time_ms = drt_onset_ms[valid_onset],
        source_trial_index = num_or_na(first_existing_col(src, c("trial_index"))),
        segment_index = NA_real_,
        frame_index = NA_real_,
        rdm_coherence = NA_real_,
        rdm_direction = NA_real_,
        rt_ms = NA_real_,
        response = NA_character_,
        response_angle_deg = NA_real_,
        response_angle_error_deg = NA_real_,
        accuracy = NA_character_,
        drt_segment_id = drt_segment_id[valid_onset],
        drt_trial_number = drt_trial_number[valid_onset],
        drt_responded = drt_responded[valid_onset],
        probe_response_q1 = NA_character_,
        probe_response_q2 = NA_character_,
        probe_response_q3 = NA_character_,
        raw_event = rep("drt_onset", nrow(src)),
        stringsAsFactors = FALSE
      )

      has_response <- valid_onset[!is.na(drt_rt_ms[valid_onset])]
      if (length(has_response) > 0) {
        src_r <- drt_source[has_response, , drop = FALSE]
        drt_responses <- data.frame(
          file_path = char_col(src_r, "file_path"),
          run_session_id = char_col(src_r, "run_session_id"),
          task_type = char_col(src_r, "task_type"),
          config_id = char_col(src_r, "config_id"),
          stream = rep("drt", nrow(src_r)),
          event_type = rep("drt_response", nrow(src_r)),
          event_time_ms = drt_onset_ms[has_response] + drt_rt_ms[has_response],
          source_trial_index = num_or_na(first_existing_col(src_r, c("trial_index"))),
          segment_index = NA_real_,
          frame_index = NA_real_,
          rdm_coherence = NA_real_,
          rdm_direction = NA_real_,
          rt_ms = drt_rt_ms[has_response],
          response = drt_key[has_response],
          response_angle_deg = NA_real_,
          response_angle_error_deg = NA_real_,
          accuracy = drt_accuracy[has_response],
          drt_segment_id = drt_segment_id[has_response],
          drt_trial_number = drt_trial_number[has_response],
          drt_responded = drt_responded[has_response],
          probe_response_q1 = NA_character_,
          probe_response_q2 = NA_character_,
          probe_response_q3 = NA_character_,
          raw_event = rep("drt_response", nrow(src_r)),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Thought probes use survey-response rows and retain parsed q1/q2/q3 answers.
  probes <- ingested$survey_trials %||% data.frame(stringsAsFactors = FALSE)
  probes_out <- empty_event_log_df()
  if (is.data.frame(probes) && nrow(probes) > 0) {
    probe_plugin <- tolower(as.character(probes$plugin_type %||% ""))
    keep <- which(probe_plugin == "survey-response")
    if (length(keep) > 0) {
      p <- probes[keep, , drop = FALSE]
      probe_time_ms <- num_or_na(first_existing_col(p, c("time_elapsed"))) - num_or_na(first_existing_col(p, c("rt_ms", "rt")))
      probes_out <- data.frame(
        file_path = char_col(p, "file_path"),
        run_session_id = char_col(p, "run_session_id"),
        task_type = char_col(p, "task_type"),
        config_id = char_col(p, "config_id"),
        stream = rep("probe", nrow(p)),
        event_type = rep("thought_probe", nrow(p)),
        event_time_ms = probe_time_ms,
        source_trial_index = num_or_na(first_existing_col(p, c("trial_index"))),
        segment_index = NA_real_,
        frame_index = NA_real_,
        rdm_coherence = NA_real_,
        rdm_direction = NA_real_,
        rt_ms = num_or_na(first_existing_col(p, c("rt_ms", "rt"))),
        response = char_col(p, "survey_responses_json"),
        response_angle_deg = NA_real_,
        response_angle_error_deg = NA_real_,
        accuracy = NA_character_,
        drt_segment_id = NA_character_,
        drt_trial_number = NA_real_,
        drt_responded = NA_character_,
        probe_response_q1 = extract_probe_answer(first_existing_col(p, c("survey_responses_json"), ""), "q1"),
        probe_response_q2 = extract_probe_answer(first_existing_col(p, c("survey_responses_json"), ""), "q2"),
        probe_response_q3 = extract_probe_answer(first_existing_col(p, c("survey_responses_json"), ""), "q3"),
        raw_event = rep("survey-response", nrow(p)),
        stringsAsFactors = FALSE
      )
    }
  }

  event_log <- rbind(updates, responses, drt_onsets, drt_responses, probes_out)
  if (!is.data.frame(event_log) || nrow(event_log) == 0) {
    return(empty_event_log_df())
  }

  event_log$event_time_ms <- num_or_na(event_log$event_time_ms)
  event_log$source_trial_index <- num_or_na(event_log$source_trial_index)
  event_log$.event_priority <- event_priority(event_log$event_type)

  event_log <- event_log[order(
    as.character(event_log$run_session_id),
    event_log$event_time_ms,
    event_log$.event_priority,
    event_log$source_trial_index,
    na.last = TRUE
  ), , drop = FALSE]

  event_log <- fill_down_by_session(event_log)
  event_log$event_order <- seq_len(nrow(event_log))
  event_log$.event_priority <- NULL
  rownames(event_log) <- NULL
  event_log
}
