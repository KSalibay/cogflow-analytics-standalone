#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  if (interactive()) {
    if (!exists("output_dir") || !exists("input_paths")) {
      stop(
        "Interactive mode: set output_dir and input_paths before sourcing.\n",
        "Example:\n",
        "  output_dir <- 'output/data_dictionaries'\n",
        "  input_paths <- c('/path/to/results')"
      )
    }
    args <- c(as.character(output_dir), as.character(input_paths))
  } else {
    stop(
      "Usage: Rscript scripts/compile_data_dictionary.R <output_dir> <directory-or-file1> [file2 file3 ...]"
    )
  }
}

output_dir <- args[[1]]
input_args <- args[-1]

suppressPackageStartupMessages({
  library(jsonlite)
})

source("R/rdm_ingest.R")
source("R/gabor_ingest.R")
source("R/sart_ingest.R")
source("R/soc_dashboard_ingest.R")
source("R/mixed_task_ingest.R")

resolve_input_files <- function(paths) {
  out <- c()
  for (p in paths) {
    if (dir.exists(p)) {
      found <- list.files(
        path = p,
        pattern = "\\.(json|csv)$",
        full.names = TRUE,
        recursive = TRUE
      )
      out <- c(out, found)
    } else if (file.exists(p)) {
      out <- c(out, p)
    } else {
      warning(sprintf("Skipping missing path: %s", p), call. = FALSE)
    }
  }
  unique(out)
}

infer_type <- function(x) {
  if (length(x) == 0) return("unknown")

  vals <- x[!is.na(x)]
  vals <- vals[trimws(as.character(vals)) != ""]
  if (length(vals) == 0) return("mostly-empty")

  chars <- as.character(vals)

  logical_mask <- tolower(chars) %in% c("true", "false")
  if (all(logical_mask)) return("logical")

  num_mask <- suppressWarnings(!is.na(as.numeric(chars)))
  if (all(num_mask)) {
    nums <- suppressWarnings(as.numeric(chars))
    if (all(abs(nums - round(nums)) < 1e-9, na.rm = TRUE)) {
      return("integer")
    }
    return("numeric")
  }

  json_mask <- grepl("^[\\[{]", trimws(chars), perl = TRUE)
  if (mean(json_mask) > 0.8) return("json-string")

  "character"
}

sample_values <- function(x, n = 3) {
  vals <- x[!is.na(x)]
  vals <- trimws(as.character(vals))
  vals <- vals[vals != "" & vals != "NA" & vals != "null"]
  vals <- unique(vals)
  if (length(vals) == 0) return(NA_character_)

  vals <- vals[seq_len(min(length(vals), n))]
  vals <- substr(vals, 1, 120)
  paste(vals, collapse = " | ")
}

field_description <- function(field_name) {
  desc <- list(
    file_path = "Absolute source file path used in ingestion.",
    source_format = "Detected source envelope format (wrapper/json/csv/jsPsych array).",
    run_session_id = "Run session UUID from portal wrapper when present.",
    study_slug = "Study slug/code from portal wrapper when present.",
    run_status = "Run completion status from portal wrapper.",
    participant_key_preview = "Participant key preview from portal wrapper.",
    started_at = "Run start timestamp from wrapper.",
    completed_at = "Run completion timestamp from wrapper.",
    task_type = "Task type label attached to trial (or envelope fallback).",
    experiment_type = "Experiment mode label (for example trial-based or continuous).",
    config_id = "Config identifier associated with the trial/run.",
    created_at = "Result payload creation timestamp.",
    trial_index = "Sequential trial index in the exported timeline.",
    trial_row_index = "Row index if payload came from flat CSV export.",
    plugin_type = "Plugin identifier recorded for the trial.",
    trial_type = "jsPsych trial type identifier.",
    rt_ms = "Response time in milliseconds for this trial/event.",
    response = "Raw response field from trial row.",
    stimulus = "Stimulus content captured in trial row.",
    plugin_version = "Plugin version string when provided.",
    time_elapsed = "Elapsed time since task/run start at this row.",
    correct_side = "Correct side/category label for side-based scoring.",
    response_side = "Participant chosen side/category label.",
    response_key = "Raw response key/button identifier.",
    response_angle_deg = "Participant response direction in degrees.",
    response_segment_index = "Selected segment index in segmented response modes.",
    accuracy = "Correctness/accuracy value for the row.",
    end_reason = "End reason from trial-level row.",
    survey_responses_json = "Survey responses encoded as JSON string.",
    block_index = "Compiler-generated block index where available.",
    frames_count = "Number of continuous frames in the segment.",
    trial_end_reason = "Continuous trial-level end reason.",
    event = "Continuous event type (for example response or frame_end).",
    frame_index = "Continuous frame index inside segment.",
    t_ms = "Event timestamp in milliseconds relative to segment/task timeline.",
    event_end_reason = "Event-level end reason recorded in continuous events."
  )

  if (!is.null(desc[[field_name]])) return(desc[[field_name]])

  if (startsWith(field_name, "trial_")) {
    return("Flattened scalar copied from raw trial payload (trial.*).")
  }
  if (startsWith(field_name, "rdm_")) {
    return("Flattened scalar from continuous event rdm object.")
  }
  if (startsWith(field_name, "response_")) {
    return("Flattened scalar from response object (trial-level or continuous event-level).")
  }
  if (startsWith(field_name, "response_params_")) {
    return("Flattened scalar from response_parameters object.")
  }
  if (startsWith(field_name, "timing_")) {
    return("Flattened scalar from timing_parameters object.")
  }

  "Field present in payload or derived normalization output; inspect sample values for semantics."
}

field_glossary_from_tables <- function(task_name, table_map, field_name) {
  present_tables <- c()
  col_values <- c()

  for (tbl in names(table_map)) {
    df <- table_map[[tbl]]
    if (!is.data.frame(df) || ncol(df) == 0) next
    if (!(field_name %in% names(df))) next

    present_tables <- c(present_tables, tbl)
    col_values <- c(col_values, as.character(df[[field_name]]))
  }

  non_null <- if (length(col_values) == 0) 0L else {
    sum(!(is.na(col_values) | trimws(col_values) %in% c("", "NA", "null")))
  }
  total <- length(col_values)
  coverage <- if (total > 0) round(100 * non_null / total, 2) else NA_real_

  data.frame(
    task = task_name,
    field_name = field_name,
    detected_type = infer_type(col_values),
    appears_in_tables = paste(unique(present_tables), collapse = " | "),
    table_count = length(unique(present_tables)),
    non_null_rows = non_null,
    total_rows = total,
    coverage_pct = coverage,
    sample_values = sample_values(col_values),
    description = field_description(field_name),
    stringsAsFactors = FALSE
  )
}

build_task_glossary <- function(task_name, table_map) {
  fields <- unique(unlist(lapply(table_map, function(df) {
    if (!is.data.frame(df) || ncol(df) == 0) return(character())
    names(df)
  }), use.names = FALSE))

  if (length(fields) == 0) {
    return(data.frame(
      task = character(),
      field_name = character(),
      detected_type = character(),
      appears_in_tables = character(),
      table_count = integer(),
      non_null_rows = integer(),
      total_rows = integer(),
      coverage_pct = numeric(),
      sample_values = character(),
      description = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(fields, function(field_name) {
    field_glossary_from_tables(task_name, table_map, field_name)
  })

  out <- do.call(rbind, rows)
  out <- out[order(out$field_name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

files <- resolve_input_files(input_args)
if (length(files) == 0) {
  stop("No readable .json/.csv input files found.")
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat(sprintf("Resolved files: %d\n", length(files)))

ingested <- list()
skipped <- list()

for (f in files) {
  res <- tryCatch(
    ingest_rdm_file(f),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    skipped[[length(skipped) + 1]] <- list(file_path = f, reason = conditionMessage(res))
  } else {
    ingested[[length(ingested) + 1]] <- res
  }
}

if (length(ingested) == 0) {
  stop("No input files could be ingested. Check that files are CogFlow result JSON/CSV payloads.")
}

cat(sprintf("Successfully ingested: %d\n", length(ingested)))
cat(sprintf("Skipped as non-CogFlow payloads: %d\n", length(skipped)))

rdm <- list(
  all_trials = bind_df_list(lapply(ingested, function(x) x$all_trials)),
  trial_based = bind_df_list(lapply(ingested, function(x) x$trial_based)),
  continuous_events = bind_df_list(lapply(ingested, function(x) x$continuous_events)),
  continuous_frame_summary = bind_df_list(lapply(ingested, function(x) x$continuous_frame_summary))
)

all_trials <- rdm$all_trials

if (nrow(all_trials) == 0) {
  empty <- all_trials
  gabor_trials <- empty
  sart_trials <- empty
  soc_trials <- empty
  mixed_split <- list(
    rdm_trials = empty,
    gabor_trials = empty,
    drt_trials = empty,
    mw_probe_trials = empty,
    survey_trials = empty,
    other_trials = empty
  )
} else {
  gabor_keep <- mapply(
    is_gabor_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  sart_keep <- mapply(
    is_sart_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  soc_keep <- mapply(
    is_soc_row,
    all_trials$task_type,
    all_trials$plugin_type,
    all_trials$trial_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  gabor_trials <- all_trials[which(gabor_keep), , drop = FALSE]
  sart_trials <- all_trials[which(sart_keep), , drop = FALSE]
  soc_trials <- all_trials[which(soc_keep), , drop = FALSE]
  mixed_split <- split_task_types(all_trials)
}

gabor <- list(all_trials = all_trials, gabor_trials = gabor_trials)
sart <- list(all_trials = all_trials, sart_trials = sart_trials)
soc <- list(all_trials = all_trials, soc_dashboard_trials = soc_trials)
mixed <- c(list(all_trials = all_trials), mixed_split)

# Replace previous CSV output with glossary-only CSV files.
existing_csv <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(existing_csv) > 0) {
  unlink(existing_csv, force = TRUE)
}

write_task_glossary <- function(task_name, table_map) {
  glossary <- build_task_glossary(task_name, table_map)
  out_path <- file.path(output_dir, sprintf("%s_glossary.csv", task_name))
  utils::write.csv(glossary, out_path, row.names = FALSE, na = "")
  cat(sprintf("Wrote %s (%d fields)\n", out_path, nrow(glossary)))
}

write_task_glossary("rdm", list(
  all_trials = rdm$all_trials,
  trial_based = rdm$trial_based,
  continuous_events = rdm$continuous_events,
  continuous_frame_summary = rdm$continuous_frame_summary
))

write_task_glossary("gabor", list(
  all_trials = gabor$all_trials,
  gabor_trials = gabor$gabor_trials
))

write_task_glossary("sart", list(
  all_trials = sart$all_trials,
  sart_trials = sart$sart_trials
))

write_task_glossary("soc_dashboard", list(
  all_trials = soc$all_trials,
  soc_dashboard_trials = soc$soc_dashboard_trials
))

write_task_glossary("mixed", list(
  all_trials = mixed$all_trials,
  rdm_trials = mixed$rdm_trials,
  gabor_trials = mixed$gabor_trials,
  drt_trials = mixed$drt_trials,
  mw_probe_trials = mixed$mw_probe_trials,
  survey_trials = mixed$survey_trials,
  other_trials = mixed$other_trials
))

cat("Done.\n")
