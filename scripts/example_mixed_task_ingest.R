#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  if (interactive()) {
    if (!exists("input_paths")) {
      stop(
        "No input specified. Set input_paths to a directory or file list before sourcing.\n",
        "  Example: input_paths <- c('path/to/run-a.json', 'path/to/run-b.csv')"
      )
    }
    args <- if (is.character(input_paths)) input_paths else as.character(input_paths)
  } else {
    stop("Usage: Rscript scripts/example_mixed_task_ingest.R <directory-or-file1> [file2 file3 ...]")
  }
}

suppressPackageStartupMessages({
  library(jsonlite)
})

source("R/mixed_task_ingest.R")

input_path <- args[[1]]
if (length(args) == 1 && dir.exists(input_path)) {
  cat(sprintf("Scanning directory: %s\n", input_path))
  out <- ingest_mixed_task_dir(input_path)
} else {
  cat(sprintf("Scanning explicit file list (%d files)\n", length(args)))
  out <- ingest_mixed_task_paths(args)
}

cat(sprintf("Files scanned: %d\n", length(out$files_scanned)))
cat(sprintf("All trial rows: %d\n", nrow(out$all_trials)))
cat(sprintf("RDM rows: %d\n", nrow(out$rdm_trials)))
cat(sprintf("Gabor rows: %d\n", nrow(out$gabor_trials)))
cat(sprintf("DRT rows: %d\n", nrow(out$drt_trials)))
cat(sprintf("MW probe rows: %d\n", nrow(out$mw_probe_trials)))
cat(sprintf("Survey rows: %d\n", nrow(out$survey_trials)))
cat(sprintf("Other rows: %d\n", nrow(out$other_trials)))

cat("\nTask-type counts:\n")
print(summarize_mixed_task_counts(out$all_trials))
