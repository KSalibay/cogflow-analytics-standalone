#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# When run interactively (e.g. RStudio), commandArgs() returns no trailing args.
# In that case, prompt the user to set `input_paths` manually, or fall back to
# the bundled example configs directory.
if (length(args) < 1) {
  if (interactive()) {
    # Researchers using RStudio: set this variable before sourcing the script,
    # or edit the path below to point at your own data directory or file list.
    if (!exists("input_paths")) {
      input_paths <- file.path(dirname(dirname(sys.frame(1)$ofile)), "configs")
      if (!dir.exists(input_paths)) {
        stop(
          "No input specified. Set `input_paths` to a directory or character ",
          "vector of file paths before sourcing this script.\n",
          "  Example:  input_paths <- c('path/to/run-abc.json', 'path/to/run-abc.csv')"
        )
      }
      message(sprintf("No input_paths set — using bundled configs dir: %s", input_paths))
    }
    args <- if (is.character(input_paths)) input_paths else as.character(input_paths)
  } else {
    stop("Usage: Rscript scripts/example_rdm_ingest.R <directory-or-file1> [file2 file3 ...]")
  }
}

suppressPackageStartupMessages({
  library(jsonlite)
})

source("R/rdm_ingest.R")

input_path <- args[[1]]
if (length(args) == 1 && dir.exists(input_path)) {
  cat(sprintf("Scanning directory: %s\n", input_path))
  rdm <- ingest_rdm_dir(input_path)
} else {
  files <- args
  cat(sprintf("Scanning explicit file list (%d files)\n", length(files)))
  rdm <- ingest_rdm_paths(files)
}

cat(sprintf("Files scanned: %d\n", length(rdm$files_scanned)))
cat(sprintf("All trial rows: %d\n", nrow(rdm$all_trials)))
cat(sprintf("Trial-based RDM rows: %d\n", nrow(rdm$trial_based)))
cat(sprintf("Continuous RDM event rows: %d\n", nrow(rdm$continuous_events)))
cat(sprintf("Continuous RDM frame summary rows: %d\n", nrow(rdm$continuous_frame_summary)))

if (nrow(rdm$trial_based) > 0) {
  cat("\nTrial-based summary (mean accuracy / response rate by file+config):\n")
  print(summarize_rdm_trial_based(rdm$trial_based))
} else {
  cat("\nTrial-based summary: no rows (likely continuous-only RDM export).\n")
}

if (nrow(rdm$continuous_frame_summary) > 0) {
  cat("\nContinuous frame summary (mean accuracy / response rate by file+config):\n")
  print(summarize_rdm_continuous_frames(rdm$continuous_frame_summary))
}
