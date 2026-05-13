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
    stop("Usage: Rscript scripts/example_gabor_ingest.R <directory-or-file1> [file2 file3 ...]")
  }
}

suppressPackageStartupMessages({
  library(jsonlite)
})

source("R/gabor_ingest.R")

input_path <- args[[1]]
if (length(args) == 1 && dir.exists(input_path)) {
  cat(sprintf("Scanning directory: %s\n", input_path))
  out <- ingest_gabor_dir(input_path)
} else {
  cat(sprintf("Scanning explicit file list (%d files)\n", length(args)))
  out <- ingest_gabor_paths(args)
}

cat(sprintf("Files scanned: %d\n", length(out$files_scanned)))
cat(sprintf("All trial rows: %d\n", nrow(out$all_trials)))
cat(sprintf("Gabor trial rows: %d\n", nrow(out$gabor_trials)))

if (nrow(out$gabor_trials) > 0) {
  cat("\nGabor summary (mean rt/accuracy/contrast/orientation by file+config):\n")
  print(summarize_gabor_trials(out$gabor_trials))
}
