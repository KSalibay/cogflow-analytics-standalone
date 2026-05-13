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
    stop("Usage: Rscript scripts/example_sart_ingest.R <directory-or-file1> [file2 file3 ...]")
  }
}

suppressPackageStartupMessages({
  library(jsonlite)
})

source("R/sart_ingest.R")

input_path <- args[[1]]
if (length(args) == 1 && dir.exists(input_path)) {
  cat(sprintf("Scanning directory: %s\n", input_path))
  out <- ingest_sart_dir(input_path)
} else {
  cat(sprintf("Scanning explicit file list (%d files)\n", length(args)))
  out <- ingest_sart_paths(args)
}

cat(sprintf("Files scanned: %d\n", length(out$files_scanned)))
cat(sprintf("All trial rows: %d\n", nrow(out$all_trials)))
cat(sprintf("SART trial rows: %d\n", nrow(out$sart_trials)))

if (nrow(out$sart_trials) > 0) {
  cat("\nSART summary (mean rt/accuracy/commission/omission by file+config):\n")
  print(summarize_sart_trials(out$sart_trials))
}
