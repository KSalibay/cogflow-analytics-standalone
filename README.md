# cogflow-analytics-standalone

Standalone analysis utilities for CogFlow result payloads.

## Current scope

This repo now includes a basic RDM ingestion pipeline in R for:

- trial-based RDM payloads (`rdm-trial`)
- continuous RDM payloads (`rdm-continuous`, including per-frame records)
- both JSON and CSV CogFlow exports

It also includes task-specific ingestion helpers for:

- SART (`R/sart_ingest.R`)
- SOC Dashboard (`R/soc_dashboard_ingest.R`)
- Gabor patch (`R/gabor_ingest.R`)

And a combined mixed-task splitter for a single result file containing multiple task elements:

- Mixed task split (`R/mixed_task_ingest.R`) into `rdm_trials`, `gabor_trials`, `drt_trials`, `mw_probe_trials`, `survey_trials`, and `other_trials`

The normalizer is designed for offline analysis and can later be reused to inform
platform-side Analysis tab logic.

## Files

- `R/rdm_ingest.R`: reusable ingestion + normalization functions
- `scripts/example_rdm_ingest.R`: minimal command-line example
- `R/sart_ingest.R`: SART-focused extraction + summary helpers
- `scripts/example_sart_ingest.R`: SART command-line example
- `R/soc_dashboard_ingest.R`: SOC Dashboard-focused extraction + summary helpers
- `scripts/example_soc_dashboard_ingest.R`: SOC Dashboard command-line example
- `R/gabor_ingest.R`: Gabor-focused extraction + summary helpers
- `scripts/example_gabor_ingest.R`: Gabor command-line example
- `R/mixed_task_ingest.R`: mixed-task extraction + split helpers (RDM/DRT/MW/Survey/Other)
- `scripts/example_mixed_task_ingest.R`: mixed-task command-line example

## Requirements

- R >= 4.1
- package: `jsonlite`

Install package once:

```r
install.packages("jsonlite")
```

## Quick start

Run the example against a folder containing CogFlow result files (`.json` and/or `.csv`):

```bash
Rscript scripts/example_rdm_ingest.R /path/to/results
```

Or pass explicit files (useful for ad hoc comparisons):

```bash
Rscript scripts/example_rdm_ingest.R /path/a.json /path/b.csv

# SART
Rscript scripts/example_sart_ingest.R /path/to/results

# SOC Dashboard
Rscript scripts/example_soc_dashboard_ingest.R /path/to/results

# Gabor patch
Rscript scripts/example_gabor_ingest.R /path/to/results

# Mixed task split (RDM + Gabor + DRT + MW probe + Survey + Other)
Rscript scripts/example_mixed_task_ingest.R /path/to/results

# Build task glossary CSVs (column-level)
Rscript scripts/compile_data_dictionary.R output/data_dictionaries /path/to/results

# Build from explicit file list
Rscript scripts/compile_data_dictionary.R output/data_dictionaries /path/a.json /path/b.csv
```

Glossary output includes one CSV per task (`rdm`, `gabor`, `sart`, `soc_dashboard`, `mixed`) with one row per column/field name (not one row per table) and:

- `field_name`: column/field name
- `detected_type`: inferred type from observed values
- `appears_in_tables`: which normalized tables include the field
- `non_null_rows` / `coverage_pct`: population metrics
- `sample_values`: quick examples
- `description`: semantic definition

## Programmatic use

```r
library(jsonlite)
source("R/rdm_ingest.R")

rdm <- ingest_rdm_dir("/path/to/results")

# Or explicit file list
# rdm <- ingest_rdm_paths(c("/path/a.json", "/path/b.csv"))

# All normalized trial rows (format-agnostic)
head(rdm$all_trials)

# Normalized trial-based rows
head(rdm$trial_based)

# Continuous event-level rows (response + frame_end)
head(rdm$continuous_events)

# Continuous frame-end rows only (one per frame)
head(rdm$continuous_frame_summary)

# Basic aggregates for trial-based RDM
summarize_rdm_trial_based(rdm$trial_based)
```

## Identifier fields included for researcher workflows

Each normalized table includes transparent provenance and participant/run context columns where available:

- `file_path`
- `source_format`
- `run_session_id`
- `study_slug`
- `participant_key_preview`
- `run_status`
- `started_at`
- `completed_at`

This makes it straightforward to separate one participant/run from another when batching many files.
