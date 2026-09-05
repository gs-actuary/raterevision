# 07_harvest_legacy_workbook.R
# Demonstrate the heuristic workbook-profiler / table-extraction workflow.
# Run from the raterevision package root after devtools::load_all().

library(raterevision)
library(openxlsx2)

# ---------------------------------------------------------------------------
# 1. Build a deliberately messy legacy workbook
# ---------------------------------------------------------------------------

legacy_file <- file.path(getwd(), "legacy_rater_demo.xlsx")

wb <- wb_workbook()
wb <- wb_add_worksheet(wb, "Auto Rates")
wb <- wb_add_worksheet(wb, "Fees and Notes")

wb <- wb_add_data(
  wb, "Auto Rates", "Territory Factors",
  start_col = 2, start_row = 2, col_names = FALSE
)
territory <- data.frame(
  Territory = paste0("T", 1:5),
  BI = c(.90, .96, 1.00, 1.08, 1.18),
  PD = c(.93, .98, 1.00, 1.06, 1.14),
  CL = c(.96, .99, 1.00, 1.04, 1.10),
  check.names = FALSE
)
wb <- wb_add_data(wb, "Auto Rates", territory,
                  start_col = 2, start_row = 4, col_names = TRUE)

wb <- wb_add_data(
  wb, "Auto Rates", "Driver Age Relativities",
  start_col = 2, start_row = 12, col_names = FALSE
)
age <- data.frame(
  Age = c(16, 17, 18, 20, 25, 35, 50, 70),
  BI = c(1.65, 1.48, 1.34, 1.20, 1.08, 1.02, 1.00, 1.10),
  PD = c(1.50, 1.38, 1.27, 1.16, 1.06, 1.01, 1.00, 1.08),
  CL = c(1.30, 1.25, 1.18, 1.12, 1.05, 1.01, 1.00, 1.06),
  check.names = FALSE
)
wb <- wb_add_data(wb, "Auto Rates", age,
                  start_col = 2, start_row = 14, col_names = TRUE)

# A small base-rate rectangle placed off to the side.
wb <- wb_add_data(
  wb, "Auto Rates", "Base Rates",
  start_col = 8, start_row = 2, col_names = FALSE
)
base <- data.frame(
  Charter = c("A", "B"),
  BI = c(220, 230),
  PD = c(175, 185),
  CL = c(180, 190),
  check.names = FALSE
)
wb <- wb_add_data(wb, "Auto Rates", base,
                  start_col = 8, start_row = 4, col_names = TRUE)

# Free-form notes should either be ignored or score poorly.
wb <- wb_add_data(
  wb, "Fees and Notes",
  data.frame(Note = c(
    "Legacy file used by pricing",
    "Yellow cells are inputs",
    "Do not change formulas"
  )),
  start_col = 1, start_row = 1, col_names = TRUE
)

wb_save(wb, legacy_file, overwrite = TRUE)
legacy_file

# ---------------------------------------------------------------------------
# 2. Profile the workbook
# ---------------------------------------------------------------------------

profile <- profile_rate_workbook(legacy_file)
profile

# The profile is an ordinary editable data frame. These are the most useful
# review columns.
profile[, c(
  "candidate_id", "source_sheet", "source_range", "extract_range",
  "table_name", "confidence_label", "confidence", "include", "notes"
)]

# ---------------------------------------------------------------------------
# 3. Review/edit the guesses
# ---------------------------------------------------------------------------

# This is the intended human-in-the-loop step. A real migration may need only a
# few changes after the profiler does the first pass.
#
# Examples:
# profile$include[profile$candidate_id == "candidate_004"] <- FALSE
# profile$table_name[profile$candidate_id == "candidate_002"] <- "driver_age"
# profile$extract_range[profile$candidate_id == "candidate_003"] <- "H4:K6"
# profile$header_row[profile$candidate_id == "candidate_003"] <- 4L

# For the synthetic demo, keep plausible Auto Rates candidates and drop notes.
profile$include <- profile$source_sheet == "Auto Rates" & profile$confidence >= .35

# ---------------------------------------------------------------------------
# 4. Extract the selected rectangles
# ---------------------------------------------------------------------------

harvested <- extract_rate_tables(profile)
harvested
names(harvested)
attr(harvested, "manifest")

# Inspect individual tables just as you would inspect rate_tables_wide() output.
for (nm in names(harvested)) {
  cat("\n---", nm, "---\n")
  print(harvested[[nm]])
}

# These are raw harvested rectangles, not yet guaranteed to be normalized
# ratingtables factor data. The next migration step is to map/clean the tables
# into the company's chosen rating-table schema, then validate the plan.
