# 01_excel_roundtrip.R
# Run line-by-line. This script demonstrates the human-editable Excel workflow.

library(raterevision)
source("scripts/_demo_helpers.R")

obj <- demo_objects()
current_factors <- obj$factor_table

# The source table is normalized and long.
head(current_factors)
dim(current_factors)

# Restrict the workbook to one state while keeping both charters.
# Driver ages will appear 16:90 for Charter A, then 16:90 for Charter B.
wide <- rate_tables_wide(
  current_factors,
  filters = list(state = "IL"),
  matrix_terms = "age_gender_interaction"
)
wide
names(wide)
attr(wide, "manifest")

# Inspect an ordinary factor table.
head(wide$driver_age, 12)
tail(wide$driver_age, 12)

# The two-way interaction is split into one matrix sheet per coverage.
interaction_sheets <- grep("age_gender_interaction", names(wide), value = TRUE)
interaction_sheets
wide[[interaction_sheets[1]]][1:10, ]

# Convert the named list back to long without using Excel.
roundtrip_in_memory <- rate_tables_long(wide)
rate_plan_diff(current_factors, roundtrip_in_memory, include_unchanged = FALSE)

# Write a real Excel workbook in the working directory.
workbook_file <- file.path(getwd(), "raterevision_demo.xlsx")
write_rate_workbook(wide, workbook_file)
workbook_file

# At this point, open raterevision_demo.xlsx in Excel.
# Try changing a few factors or adding a new rating level, save, then continue.
# The _raterevision sheet is deliberately very hidden machine metadata.

from_excel <- read_rate_workbook(workbook_file)
validate_rate_plan(from_excel)

# Any manual edits are now explicit.
excel_changes <- rate_plan_diff(current_factors, from_excel, include_unchanged = FALSE)
excel_changes

# You can also read the workbook as the intermediate list instead of long data.
excel_tables <- read_rate_workbook(workbook_file, return = "tables")
excel_tables
