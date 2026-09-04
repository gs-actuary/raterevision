# 02_programmatic_revision.R
# Programmatic factor-set replacement, sparse updates, rebasing and normalization.

library(raterevision)
source("scripts/_demo_helpers.R")

current <- demo_make_factor_table()
proposed <- current

# --- Complete factor-set replacement ---------------------------------------
# Pretend a modeling team delivered a new driver-age curve.
new_age <- current[current$term_name == "driver_age", , drop = FALSE]
age <- as.numeric(new_age$level1)
new_age$term_value <- new_age$term_value * (1 + 0.04 * pmax(30 - age, 0) / 14 - 0.02 * pmax(age - 60, 0) / 30)

# The entire matching driver_age slice is replaced, not merely overlapping rows.
proposed <- replace_factor_set(proposed, new_age)

# --- Sparse cell update -----------------------------------------------------
# Change one specific territory factor. All other factors are untouched.
one_change <- proposed[
  proposed$rate_set_key == "IL_A" & proposed$coverage == "BI" &
    proposed$term_name == "territory" & proposed$level1 == "T4",
  , drop = FALSE
]
one_change$term_value <- 1.25
proposed <- update_factors(proposed, one_change)

# --- Add a newly supported rating level ------------------------------------
# Copy an existing complete key, change the level, then explicitly allow it.
new_level <- proposed[
  proposed$rate_set_key == "IL_A" & proposed$coverage == "BI" &
    proposed$term_name == "underwriting_level" & proposed$level1 == "preferred",
  , drop = FALSE
]
new_level$level1 <- "ultra_preferred"
new_level$term_value <- 0.86
new_level$factor_row_id <- NULL
proposed <- update_factors(proposed, new_level, allow_new_levels = TRUE)

# --- Rebase -----------------------------------------------------------------
# Make age 20 the displayed 1.000 level separately by rate set and coverage.
# This preserves relative relationships but changes the overall scale of the
# age table, so a later rate-level solve may be appropriate.
proposed <- rebase_factor_set(proposed, "driver_age", base_level = "20")

age_check <- proposed[proposed$term_name == "driver_age" & proposed$level1 == "20",
                      c("rate_set_key", "coverage", "level1", "term_value")]
age_check

# --- Normalize --------------------------------------------------------------
# This example uses exposure weights attached to territory rows. The helper
# scales each charter/coverage territory table to weighted mean 1.000.
territory <- proposed[proposed$term_name == "territory", , drop = FALSE]
territory$exposure <- c(T1 = 35, T2 = 30, T3 = 22, T4 = 13)[territory$level1]
territory_norm <- normalize_factor_set(
  territory,
  term_name = "territory",
  weights = "exposure",
  target_mean = 1
)

# exposure is analytical metadata, not part of the production rating table.
territory_norm$exposure <- NULL
proposed <- replace_factor_set(proposed, territory_norm)

# --- Review -----------------------------------------------------------------
validate_rate_plan(proposed)
changes <- rate_plan_diff(current, proposed, include_unchanged = FALSE)
head(changes, 30)
table(changes$status)
