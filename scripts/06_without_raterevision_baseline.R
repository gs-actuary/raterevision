# 06_without_raterevision_baseline.R
# A deliberately plain baseline showing that the workflow is possible without
# raterevision, but requires repeated key-management and reshape code.
#
# This is useful in a demo immediately before the raterevision version.

library(ratingtables)
source("scripts/_demo_helpers.R")

current <- demo_make_factor_table()
proposed <- current

# --- Complete driver-age replacement without replace_factor_set() ----------
new_age <- current[current$term_name == "driver_age", , drop = FALSE]
age <- as.numeric(new_age$level1)
new_age$term_value <- new_age$term_value * ifelse(age < 25, 1.04, .995)

# Define manually what makes this a complete factor-set slice. The level itself
# must NOT be in the slice key, or a removed level would accidentally survive.
slice_cols <- c(
  "rate_set_key", "state", "charter", "rate_eff_date", "rate_exp_date",
  "coverage", "term_name", "variable1", "variable2"
)

make_key <- function(d, cols) {
  do.call(paste, c(lapply(d[cols], as.character), sep = "\r"))
}

replacement_slices <- unique(make_key(new_age, slice_cols))
keep <- !(make_key(proposed, slice_cols) %in% replacement_slices)
proposed <- rbind(proposed[keep, , drop = FALSE], new_age)
proposed$factor_row_id <- seq_len(nrow(proposed))

# --- Sparse update without update_factors() --------------------------------
hit <- proposed$term_name == "territory" & proposed$level1 == "T4"
proposed$term_value[hit] <- proposed$term_value[hit] * 1.05

# --- Hand-built coverage-wide driver-age table ------------------------------
# This is only one term. A general workbook export also has to repeat this for
# every term, preserve metadata, handle sparse coverages, interaction depth,
# sheet naming, and the reverse transformation.
age_long <- proposed[proposed$term_name == "driver_age", , drop = FALSE]
id_cols <- c(
  "rate_set_key", "state", "charter", "rate_eff_date", "rate_exp_date",
  "term_name", "variable1", "level1"
)
age_ids <- unique(age_long[id_cols])
age_wide <- age_ids
for (cov in sort(unique(age_long$coverage))) {
  z <- age_long[age_long$coverage == cov, , drop = FALSE]
  age_wide[[cov]] <- z$term_value[match(make_key(age_ids, id_cols), make_key(z, id_cols))]
}
age_wide$level_order <- suppressWarnings(as.numeric(age_wide$level1))
age_wide <- age_wide[order(age_wide$rate_set_key, age_wide$level_order), ]
age_wide$level_order <- NULL
head(age_wide, 12)

# --- Rate and compare without raterevision impact helpers -------------------
current_output <- demo_rate_book(current)
proposed_output <- demo_rate_book(proposed)

current_total <- sum(current_output$indicated_BI + current_output$indicated_CL)
proposed_total <- sum(proposed_output$indicated_BI + proposed_output$indicated_CL)
manual_overall_change <- proposed_total / current_total - 1
manual_overall_change

# By coverage requires another explicit calculation.
current_by_cov <- colSums(current_output[c("indicated_BI", "indicated_CL")])
proposed_by_cov <- colSums(proposed_output[c("indicated_BI", "indicated_CL")])
proposed_by_cov / current_by_cov - 1

# Nothing here is conceptually difficult. The point is that the repeated
# key-management, reshape, validation, diff, and impact plumbing is exactly the
# narrow layer raterevision packages into tested functions.
