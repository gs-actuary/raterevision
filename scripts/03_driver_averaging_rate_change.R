# 03_driver_averaging_rate_change.R
# A realistic multi-stage rating workflow using ratingtables + raterevision.

library(raterevision)
library(ratingtables)
source("scripts/_demo_helpers.R")

current_factors <- demo_make_factor_table()

# Current book rating:
#   drivers -> driver relativities -> household averages -> vehicles -> premium
current_output <- demo_rate_book(current_factors)
current_output[, c("vehicle_id", "charter", "territory", "indicated_BI", "indicated_CL")]

# Build a proposed factor table programmatically.
proposed_factors <- current_factors

# New age shape.
new_age <- proposed_factors[proposed_factors$term_name == "driver_age", , drop = FALSE]
age <- as.numeric(new_age$level1)
new_age$term_value <- new_age$term_value * ifelse(age < 25, 1.05, ifelse(age > 70, 1.03, .99))
proposed_factors <- replace_factor_set(proposed_factors, new_age)

# New territory shape.
new_territory <- proposed_factors[proposed_factors$term_name == "territory", , drop = FALSE]
new_territory$term_value <- new_territory$term_value * c(T1 = .97, T2 = 1.00, T3 = 1.03, T4 = 1.06)[new_territory$level1]
proposed_factors <- replace_factor_set(proposed_factors, new_territory)

# Proposed book rating runs exactly the same multi-stage orchestration.
proposed_output <- demo_rate_book(proposed_factors)

impact <- compare_rating_outputs(
  current_output,
  proposed_output,
  id_cols = "vehicle_id",
  keep_cols = c("policy_id", "charter", "territory")
)
head(impact, 20)

# Overall and by common actuarial dimensions.
summarize_rate_change(impact, by = NULL)
summarize_rate_change(impact, by = "coverage")
summarize_rate_change(impact, by = c("charter", "coverage"))
summarize_rate_change(impact, by = c("territory", "coverage"))

# Distribution / dislocation view.
rate_change_distribution(impact)
rate_change_distribution(impact, by = "coverage")

# The factor change audit is independent of how complicated the rater is.
rate_plan_diff(current_factors, proposed_factors, include_unchanged = FALSE)
