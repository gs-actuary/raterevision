# 05_full_demo_workflow.R
# End-to-end demo outline: workbook route and programmatic route converge on the
# same downstream rating and target-solving tools.

library(raterevision)
library(ratingtables)
source("scripts/_demo_helpers.R")

current <- demo_make_factor_table()

# 1. Human-editable representation.
wide <- rate_tables_wide(current, filters = list(state = "IL"),
                         matrix_terms = "age_gender_interaction")
write_rate_workbook(wide, "rate_revision_working.xlsx")

# DEMO OPTION A:
# Open rate_revision_working.xlsx, edit factors, save, then run:
# proposed_excel <- read_rate_workbook("rate_revision_working.xlsx")
# rate_plan_diff(current, proposed_excel, include_unchanged = FALSE)

# 2. Programmatic route.
proposed <- current
new_age <- current[current$term_name == "driver_age", , drop = FALSE]
age <- as.numeric(new_age$level1)
new_age$term_value <- new_age$term_value * ifelse(age < 25, 1.04, .995)
proposed <- replace_factor_set(proposed, new_age)

territory_changes <- proposed[
  proposed$term_name == "territory" & proposed$level1 == "T4",
  , drop = FALSE
]
territory_changes$term_value <- territory_changes$term_value * 1.05
proposed <- update_factors(proposed, territory_changes)

validate_rate_plan(proposed)
rate_plan_diff(current, proposed, include_unchanged = FALSE)

# 3. Rate current and proposed through a driver-averaging workflow.
current_output <- demo_rate_book(current)
proposed_output <- demo_rate_book(proposed)
impact <- compare_rating_outputs(
  current_output, proposed_output,
  id_cols = "vehicle_id",
  keep_cols = c("policy_id", "charter", "territory")
)
summarize_rate_change(impact, by = c("charter", "coverage"))
rate_change_distribution(impact, by = "coverage")

# 4. Solve one final common base-rate multiplier to reach the desired overall
# rate level after all classification changes.
current_total <- sum(current_output$indicated_BI + current_output$indicated_CL)
solution <- solve_rate_target(
  current_premium = current_total,
  target = .075,
  rate_function = function(x) {
    out <- demo_rate_book(proposed, base_multiplier = x)
    out$indicated_BI + out$indicated_CL
  },
  interval = c(.75, 1.40),
  parameter_name = "final base-rate multiplier"
)
solution

final_output <- demo_rate_book(proposed, base_multiplier = solution$parameter)
final_impact <- compare_rating_outputs(
  current_output, final_output,
  id_cols = "vehicle_id",
  keep_cols = c("policy_id", "charter", "territory")
)
summarize_rate_change(final_impact, by = NULL)
summarize_rate_change(final_impact, by = c("charter", "coverage"))
