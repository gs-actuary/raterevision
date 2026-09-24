# 09_rare_discount_migration.R
# Run from the raterevision repository root after devtools::load_all().
# One deliberately missing rating step: a rare BI-only discount.

library(raterevision)
library(ratingtables)
source('scripts/_demo_helpers.R')
set.seed(20260922)

# 1. Build a 50,000-household book, with 1-3 drivers and 1-2 vehicles each.
n_households <- 50000L
households <- data.frame(
  household_id = paste0('H', seq_len(n_households)),
  rate_set_key = sample(c('IL_A', 'IL_B'), n_households, TRUE),
  rare_discount = sample(c('no', 'yes'), n_households, TRUE,
                         prob = c(.998, .002)),
  stringsAsFactors = FALSE
)
households$charter <- ifelse(households$rate_set_key == 'IL_A', 'A', 'B')

driver_counts <- sample(1:3, n_households, TRUE, prob = c(.25, .50, .25))
driver_household <- rep(seq_len(n_households), driver_counts)
drivers <- data.frame(
  driver_id = paste0('D', seq_along(driver_household)),
  household_id = households$household_id[driver_household],
  rate_set_key = households$rate_set_key[driver_household],
  gender = sample(c('M', 'F'), length(driver_household), TRUE),
  marital_status = sample(c('single', 'married'), length(driver_household), TRUE),
  driver_age = as.character(sample(16:90, length(driver_household), TRUE)),
  prior_chargeable_claims = sample(c('0', '1', '2_plus'),
                                   length(driver_household), TRUE,
                                   prob = c(.78, .17, .05)),
  stringsAsFactors = FALSE
)

vehicle_counts <- sample(1:2, n_households, TRUE, prob = c(.65, .35))
vehicle_household <- rep(seq_len(n_households), vehicle_counts)
vehicles <- data.frame(
  vehicle_id = paste0('V', seq_along(vehicle_household)),
  policy_id = households$household_id[vehicle_household],
  household_id = households$household_id[vehicle_household],
  rate_set_key = households$rate_set_key[vehicle_household],
  charter = households$charter[vehicle_household],
  rare_discount = households$rare_discount[vehicle_household],
  territory = sample(paste0('T', 1:4), length(vehicle_household), TRUE),
  credit = sample(c('A', 'B', 'C', 'D'), length(vehicle_household), TRUE),
  underwriting_level = sample(c('preferred', 'standard', 'nonstandard'),
                              length(vehicle_household), TRUE,
                              prob = c(.25, .60, .15)),
  cl_deductible = sample(c('no coverage', '1000', '2000'),
                         length(vehicle_household), TRUE,
                         prob = c(.15, .65, .20)),
  stringsAsFactors = FALSE
)

# Sanity checks before rating.
cat('Households:', nrow(households), '\n')
cat('Drivers:', nrow(drivers), '\n')
cat('Vehicles:', nrow(vehicles), '\n')
cat('Discounted households:', sum(households$rare_discount == 'yes'), '\n')
cat('Discounted vehicles:', sum(vehicles$rare_discount == 'yes'), '\n')
stopifnot(sum(households$rare_discount == 'yes') > 0L)

# 2. One factor set; the reference BI plan includes a final discount step.
factors <- demo_make_factor_table()
reference_spec <- demo_vehicle_spec()
candidate_spec <- demo_vehicle_spec()

# Add both eligibility levels for each rate set, on BI only.
# Keep the candidate's factors identical: its SPEC is where the mistake lives.
rare_rows <- do.call(rbind, lapply(c('IL_A', 'IL_B'), function(key) {
  demo_make_rows(
    rate_set_key = key, state = 'IL',
    charter = if (key == 'IL_A') 'A' else 'B',
    term_name = 'rare_discount', coverage = 'BI',
    term_value = c(1, .80),
    variable1 = 'rare_discount', level1 = c('no', 'yes')
  )
}))
rare_rows$factor_row_id <- max(factors$factor_row_id) + seq_len(nrow(rare_rows))
factors <- rbind(factors, rare_rows)

# Append at the end of the BI calculation, AFTER the additive expense fee.
new_step <- reference_spec[reference_spec$coverage == 'BI', , drop = FALSE][1, ]
new_step$step_number <- max(reference_spec$step_number[reference_spec$coverage == 'BI']) + 1L
new_step$term_name <- 'rare_discount'
new_step$value_source <- 'factor_lookup'
new_step$calculation_type <- 'multiplicative'
new_step$input_var <- NA_character_
reference_spec <- rbind(reference_spec, new_step)
rownames(reference_spec) <- NULL

cat('\nReference BI rating steps:\n')
print(reference_spec[reference_spec$coverage == 'BI',
                     c('step_number', 'term_name')], row.names = FALSE)
cat('\nCandidate BI rating steps (discount omitted):\n')
print(candidate_spec[candidate_spec$coverage == 'BI',
                     c('step_number', 'term_name')], row.names = FALSE)

# 3. Rate the same drivers ONCE and average factors at the household level.
driver_terms <- unique(demo_driver_spec()$term_name)
driver_factors <- factors[factors$term_name %in% driver_terms, , drop = FALSE]
driver_plan <- ratingtables::new_rating_plan(
  factor_table = driver_factors, rating_spec = demo_driver_spec(),
  coverages = c('BI', 'CL'), use_rate_set_key = TRUE,
  max_vars = 2, policy_id_col = 'driver_id'
)

t0 <- Sys.time()
drivers_rated <- ratingtables::score_entity_rows(drivers, driver_plan)
driver_avgs <- ratingtables::aggregate_entity_values(
  rated_entity_data = drivers_rated,
  group_col = 'household_id',
  value_cols = c('indicated_BI', 'indicated_CL'),
  aggregation = 'mean',
  output_names = c('avg_driver_factor_BI', 'avg_driver_factor_CL')
)
vehicles_with_driver_factors <- ratingtables::join_entity_values(
  parent_data = vehicles, entity_values = driver_avgs, by = 'household_id'
)
cat('\nDriver rating and averaging seconds:',
    round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 2), '\n')

# 4. Rate both vehicle books using the SAME data and factor table.
reference_terms <- unique(reference_spec$term_name[reference_spec$value_source == 'factor_lookup'])
reference_factors <- factors[factors$term_name %in% reference_terms, , drop = FALSE]
reference_plan <- ratingtables::new_rating_plan(
  factor_table = reference_factors, rating_spec = reference_spec,
  coverages = c('BI', 'CL'), use_rate_set_key = TRUE,
  max_vars = 2, policy_id_col = 'vehicle_id'
)

candidate_terms <- unique(candidate_spec$term_name[candidate_spec$value_source == 'factor_lookup'])
candidate_factors <- factors[factors$term_name %in% candidate_terms, , drop = FALSE]
candidate_plan <- ratingtables::new_rating_plan(
  factor_table = candidate_factors, rating_spec = candidate_spec,
  coverages = c('BI', 'CL'), use_rate_set_key = TRUE,
  max_vars = 2, policy_id_col = 'vehicle_id'
)

t0 <- Sys.time()
reference <- ratingtables::rate_policies(vehicles_with_driver_factors, reference_plan)
cat('Reference vehicle rating seconds:',
    round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 2), '\n')
t0 <- Sys.time()
candidate <- ratingtables::rate_policies(vehicles_with_driver_factors, candidate_plan)
cat('Candidate vehicle rating seconds:',
    round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 2), '\n')

# Join rating-variable metadata from the INPUTS, without relying on a rater
# to retain every input variable in its returned output.
metadata <- vehicles[, c('vehicle_id', 'policy_id', 'household_id',
                         'charter', 'territory', 'credit',
                         'underwriting_level', 'rare_discount')]
reference <- merge(reference, metadata, by = 'vehicle_id', all.x = TRUE,
                   sort = FALSE, suffixes = c('', '.input'))
candidate <- merge(candidate, metadata, by = 'vehicle_id', all.x = TRUE,
                   sort = FALSE, suffixes = c('', '.input'))
# Prefer the explicit input metadata in case the rater also returned these columns.
for (v in setdiff(names(metadata), 'vehicle_id')) {
  input_v <- paste0(v, '.input')
  if (input_v %in% names(reference)) {
    reference[[v]] <- reference[[input_v]]
    reference[[input_v]] <- NULL
  }
  if (input_v %in% names(candidate)) {
    candidate[[v]] <- candidate[[input_v]]
    candidate[[input_v]] <- NULL
  }
}

# 5. Standard comparison. Each input is a WIDE VEHICLE-LEVEL data frame
# with vehicle_id, indicated_BI, indicated_CL and optional rating variables.
comparison <- raterevision::compare_rating_outputs(
  current = reference,
  proposed = candidate,
  id_cols = 'vehicle_id',
  keep_cols = c('household_id', 'policy_id', 'charter', 'territory',
                'credit', 'underwriting_level', 'rare_discount')
)

cat('\nComparison: first rows\n')
print(head(comparison, 10))
cat('\nRate change by coverage\n')
print(raterevision::summarize_rate_change(comparison, by = 'coverage'))
cat('\nRate change by rare-discount eligibility and coverage\n')
print(raterevision::summarize_rate_change(
  comparison, by = c('rare_discount', 'coverage')))

# 6. Diagnosis: predictor list is deliberately explicit so identifying IDs,
# coverage labels and duplicate metadata are not treated as competing signals.
t0 <- Sys.time()
diagnosis <- raterevision::diagnose_rating_output(
  comparison,
  predictors = c('rare_discount', 'charter', 'territory',
                 'credit', 'underwriting_level'),
  absolute_tolerance = .01,
  relative_tolerance = 1e-8,
  top_n = 5,
  interaction_top = 0,
  model_sample = 50000L,
  seed = 20260922L
)
cat('\nDiagnosis seconds:',
    round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 2), '\n')
cat('\nDiagnosis summary\n')
print(diagnosis)
cat('\nSuspect variables\n')
print(diagnosis$suspects)
cat('\nLevel details for rare_discount\n')
print(diagnosis$level_details[
  diagnosis$level_details$variable == 'rare_discount', , drop = FALSE
])
cat('\nModel signals\n')
print(diagnosis$model_signals)

# 7. Independent assertions: these checks are NOT used to train the diagnosis.
# The rare discount must be the sole reason for any dollar mismatch.
bi_discount <- comparison$coverage == 'BI' & comparison$rare_discount == 'yes'
should_match <- !bi_discount
stopifnot(!anyNA(comparison$current_premium),
          !anyNA(comparison$proposed_premium),
          all(abs(comparison$proposed_premium[should_match] -
                  comparison$current_premium[should_match]) < .01),
          all(comparison$proposed_premium[bi_discount] >
                comparison$current_premium[bi_discount]))
cat('\nPASS: only discount-eligible BI rows differ; candidate premium is higher.\n')
cat('Differing BI rows:', sum(bi_discount), '\n')
cat('All comparison rows:', nrow(comparison), '\n')

# Optional: inspect in RStudio via View(comparison), View(diagnosis$suspects),
# View(diagnosis$level_details). Avoid printing the entire 100k+ row table.
