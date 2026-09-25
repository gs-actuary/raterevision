# 11_multiple_migration_defects.R
# Synthetic black-box legacy premiums versus an intentionally defective candidate.
# Known causes are independently recorded; the diagnostic scores are exploratory.
library(raterevision)
set.seed(20260925)
n <- 8000L
vehicle_id <- sprintf('V%05d', seq_len(n))
territory <- sample(c('T1', 'T2', 'T3', 'T4'), n, TRUE)
youth <- sample(c('yes', 'no'), n, TRUE, prob = c(.18, .82))
rare_discount <- ifelse(seq_len(n) %% 61L == 0L, 'yes', 'no')
charter <- sample(c('A', 'B'), n, TRUE)
base_BI <- round(180 + 26 * match(territory, c('T1','T2','T3','T4')) +
                 52 * (youth == 'yes') + 13 * (charter == 'B'), 2)
base_CL <- round(110 + 9 * match(territory, c('T1','T2','T3','T4')), 2)

reference <- data.frame(vehicle_id = vehicle_id,
                        indicated_BI = round(base_BI * ifelse(rare_discount == 'yes', .80, 1), 2),
                        indicated_CL = base_CL)
candidate <- data.frame(vehicle_id = vehicle_id, territory = territory, youth = youth,
                        rare_discount = rare_discount, charter = charter,
                        indicated_BI = base_BI, indicated_CL = base_CL)

# Defect A: discount omitted from BI; candidate is higher on eligible records.
flag_discount <- rare_discount == 'yes'
# Defect B: candidate T3 BI factor is incorrectly 10% higher.
flag_territory <- territory == 'T3'
candidate$indicated_BI[flag_territory] <- candidate$indicated_BI[flag_territory] * 1.10
# Defect C: a missing candidate BI result for some youthful drivers, including
# some T3 records. Missingness takes precedence over comparing dollar errors.
flag_missing <- youth == 'yes' & seq_len(n) %% 23L == 0L
candidate$indicated_BI[flag_missing] <- NA_real_
candidate$indicated_BI <- round(candidate$indicated_BI, 2)
candidate <- candidate[sample(n), , drop = FALSE]
comparison <- compare_rating_outputs(reference, candidate, 'vehicle_id',
                                    keep_cols = c('territory', 'youth',
                                                  'rare_discount', 'charter'))

# Build expected values from the separate reference and known defect flags;
# do not derive the truth from the comparison function under test.
ix <- match(comparison$vehicle_id, vehicle_id)
bi <- comparison$coverage == 'BI'
expected_missing <- bi & flag_missing[ix]
expected_wrong <- bi & !flag_missing[ix] & (flag_discount[ix] | flag_territory[ix])
actual_missing <- is.na(comparison$proposed_premium)
actual_wrong <- !actual_missing & abs(comparison$dollar_change) > .01
stopifnot(nrow(comparison) == 2L * n,
          identical(actual_missing, expected_missing),
          identical(actual_wrong, expected_wrong),
          all(comparison$dollar_change[!bi] == 0),
          all(comparison$dollar_change[bi & !expected_missing &
                                     !(flag_discount[ix] | flag_territory[ix])] == 0),
          any(flag_discount & flag_territory & !flag_missing),
          any(flag_missing & flag_territory))
cat('PASS independent reconciliation of all BI missing/wrong records and CL matches.\n')
cat('Known overlapping nonmissing discount+territory defects:',
    sum(flag_discount & flag_territory & !flag_missing), '\n')
print(summarize_rate_change(comparison, by = 'coverage'))

# Diagnosis: no reliance on exact model ranks/scores for correlated factors.
diagnosis <- diagnose_rating_output(comparison,
  predictors = c('territory', 'youth', 'rare_discount', 'charter'),
  absolute_tolerance = .01, relative_tolerance = 1e-8,
  top_n = 4L, interaction_top = 0L, model_sample = 8000L, seed = 20260925L)
print(diagnosis)
stopifnot(diagnosis$summary$material_wrong == sum(expected_wrong) ||
          # Summary field names may evolve; inspect below if this fails.
          sum(diagnosis$level_details$mismatch_n[
            diagnosis$level_details$variable == 'rare_discount']) == sum(expected_wrong))
cat('Relevant coverage-level evidence (if available):\n')
if (!is.null(diagnosis$coverage_level_details)) {
  print(subset(diagnosis$coverage_level_details,
               variable %in% c('rare_discount', 'territory', 'youth') &
                 coverage == 'BI'))
}
cat('SCRIPT 11 PASSED\n')
