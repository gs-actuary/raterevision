# 13_reconciliation_edge_cases.R
# Rounding, zero premiums, missing candidate values, coverage minima,
# and independently billed total premiums. This is an observational test of
# current API semantics, not a claim that a total is automatically reconciled.
library(raterevision)
id <- paste0('V', 1:8)
raw_BI <- c(100, 0, 79, 120, 0, 99.99, 135, 80)
min_BI <- pmax(raw_BI, 80)  # illustrative coverage-specific minimum
reference <- data.frame(vehicle_id = id, indicated_BI = min_BI,
                        indicated_CL = c(45, 0, 25, 35, 0, 50, 0, 10))
candidate <- data.frame(vehicle_id = id,
  indicated_BI = min_BI + c(.004, 0, 0, 0, 0, .005, 0, .02),
  indicated_CL = reference$indicated_CL,
  territory = c('T1','T1','T2','T2','T3','T3','T1','T2'))
candidate$indicated_CL[7] <- NA_real_
candidate <- candidate[c(8, 3, 5, 1, 7, 6, 4, 2), , drop = FALSE]
comparison <- compare_rating_outputs(reference, candidate, 'vehicle_id',
                                    keep_cols = 'territory')
stopifnot(nrow(comparison) == 16L,
          all(comparison$territory == candidate$territory[
            match(comparison$vehicle_id, candidate$vehicle_id)]),
          comparison$current_premium[comparison$vehicle_id == 'V2' &
                                      comparison$coverage == 'CL'] == 0,
          is.na(comparison$percent_change[comparison$vehicle_id == 'V2' &
                                          comparison$coverage == 'CL']),
          sum(is.na(comparison$proposed_premium)) == 1L)
cat('PASS zero-premium, shuffled rows and missing premium semantics.\n')

# Tolerance is evaluated by diagnosis, not by compare_rating_outputs itself.
small <- comparison$vehicle_id %in% c('V1','V6') & comparison$coverage == 'BI'
large <- comparison$vehicle_id == 'V8' & comparison$coverage == 'BI'
stopifnot(all(comparison$dollar_change[small] > 0),
          all(abs(comparison$dollar_change[small]) < .01),
          abs(comparison$dollar_change[large] - .02) < 1e-10)
cat('PASS comparison preserves small dollar differences; it does not apply tolerance.\n')

# Explicitly evaluate the expected discrepancy criterion separately from a
# fitted diagnostic; this dataset has only eight IDs and is not a model test.
known_wrong <- !is.na(comparison$proposed_premium) &
  abs(comparison$dollar_change) > .01
known_missing <- !is.na(comparison$current_premium) &
  is.na(comparison$proposed_premium)
stopifnot(sum(known_wrong) == 1L, sum(known_missing) == 1L,
          all(known_wrong == large))
cat('PASS .01 materiality: one wrong BI row, one missing CL row.\n')

# Do NOT include indicated_total in this by-coverage comparison: a billed total
# may include non-additive policy fees, discounts, minimums, and final rounding.
reference_total <- data.frame(vehicle_id = id,
  indicated_total = rowSums(reference[c('indicated_BI','indicated_CL')]) + 17)
# Candidate billed total computed independently, not rowSums(..., na.rm=TRUE).
candidate_total <- data.frame(vehicle_id = id,
  indicated_total = rowSums(candidate[c('indicated_BI','indicated_CL')]) + 17)
# V7 has a missing coverage, so its billed total is also missing.
stopifnot(is.na(candidate_total$indicated_total[candidate_total$vehicle_id == 'V7']))
total_cmp <- compare_rating_outputs(reference_total, candidate_total, 'vehicle_id',
                                    premium_cols = 'indicated_total')
stopifnot(nrow(total_cmp) == length(id),
          all(total_cmp$coverage == 'total'),
          sum(is.na(total_cmp$proposed_premium)) == 1L)
cat('PASS separately compare billed totals; no automatic aggregation or additivity assumed.\n')
print(summarize_rate_change(comparison, by = 'coverage'))
cat('CAUTION: summarize_rate_change(..., na.rm=TRUE) excludes missing premiums from sums;\n',
    'reconcile missing counts before interpreting portfolio totals.\n', sep = '')
cat('SCRIPT 13 PASSED\n')
