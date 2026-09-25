# 10_black_box_reference.R
# Run from anywhere after installing/loading the locally updated raterevision.
# No reference-side rating variables, no ratingtables dependency.
library(raterevision)
set.seed(20260924)

expect_error_matching <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  stopifnot(is.character(err), length(err) == 1L,
            grepl(pattern, err, ignore.case = TRUE))
  invisible(err)
}

id <- sprintf('V%04d', 1:300)
inputs <- data.frame(
  vehicle_id = id,
  policy_id = sprintf('P%04d', rep(seq_len(150), each = 2)),
  territory = rep(c('T1', 'T2', 'T3'), length.out = 300),
  discount = ifelse(seq_along(id) %% 37L == 0L, 'yes', 'no'),
  stringsAsFactors = FALSE
)

# The legacy export has IDs and premiums ONLY. No territory/discount/policy metadata.
reference <- data.frame(vehicle_id = id,
                        indicated_BI = 180 + (seq_along(id) %% 31) * 7,
                        indicated_CL = 95 + (seq_along(id) %% 19) * 3)
candidate <- merge(inputs, reference, by = 'vehicle_id', sort = FALSE)
candidate$indicated_BI[candidate$discount == 'yes'] <-
  candidate$indicated_BI[candidate$discount == 'yes'] * 1.25
candidate <- candidate[sample(nrow(candidate)), , drop = FALSE]

comparison <- compare_rating_outputs(
  current = reference, proposed = candidate, id_cols = 'vehicle_id',
  keep_cols = c('policy_id', 'territory', 'discount'))
stopifnot(nrow(comparison) == 600L,
          identical(attr(comparison, 'id_cols'), 'vehicle_id'))
lookup <- match(comparison$vehicle_id, inputs$vehicle_id)
stopifnot(!anyNA(lookup),
          identical(as.character(comparison$territory), inputs$territory[lookup]),
          identical(as.character(comparison$discount), inputs$discount[lookup]),
          identical(as.character(comparison$policy_id), inputs$policy_id[lookup]))
bi <- comparison$coverage == 'BI'
cl <- comparison$coverage == 'CL'
affected <- bi & comparison$discount == 'yes'
stopifnot(all(abs(comparison$dollar_change[!affected]) < 1e-10),
          all(abs(comparison$percent_change[affected] - .25) < 1e-12),
          sum(affected) == sum(inputs$discount == 'yes'),
          all(comparison$coverage %in% c('BI', 'CL')))
cat('PASS wide black-box input: matched premiums and candidate-only metadata despite shuffled rows.\n')
print(summarize_rate_change(comparison, by = 'coverage'))

# Wide: a composite policy+vehicle identifier is supported when truly unique.
ref_multi <- reference
ref_multi$policy_id <- inputs$policy_id
cand_multi <- candidate
comp_multi <- compare_rating_outputs(ref_multi, cand_multi,
                                     id_cols = c('policy_id', 'vehicle_id'),
                                     keep_cols = c('territory', 'discount'))
stopifnot(nrow(comp_multi) == 600L,
          all(comp_multi$discount[comp_multi$vehicle_id %in%
              inputs$vehicle_id[inputs$discount == 'yes']] == 'yes'))
cat('PASS composite IDs.\n')

# Long: deliberately different order, candidate-only variables, and an NA premium.
ref_long <- rbind(data.frame(vehicle_id = id, peril = 'BI', premium = reference$indicated_BI),
                  data.frame(vehicle_id = id, peril = 'CL', premium = reference$indicated_CL))
cand_long <- rbind(data.frame(vehicle_id = candidate$vehicle_id, peril = 'BI',
                              premium = candidate$indicated_BI,
                              territory = candidate$territory, discount = candidate$discount),
                   data.frame(vehicle_id = candidate$vehicle_id, peril = 'CL',
                              premium = candidate$indicated_CL,
                              territory = candidate$territory, discount = candidate$discount))
cand_long$premium[cand_long$vehicle_id == 'V0001' & cand_long$peril == 'CL'] <- NA_real_
cand_long <- cand_long[sample(nrow(cand_long)), , drop = FALSE]
long_cmp <- compare_rating_outputs(ref_long, cand_long, id_cols = 'vehicle_id',
                                   premium_col = 'premium', coverage_col = 'peril',
                                   keep_cols = c('territory', 'discount'))
stopifnot(nrow(long_cmp) == 600L,
          all(long_cmp$coverage == ref_long$peril),
          identical(as.character(long_cmp$territory),
                    inputs$territory[match(long_cmp$vehicle_id, inputs$vehicle_id)]),
          sum(is.na(long_cmp$proposed_premium)) == 1L,
          is.na(long_cmp$proposed_premium[long_cmp$vehicle_id == 'V0001' &
                                            long_cmp$coverage == 'CL']))
cat('PASS long black-box input: candidate-only metadata, shuffled coverage rows, and missing premium.\n')

# Invalid keys should be rejected, not silently merged or recycled.
expect_error_matching(compare_rating_outputs(reference,
                         rbind(candidate, candidate[1, ]), 'vehicle_id'), 'uniquely')
expect_error_matching(compare_rating_outputs(reference[-1, ], candidate,
                         'vehicle_id'), 'same record keys')
expect_error_matching(compare_rating_outputs(ref_long,
                         rbind(cand_long, cand_long[1, ]), 'vehicle_id',
                         premium_col = 'premium', coverage_col = 'peril'), 'uniquely')
expect_error_matching(compare_rating_outputs(ref_long[-1, ], cand_long,
                         'vehicle_id', premium_col = 'premium',
                         coverage_col = 'peril'), 'same rating keys')
expect_error_matching(compare_rating_outputs(reference, candidate, 'vehicle_id',
                         keep_cols = 'not_in_candidate'), 'not_in_candidate')
cat('PASS invalid-key and missing-analysis-column error handling.\n')
cat('SCRIPT 10 PASSED\n')
