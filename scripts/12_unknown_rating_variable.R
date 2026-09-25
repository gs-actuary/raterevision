# 12_unknown_rating_variable.R
# The actual discount indicator is deliberately withheld from diagnose_rating_output.
# The premium comparison must remain correct; suspect ranking is NOT preordained.
library(raterevision)
set.seed(20260926)
n <- 6000L
id <- sprintf('V%05d', seq_len(n))
territory <- sample(c('Urban', 'Rural', 'Suburban'), n, TRUE)
credit <- sample(c('A', 'B', 'C'), n, TRUE)
driver_age <- sample(18:85, n, TRUE)
noise <- sample(c('X', 'Y'), n, TRUE)
# Eligibility is hidden. Some correlation with geography, but not deterministically.
hidden_eligible <- (territory == 'Rural' & seq_len(n) %% 29L == 0L) |
                   (territory == 'Urban' & seq_len(n) %% 97L == 0L)
base_BI <- 150 + 8 * (territory == 'Urban') + 16 * (credit == 'C') +
           65 * (driver_age < 25)
base_CL <- rep(120, n)
reference <- data.frame(vehicle_id = id,
  indicated_BI = round(base_BI * ifelse(hidden_eligible, .8, 1), 2),
  indicated_CL = base_CL)
candidate <- data.frame(vehicle_id = id, indicated_BI = base_BI,
  indicated_CL = base_CL, territory = territory, credit = credit,
  driver_age = driver_age, noise = noise)
candidate <- candidate[sample(n), , drop = FALSE]
comparison <- compare_rating_outputs(reference, candidate, 'vehicle_id',
                   keep_cols = c('territory', 'credit', 'driver_age', 'noise'))
ix <- match(comparison$vehicle_id, id)
wrong <- !is.na(comparison$dollar_change) & abs(comparison$dollar_change) > .01
stopifnot(nrow(comparison) == 2L * n,
          identical(wrong, comparison$coverage == 'BI' & hidden_eligible[ix]),
          !('hidden_eligible' %in% names(comparison)),
          all(comparison$dollar_change[comparison$coverage == 'CL'] == 0))
cat('PASS comparison finds every omitted discount without exposing eligibility.\n')
cat('Actual hidden eligibility count:', sum(hidden_eligible), '\n')
print(summarize_rate_change(comparison, by = c('territory', 'coverage')))

diagnosis <- diagnose_rating_output(comparison,
  predictors = c('territory', 'credit', 'driver_age', 'noise'),
  absolute_tolerance = .01, relative_tolerance = 1e-8,
  top_n = 4L, interaction_top = 0L, model_sample = 6000L, seed = 20260926L)
print(diagnosis)
stopifnot(!('hidden_eligible' %in% diagnosis$suspects$variable),
          all(diagnosis$suspects$variable %in%
              c('territory', 'credit', 'driver_age', 'noise')))
cat('Interpretation: the listed variables are associations, not proof of the hidden rule.\n')
cat('SCRIPT 12 PASSED\n')
