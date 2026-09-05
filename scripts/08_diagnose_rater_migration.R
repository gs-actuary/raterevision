# 08_diagnose_rater_migration.R
# Demonstrate automated diagnosis of a partially-correct rater migration.
# Run from the raterevision package root after devtools::load_all().

library(raterevision)

set.seed(2026)
n <- 6000

# ---------------------------------------------------------------------------
# 1. Create a synthetic book and a trusted/reference premium
# ---------------------------------------------------------------------------

book <- data.frame(
  vehicle_id = paste0("V", seq_len(n)),
  territory = sample(paste0("T", 1:8), n, replace = TRUE),
  driver_age = sample(16:80, n, replace = TRUE),
  marital_status = sample(c("single", "married"), n, replace = TRUE),
  credit = sample(c("A", "B", "C", "D"), n, replace = TRUE),
  stringsAsFactors = FALSE
)

territory_factor <- c(T1 = .90, T2 = .95, T3 = 1.00, T4 = 1.04,
                      T5 = 1.08, T6 = 1.12, T7 = 1.18, T8 = 1.24)
credit_factor <- c(A = .92, B = 1.00, C = 1.08, D = 1.18)
youth_factor <- ifelse(book$driver_age < 21, 1.35,
                       ifelse(book$driver_age < 25, 1.15, 1.00))
married_youth_interaction <- ifelse(
  book$driver_age < 25 & book$marital_status == "married",
  .94,
  1.00
)

reference <- book
reference$indicated_BI <- 500 *
  territory_factor[book$territory] *
  credit_factor[book$credit] *
  youth_factor *
  married_youth_interaction

# ---------------------------------------------------------------------------
# 2. Pretend the first ratingtables implementation has three defects
# ---------------------------------------------------------------------------

candidate <- reference

# Defect A: territory T8 was never loaded, so the candidate premium is missing.
candidate$indicated_BI[candidate$territory == "T8"] <- NA_real_

# Defect B: the youthful-driver table was accidentally omitted.
nonmissing <- candidate$territory != "T8"
candidate$indicated_BI[nonmissing] <- candidate$indicated_BI[nonmissing] / youth_factor[nonmissing]

# Defect C: the married x youthful interaction was also omitted.
candidate$indicated_BI[nonmissing] <-
  candidate$indicated_BI[nonmissing] / married_youth_interaction[nonmissing]

# ---------------------------------------------------------------------------
# 3. Compare the trusted and candidate outputs
# ---------------------------------------------------------------------------

comparison <- compare_rating_outputs(
  current = reference,
  proposed = candidate,
  id_cols = "vehicle_id",
  keep_cols = c("territory", "driver_age", "marital_status", "credit")
)

head(comparison)

# Straightforward aggregate checks still come first.
summarize_rate_change(comparison, by = "coverage")
summarize_rate_change(comparison, by = c("territory", "coverage"))

# ---------------------------------------------------------------------------
# 4. Let raterevision triage where the defects probably live
# ---------------------------------------------------------------------------

diagnosis <- diagnose_rating_output(
  comparison,
  absolute_tolerance = .01,
  relative_tolerance = 1e-8,
  top_n = 5,
  interaction_top = 4
)

diagnosis

# The default print is intentionally high-level. Advanced users can inspect the
# evidence without being forced to read model internals.
diagnosis$suspects
diagnosis$interaction_signals

# Grouped evidence is usually the most useful next stop for an actuary.
head(
  diagnosis$level_details[
    diagnosis$level_details$variable %in% diagnosis$suspects$variable[1:3],
  ],
  30
)

# Background model summaries are compact diagnostic signals. The fitted model
# objects are retained in diagnosis$models for users who explicitly want them.
diagnosis$model_signals
