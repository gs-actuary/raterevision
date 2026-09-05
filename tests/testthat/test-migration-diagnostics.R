test_that("migration diagnostics identify variables tied to known defects", {
  set.seed(42)
  n <- 2000
  dat <- data.frame(
    vehicle_id = paste0("V", seq_len(n)),
    territory = sample(c("T1", "T2", "T3", "T4"), n, replace = TRUE),
    driver_age = sample(16:80, n, replace = TRUE),
    marital_status = sample(c("single", "married"), n, replace = TRUE),
    indicated_BI = rep(500, n),
    stringsAsFactors = FALSE
  )

  true <- dat
  candidate <- dat

  # Defect 1: T4 lookup is missing entirely.
  candidate$indicated_BI[candidate$territory == "T4"] <- NA_real_

  # Defect 2: youthful-driver factor is omitted where a premium was produced.
  youthful <- candidate$driver_age < 21 & candidate$territory != "T4"
  candidate$indicated_BI[youthful] <- candidate$indicated_BI[youthful] * 0.80

  cmp <- compare_rating_outputs(
    true,
    candidate,
    id_cols = "vehicle_id",
    keep_cols = c("territory", "driver_age", "marital_status")
  )

  d <- diagnose_rating_output(cmp, top_n = 3, model_sample = 2000, interaction_top = 3)
  expect_s3_class(d, "raterevision_rater_diagnosis")
  expect_equal(d$summary$candidate_missing, sum(dat$territory == "T4"))
  expect_true("territory" %in% d$suspects$variable)
  expect_true("driver_age" %in% d$suspects$variable)
  expect_true(nrow(d$model_signals) > 0L)
})

test_that("diagnostics can surface an interaction pattern", {
  set.seed(7)
  n <- 3000
  current <- data.frame(
    id = seq_len(n),
    driver_age = sample(16:70, n, replace = TRUE),
    marital_status = sample(c("single", "married"), n, replace = TRUE),
    indicated_BI = rep(600, n),
    stringsAsFactors = FALSE
  )
  proposed <- current
  hit <- current$driver_age < 25 & current$marital_status == "married"
  proposed$indicated_BI[hit] <- proposed$indicated_BI[hit] * 1.15

  cmp <- compare_rating_outputs(
    current, proposed, id_cols = "id",
    keep_cols = c("driver_age", "marital_status")
  )
  d <- diagnose_rating_output(cmp, interaction_top = 2, model_sample = 3000)

  expect_true(nrow(d$interaction_signals) > 0L)
  pairs <- paste(d$interaction_signals$variable1, d$interaction_signals$variable2, sep = "|")
  expect_true(any(pairs %in% c("driver_age|marital_status", "marital_status|driver_age")))
})

test_that("diagnostics require rating variables to investigate", {
  x <- data.frame(
    id = 1:3,
    current_premium = c(100, 100, 100),
    proposed_premium = c(100, 90, 100),
    dollar_change = c(0, -10, 0),
    percent_change = c(0, -.1, 0)
  )
  attr(x, "id_cols") <- "id"
  expect_error(diagnose_rating_output(x), "No usable predictor")
})

test_that("diagnostics do not invent a variable-level suspect for a clean or global result", {
  set.seed(11)
  n <- 500
  current <- data.frame(
    id = seq_len(n),
    territory = sample(c("T1", "T2", "T3"), n, replace = TRUE),
    indicated_BI = rep(400, n),
    stringsAsFactors = FALSE
  )

  clean <- compare_rating_outputs(
    current, current, id_cols = "id", keep_cols = "territory"
  )
  d_clean <- diagnose_rating_output(clean, interaction_top = 0)
  expect_equal(d_clean$summary$total_mismatch, 0L)
  expect_equal(nrow(d_clean$suspects), 0L)

  global <- current
  global$indicated_BI <- global$indicated_BI * 1.10
  cmp_global <- compare_rating_outputs(
    current, global, id_cols = "id", keep_cols = "territory"
  )
  d_global <- diagnose_rating_output(cmp_global, interaction_top = 0)
  expect_equal(d_global$summary$total_mismatch, n)
  expect_equal(nrow(d_global$suspects), 0L)
})
