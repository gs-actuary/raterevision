test_that("rare BI-only omission selects the affected level, not the large clean group", {
  n <- 1000L
  eligible <- seq_len(n) <= 2L  # 4/2000 = 0.2%, below the former cutoff.
  reference <- data.frame(
    vehicle_id = paste0("V", seq_len(n)),
    rare_discount = ifelse(eligible, "yes", "no"),
    indicated_BI = ifelse(eligible, 80, 100),
    indicated_CL = rep(60, n), stringsAsFactors = FALSE
  )
  candidate <- reference
  candidate$indicated_BI[eligible] <- 100
  comparison <- compare_rating_outputs(
    reference, candidate, id_cols = "vehicle_id", keep_cols = "rare_discount"
  )
  result <- diagnose_rating_output(
    comparison, predictors = "rare_discount", top_n = 1L,
    interaction_top = 0L, model_sample = 2000L
  )
  expect_equal(result$suspects$variable[1], "rare_discount")
  expect_match(result$suspects$conclusion[1], "rare_discount = yes", fixed = TRUE)
  expect_match(result$suspects$conclusion[1], "2/4", fixed = TRUE)
  expect_false(grepl("rare_discount = no", result$suspects$conclusion[1], fixed = TRUE))
  expect_equal(result$coverage_summary$mismatch_n[match("BI", result$coverage_summary$coverage)], 2L)
  expect_equal(result$coverage_summary$mismatch_n[match("CL", result$coverage_summary$coverage)], 0L)
  affected <- subset(result$coverage_level_details,
                     variable == "rare_discount" & level == "yes" & coverage == "BI")
  expect_equal(affected$mismatch_n, 2L)
  expect_equal(affected$mismatch_rate, 1)
  expect_equal(affected$mean_error_if_wrong, 20)
  expect_output(print(result), "Coverage summary")
})

test_that("coverage breakdown is optional when comparison lacks a coverage column", {
  x <- data.frame(current_premium = c(100, 80, 100, 80),
                  proposed_premium = c(100, 100, 100, 100),
                  discount = c("no", "yes", "no", "yes"))
  result <- diagnose_rating_output(x, predictors = "discount", interaction_top = 0L)
  expect_null(result$coverage_summary)
  expect_null(result$coverage_level_details)
  expect_match(result$suspects$conclusion[1], "discount = yes", fixed = TRUE)
})
