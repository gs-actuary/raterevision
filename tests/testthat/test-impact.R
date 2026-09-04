test_that("wide rating output comparison and summaries work", {
  current <- data.frame(vehicle_id = c("V1", "V2"), charter = c("A", "B"),
                        indicated_BI = c(100, 200), indicated_CL = c(50, 100))
  proposed <- current
  proposed$indicated_BI <- c(110, 210)
  proposed$indicated_CL <- c(45, 120)
  cmp <- compare_rating_outputs(current, proposed, id_cols = "vehicle_id", keep_cols = "charter")
  expect_equal(nrow(cmp), 4L)
  expect_equal(sort(unique(cmp$coverage)), c("BI", "CL"))
  s <- summarize_rate_change(cmp, by = "coverage")
  expect_equal(s$current_premium[s$coverage == "BI"], 300)
  expect_equal(s$proposed_premium[s$coverage == "BI"], 320)
  overall <- summarize_rate_change(cmp, by = NULL)
  expect_equal(overall$current_premium, 450)
  expect_equal(overall$proposed_premium, 485)
})

test_that("long rating output comparison works", {
  current <- data.frame(id = c(1, 1, 2, 2), cov = c("BI", "CL", "BI", "CL"), premium = c(100, 50, 200, 100))
  proposed <- current; proposed$premium <- c(110, 45, 210, 120)
  cmp <- compare_rating_outputs(current, proposed, id_cols = "id", premium_col = "premium", coverage_col = "cov")
  expect_equal(sum(cmp$dollar_change), 35)
})

test_that("rate change distributions account for records and premium", {
  current <- data.frame(vehicle_id = c("V1", "V2"), indicated_BI = c(100, 200))
  proposed <- data.frame(vehicle_id = c("V1", "V2"), indicated_BI = c(90, 240))
  cmp <- compare_rating_outputs(current, proposed, id_cols = "vehicle_id")
  d <- rate_change_distribution(cmp)
  expect_equal(sum(d$record_count), 2L)
  expect_equal(sum(d$current_premium_share), 1, tolerance = 1e-12)
})
