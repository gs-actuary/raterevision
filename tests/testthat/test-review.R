test_that("rate_plan_diff reports added removed changed and unchanged", {
  x <- rr_test_factors()
  y <- x
  y$term_value[1] <- y$term_value[1] * 1.1
  y <- y[-2, , drop = FALSE]
  add <- y[3, , drop = FALSE]; add$level1 <- "NEW"; add$factor_row_id <- max(y$factor_row_id) + 1
  y <- rbind(y, add)
  d <- rate_plan_diff(x, y)
  expect_true(all(c("changed", "removed", "added", "unchanged") %in% d$status))
})

test_that("validate_rate_plan detects common structural problems", {
  x <- rr_test_factors()
  expect_equal(nrow(validate_rate_plan(x, error = FALSE)), 0L)

  dup <- rbind(x, x[1, ])
  v <- validate_rate_plan(dup, error = FALSE)
  expect_true("duplicate_factor_key" %in% v$issue)
  expect_error(validate_rate_plan(dup), "validation failed")

  bad <- x
  i <- which(bad$term_name == "driver_age")[1]
  bad$level1[i] <- NA
  expect_true("slot_mismatch" %in% validate_rate_plan(bad, error = FALSE)$issue)
})

test_that("multiplicative term positivity can be checked explicitly", {
  x <- rr_test_factors()
  i <- which(x$term_name == "territory")[1]
  x$term_value[i] <- 0
  v <- validate_rate_plan(x, multiplicative_terms = "territory", error = FALSE)
  expect_true("nonpositive_multiplicative_factor" %in% v$issue)
})
