test_that("coverage-wide round trip is lossless including sparse coverage keys", {
  x <- rr_test_factors()
  w <- rate_tables_wide(x)
  y <- rate_tables_long(w)
  d <- rate_plan_diff(x, y, include_unchanged = FALSE)
  expect_equal(nrow(d), 0L)
})

test_that("two-way matrix round trip is lossless", {
  x <- rr_test_factors()
  w <- rate_tables_wide(x, matrix_terms = "age_gender")
  expect_true(any(attr(w, "manifest")$layout == "interaction_matrix"))
  y <- rate_tables_long(w)
  expect_equal(nrow(rate_plan_diff(x, y, include_unchanged = FALSE)), 0L)
})

test_that("matrix layout rejects interactions deeper than two variables", {
  x <- rr_test_factors(include_three_way = TRUE)
  expect_error(rate_tables_wide(x, matrix_terms = "three_way"), "only supported for two-variable")
})

test_that("numeric-looking levels sort naturally within metadata groups", {
  x <- rr_test_factors()
  # Reverse source order so successful ordering is not accidental.
  x <- x[nrow(x):1, , drop = FALSE]
  w <- rate_tables_wide(x)
  age <- w$driver_age
  a <- age[age$charter == "A", "level1"]
  b <- age[age$charter == "B", "level1"]
  expect_equal(as.character(a), c("16", "17", "18"))
  expect_equal(as.character(b), c("16", "17", "18"))
})

test_that("filters restrict exports without choosing an implicit rate set", {
  x <- rr_test_factors()
  w <- rate_tables_wide(x, filters = list(charter = "A"))
  expect_true(all(vapply(w, function(z) all(z$charter == "A"), logical(1))))
  expect_error(rate_tables_wide(x, filters = list(charter = "Z")), "No factor rows")
})

test_that("matrix request must name an existing term", {
  expect_error(rate_tables_wide(rr_test_factors(), matrix_terms = "missing"), "not found")
})

test_that("coverage columns preserve first appearance rather than sorting alphabetically", {
  x <- rr_test_factors()
  # Put CL rows first so the source coverage order is CL, BI.
  x <- rbind(x[x$coverage == "CL", , drop = FALSE],
             x[x$coverage == "BI", , drop = FALSE])
  w <- rate_tables_wide(x)
  structural <- c("state", "charter", "rate_eff_date", "term_name",
                  "variable1", "level1", "variable2", "level2",
                  "variable3", "level3")
  coverage_cols <- setdiff(names(w$driver_age), structural)
  expect_equal(coverage_cols, c("CL", "BI"))
})
