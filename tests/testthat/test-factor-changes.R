test_that("replace_factor_set replaces complete slices and can remove levels", {
  x <- rr_test_factors()
  repl <- x[x$charter == "A" & x$coverage == "BI" & x$term_name == "driver_age" & x$level1 != "18", , drop = FALSE]
  repl$term_value <- repl$term_value * .9
  y <- replace_factor_set(x, repl)
  z <- y[y$charter == "A" & y$coverage == "BI" & y$term_name == "driver_age", ]
  expect_equal(sort(z$level1), c("16", "17"))
  untouched <- y[y$charter == "B" & y$coverage == "BI" & y$term_name == "driver_age", ]
  expect_equal(sort(untouched$level1), c("16", "17", "18"))
})

test_that("replacement-only analytical columns do not become slice keys", {
  x <- rr_test_factors()
  repl <- x[x$charter == "A" & x$coverage == "BI" & x$term_name == "territory", , drop = FALSE]
  repl$temporary_weight <- c(10, 20)
  repl$term_value <- repl$term_value + .01
  y <- replace_factor_set(x, repl)
  z <- y[y$charter == "A" & y$coverage == "BI" & y$term_name == "territory", ]
  expect_equal(nrow(z), 2L)
})

test_that("update_factors is sparse and new levels require explicit permission", {
  x <- rr_test_factors()
  u <- x[x$charter == "A" & x$coverage == "BI" & x$term_name == "territory" & x$level1 == "T2", ]
  u$term_value <- 1.25
  y <- update_factors(x, u)
  expect_equal(y$term_value[y$charter == "A" & y$coverage == "BI" & y$term_name == "territory" & y$level1 == "T2"], 1.25)

  n <- u; n$level1 <- "T3"; n$term_value <- 1.4; n$factor_row_id <- NULL
  expect_error(update_factors(y, n), "allow_new_levels")
  z <- update_factors(y, n, allow_new_levels = TRUE)
  expect_true(any(z$term_name == "territory" & z$level1 == "T3" & z$charter == "A" & z$coverage == "BI"))
})

test_that("rebase_factor_set makes the selected level one and preserves ratios", {
  x <- rr_test_factors()
  old <- x[x$term_name == "driver_age" & x$charter == "A" & x$coverage == "BI", ]
  y <- rebase_factor_set(x, "driver_age", base_level = "17")
  new <- y[y$term_name == "driver_age" & y$charter == "A" & y$coverage == "BI", ]
  expect_equal(new$term_value[new$level1 == "17"], 1)
  expect_equal(new$term_value[new$level1 == "16"] / new$term_value[new$level1 == "18"],
               old$term_value[old$level1 == "16"] / old$term_value[old$level1 == "18"])
})

test_that("normalize_factor_set supports exposure-weighted normalization", {
  x <- rr_test_factors()
  t <- x[x$term_name == "territory", , drop = FALSE]
  t$exposure <- ifelse(t$level1 == "T1", 3, 1)
  y <- normalize_factor_set(t, "territory", weights = "exposure")
  one <- y[y$charter == "A" & y$coverage == "BI", ]
  expect_equal(weighted.mean(one$term_value, one$exposure), 1, tolerance = 1e-12)
})
