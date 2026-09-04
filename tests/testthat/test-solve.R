test_that("solve_rate_target solves a scalar premium level exactly", {
  s <- solve_rate_target(
    current_premium = 1000,
    target = .10,
    rate_function = function(x) 900 * x,
    interval = c(.5, 2)
  )
  expect_s3_class(s, "raterevision_target_solution")
  expect_equal(s$parameter, 1100 / 900, tolerance = 1e-7)
  expect_equal(s$achieved_change, .10, tolerance = 1e-7)
})

test_that("solver can aggregate a numeric vector and territory bases", {
  bases <- c(T1 = 250, T2 = 300, T3 = 400)
  counts <- c(T1 = 10, T2 = 20, T3 = 5)
  cur <- sum(c(240, 290, 380) * counts)
  s <- solve_rate_target(cur, .08, function(x) bases * x * counts, interval = c(.5, 1.5))
  expect_equal(sum(bases * s$parameter * counts) / cur - 1, .08, tolerance = 1e-7)
})

test_that("premium_extractor permits arbitrary structural output", {
  s <- solve_rate_target(
    1000, .05,
    rate_function = function(x) data.frame(a = 400 * x, b = 500 * x),
    premium_extractor = function(z) sum(z$a + z$b),
    interval = c(.5, 2)
  )
  expect_equal(s$achieved_premium, 1050, tolerance = 1e-6)
})

test_that("solver reports targets that cannot be bracketed", {
  expect_error(
    solve_rate_target(1000, .10, function(x) 500, interval = c(.5, 1.5)),
    "Could not bracket/solve"
  )
})
