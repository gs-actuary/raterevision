test_that("raterevision supports a multi-stage ratingtables workflow and structural proposal", {
  skip_if_not_installed("ratingtables", minimum_version = "0.2.0")

  driver_ft <- data.frame(
    rate_set_key = "A", coverage = "BI", term_name = "driver_age",
    term_value = c(1.2, .8), variable1 = "driver_age",
    level1 = c("young", "old"), stringsAsFactors = FALSE
  )
  driver_spec <- data.frame(
    coverage = "BI", step_number = 1, term_name = "driver_age",
    value_source = "factor_lookup", calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )
  drivers <- data.frame(
    driver_id = c("D1", "D2", "D3"), household_id = c("H1", "H1", "H2"),
    rate_set_key = "A", driver_age = c("young", "old", "young"),
    stringsAsFactors = FALSE
  )
  vehicles <- data.frame(
    vehicle_id = c("V1", "V2"), household_id = c("H1", "H2"),
    rate_set_key = "A", territory = c("T1", "T2"),
    telematics = c(.98, 1.03), stringsAsFactors = FALSE
  )

  make_vehicle_ft <- function(base_multiplier = 1) {
    rbind(
      data.frame(rate_set_key = "A", coverage = "BI", term_name = "base_rate",
                 term_value = 100 * base_multiplier, variable1 = NA_character_, level1 = NA_character_),
      data.frame(rate_set_key = "A", coverage = "BI", term_name = "territory",
                 term_value = c(1, 1.2), variable1 = "territory", level1 = c("T1", "T2"))
    )
  }
  vehicle_spec <- function(structural = FALSE) {
    ans <- data.frame(
      coverage = "BI", step_number = 1:3,
      term_name = c("base_rate", "average_driver_factor", "territory"),
      value_source = c("factor_lookup", "input_value", "factor_lookup"),
      calculation_type = "multiplicative",
      input_var = c(NA, "avg_driver_BI", NA), stringsAsFactors = FALSE
    )
    if (isTRUE(structural)) {
      ans <- rbind(
        ans[1:2, ],
        data.frame(coverage = "BI", step_number = 3, term_name = "telematics",
                   value_source = "input_value", calculation_type = "multiplicative",
                   input_var = "telematics", stringsAsFactors = FALSE),
        ans[3, ]
      )
      ans$step_number <- seq_len(nrow(ans))
    }
    ans
  }

  rate_book <- function(base_multiplier = 1, structural = FALSE) {
    dp <- ratingtables::new_rating_plan(
      driver_ft, driver_spec, "BI", use_rate_set_key = TRUE,
      max_vars = 1, policy_id_col = "driver_id"
    )
    dr <- ratingtables::rate_entities(drivers, dp)
    av <- ratingtables::aggregate_entity_values(
      dr$rated_data, "household_id", "indicated_BI", "mean",
      output_names = "avg_driver_BI"
    )
    v <- ratingtables::join_entity_values(vehicles, av, by = "household_id")
    vp <- ratingtables::new_rating_plan(
      make_vehicle_ft(base_multiplier), vehicle_spec(structural), "BI",
      use_rate_set_key = TRUE, max_vars = 1, policy_id_col = "vehicle_id"
    )
    ratingtables::rate_policies(v, vp)
  }

  current <- rate_book()
  expect_equal(current$indicated_BI, c(100, 144), tolerance = 1e-10)

  proposed <- rate_book(structural = TRUE)
  cmp <- compare_rating_outputs(current, proposed, id_cols = "vehicle_id")
  expect_equal(nrow(cmp), 2L)

  cur_total <- sum(current$indicated_BI)
  sol <- solve_rate_target(
    cur_total, .05,
    rate_function = function(x) rate_book(base_multiplier = x, structural = TRUE)$indicated_BI,
    interval = c(.5, 1.5)
  )
  final <- rate_book(base_multiplier = sol$parameter, structural = TRUE)
  expect_equal(sum(final$indicated_BI) / cur_total - 1, .05, tolerance = 1e-7)
})
