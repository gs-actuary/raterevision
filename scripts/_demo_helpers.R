# Demo helpers for the raterevision repository.
#
# This file is intentionally verbose. The scripts source it so the individual
# walkthroughs can focus on one workflow at a time. It is excluded from the
# built package by .Rbuildignore.

demo_make_rows <- function(rate_set_key, state, charter, term_name, coverage,
                           term_value, variable1 = NA_character_, level1 = NA_character_,
                           variable2 = NA_character_, level2 = NA_character_) {
  n <- max(length(term_value), length(level1), length(level2), 1L)
  data.frame(
    rate_set_key = rep(rate_set_key, n),
    state = rep(state, n),
    charter = rep(charter, n),
    rate_eff_date = rep(as.Date("2026-01-01"), n),
    rate_exp_date = rep(as.Date("2026-12-31"), n),
    coverage = rep(coverage, n),
    term_name = rep(term_name, n),
    term_value = rep(term_value, length.out = n),
    variable1 = rep(variable1, length.out = n),
    level1 = rep(level1, length.out = n),
    variable2 = rep(variable2, length.out = n),
    level2 = rep(level2, length.out = n),
    stringsAsFactors = FALSE
  )
}

demo_age_curve <- function(age, coverage = "BI") {
  age <- as.numeric(age)
  young <- 0.58 * exp(-(age - 16) / 9)
  old <- 0.24 * pmax(age - 62, 0) / 28
  shape <- 1 + young + old
  if (coverage == "CL") shape <- 1 + 0.8 * (shape - 1)
  base <- shape[match(52, age)]
  shape / base
}

demo_make_factor_table <- function() {
  out <- list(); k <- 1L
  charters <- data.frame(
    rate_set_key = c("IL_A", "IL_B"), state = "IL", charter = c("A", "B"),
    stringsAsFactors = FALSE
  )
  ages <- 16:90
  for (r in seq_len(nrow(charters))) {
    key <- charters$rate_set_key[r]; state <- charters$state[r]; charter <- charters$charter[r]
    for (cov in c("BI", "CL")) {
      base <- if (charter == "A") c(BI = 220, CL = 180)[cov] else c(BI = 230, CL = 190)[cov]
      out[[k]] <- demo_make_rows(key, state, charter, "base_rate", cov, base); k <- k + 1L
      out[[k]] <- demo_make_rows(key, state, charter, "territory", cov,
                                 if (cov == "BI") c(.90, 1.00, 1.10, 1.20) else c(.95, 1.00, 1.08, 1.15),
                                 "territory", c("T1", "T2", "T3", "T4")); k <- k + 1L
      out[[k]] <- demo_make_rows(key, state, charter, "credit", cov,
                                 if (cov == "BI") c(.90, 1.00, 1.10, 1.20) else c(.92, 1.00, 1.08, 1.15),
                                 "credit", c("A", "B", "C", "D")); k <- k + 1L
      out[[k]] <- demo_make_rows(key, state, charter, "underwriting_level", cov,
                                 if (cov == "BI") c(.92, 1.00, 1.20) else c(.95, 1.00, 1.15),
                                 "underwriting_level", c("preferred", "standard", "nonstandard")); k <- k + 1L
      out[[k]] <- demo_make_rows(key, state, charter, "expense_fee", cov,
                                 if (cov == "BI") 18 else 12); k <- k + 1L

      out[[k]] <- demo_make_rows(key, state, charter, "gender", cov,
                                 if (cov == "BI") c(1.04, .97) else c(1.02, .99),
                                 "gender", c("M", "F")); k <- k + 1L
      out[[k]] <- demo_make_rows(key, state, charter, "marital_status", cov,
                                 c(1.08, .96), "marital_status", c("single", "married")); k <- k + 1L
      out[[k]] <- demo_make_rows(key, state, charter, "driver_age", cov,
                                 demo_age_curve(ages, cov), "driver_age", as.character(ages)); k <- k + 1L
      out[[k]] <- demo_make_rows(key, state, charter, "prior_chargeable_claims", cov,
                                 if (cov == "BI") c(.95, 1.15, 1.35) else c(.96, 1.12, 1.30),
                                 "prior_chargeable_claims", c("0", "1", "2_plus")); k <- k + 1L

      # Two-way interaction: intentionally mild, but large enough to see.
      grid <- expand.grid(driver_age = as.character(ages), gender = c("M", "F"),
                          stringsAsFactors = FALSE)
      a <- as.numeric(grid$driver_age)
      interaction <- 1 + ifelse(grid$gender == "M", pmax(25 - a, 0) * .0025, -pmax(25 - a, 0) * .001)
      if (cov == "CL") interaction <- 1 + .7 * (interaction - 1)
      out[[k]] <- demo_make_rows(key, state, charter, "age_gender_interaction", cov,
                                 interaction, "driver_age", grid$driver_age,
                                 "gender", grid$gender); k <- k + 1L
    }
    out[[k]] <- demo_make_rows(key, state, charter, "cl_deductible", "CL",
                               c(0, 1, .88), "cl_deductible",
                               c("no coverage", "1000", "2000")); k <- k + 1L
  }
  ans <- do.call(rbind, out)
  ans$factor_row_id <- seq_len(nrow(ans))
  rownames(ans) <- NULL
  ans
}

demo_make_inputs <- function() {
  households <- data.frame(
    household_id = paste0("H", 1:6),
    rate_set_key = c("IL_A", "IL_A", "IL_A", "IL_B", "IL_B", "IL_B"),
    charter = c("A", "A", "A", "B", "B", "B"),
    stringsAsFactors = FALSE
  )
  drivers <- data.frame(
    driver_id = paste0("D", 1:12),
    household_id = rep(households$household_id, each = 2),
    rate_set_key = rep(households$rate_set_key, each = 2),
    gender = rep(c("M", "F"), 6),
    marital_status = c("single", "single", "married", "married", "single", "married",
                       "single", "single", "married", "married", "single", "married"),
    driver_age = as.character(c(18, 44, 32, 67, 24, 39, 19, 47, 53, 72, 26, 61)),
    prior_chargeable_claims = c("0", "0", "0", "1", "1", "0", "2_plus", "1", "0", "0", "0", "1"),
    stringsAsFactors = FALSE
  )
  vehicles <- data.frame(
    vehicle_id = paste0("V", 1:9),
    policy_id = c("P1", "P1", "P2", "P3", "P4", "P4", "P5", "P6", "P6"),
    household_id = c("H1", "H1", "H2", "H3", "H4", "H4", "H5", "H6", "H6"),
    rate_set_key = c("IL_A", "IL_A", "IL_A", "IL_A", "IL_B", "IL_B", "IL_B", "IL_B", "IL_B"),
    charter = c("A", "A", "A", "A", "B", "B", "B", "B", "B"),
    territory = c("T1", "T2", "T3", "T4", "T2", "T4", "T3", "T4", "T1"),
    credit = c("A", "B", "C", "D", "B", "C", "D", "C", "A"),
    underwriting_level = c("preferred", "standard", "nonstandard", "standard", "standard",
                           "nonstandard", "nonstandard", "preferred", "standard"),
    cl_deductible = c("1000", "no coverage", "2000", "1000", "no coverage", "1000", "2000", "no coverage", "1000"),
    telematics_factor = c(.98, 1.02, .97, 1.04, .99, 1.01, .96, 1.03, .98),
    stringsAsFactors = FALSE
  )
  list(drivers = drivers, vehicles = vehicles)
}

demo_driver_spec <- function() {
  terms <- c("gender", "marital_status", "driver_age", "prior_chargeable_claims", "age_gender_interaction")
  do.call(rbind, lapply(c("BI", "CL"), function(cov) data.frame(
    coverage = cov,
    step_number = seq_along(terms),
    term_name = terms,
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )))
}

demo_vehicle_spec <- function(structural_change = FALSE) {
  make_one <- function(cov) {
    terms <- c("base_rate", "territory", "average_driver_factor", "credit", "underwriting_level", "expense_fee")
    sources <- c("factor_lookup", "factor_lookup", "input_value", "factor_lookup", "factor_lookup", "factor_lookup")
    types <- c("multiplicative", "multiplicative", "multiplicative", "multiplicative", "multiplicative", "additive")
    input_var <- c(NA, NA, if (cov == "BI") "avg_driver_factor_BI" else "avg_driver_factor_CL", NA, NA, NA)
    if (isTRUE(structural_change)) {
      terms <- append(terms, "telematics_adjustment", after = 3)
      sources <- append(sources, "input_value", after = 3)
      types <- append(types, "multiplicative", after = 3)
      input_var <- append(input_var, "telematics_factor", after = 3)
    }
    if (cov == "CL") {
      terms <- c(terms, "cl_deductible")
      sources <- c(sources, "factor_lookup")
      types <- c(types, "multiplicative")
      input_var <- c(input_var, NA)
    }
    data.frame(
      coverage = cov, step_number = seq_along(terms), term_name = terms,
      value_source = sources, calculation_type = types, input_var = input_var,
      stringsAsFactors = FALSE
    )
  }
  rbind(make_one("BI"), make_one("CL"))
}

demo_rate_book <- function(factor_table, structural_change = FALSE, base_multiplier = 1) {
  if (!requireNamespace("ratingtables", quietly = TRUE)) {
    stop("This demo requires the ratingtables package.")
  }
  ft <- factor_table
  is_base <- ft$term_name == "base_rate"
  ft$term_value[is_base] <- ft$term_value[is_base] * base_multiplier

  driver_terms <- unique(demo_driver_spec()$term_name)
  driver_ft <- ft[ft$term_name %in% driver_terms, , drop = FALSE]
  driver_plan <- ratingtables::new_rating_plan(
    factor_table = driver_ft,
    rating_spec = demo_driver_spec(),
    coverages = c("BI", "CL"), use_rate_set_key = TRUE, max_vars = 2,
    policy_id_col = "driver_id"
  )
  inputs <- demo_make_inputs()
  drivers_rated <- ratingtables::score_entity_rows(
    inputs$drivers,
    driver_plan
  )
  driver_avgs <- ratingtables::aggregate_entity_values(
    rated_entity_data = drivers_rated,
    group_col = "household_id",
    value_cols = c("indicated_BI", "indicated_CL"),
    aggregation = "mean",
    output_names = c("avg_driver_factor_BI", "avg_driver_factor_CL")
  )
  vehicles <- ratingtables::join_entity_values(
    parent_data = inputs$vehicles, entity_values = driver_avgs, by = "household_id"
  )

  vehicle_spec <- demo_vehicle_spec(structural_change)
  vehicle_terms <- unique(vehicle_spec$term_name[vehicle_spec$value_source == "factor_lookup"])
  vehicle_ft <- ft[ft$term_name %in% vehicle_terms, , drop = FALSE]
  vehicle_plan <- ratingtables::new_rating_plan(
    factor_table = vehicle_ft,
    rating_spec = vehicle_spec,
    coverages = c("BI", "CL"), use_rate_set_key = TRUE, max_vars = 2,
    policy_id_col = "vehicle_id"
  )
  ratingtables::rate_policies(vehicles, vehicle_plan)
}

demo_objects <- function() {
  list(factor_table = demo_make_factor_table(), inputs = demo_make_inputs())
}
