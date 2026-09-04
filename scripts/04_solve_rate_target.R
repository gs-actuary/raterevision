# 04_solve_rate_target.R
# Scalar target solving with simple, multi-stage, structural-change, and
# territory-base examples.

library(raterevision)
library(ratingtables)
source("scripts/_demo_helpers.R")

current_factors <- demo_make_factor_table()
current_output <- demo_rate_book(current_factors)
current_total <- sum(current_output$indicated_BI + current_output$indicated_CL)
current_total

# Make some non-base proposed changes first.
proposed <- current_factors
territory <- proposed[proposed$term_name == "territory", , drop = FALSE]
territory$term_value <- territory$term_value * c(T1 = .96, T2 = 1.00, T3 = 1.04, T4 = 1.08)[territory$level1]
proposed <- replace_factor_set(proposed, territory)

# solve_rate_target() is not passed a rating plan object. It is passed a
# function. Here x multiplies every base-rate row by the same scalar and then
# reruns the ENTIRE driver-averaging + vehicle workflow.
solution <- solve_rate_target(
  current_premium = current_total,
  target = 0.06,
  rate_function = function(x) {
    rated <- demo_rate_book(proposed, base_multiplier = x)
    rated$indicated_BI + rated$indicated_CL
  },
  interval = c(.75, 1.35),
  parameter_name = "common base-rate multiplier"
)
solution

# Verify independently.
final_output <- demo_rate_book(proposed, base_multiplier = solution$parameter)
final_total <- sum(final_output$indicated_BI + final_output$indicated_CL)
final_total / current_total - 1

# --- Structural/spec change -------------------------------------------------
# The proposed algorithm now also applies an input telematics factor. The
# current algorithm did not. The solver does not care; it reruns whatever code
# is inside the callback and finds the base scalar that still hits +6%.
structural_solution <- solve_rate_target(
  current_premium = current_total,
  target = 0.06,
  rate_function = function(x) {
    rated <- demo_rate_book(proposed, structural_change = TRUE, base_multiplier = x)
    rated$indicated_BI + rated$indicated_CL
  },
  interval = c(.75, 1.35),
  parameter_name = "base multiplier after spec change"
)
structural_solution

# --- Separate targets by coverage -----------------------------------------
# Keep the public solver one-dimensional. If BI and CL have independent base
# rates and independent targets, call the scalar solver once for each coverage.
coverage_targets <- c(BI = .05, CL = .08)
coverage_solutions <- lapply(names(coverage_targets), function(cov) {
  current_cov <- sum(current_output[[paste0("indicated_", cov)]])
  solve_rate_target(
    current_premium = current_cov,
    target = coverage_targets[[cov]],
    rate_function = function(x) {
      candidate <- proposed
      hit <- candidate$term_name == "base_rate" & candidate$coverage == cov
      candidate$term_value[hit] <- candidate$term_value[hit] * x
      rated <- demo_rate_book(candidate)
      rated[[paste0("indicated_", cov)]]
    },
    interval = c(.75, 1.35),
    parameter_name = paste(cov, "base-rate multiplier")
  )
})
names(coverage_solutions) <- names(coverage_targets)
coverage_solutions

# Materialize both solved coverage changes into one final proposed factor table.
coverage_final <- proposed
for (cov in names(coverage_solutions)) {
  hit <- coverage_final$term_name == "base_rate" & coverage_final$coverage == cov
  coverage_final$term_value[hit] <- coverage_final$term_value[hit] * coverage_solutions[[cov]]$parameter
}
coverage_output <- demo_rate_book(coverage_final)
colSums(coverage_output[c("indicated_BI", "indicated_CL")]) /
  colSums(current_output[c("indicated_BI", "indicated_CL")]) - 1

# --- Separate base rates by territory --------------------------------------
# This is the important conceptual case: there are four absolute base rates,
# but the actuary has chosen ONE rate-level degree of freedom. x multiplies all
# proposed territory bases and preserves their proposed relative shape.
current_territory_bases <- c(T1 = 250, T2 = 275, T3 = 300, T4 = 330)
proposed_territory_shape <- c(T1 = 245, T2 = 280, T3 = 315, T4 = 360)
book_count <- c(T1 = 400, T2 = 350, T3 = 180, T4 = 70)
current_premium_territory <- sum(current_territory_bases * book_count)

territory_solution <- solve_rate_target(
  current_premium = current_premium_territory,
  target = 0.08,
  rate_function = function(x) {
    candidate_bases <- proposed_territory_shape * x
    candidate_bases * book_count
  },
  interval = c(.5, 1.5),
  parameter_name = "common territory-base multiplier"
)
territory_solution
proposed_territory_shape * territory_solution$parameter

# What solve_rate_target() intentionally does NOT do:
# allow T1, T2, T3, and T4 to move independently while supplying only one
# statewide target. That has infinitely many solutions unless the actuary adds
# more targets or constraints.
