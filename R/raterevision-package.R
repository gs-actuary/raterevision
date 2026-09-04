#' raterevision: insurance rate revision workflow tools
#'
#' `raterevision` provides a deliberately narrow set of tools for moving from a
#' current insurance rating plan to a proposed one. It complements
#' `ratingtables`, but the analysis helpers accept ordinary data frames and the
#' target solver accepts arbitrary user-supplied rating code.
#'
#' The main workflow is:
#'
#' * reshape normalized factor tables for human review with [rate_tables_wide()];
#' * round-trip those tables through Excel with [write_rate_workbook()] and
#'   [read_rate_workbook()];
#' * construct proposed tables with [replace_factor_set()], [update_factors()],
#'   [rebase_factor_set()], and [normalize_factor_set()];
#' * review changes with [rate_plan_diff()] and [validate_rate_plan()];
#' * compare rated outputs using [compare_rating_outputs()],
#'   [summarize_rate_change()], and [rate_change_distribution()]; and
#' * solve a scalar rate-level adjustment with [solve_rate_target()].
#'
#' @keywords internal
"_PACKAGE"
