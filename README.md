# raterevision

`raterevision` is a companion workflow package for insurance rate revisions. It
focuses on the work around a rating engine rather than replacing the engine:

- convert normalized long rating factors into actuary-friendly wide tables;
- explicitly round-trip those tables through Excel;
- apply complete factor-set replacements or sparse programmatic edits;
- rebase and normalize relativities;
- audit current-versus-proposed factor changes;
- compare current and proposed premium output;
- summarize overall and distributional rate impacts; and
- solve a one-dimensional rate-level adjustment to hit a target aggregate rate change.

The package is designed to work naturally with `ratingtables`, but most
functions operate on ordinary data frames. `solve_rate_target()` is deliberately
agnostic to the rating implementation: its callback may run a different spec,
perform driver averaging, call custom code, or multiply an entire vector of
territory-specific base rates by a common scalar.

## Development installation

From a local checkout:

```r
devtools::document()
devtools::install()
```

Once the GitHub repository is public, the development version can be installed
with `pak::pkg_install("gs-actuary/raterevision")`.

## Excel round trip

```r
library(raterevision)

wide <- rate_tables_wide(
  factor_table,
  filters = list(state = "IL"),
  matrix_terms = "age_gender_interaction"
)

write_rate_workbook(wide, "IL_rate_revision.xlsx")
proposed <- read_rate_workbook("IL_rate_revision.xlsx")
rate_plan_diff(factor_table, proposed, include_unchanged = FALSE)
```

The workbook contains a very-hidden manifest so the import does not have to
guess which sheets are ordinary coverage-wide tables versus two-way interaction
matrices. Sparse factor cells are written as true Excel blanks; clearing a factor
cell on a coverage-wide sheet removes that coverage-specific key on import. Interactions deeper than two variables are intentionally never
rendered as matrices.

## Programmatic changes

```r
proposed <- replace_factor_set(current, new_age_table)
proposed <- update_factors(proposed, sparse_changes)
proposed <- rebase_factor_set(proposed, "driver_age", base_level = "20")
validate_rate_plan(proposed)
```

`replace_factor_set()` treats the replacement as a complete table slice;
`update_factors()` changes only supplied keys and rejects new keys unless
`allow_new_levels = TRUE` is explicit.

## Premium impact

`ratingtables::rate_policies()` returns `indicated_<coverage>` columns, which
`compare_rating_outputs()` detects automatically:

```r
impact <- compare_rating_outputs(
  current_output,
  proposed_output,
  id_cols = "vehicle_id",
  keep_cols = c("charter", "territory")
)

summarize_rate_change(impact, by = c("charter", "coverage"))
rate_change_distribution(impact, by = "coverage")
```

## Solve a target rate level

```r
solution <- solve_rate_target(
  current_premium = 10e6,
  target = 0.06,
  rate_function = function(x) {
    # x is the one scalar the actuary has chosen to solve.
    # This code can be arbitrarily complicated.
    rate_proposed_book(base_multiplier = x)
  }
)
```

For separate base rates by territory, the same scalar can multiply all proposed
territory bases. If multiple territory bases are allowed to move independently
against only one aggregate target, the problem is underdetermined; the package
does not invent a solution.

## Learning material

The repository `scripts/` directory contains line-by-line runnable walkthroughs:

- `00_setup_check_install.R`
- `01_excel_roundtrip.R`
- `02_programmatic_revision.R`
- `03_driver_averaging_rate_change.R`
- `04_solve_rate_target.R`
- `05_full_demo_workflow.R`
- `06_without_raterevision_baseline.R`

These scripts are intentionally excluded from the built package so they can be
verbose, exploratory, and useful for live demonstrations. The installed package
also includes the vignette `vignette("rate-revision-workflow", package = "raterevision")`.
