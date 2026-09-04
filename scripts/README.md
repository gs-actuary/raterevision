# Demo scripts

These files are repository-only teaching/demo material. They are intentionally
excluded from the built package so they can be verbose and edited freely.

Recommended order:

1. `00_setup_check_install.R` — dependencies, roxygen, tests, vignette build,
   CRAN-style check, local install.
2. `01_excel_roundtrip.R` — normalized long factors -> named wide tables ->
   Excel -> normalized long factors; includes a two-way interaction matrix.
3. `02_programmatic_revision.R` — complete factor-set replacement, sparse
   updates, adding a new level, rebasing, normalization, validation and diff.
4. `03_driver_averaging_rate_change.R` — realistic `ratingtables` orchestration:
   rate drivers, average driver factors, join to vehicles, rate vehicles, then
   compare current/proposed impacts.
5. `04_solve_rate_target.R` — overall scalar solve, structural/spec change,
   independent by-coverage solves, and a common multiplier across separate
   territory base rates.
6. `05_full_demo_workflow.R` — concise end-to-end video/demo sequence.
7. `06_without_raterevision_baseline.R` — the same kind of revision with manual
   base-R key and reshape plumbing, useful for a before/after comparison.

`_demo_helpers.R` contains the synthetic data and realistic rating workflow used
by the walkthroughs. It is worth reading after script 02 and before script 03 if
you want to understand every `ratingtables` call in detail.
