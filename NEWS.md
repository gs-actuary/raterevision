# raterevision 0.1.0

* Initial development release.
* Excel round-trip for coverage-wide factor tables and optional two-way interaction matrices.
* Programmatic factor replacement, sparse updates, rebasing, and normalization.
* Factor-table diff and validation helpers.
* Current/proposed premium comparison, summaries, and dislocation distributions.
* Generic scalar target-rate solver accepting arbitrary user rating code.

- Added heuristic legacy-workbook migration helpers `profile_rate_workbook()` and `extract_rate_tables()` for finding, reviewing, and extracting rectangular rating-table candidates from Excel workbooks.
- Added `diagnose_rating_output()` for triaging missing and incorrect premiums during rater migration using grouped discrepancy scans, lightweight background models, and pairwise interaction checks.
- Added runnable migration-harvesting and rater-diagnostic demo scripts plus unit tests.