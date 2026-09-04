# 00_setup_check_install.R
# Repository setup and pre-demo package verification.
# Run from the raterevision package root, line-by-line if desired.

# Use a literal CRAN URL. Do not paste a Markdown-formatted hyperlink here.
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install development/runtime dependencies if they are not already installed.
needed <- c(
  "devtools", "testthat", "openxlsx2", "knitr", "rmarkdown", "ratingtables"
)
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
missing
if (length(missing)) install.packages(missing)

# Regenerate NAMESPACE and man pages from the roxygen comments.
# Do this before testing/checking whenever function documentation changes.
devtools::document()

# Run the automated unit/integration tests.
devtools::test()

# Build vignettes explicitly. This catches vignette problems before R CMD check.
devtools::build_vignettes()

# CRAN-style package check. Aim for 0 errors, 0 warnings, and 0 notes that are
# attributable to the package itself.
check_result <- devtools::check(args = "--as-cran")
check_result

# Install this checkout into the active R library for the demo scripts.
devtools::install(upgrade = "never")

# Smoke test the installed package and its main dependency.
library(raterevision)
packageVersion("raterevision")
packageVersion("ratingtables")

# Then run the walkthroughs in order:
# source("scripts/01_excel_roundtrip.R")
# source("scripts/02_programmatic_revision.R")
# source("scripts/03_driver_averaging_rate_change.R")
# source("scripts/04_solve_rate_target.R")
# source("scripts/05_full_demo_workflow.R")
