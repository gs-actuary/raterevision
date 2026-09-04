#' Write rate tables to an Excel workbook
#'
#' Writes normalized factors or a [rate_tables_wide()] list to an `.xlsx`
#' workbook. The workbook contains one visible sheet per rate table, an optional
#' instruction sheet, and a very-hidden `_raterevision` manifest used for a
#' deterministic round trip back into R.
#'
#' @param x A normalized factor table or `raterevision_tables` object.
#' @param file Output `.xlsx` path.
#' @param filters Optional filters passed to [rate_tables_wide()] when `x` is a
#'   long factor table.
#' @param matrix_terms Two-way interaction terms to display as matrices.
#' @param overwrite Logical; overwrite an existing file.
#' @param include_readme Logical; include a visible `_README` sheet.
#'
#' @return The file path, invisibly.
#' @export
write_rate_workbook <- function(x, file, filters = NULL, matrix_terms = character(),
                                overwrite = TRUE, include_readme = TRUE) {
  if (missing(file) || length(file) != 1L || !nzchar(file)) stop("file must be a single path.", call. = FALSE)
  if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) stop("file must have an .xlsx extension.", call. = FALSE)
  if (file.exists(file) && !isTRUE(overwrite)) stop("File already exists: ", file, call. = FALSE)

  tables <- if (inherits(x, "raterevision_tables")) x else rate_tables_wide(x, filters = filters, matrix_terms = matrix_terms)
  manifest <- attr(tables, "manifest")
  if (is.null(manifest)) stop("raterevision_tables object has no manifest.", call. = FALSE)
  manifest$schema_version <- "1"
  manifest$raterevision_version <- .rr_pkg_version()

  wb <- openxlsx2::wb_workbook(
    creator = "raterevision",
    title = "Insurance rate revision workbook",
    subject = "Editable rating factor tables"
  )

  if (isTRUE(include_readme)) {
    readme <- data.frame(
      item = c("Purpose", "Editing", "Coverage columns", "Interactions", "Adding levels", "Deleting levels", "Important"),
      guidance = c(
        "This workbook is an editable representation of a normalized rating factor table.",
        "Edit factor cells or add/remove rating-level rows. Do not rename structural columns.",
        "Coverage names appear as factor-value columns in ordinary sheets.",
        "Two-way interactions may use matrix sheets when requested. Deeper interactions remain long.",
        "New rows are allowed. read_rate_workbook() will reconstruct them as new factor levels.",
        "Deleting a row removes its factor keys. Clearing one coverage factor cell removes only that coverage-specific key; review rate_plan_diff() afterwards.",
        "The very-hidden _raterevision sheet is machine metadata. Do not remove it if you want a deterministic round trip."
      ), stringsAsFactors = FALSE
    )
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = "_README", grid_lines = FALSE)
    wb <- openxlsx2::wb_add_data(wb, sheet = "_README", x = readme)
    wb <- openxlsx2::wb_freeze_pane(wb, sheet = "_README", first_row = TRUE)
  }

  for (sheet in names(tables)) {
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet)
    wb <- openxlsx2::wb_add_data(
      wb, sheet = sheet, x = tables[[sheet]], with_filter = TRUE, na = NULL
    )
    wb <- openxlsx2::wb_freeze_pane(wb, sheet = sheet, first_row = TRUE)
    wb <- openxlsx2::wb_set_col_widths(
      wb, sheet = sheet, cols = seq_along(tables[[sheet]]), widths = "auto"
    )
  }

  wb <- openxlsx2::wb_add_worksheet(wb, sheet = "_raterevision", visible = "veryhidden")
  wb <- openxlsx2::wb_add_data(wb, sheet = "_raterevision", x = manifest)
  openxlsx2::wb_save(wb, file = file, overwrite = overwrite)
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}

#' Read an Excel rate workbook
#'
#' Reads a workbook created by [write_rate_workbook()]. By default the visible
#' tables are reconstructed into a normalized long factor table suitable for
#' `ratingtables` or further programmatic revision.
#'
#' @param file Input `.xlsx` path.
#' @param return Either `"long"` (default) or `"tables"`.
#' @param validate Logical; validate the reconstructed long factor table.
#'
#' @return A normalized factor table or `raterevision_tables` object.
#' @export
read_rate_workbook <- function(file, return = c("long", "tables"), validate = TRUE) {
  return <- match.arg(return)
  if (!file.exists(file)) stop("File does not exist: ", file, call. = FALSE)
  wb <- openxlsx2::wb_load(file)
  sheets <- unname(openxlsx2::wb_get_sheet_names(wb))
  if (!("_raterevision" %in% sheets)) {
    stop("Workbook does not contain the _raterevision manifest. It may not have been created by write_rate_workbook().", call. = FALSE)
  }
  manifest <- openxlsx2::wb_to_df(wb, sheet = "_raterevision", check_names = FALSE)
  .rr_assert_cols(manifest, c("sheet_name", "term_name", "layout", "coverage", "depth", "metadata_cols"), "workbook manifest")
  needed <- as.character(manifest$sheet_name)
  miss <- setdiff(needed, sheets)
  if (length(miss)) stop("Workbook is missing rate sheet(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)

  tabs <- setNames(lapply(needed, function(s) openxlsx2::wb_to_df(wb, sheet = s, check_names = FALSE)), needed)
  attr(tabs, "manifest") <- manifest[c("sheet_name", "term_name", "layout", "coverage", "depth", "metadata_cols")]
  attr(tabs, "schema_version") <- if ("schema_version" %in% names(manifest)) as.character(manifest$schema_version[1]) else "1"
  class(tabs) <- c("raterevision_tables", "list")
  if (return == "tables") return(tabs)
  out <- rate_tables_long(tabs)
  if (isTRUE(validate)) validate_rate_plan(out, error = TRUE)
  out
}
