rr_make_legacy_workbook <- function(file) {
  wb <- openxlsx2::wb_workbook()
  wb <- openxlsx2::wb_add_worksheet(wb, "Auto Rates")
  wb <- openxlsx2::wb_add_worksheet(wb, "Notes")

  wb <- openxlsx2::wb_add_data(
    wb, "Auto Rates", x = "Territory Factors",
    start_col = 2, start_row = 2, col_names = FALSE
  )
  territory <- data.frame(
    Territory = paste0("T", 1:4),
    BI = c(.90, 1.00, 1.10, 1.20),
    PD = c(.94, 1.00, 1.08, 1.16),
    CL = c(.96, 1.00, 1.05, 1.12),
    check.names = FALSE
  )
  wb <- openxlsx2::wb_add_data(
    wb, "Auto Rates", x = territory,
    start_col = 2, start_row = 4, col_names = TRUE
  )

  wb <- openxlsx2::wb_add_data(
    wb, "Auto Rates", x = "Driver Age",
    start_col = 2, start_row = 11, col_names = FALSE
  )
  age <- data.frame(
    Age = c(16, 17, 18, 21, 30, 50),
    BI = c(1.60, 1.42, 1.28, 1.12, 1.02, 1.00),
    PD = c(1.45, 1.33, 1.22, 1.10, 1.01, 1.00),
    CL = c(1.28, 1.22, 1.16, 1.08, 1.01, 1.00),
    check.names = FALSE
  )
  wb <- openxlsx2::wb_add_data(
    wb, "Auto Rates", x = age,
    start_col = 2, start_row = 13, col_names = TRUE
  )


  wb <- openxlsx2::wb_add_data(
    wb, "Auto Rates", x = "Base Rates",
    start_col = 8, start_row = 2, col_names = FALSE
  )
  base <- data.frame(
    Label = "Base Rate",
    BI = 220,
    CL = 180,
    check.names = FALSE
  )
  wb <- openxlsx2::wb_add_data(
    wb, "Auto Rates", x = base,
    start_col = 8, start_row = 4, col_names = FALSE
  )

  wb <- openxlsx2::wb_add_data(
    wb, "Notes", x = data.frame(note = c("Legacy workbook", "Do not edit")),
    start_col = 1, start_row = 1, col_names = TRUE
  )
  openxlsx2::wb_save(wb, file, overwrite = TRUE)
  file
}

test_that("workbook profiler finds plausible rectangular rate tables", {
  skip_if_not_installed("openxlsx2")
  f <- tempfile(fileext = ".xlsx")
  rr_make_legacy_workbook(f)

  p <- profile_rate_workbook(f)
  expect_s3_class(p, "raterevision_workbook_profile")
  expect_true(nrow(p) >= 3L)
  expect_true(all(c("source_sheet", "extract_range", "table_name", "confidence", "include") %in% names(p)))

  auto <- p[p$source_sheet == "Auto Rates", , drop = FALSE]
  expect_true(nrow(auto) >= 3L)
  expect_true(any(grepl("territory", auto$table_name)))
  expect_true(any(grepl("driver_age|age", auto$table_name)))
  expect_true(any(grepl("base", auto$table_name)))
})

test_that("extracted tables are named rectangular data frames with source metadata", {
  skip_if_not_installed("openxlsx2")
  f <- tempfile(fileext = ".xlsx")
  rr_make_legacy_workbook(f)
  p <- profile_rate_workbook(f)

  p$include <- p$source_sheet == "Auto Rates" & p$confidence >= 0.35
  tabs <- extract_rate_tables(p)

  expect_s3_class(tabs, "raterevision_extracted_tables")
  expect_true(length(tabs) >= 3L)
  expect_true(all(vapply(tabs, is.data.frame, logical(1))))
  expect_true(is.data.frame(attr(tabs, "manifest")))
  expect_true(all(attr(tabs, "manifest")$layout == "raw_extracted"))
  expect_true(all(vapply(tabs, function(z) !is.null(attr(z, "source_range")), logical(1))))

  # A one-row, headerless base-rate table should be surfaced without losing its
  # only data row.
  base_name <- grep("base", names(tabs), value = TRUE)[1]
  expect_false(is.na(base_name))
  expect_equal(nrow(tabs[[base_name]]), 1L)
  expect_equal(as.character(tabs[[base_name]][1, 1]), "Base Rate")
})

test_that("profile edits control extraction", {
  skip_if_not_installed("openxlsx2")
  f <- tempfile(fileext = ".xlsx")
  rr_make_legacy_workbook(f)
  p <- profile_rate_workbook(f)
  p$include <- FALSE
  i <- which(p$source_sheet == "Auto Rates")[1]
  p$include[i] <- TRUE
  p$table_name[i] <- "my_manual_name"

  tabs <- extract_rate_tables(p)
  expect_equal(length(tabs), 1L)
  expect_equal(names(tabs), "my_manual_name")
})
