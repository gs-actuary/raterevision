#' Convert a normalized factor table to wide rate tables
#'
#' Converts a `ratingtables`-style normalized factor table into a named list of
#' actuary-friendly data frames. Coverages become columns. One-way and higher
#' interactions remain long by default; selected two-way interactions can be
#' displayed as matrices with one interaction variable down rows and the other
#' across columns. Matrix layout is intentionally prohibited for interactions
#' deeper than two variables.
#'
#' @param factor_table A normalized factor table containing `coverage`,
#'   `term_name`, `term_value`, and optional `variable1`/`level1`,
#'   `variable2`/`level2`, ... columns.
#' @param filters Optional named list of exact-value filters, for example
#'   `list(state = "IL", rate_eff_date = as.Date("2026-01-01"))`.
#' @param matrix_terms Character vector of two-way `term_name` values to display
#'   in matrix form. All other terms use coverage-wide long format.
#' @param sort_rows Logical; sort metadata and level columns using numeric-aware
#'   ordering where possible.
#'
#' @return A named list of data frames with class `raterevision_tables`. A
#'   machine-readable manifest is stored in the `manifest` attribute.
#' @export
rate_tables_wide <- function(factor_table, filters = NULL, matrix_terms = character(),
                             sort_rows = TRUE) {
  .rr_check_factor_table(factor_table)
  x <- as.data.frame(factor_table, stringsAsFactors = FALSE)
  x <- .rr_apply_filters(x, filters)
  if (!nrow(x)) stop("No factor rows remain after filtering.", call. = FALSE)
  if (!is.numeric(x$term_value)) {
    z <- suppressWarnings(as.numeric(x$term_value))
    if (any(is.na(z) & !is.na(x$term_value))) stop("term_value must be numeric.", call. = FALSE)
    x$term_value <- z
  }

  slots <- .rr_slot_columns(names(x))
  metadata <- .rr_metadata_cols(x)
  terms <- unique(as.character(x$term_name))
  unknown_matrix <- setdiff(matrix_terms, terms)
  if (length(unknown_matrix)) stop("matrix_terms not found: ", paste(unknown_matrix, collapse = ", "), ".", call. = FALSE)

  tables <- list()
  manifest <- list()
  used <- character()
  mi <- 1L

  for (term in terms) {
    z <- x[as.character(x$term_name) == term, , drop = FALSE]
    depth <- .rr_depth(z)
    if (term %in% matrix_terms && depth != 2L) {
      stop("Matrix layout is only supported for two-variable interactions. Term '", term,
           "' has depth ", depth, ".", call. = FALSE)
    }

    if (!(term %in% matrix_terms)) {
      slot_cols <- character()
      if (depth > 0L) {
        for (i in seq_len(depth)) slot_cols <- c(slot_cols, slots$variable[i], slots$level[i])
      }
      id_cols <- unique(c(metadata, "term_name", slot_cols))
      id_cols <- intersect(id_cols, names(z))
      tab <- .rr_pivot_coverage(z, id_cols)
      if (isTRUE(sort_rows)) tab <- .rr_order_df(tab, c(metadata, slot_cols))
      sheet <- .rr_safe_sheet_name(term, used)
      used <- c(used, sheet)
      tables[[sheet]] <- tab
      manifest[[mi]] <- data.frame(
        sheet_name = sheet, term_name = term, layout = "coverage_wide",
        coverage = "", depth = depth,
        metadata_cols = .rr_encode_cols(metadata), stringsAsFactors = FALSE
      )
      mi <- mi + 1L
    } else {
      covs <- unique(as.character(z$coverage))
      for (cov in covs) {
        q <- z[as.character(z$coverage) == cov, , drop = FALSE]
        id_cols <- unique(c(metadata, "term_name", slots$variable[1], slots$level[1], slots$variable[2]))
        id_cols <- intersect(id_cols, names(q))
        key_cell <- .rr_make_key(q, c(id_cols, slots$level[2]))
        if (anyDuplicated(key_cell)) stop("Duplicate interaction cells prevent matrix layout for term '", term, "'.", call. = FALSE)
        ids <- unique(q[id_cols])
        id_key <- .rr_make_key(ids, id_cols)
        levels2 <- unique(as.character(q[[slots$level[2]]]))
        levels2 <- levels2[order(.rr_order_key(levels2), na.last = TRUE)]
        tab <- ids
        for (lev in levels2) {
          vals <- rep(NA_real_, nrow(ids))
          w <- q[as.character(q[[slots$level[2]]]) == lev, , drop = FALSE]
          m <- match(.rr_make_key(w, id_cols), id_key)
          vals[m] <- as.numeric(w$term_value)
          tab[[lev]] <- vals
        }
        if (isTRUE(sort_rows)) tab <- .rr_order_df(tab, c(metadata, slots$level[1]))
        sheet <- .rr_safe_sheet_name(paste(term, cov, sep = "_"), used)
        used <- c(used, sheet)
        tables[[sheet]] <- tab
        manifest[[mi]] <- data.frame(
          sheet_name = sheet, term_name = term, layout = "interaction_matrix",
          coverage = cov, depth = 2L,
          metadata_cols = .rr_encode_cols(metadata), stringsAsFactors = FALSE
        )
        mi <- mi + 1L
      }
    }
  }

  manifest <- do.call(rbind, manifest)
  attr(tables, "manifest") <- manifest
  attr(tables, "schema_version") <- "1"
  attr(tables, "filters") <- filters
  class(tables) <- c("raterevision_tables", "list")
  tables
}

#' @export
print.raterevision_tables <- function(x, ...) {
  manifest <- attr(x, "manifest")
  cat("<raterevision_tables>\n")
  cat(" sheets:", length(x), "\n")
  if (!is.null(manifest) && nrow(manifest)) {
    cat(" terms:", length(unique(manifest$term_name)), "\n")
    cat(" matrix sheets:", sum(manifest$layout == "interaction_matrix"), "\n")
  }
  invisible(x)
}

#' Convert wide rate tables back to normalized long format
#'
#' Reconstructs a normalized factor table from the object produced by
#' [rate_tables_wide()]. This is the non-Excel reverse transformation used by
#' [read_rate_workbook()].
#'
#' @param tables A `raterevision_tables` object or a named list of data frames.
#' @param manifest Optional manifest. Required if the `manifest` attribute has
#'   been removed.
#' @param add_factor_row_id Logical; add a fresh sequential `factor_row_id`.
#'
#' @return A normalized long-format factor table.
#' @export
rate_tables_long <- function(tables, manifest = attr(tables, "manifest"), add_factor_row_id = TRUE) {
  if (!is.list(tables) || !length(tables)) stop("tables must be a non-empty named list of data frames.", call. = FALSE)
  if (is.null(manifest) || !is.data.frame(manifest)) stop("A rate-table manifest is required.", call. = FALSE)
  .rr_assert_cols(manifest, c("sheet_name", "term_name", "layout", "coverage", "depth", "metadata_cols"), "manifest")

  rows <- list(); ri <- 1L
  for (j in seq_len(nrow(manifest))) {
    meta <- manifest[j, , drop = FALSE]
    sheet <- as.character(meta$sheet_name)
    if (!(sheet %in% names(tables))) stop("Manifest sheet '", sheet, "' is missing from tables.", call. = FALSE)
    tab <- as.data.frame(tables[[sheet]], stringsAsFactors = FALSE, check.names = FALSE)
    metadata <- .rr_decode_cols(meta$metadata_cols)
    depth <- as.integer(meta$depth)
    slot_cols <- character()
    if (depth > 0L) {
      for (i in seq_len(depth)) slot_cols <- c(slot_cols, paste0("variable", i), paste0("level", i))
    }

    if (meta$layout == "coverage_wide") {
      id_cols <- unique(c(metadata, "term_name", slot_cols))
      id_cols <- intersect(id_cols, names(tab))
      cov_cols <- setdiff(names(tab), id_cols)
      if (!length(cov_cols)) stop("Sheet '", sheet, "' contains no coverage columns.", call. = FALSE)
      for (cov in cov_cols) {
        raw <- tab[[cov]]
        z <- tab[id_cols]
        z$coverage <- cov
        z$term_value <- suppressWarnings(as.numeric(raw))
        if (any(is.na(z$term_value) & !.rr_is_blank(raw))) {
          stop("Non-numeric factor value found on sheet '", sheet, "', coverage '", cov, "'.", call. = FALSE)
        }
        keep <- !.rr_is_blank(raw)
        if (any(keep)) {
          rows[[ri]] <- z[keep, , drop = FALSE]; ri <- ri + 1L
        }
      }
    } else if (meta$layout == "interaction_matrix") {
      id_cols <- unique(c(metadata, "term_name", "variable1", "level1", "variable2"))
      id_cols <- intersect(id_cols, names(tab))
      level2_cols <- setdiff(names(tab), id_cols)
      if (!length(level2_cols)) stop("Matrix sheet '", sheet, "' contains no level-2 columns.", call. = FALSE)
      for (lev2 in level2_cols) {
        raw <- tab[[lev2]]
        z <- tab[id_cols]
        z$level2 <- lev2
        z$coverage <- as.character(meta$coverage)
        z$term_value <- suppressWarnings(as.numeric(raw))
        if (any(is.na(z$term_value) & !.rr_is_blank(raw))) {
          stop("Non-numeric matrix factor found on sheet '", sheet, "'.", call. = FALSE)
        }
        keep <- !.rr_is_blank(raw)
        if (any(keep)) {
          rows[[ri]] <- z[keep, , drop = FALSE]; ri <- ri + 1L
        }
      }
    } else {
      stop("Unknown manifest layout '", meta$layout, "'.", call. = FALSE)
    }
  }

  out <- .rr_rbind_fill(rows)
  slots <- .rr_slot_columns(names(out))
  canonical <- unique(c(unlist(lapply(manifest$metadata_cols, .rr_decode_cols), use.names = FALSE),
                        "coverage", "term_name", "term_value",
                        as.vector(rbind(slots$variable, slots$level))))
  canonical <- canonical[canonical %in% names(out)]
  out <- out[c(canonical, setdiff(names(out), canonical))]
  if (isTRUE(add_factor_row_id)) out <- .rr_refresh_factor_ids(out)
  rownames(out) <- NULL
  out
}
