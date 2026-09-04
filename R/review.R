#' Compare current and proposed factor tables
#'
#' Produces a row-level audit of added, removed, changed, and unchanged factor
#' keys. `factor_row_id` is ignored by default because it is not a rating key.
#'
#' @param current Current normalized factor table.
#' @param proposed Proposed normalized factor table.
#' @param key_cols Optional key columns. By default all non-value, non-internal
#'   columns from either table are used.
#' @param tolerance Numeric tolerance for treating values as unchanged.
#' @param include_unchanged Logical; retain unchanged rows.
#'
#' @return A data frame with current/proposed values, changes, and `status`.
#' @export
rate_plan_diff <- function(current, proposed, key_cols = NULL,
                           tolerance = sqrt(.Machine$double.eps), include_unchanged = TRUE) {
  .rr_check_factor_table(current); .rr_check_factor_table(proposed)
  a <- as.data.frame(current, stringsAsFactors = FALSE)
  b <- as.data.frame(proposed, stringsAsFactors = FALSE)
  all_cols <- unique(c(names(a), names(b)))
  for (nm in setdiff(all_cols, names(a))) a[[nm]] <- NA
  for (nm in setdiff(all_cols, names(b))) b[[nm]] <- NA
  if (is.null(key_cols)) key_cols <- setdiff(all_cols, c("term_value", "factor_row_id"))
  .rr_assert_cols(a, key_cols, "current"); .rr_assert_cols(b, key_cols, "proposed")
  ka <- .rr_make_key(a, key_cols); kb <- .rr_make_key(b, key_cols)
  if (anyDuplicated(ka)) stop("current has duplicate factor keys.", call. = FALSE)
  if (anyDuplicated(kb)) stop("proposed has duplicate factor keys.", call. = FALSE)
  keys <- unique(c(ka, kb))
  ia <- match(keys, ka); ib <- match(keys, kb)
  base <- .rr_rbind_fill(lapply(seq_along(keys), function(i) {
    if (!is.na(ib[i])) b[ib[i], key_cols, drop = FALSE] else a[ia[i], key_cols, drop = FALSE]
  }))
  cv <- ifelse(is.na(ia), NA_real_, as.numeric(a$term_value[ia]))
  pv <- ifelse(is.na(ib), NA_real_, as.numeric(b$term_value[ib]))
  status <- ifelse(is.na(ia), "added",
                   ifelse(is.na(ib), "removed",
                          ifelse(abs(pv - cv) <= tolerance, "unchanged", "changed")))
  out <- base
  out$current_value <- cv
  out$proposed_value <- pv
  out$difference <- pv - cv
  out$percent_change <- ifelse(!is.na(cv) & cv != 0, (pv - cv) / cv, NA_real_)
  out$status <- status
  if (!isTRUE(include_unchanged)) out <- out[out$status != "unchanged", , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Validate a normalized rate factor table
#'
#' Performs revision-oriented checks without duplicating the full rating-spec
#' validation in `ratingtables`. It checks required columns, factor values,
#' duplicate keys, variable/level slot consistency, and optionally positivity
#' for known multiplicative terms.
#'
#' @param factor_table Normalized factor table.
#' @param multiplicative_terms Optional character vector of terms whose values
#'   must be strictly positive.
#' @param error Logical; stop if any errors are found.
#'
#' @return A `raterevision_validation` data frame. If `error = TRUE`, validation
#'   errors are also raised as an error.
#' @export
validate_rate_plan <- function(factor_table, multiplicative_terms = NULL, error = TRUE) {
  issues <- list(); ii <- 1L
  add_issue <- function(issue, detail, rows = NA_character_) {
    issues[[ii]] <<- data.frame(severity = "error", issue = issue, rows = rows, detail = detail, stringsAsFactors = FALSE)
    ii <<- ii + 1L
  }
  if (!is.data.frame(factor_table)) {
    add_issue("not_data_frame", "factor_table must be a data frame.")
  } else {
    x <- as.data.frame(factor_table, stringsAsFactors = FALSE)
    miss <- setdiff(.rr_required_cols, names(x))
    if (length(miss)) add_issue("missing_columns", paste("Missing:", paste(miss, collapse = ", ")))
    if (!length(miss)) {
      if (any(.rr_is_blank(x$term_name))) add_issue("missing_term_name", "term_name contains missing or blank values.", paste(which(.rr_is_blank(x$term_name)), collapse = ","))
      if (any(.rr_is_blank(x$coverage))) add_issue("missing_coverage", "coverage contains missing or blank values.", paste(which(.rr_is_blank(x$coverage)), collapse = ","))
      vals <- suppressWarnings(as.numeric(x$term_value))
      bad_val <- which(is.na(vals) | !is.finite(vals))
      if (length(bad_val)) add_issue("invalid_term_value", "term_value must be finite numeric data.", paste(bad_val, collapse = ","))
      key_cols <- setdiff(names(x), c("term_value", "factor_row_id"))
      key <- .rr_make_key(x, key_cols)
      dup <- which(duplicated(key) | duplicated(key, fromLast = TRUE))
      if (length(dup)) add_issue("duplicate_factor_key", "Duplicate factor keys found.", paste(dup, collapse = ","))
      slots <- .rr_slot_columns(names(x))
      if (length(slots$variable) != length(slots$level)) {
        add_issue("unpaired_slot_columns", "Each variableN column must have a corresponding levelN column and vice versa.")
      }
      n <- min(length(slots$variable), length(slots$level))
      if (n) {
        for (s in seq_len(n)) {
          vb <- .rr_is_blank(x[[slots$variable[s]]]); lb <- .rr_is_blank(x[[slots$level[s]]])
          mismatch <- which(xor(vb, lb))
          if (length(mismatch)) add_issue("slot_mismatch", paste0(slots$variable[s], " and ", slots$level[s], " must be jointly present or absent."), paste(mismatch, collapse = ","))
          if (s > 1L) {
            prev_blank <- .rr_is_blank(x[[slots$variable[s - 1L]]])
            gap <- which(!vb & prev_blank)
            if (length(gap)) add_issue("slot_gap", paste0("Variable slot ", s, " is populated while slot ", s - 1L, " is empty."), paste(gap, collapse = ","))
          }
        }
      }
      if (!is.null(multiplicative_terms)) {
        bad <- which(as.character(x$term_name) %in% multiplicative_terms & vals <= 0)
        if (length(bad)) add_issue("nonpositive_multiplicative_factor", "Known multiplicative terms must be positive.", paste(bad, collapse = ","))
      }
    }
  }
  out <- if (length(issues)) do.call(rbind, issues) else data.frame(severity = character(), issue = character(), rows = character(), detail = character(), stringsAsFactors = FALSE)
  class(out) <- c("raterevision_validation", "data.frame")
  if (isTRUE(error) && nrow(out)) stop("Rate-plan validation failed with ", nrow(out), " error(s). Run validate_rate_plan(..., error = FALSE) for details.", call. = FALSE)
  out
}

#' @export
print.raterevision_validation <- function(x, ...) {
  if (!nrow(x)) {
    cat("<raterevision_validation> OK: no issues found.\n")
  } else {
    cat("<raterevision_validation>", nrow(x), "issue(s)\n")
    print.data.frame(x, row.names = FALSE)
  }
  invisible(x)
}
