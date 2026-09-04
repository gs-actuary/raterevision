#' Compare current and proposed rating outputs
#'
#' Standardizes current/proposed output into one long comparison table. It can
#' consume the wide `indicated_<coverage>` columns produced by `ratingtables`,
#' or already-long output with an explicit premium and coverage column.
#'
#' @param current Current rating output.
#' @param proposed Proposed rating output.
#' @param id_cols Columns uniquely identifying a rated record.
#' @param premium_cols Optional wide premium columns. If omitted, common columns
#'   beginning with `premium_prefix` are detected.
#' @param premium_prefix Prefix removed from wide premium columns to obtain the
#'   coverage label. Defaults to `"indicated_"`.
#' @param premium_col For long input, the premium column name.
#' @param coverage_col For long input, the coverage/peril column name.
#' @param keep_cols Additional columns from current output to carry into the
#'   comparison (for example `charter` or `territory`).
#'
#' @return Long comparison data with current/proposed premium and changes.
#' @export
compare_rating_outputs <- function(current, proposed, id_cols,
                                   premium_cols = NULL, premium_prefix = "indicated_",
                                   premium_col = NULL, coverage_col = NULL,
                                   keep_cols = NULL) {
  if (!is.data.frame(current) || !is.data.frame(proposed)) stop("current and proposed must be data frames.", call. = FALSE)
  .rr_assert_cols(current, unique(c(id_cols, keep_cols)), "current")
  .rr_assert_cols(proposed, id_cols, "proposed")

  if (!is.null(premium_col) || !is.null(coverage_col)) {
    if (is.null(premium_col) || is.null(coverage_col)) stop("premium_col and coverage_col must be supplied together.", call. = FALSE)
    .rr_assert_cols(current, c(premium_col, coverage_col), "current")
    .rr_assert_cols(proposed, c(premium_col, coverage_col), "proposed")
    key_cols <- c(id_cols, coverage_col)
    ka <- .rr_make_key(current, key_cols); kb <- .rr_make_key(proposed, key_cols)
    if (anyDuplicated(ka) || anyDuplicated(kb)) stop("id_cols + coverage_col must uniquely identify rating output rows.", call. = FALSE)
    if (!setequal(ka, kb)) stop("Current and proposed output do not contain the same rating keys.", call. = FALSE)
    m <- match(ka, kb)
    out <- current[unique(c(id_cols, keep_cols, coverage_col))]
    names(out)[names(out) == coverage_col] <- "coverage"
    out$current_premium <- as.numeric(current[[premium_col]])
    out$proposed_premium <- as.numeric(proposed[[premium_col]][m])
  } else {
    if (is.null(premium_cols)) {
      cand <- intersect(names(current), names(proposed))
      premium_cols <- cand[startsWith(cand, premium_prefix)]
    }
    if (!length(premium_cols)) stop("No premium columns supplied or detected.", call. = FALSE)
    .rr_assert_cols(current, premium_cols, "current"); .rr_assert_cols(proposed, premium_cols, "proposed")
    ka <- .rr_make_key(current, id_cols); kb <- .rr_make_key(proposed, id_cols)
    if (anyDuplicated(ka) || anyDuplicated(kb)) stop("id_cols must uniquely identify current and proposed rows.", call. = FALSE)
    if (!setequal(ka, kb)) stop("Current and proposed output do not contain the same record keys.", call. = FALSE)
    m <- match(ka, kb)
    pieces <- lapply(premium_cols, function(pc) {
      z <- current[unique(c(id_cols, keep_cols))]
      z$coverage <- if (nzchar(premium_prefix) && startsWith(pc, premium_prefix)) substring(pc, nchar(premium_prefix) + 1L) else pc
      z$current_premium <- as.numeric(current[[pc]])
      z$proposed_premium <- as.numeric(proposed[[pc]][m])
      z
    })
    out <- .rr_rbind_fill(pieces)
  }
  out$dollar_change <- out$proposed_premium - out$current_premium
  out$percent_change <- ifelse(out$current_premium != 0, out$dollar_change / out$current_premium, NA_real_)
  attr(out, "id_cols") <- id_cols
  rownames(out) <- NULL
  out
}

#' Summarize aggregate rate change
#'
#' @param comparison Output from [compare_rating_outputs()].
#' @param by Grouping columns, typically `coverage`, `charter`, or both. Use
#'   `NULL` for one overall row.
#' @param exposure_col Optional exposure column to sum.
#'
#' @return Aggregate current/proposed premium and rate change by group.
#' @export
summarize_rate_change <- function(comparison, by = "coverage", exposure_col = NULL) {
  .rr_assert_cols(comparison, c("current_premium", "proposed_premium", by, exposure_col), "comparison")
  if (is.null(by) || !length(by)) {
    groups <- list(seq_len(nrow(comparison))); keys <- data.frame(.overall = "Overall", stringsAsFactors = FALSE)
  } else {
    gkey <- .rr_make_key(comparison, by)
    u <- unique(gkey)
    groups <- lapply(u, function(k) which(gkey == k))
    keys <- .rr_rbind_fill(lapply(groups, function(ix) comparison[ix[1], by, drop = FALSE]))
  }
  stats_rows <- lapply(groups, function(ix) {
    cur <- sum(comparison$current_premium[ix], na.rm = TRUE)
    prop <- sum(comparison$proposed_premium[ix], na.rm = TRUE)
    data.frame(record_count = length(ix), current_premium = cur, proposed_premium = prop,
               dollar_change = prop - cur,
               percent_change = if (cur != 0) (prop - cur) / cur else NA_real_,
               average_current = if (length(ix)) cur / length(ix) else NA_real_,
               average_proposed = if (length(ix)) prop / length(ix) else NA_real_,
               stringsAsFactors = FALSE)
  })
  out <- cbind(keys, do.call(rbind, stats_rows))
  if (!is.null(exposure_col)) out$exposure <- vapply(groups, function(ix) sum(comparison[[exposure_col]][ix], na.rm = TRUE), numeric(1))
  rownames(out) <- NULL
  out
}

.rr_break_label <- function(a, b) {
  f <- function(x) {
    if (is.infinite(x) && x < 0) return("-Inf")
    if (is.infinite(x) && x > 0) return("Inf")
    paste0(formatC(100 * x, format = "fg", digits = 4), "%")
  }
  paste0("[", f(a), ", ", f(b), ")")
}

#' Summarize the distribution of rate impacts
#'
#' @param comparison Output from [compare_rating_outputs()].
#' @param breaks Numeric rate-change breakpoints, including desired infinities.
#' @param by Optional additional grouping columns.
#'
#' @return Rate-change distribution by bucket and optional groups.
#' @export
rate_change_distribution <- function(comparison,
                                     breaks = c(-Inf, -0.20, -0.10, -0.05, 0, 0.05, 0.10, 0.20, Inf),
                                     by = NULL) {
  .rr_assert_cols(comparison, c("current_premium", "proposed_premium", "percent_change", by), "comparison")
  if (length(breaks) < 2L || is.unsorted(breaks, strictly = TRUE)) stop("breaks must be a strictly increasing numeric vector.", call. = FALSE)
  bucket_num <- cut(comparison$percent_change, breaks = breaks, right = FALSE, include.lowest = TRUE, labels = FALSE)
  labels <- vapply(seq_len(length(breaks) - 1L), function(i) .rr_break_label(breaks[i], breaks[i + 1L]), character(1))
  x <- comparison
  x$rate_change_bucket <- ifelse(is.na(bucket_num), "undefined", labels[bucket_num])
  group_cols <- c(by, "rate_change_bucket")
  out <- summarize_rate_change(x, by = group_cols)
  if (is.null(by) || !length(by)) {
    total <- sum(out$current_premium, na.rm = TRUE)
    n <- sum(out$record_count)
    out$current_premium_share <- if (total != 0) out$current_premium / total else NA_real_
    out$record_share <- if (n != 0) out$record_count / n else NA_real_
  } else {
    parent_key <- .rr_make_key(out, by)
    out$current_premium_share <- ave(out$current_premium, parent_key, FUN = function(v) if (sum(v) != 0) v / sum(v) else rep(NA_real_, length(v)))
    out$record_share <- ave(out$record_count, parent_key, FUN = function(v) v / sum(v))
  }
  out
}
