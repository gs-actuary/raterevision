.rr_required_cols <- c("coverage", "term_name", "term_value")

.rr_slot_columns <- function(nms) {
  vars <- grep("^variable[0-9]+$", nms, value = TRUE)
  levs <- grep("^level[0-9]+$", nms, value = TRUE)
  ord_num <- function(x, prefix) {
    if (!length(x)) return(character())
    x[order(as.integer(sub(prefix, "", x)))]
  }
  list(
    variable = ord_num(vars, "variable"),
    level = ord_num(levs, "level")
  )
}

.rr_structural_cols <- function(nms) {
  slots <- .rr_slot_columns(nms)
  unique(c("coverage", "term_name", "term_value", "factor_row_id",
           slots$variable, slots$level))
}

.rr_metadata_cols <- function(x) {
  setdiff(names(x), .rr_structural_cols(names(x)))
}

.rr_check_factor_table <- function(x, require_coverage = TRUE) {
  if (!is.data.frame(x)) stop("factor_table must be a data frame.", call. = FALSE)
  req <- c("term_name", "term_value")
  if (isTRUE(require_coverage)) req <- c("coverage", req)
  miss <- setdiff(req, names(x))
  if (length(miss)) {
    stop("factor_table is missing required column(s): ",
         paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

.rr_is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

.rr_value_key <- function(x) {
  if (inherits(x, "Date")) return(ifelse(is.na(x), "<NA>", format(x, "%Y-%m-%d")))
  if (inherits(x, "POSIXt")) return(ifelse(is.na(x), "<NA>", format(x, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")))
  y <- as.character(x)
  y[is.na(x)] <- "<NA>"
  paste0(nchar(y), ":", y)
}

.rr_make_key <- function(df, cols) {
  if (!length(cols)) return(rep("<ALL>", nrow(df)))
  if (!nrow(df)) return(character())
  parts <- lapply(df[cols], .rr_value_key)
  do.call(paste, c(parts, sep = "\n"))
}

.rr_order_key <- function(x) {
  if (inherits(x, "Date") || is.numeric(x) || is.integer(x)) return(x)
  if (is.factor(x) && is.ordered(x)) return(as.integer(x))
  ch <- as.character(x)
  nonblank <- !is.na(ch) & nzchar(trimws(ch))
  nums <- suppressWarnings(as.numeric(ch[nonblank]))
  if (any(nonblank) && all(!is.na(nums))) {
    out <- rep(NA_real_, length(ch))
    out[nonblank] <- nums
    return(out)
  }
  ch
}

.rr_order_df <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols) || nrow(df) < 2L) return(df)
  keys <- lapply(df[cols], .rr_order_key)
  o <- do.call(order, c(keys, list(na.last = TRUE)))
  df[o, , drop = FALSE]
}

.rr_rbind_fill <- function(xs) {
  xs <- Filter(function(z) !is.null(z) && nrow(z) >= 0L, xs)
  if (!length(xs)) return(data.frame())
  all_names <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(z) {
    missing <- setdiff(all_names, names(z))
    for (nm in missing) z[[nm]] <- NA
    z[all_names]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

.rr_apply_filters <- function(x, filters) {
  if (is.null(filters) || !length(filters)) return(x)
  if (!is.list(filters) || is.null(names(filters)) || any(!nzchar(names(filters)))) {
    stop("filters must be a named list.", call. = FALSE)
  }
  miss <- setdiff(names(filters), names(x))
  if (length(miss)) stop("Unknown filter column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  keep <- rep(TRUE, nrow(x))
  for (nm in names(filters)) {
    vals <- filters[[nm]]
    col <- x[[nm]]
    if (inherits(col, "Date") && !inherits(vals, "Date")) vals <- as.Date(vals)
    keep <- keep & (col %in% vals)
  }
  x[keep, , drop = FALSE]
}

.rr_depth <- function(df) {
  slots <- .rr_slot_columns(names(df))
  n <- min(length(slots$variable), length(slots$level))
  if (!n || !nrow(df)) return(0L)
  present <- vapply(seq_len(n), function(i) {
    v <- df[[slots$variable[i]]]
    l <- df[[slots$level[i]]]
    any(!.rr_is_blank(v) | !.rr_is_blank(l))
  }, logical(1))
  if (!any(present)) 0L else max(which(present))
}

.rr_safe_sheet_name <- function(x, used = character()) {
  x <- as.character(x)
  for (bad in c("\\", "/", ":", "*", "?", "[", "]")) x <- gsub(bad, "_", x, fixed = TRUE)
  x <- trimws(x)
  if (!nzchar(x)) x <- "rates"
  x <- substr(x, 1L, 31L)
  base <- x
  i <- 1L
  while (x %in% used || x %in% c("_README", "_raterevision")) {
    i <- i + 1L
    suffix <- paste0("_", i)
    x <- paste0(substr(base, 1L, 31L - nchar(suffix)), suffix)
  }
  x
}

.rr_pivot_coverage <- function(df, id_cols) {
  key_all <- .rr_make_key(df, c(id_cols, "coverage"))
  if (anyDuplicated(key_all)) {
    stop("Duplicate factor keys prevent widening. Validate the factor table first.", call. = FALSE)
  }
  ids <- unique(df[id_cols])
  id_key <- .rr_make_key(ids, id_cols)
  # Preserve first appearance so a rating-plan coverage order survives export.
  covs <- unique(as.character(df$coverage))
  out <- ids
  for (cov in covs) {
    vals <- rep(NA_real_, nrow(ids))
    z <- df[as.character(df$coverage) == cov, , drop = FALSE]
    m <- match(.rr_make_key(z, id_cols), id_key)
    vals[m] <- as.numeric(z$term_value)
    out[[cov]] <- vals
  }
  out
}

.rr_encode_cols <- function(x) paste(x, collapse = "|")
.rr_decode_cols <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) return(character())
  strsplit(as.character(x), "|", fixed = TRUE)[[1]]
}

.rr_refresh_factor_ids <- function(x) {
  if ("factor_row_id" %in% names(x)) x$factor_row_id <- NULL
  x$factor_row_id <- seq_len(nrow(x))
  x
}

.rr_pkg_version <- function() {
  tryCatch(as.character(utils::packageVersion("raterevision")), error = function(e) "development")
}

.rr_assert_cols <- function(x, cols, object = "data") {
  miss <- setdiff(cols, names(x))
  if (length(miss)) stop(object, " is missing column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  invisible(TRUE)
}
