#' Replace complete factor-table slices
#'
#' Replaces complete slices of a current factor table with rows from a
#' replacement table. This differs from [update_factors()], which changes only
#' explicitly supplied cells. By default a slice is identified by rate-set
#' metadata, coverage, term name, and variable-name columns; level columns are
#' intentionally excluded so omitted levels are removed.
#'
#' @param current Current normalized factor table.
#' @param replacement Replacement rows.
#' @param term_names Optional terms to take from `replacement`.
#' @param slice_cols Optional columns defining a complete factor-set slice.
#'
#' @return A normalized factor table.
#' @export
replace_factor_set <- function(current, replacement, term_names = NULL, slice_cols = NULL) {
  .rr_check_factor_table(current); .rr_check_factor_table(replacement)
  cur <- as.data.frame(current, stringsAsFactors = FALSE)
  rep <- as.data.frame(replacement, stringsAsFactors = FALSE)
  if (!is.null(term_names)) rep <- rep[as.character(rep$term_name) %in% term_names, , drop = FALSE]
  if (!nrow(rep)) stop("replacement contains no rows to apply.", call. = FALSE)

  shared_metadata <- intersect(.rr_metadata_cols(cur), .rr_metadata_cols(rep))
  shared_vars <- intersect(.rr_slot_columns(names(rep))$variable, names(cur))
  all_cols <- unique(c(names(cur), names(rep)))
  for (nm in setdiff(all_cols, names(cur))) cur[[nm]] <- NA
  for (nm in setdiff(all_cols, names(rep))) rep[[nm]] <- NA
  cur <- cur[all_cols]; rep <- rep[all_cols]

  if (is.null(slice_cols)) {
    slice_cols <- unique(c(shared_metadata, "coverage", "term_name", shared_vars))
  }
  .rr_assert_cols(cur, slice_cols, "current")
  .rr_assert_cols(rep, slice_cols, "replacement")
  replacement_keys <- unique(.rr_make_key(rep, slice_cols))
  keep <- !(.rr_make_key(cur, slice_cols) %in% replacement_keys)
  out <- rbind(cur[keep, , drop = FALSE], rep)
  rownames(out) <- NULL
  .rr_refresh_factor_ids(out)
}

#' Apply sparse factor updates
#'
#' Updates only the factor keys supplied in `updates`. New keys are rejected by
#' default so that a typo cannot silently create a rating level.
#'
#' @param current Current normalized factor table.
#' @param updates Data frame containing key columns and `term_value`.
#' @param key_cols Optional matching columns. By default all update columns other
#'   than `term_value` and `factor_row_id` are used.
#' @param allow_new_levels Logical; allow unmatched update rows to be appended.
#'
#' @return Updated normalized factor table.
#' @export
update_factors <- function(current, updates, key_cols = NULL, allow_new_levels = FALSE) {
  .rr_check_factor_table(current)
  if (!is.data.frame(updates) || !("term_value" %in% names(updates))) stop("updates must be a data frame containing term_value.", call. = FALSE)
  cur <- as.data.frame(current, stringsAsFactors = FALSE)
  upd <- as.data.frame(updates, stringsAsFactors = FALSE)
  if (is.null(key_cols)) key_cols <- setdiff(names(upd), c("term_value", "factor_row_id"))
  if (!length(key_cols)) stop("No key columns were supplied or inferred.", call. = FALSE)
  .rr_assert_cols(cur, key_cols, "current")
  .rr_assert_cols(upd, key_cols, "updates")

  cur_key <- .rr_make_key(cur, key_cols)
  upd_key <- .rr_make_key(upd, key_cols)
  if (anyDuplicated(upd_key)) stop("updates contains duplicate keys.", call. = FALSE)

  new_rows <- list(); ni <- 1L
  for (i in seq_len(nrow(upd))) {
    hits <- which(cur_key == upd_key[i])
    if (length(hits) > 1L) stop("Update row ", i, " matches multiple current factor rows; supply more key columns.", call. = FALSE)
    if (length(hits) == 1L) {
      cur$term_value[hits] <- as.numeric(upd$term_value[i])
    } else {
      if (!isTRUE(allow_new_levels)) stop("Update row ", i, " does not match an existing factor key. Set allow_new_levels = TRUE to add it.", call. = FALSE)
      row <- cur[NA_integer_, , drop = FALSE]
      for (nm in intersect(names(upd), names(cur))) row[[nm]] <- upd[[nm]][i]
      new_rows[[ni]] <- row; ni <- ni + 1L
    }
  }
  if (length(new_rows)) cur <- .rr_rbind_fill(c(list(cur), new_rows))
  .rr_refresh_factor_ids(cur)
}

#' Rebase a factor set to a selected rating level
#'
#' Divides all factors within each applicable rate-set/coverage group by the
#' factor at a selected reference level. This makes the selected level equal to
#' 1.000 while preserving all relativities. Rebasing is a presentation/basis
#' operation; it is distinct from [normalize_factor_set()], which controls a
#' weighted average level.
#'
#' @param factor_table Normalized factor table.
#' @param term_name Term to rebase.
#' @param base_level For a one-way term, a scalar level. For an interaction, a
#'   named vector or list mapping variable names to reference levels.
#' @param by Optional grouping columns. The default preserves all metadata,
#'   coverage, term name, and variable-name columns.
#'
#' @return The full factor table with the selected term rebased.
#' @export
rebase_factor_set <- function(factor_table, term_name, base_level, by = NULL) {
  .rr_check_factor_table(factor_table)
  x <- as.data.frame(factor_table, stringsAsFactors = FALSE)
  idx <- which(as.character(x$term_name) == term_name)
  if (!length(idx)) stop("term_name not found: ", term_name, call. = FALSE)
  z <- x[idx, , drop = FALSE]
  depth <- .rr_depth(z)
  if (depth < 1L) stop("A constant term has no rating level to rebase.", call. = FALSE)
  slots <- .rr_slot_columns(names(z))

  if (depth == 1L && (is.atomic(base_level) && length(base_level) == 1L) && is.null(names(base_level))) {
    variable_names <- unique(as.character(z[[slots$variable[1]]]))
    variable_names <- variable_names[!is.na(variable_names) & nzchar(variable_names)]
    if (length(variable_names) != 1L) stop("The one-way term must use a single variable name.", call. = FALSE)
    ref_levels <- stats::setNames(as.character(base_level), variable_names)
  } else {
    ref_levels <- unlist(base_level, use.names = TRUE)
    if (is.null(names(ref_levels)) || any(!nzchar(names(ref_levels)))) stop("For interactions, base_level must be named by variable.", call. = FALSE)
    ref_levels <- as.character(ref_levels)
  }

  if (is.null(by)) {
    by <- unique(c(.rr_metadata_cols(z), "coverage", "term_name", slots$variable[seq_len(depth)]))
  }
  .rr_assert_cols(z, by, "factor_table")
  gkey <- .rr_make_key(z, by)
  for (g in unique(gkey)) {
    rows <- which(gkey == g)
    candidate <- rows
    for (s in seq_len(depth)) {
      vname <- unique(as.character(z[[slots$variable[s]]][rows]))
      vname <- vname[!is.na(vname) & nzchar(vname)]
      if (length(vname) != 1L || !(vname %in% names(ref_levels))) {
        stop("base_level does not uniquely specify variable slot ", s, " in term '", term_name, "'.", call. = FALSE)
      }
      candidate <- candidate[as.character(z[[slots$level[s]]][candidate]) == ref_levels[[vname]]]
    }
    if (length(candidate) != 1L) stop("Could not identify exactly one reference factor for a rebase group in term '", term_name, "'.", call. = FALSE)
    divisor <- as.numeric(z$term_value[candidate])
    if (!is.finite(divisor) || divisor == 0) stop("Reference factor must be finite and non-zero.", call. = FALSE)
    z$term_value[rows] <- as.numeric(z$term_value[rows]) / divisor
  }
  x$term_value[idx] <- z$term_value
  .rr_refresh_factor_ids(x)
}

#' Normalize the weighted mean of a factor set
#'
#' Scales factors so that their weighted mean equals `target_mean` within each
#' grouping. Equal weights are used by default. If real portfolio exposures are
#' desired, merge or align those weights to the factor rows first and supply
#' them through `weights`.
#'
#' @param factor_table Normalized factor table.
#' @param term_name Term to normalize.
#' @param weights `NULL` for equal weights, a numeric vector aligned to the
#'   selected term rows, or the name of a numeric column in `factor_table`.
#' @param target_mean Desired weighted mean, usually 1.
#' @param by Optional grouping columns. Defaults to metadata, coverage, term
#'   name, and variable-name columns.
#'
#' @return The full factor table with the selected term normalized.
#' @export
normalize_factor_set <- function(factor_table, term_name, weights = NULL, target_mean = 1, by = NULL) {
  .rr_check_factor_table(factor_table)
  x <- as.data.frame(factor_table, stringsAsFactors = FALSE)
  idx <- which(as.character(x$term_name) == term_name)
  if (!length(idx)) stop("term_name not found: ", term_name, call. = FALSE)
  z <- x[idx, , drop = FALSE]
  vals <- as.numeric(z$term_value)
  if (any(!is.finite(vals))) stop("Selected term contains non-finite factor values.", call. = FALSE)

  if (is.null(weights)) {
    w <- rep(1, nrow(z))
  } else if (is.character(weights) && length(weights) == 1L) {
    .rr_assert_cols(z, weights, "factor_table")
    w <- as.numeric(z[[weights]])
  } else {
    w <- as.numeric(weights)
    if (length(w) != nrow(z)) stop("Numeric weights must have one value per selected term row.", call. = FALSE)
  }
  if (any(!is.finite(w)) || any(w < 0)) stop("weights must be finite and non-negative.", call. = FALSE)

  depth <- .rr_depth(z)
  slots <- .rr_slot_columns(names(z))
  if (is.null(by)) {
    metadata <- .rr_metadata_cols(z)
    if (is.character(weights) && length(weights) == 1L) metadata <- setdiff(metadata, weights)
    by <- unique(c(metadata, "coverage", "term_name", if (depth) slots$variable[seq_len(depth)] else character()))
  }
  .rr_assert_cols(z, by, "factor_table")
  gkey <- .rr_make_key(z, by)
  for (g in unique(gkey)) {
    rows <- which(gkey == g)
    if (sum(w[rows]) <= 0) stop("A normalization group has zero total weight.", call. = FALSE)
    mu <- stats::weighted.mean(vals[rows], w[rows])
    if (!is.finite(mu) || mu == 0) stop("A normalization group has a zero or non-finite weighted mean.", call. = FALSE)
    vals[rows] <- vals[rows] * target_mean / mu
  }
  x$term_value[idx] <- vals
  .rr_refresh_factor_ids(x)
}
