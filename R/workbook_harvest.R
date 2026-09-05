#' Profile an Excel workbook for candidate rating tables
#'
#' Scans an arbitrary `.xlsx` workbook and identifies rectangular regions that
#' look like rating tables. The profiler is deliberately heuristic: it uses
#' connected nonblank regions, header structure, numeric density, and nearby
#' labels to make educated guesses. The returned profile is intended to
#' be reviewed and, when necessary, edited before extraction.
#'
#' @param file Path to an `.xlsx`, `.xlsm`, or `.xlsb` workbook.
#' @param sheets Optional character vector of sheet names to scan. By default all
#'   sheets are scanned except sheets beginning with an underscore.
#' @param min_cells Minimum number of nonblank cells required for a candidate.
#' @param min_rows Minimum number of occupied rows required for a candidate.
#' @param min_cols Minimum number of occupied columns required for a candidate.
#' @param include_threshold Confidence threshold used to populate the editable
#'   `include` column. Candidates below the threshold remain in the profile.
#' @param title_lookback Number of rows above a candidate searched for a nearby
#'   descriptive title.
#'
#' @return A data frame with class `raterevision_workbook_profile`. Each row is a
#'   candidate table. The `include`, `table_name`, `extract_range`, and
#'   `header_row` fields may be edited before calling [extract_rate_tables()].
#' @export
profile_rate_workbook <- function(file, sheets = NULL, min_cells = 3L,
                                  min_rows = 1L, min_cols = 2L,
                                  include_threshold = 0.50,
                                  title_lookback = 3L) {
  if (missing(file) || length(file) != 1L || !nzchar(file)) {
    stop("file must be a single path.", call. = FALSE)
  }
  if (!file.exists(file)) stop("File does not exist: ", file, call. = FALSE)
  if (!grepl("\\.(xlsx|xlsm|xlsb)$", file, ignore.case = TRUE)) {
    stop("file must be an .xlsx, .xlsm, or .xlsb workbook.", call. = FALSE)
  }

  min_cells <- .rr_positive_integer(min_cells, "min_cells")
  min_rows <- .rr_positive_integer(min_rows, "min_rows")
  min_cols <- .rr_positive_integer(min_cols, "min_cols")
  title_lookback <- .rr_nonnegative_integer(title_lookback, "title_lookback")
  if (!is.numeric(include_threshold) || length(include_threshold) != 1L ||
      is.na(include_threshold) || include_threshold < 0 || include_threshold > 1) {
    stop("include_threshold must be a number between 0 and 1.", call. = FALSE)
  }

  wb <- openxlsx2::wb_load(file)
  all_sheets <- unname(openxlsx2::wb_get_sheet_names(wb))
  if (is.null(sheets)) {
    sheets <- all_sheets[!startsWith(all_sheets, "_")]
  } else {
    sheets <- as.character(sheets)
    missing_sheets <- setdiff(sheets, all_sheets)
    if (length(missing_sheets)) {
      stop("Unknown workbook sheet(s): ", paste(missing_sheets, collapse = ", "), ".",
           call. = FALSE)
    }
  }

  rows <- list()
  ri <- 1L
  read_errors <- character()

  for (sheet in sheets) {
    read_error <- NULL
    raw <- tryCatch(
      openxlsx2::wb_to_df(
        wb,
        sheet = sheet,
        col_names = FALSE,
        skip_empty_rows = FALSE,
        skip_empty_cols = FALSE,
        detect_dates = FALSE,
        check_names = FALSE
      ),
      error = function(e) {
        read_error <<- conditionMessage(e)
        NULL
      }
    )

    if (is.null(raw)) {
      read_errors[sheet] <- if (is.null(read_error)) "unknown read error" else read_error
      next
    }
    if (!nrow(raw) || !ncol(raw)) next

    info <- .rr_sheet_matrix(raw)
    blocks <- .rr_candidate_blocks(info$values)
    if (!length(blocks)) next

    for (block in blocks) {
      nr <- length(block$rows)
      nc <- length(block$cols)
      sub <- info$values[block$rows, block$cols, drop = FALSE]
      nonblank <- sum(!.rr_blank_matrix(sub))
      if (nonblank < min_cells || nr < min_rows || nc < min_cols) next

      metrics <- .rr_score_candidate(sub)
      start_row <- info$excel_rows[min(block$rows)]
      end_row <- info$excel_rows[max(block$rows)]
      start_col <- info$excel_cols[min(block$cols)]
      end_col <- info$excel_cols[max(block$cols)]

      header_row <- if (is.na(metrics$header_rel)) {
        NA_integer_
      } else {
        info$excel_rows[min(block$rows) + metrics$header_rel - 1L]
      }

      title <- .rr_guess_candidate_title(
        values = info$values,
        row_index = min(block$rows),
        col_indices = block$cols,
        within_block_rows = if (is.na(metrics$header_rel)) integer() else
          block$rows[block$rows < (min(block$rows) + metrics$header_rel - 1L)],
        lookback = title_lookback
      )

      table_name <- .rr_clean_table_name(title)
      if (!nzchar(table_name)) table_name <- .rr_clean_table_name(sheet)
      if (!nzchar(table_name)) table_name <- "rate_table"

      extract_start <- if (is.na(header_row)) start_row else header_row
      source_range <- .rr_excel_range(start_row, start_col, end_row, end_col)
      extract_range <- .rr_excel_range(extract_start, start_col, end_row, end_col)

      score <- metrics$confidence
      rows[[ri]] <- data.frame(
        candidate_id = NA_character_,
        source_sheet = sheet,
        source_range = source_range,
        extract_range = extract_range,
        start_row = start_row,
        end_row = end_row,
        start_col = start_col,
        end_col = end_col,
        header_row = header_row,
        table_name = table_name,
        n_rows = nr,
        n_cols = nc,
        nonblank_cells = nonblank,
        density = metrics$density,
        numeric_share = metrics$numeric_share,
        body_numeric_share = metrics$body_numeric_share,
        header_score = metrics$header_score,
        title_guess = title,
        confidence = score,
        confidence_label = .rr_confidence_label(score),
        include = score >= include_threshold,
        notes = metrics$notes,
        stringsAsFactors = FALSE
      )
      ri <- ri + 1L
    }
  }

  out <- .rr_rbind_fill(rows)
  if (!nrow(out)) {
    out <- data.frame(
      candidate_id = character(), source_sheet = character(),
      source_range = character(), extract_range = character(),
      start_row = integer(), end_row = integer(), start_col = integer(),
      end_col = integer(), header_row = integer(), table_name = character(),
      n_rows = integer(), n_cols = integer(), nonblank_cells = integer(),
      density = numeric(), numeric_share = numeric(), body_numeric_share = numeric(),
      header_score = numeric(), title_guess = character(), confidence = numeric(),
      confidence_label = character(), include = logical(), notes = character(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- out[order(match(out$source_sheet, sheets), out$start_row, out$start_col), , drop = FALSE]
    out$candidate_id <- sprintf("candidate_%03d", seq_len(nrow(out)))
    out$table_name <- .rr_unique_table_names(out$table_name)
    rownames(out) <- NULL
  }

  attr(out, "file") <- normalizePath(file, winslash = "/", mustWork = FALSE)
  attr(out, "sheets_scanned") <- sheets
  attr(out, "include_threshold") <- include_threshold
  attr(out, "read_errors") <- read_errors
  class(out) <- c("raterevision_workbook_profile", "data.frame")
  out
}

#' @export
print.raterevision_workbook_profile <- function(x, ...) {
  cat("<raterevision_workbook_profile>\n")
  cat(" candidates:", nrow(x), "\n")
  if (nrow(x)) {
    cat(" included:", sum(x$include, na.rm = TRUE), "\n")
    tab <- table(factor(x$confidence_label, levels = c("high", "medium", "low")))
    cat(" confidence: high", unname(tab["high"]),
        "| medium", unname(tab["medium"]),
        "| low", unname(tab["low"]), "\n")
  }
  errors <- attr(x, "read_errors")
  if (length(errors)) cat(" unreadable sheets:", length(errors), "\n")
  file <- attr(x, "file")
  if (!is.null(file)) cat(" source:", file, "\n")
  invisible(x)
}

#' Extract candidate rate tables from a profiled workbook
#'
#' Reads the candidate regions selected in a [profile_rate_workbook()] result and
#' returns a named list of rectangular data frames. The object intentionally uses
#' the same list-oriented interface as [rate_tables_wide()], while retaining
#' source-sheet/range metadata so that a migration can be reviewed and refined.
#'
#' This function does not claim that the extracted rectangles are already valid
#' normalized `ratingtables` factor tables. It is an ingestion step: uncertain
#' candidates should be reviewed, renamed, resized, or excluded in the profile
#' before extraction.
#'
#' @param profile A `raterevision_workbook_profile` returned by
#'   [profile_rate_workbook()]. The profile may be edited before extraction.
#' @param file Optional workbook path. By default the source path stored on the
#'   profile is used.
#' @param only_included Logical; when `TRUE`, extract only rows where `include`
#'   is `TRUE`.
#' @param min_confidence Optional additional confidence threshold. Set to `NULL`
#'   to rely only on the editable `include` column.
#'
#' @return A named list of rectangular data frames with class
#'   `raterevision_tables`. A manifest is stored in the `manifest` attribute.
#' @export
extract_rate_tables <- function(profile, file = attr(profile, "file"),
                                only_included = TRUE, min_confidence = NULL) {
  if (!inherits(profile, "raterevision_workbook_profile") && !is.data.frame(profile)) {
    stop("profile must be a workbook profile returned by profile_rate_workbook().",
         call. = FALSE)
  }
  .rr_assert_cols(
    profile,
    c("candidate_id", "source_sheet", "extract_range", "header_row",
      "table_name", "confidence", "confidence_label", "include"),
    "profile"
  )
  if (is.null(file) || length(file) != 1L || !nzchar(file) || !file.exists(file)) {
    stop("A readable source workbook path is required.", call. = FALSE)
  }
  if (!is.null(min_confidence)) {
    if (!is.numeric(min_confidence) || length(min_confidence) != 1L ||
        is.na(min_confidence) || min_confidence < 0 || min_confidence > 1) {
      stop("min_confidence must be NULL or a number between 0 and 1.", call. = FALSE)
    }
  }

  keep <- rep(TRUE, nrow(profile))
  if (isTRUE(only_included)) keep <- keep & !is.na(profile$include) & profile$include
  if (!is.null(min_confidence)) keep <- keep & profile$confidence >= min_confidence
  p <- profile[keep, , drop = FALSE]
  if (!nrow(p)) stop("No candidate tables are selected for extraction.", call. = FALSE)

  wb <- openxlsx2::wb_load(file)
  wb_sheets <- unname(openxlsx2::wb_get_sheet_names(wb))
  missing_sheets <- setdiff(unique(as.character(p$source_sheet)), wb_sheets)
  if (length(missing_sheets)) {
    stop("Workbook is missing profiled sheet(s): ", paste(missing_sheets, collapse = ", "), ".",
         call. = FALSE)
  }

  table_names <- .rr_unique_table_names(as.character(p$table_name))
  tabs <- vector("list", nrow(p))
  names(tabs) <- table_names
  manifest <- vector("list", nrow(p))

  for (i in seq_len(nrow(p))) {
    raw <- openxlsx2::wb_to_df(
      wb,
      sheet = as.character(p$source_sheet[i]),
      dims = as.character(p$extract_range[i]),
      col_names = FALSE,
      skip_empty_rows = FALSE,
      skip_empty_cols = FALSE,
      detect_dates = FALSE,
      check_names = FALSE
    )
    raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
    if (!nrow(raw) || !ncol(raw)) {
      stop("Candidate '", p$candidate_id[i], "' extracted an empty range.", call. = FALSE)
    }

    has_header <- !is.na(p$header_row[i])
    if (has_header) {
      range_info <- .rr_sheet_matrix(raw)
      hpos <- match(as.integer(p$header_row[i]), range_info$excel_rows)
      if (is.na(hpos)) {
        stop(
          "Candidate '", p$candidate_id[i], "' has header_row ", p$header_row[i],
          " outside extract_range '", p$extract_range[i], "'.",
          call. = FALSE
        )
      }
      headers <- .rr_clean_headers(unlist(raw[hpos, , drop = TRUE], use.names = FALSE))
      tab <- if (hpos < nrow(raw)) raw[(hpos + 1L):nrow(raw), , drop = FALSE] else raw[0, , drop = FALSE]
      names(tab) <- headers
    } else {
      tab <- raw
      names(tab) <- paste0("column_", seq_len(ncol(tab)))
    }

    tab <- .rr_trim_extracted_table(tab)
    if (!nrow(tab)) {
      stop("Candidate '", p$candidate_id[i], "' contains no data rows after header handling.",
           call. = FALSE)
    }
    attr(tab, "source_sheet") <- as.character(p$source_sheet[i])
    attr(tab, "source_range") <- as.character(p$extract_range[i])
    attr(tab, "candidate_id") <- as.character(p$candidate_id[i])
    attr(tab, "confidence") <- as.numeric(p$confidence[i])
    tabs[[i]] <- tab

    manifest[[i]] <- data.frame(
      sheet_name = table_names[i],
      term_name = table_names[i],
      layout = "raw_extracted",
      coverage = "",
      depth = NA_integer_,
      metadata_cols = "",
      source_sheet = as.character(p$source_sheet[i]),
      source_range = as.character(p$extract_range[i]),
      candidate_id = as.character(p$candidate_id[i]),
      confidence = as.numeric(p$confidence[i]),
      confidence_label = as.character(p$confidence_label[i]),
      stringsAsFactors = FALSE
    )
  }

  attr(tabs, "manifest") <- do.call(rbind, manifest)
  attr(tabs, "schema_version") <- "harvest-1"
  attr(tabs, "source_file") <- normalizePath(file, winslash = "/", mustWork = FALSE)
  class(tabs) <- c("raterevision_extracted_tables", "raterevision_tables", "list")
  tabs
}

#' @export
print.raterevision_extracted_tables <- function(x, ...) {
  manifest <- attr(x, "manifest")
  cat("<raterevision_extracted_tables>\n")
  cat(" tables:", length(x), "\n")
  if (!is.null(manifest) && nrow(manifest)) {
    cat(" source sheets:", length(unique(manifest$source_sheet)), "\n")
    cat(" median confidence:",
        format(round(stats::median(manifest$confidence, na.rm = TRUE), 2), nsmall = 2),
        "\n")
  }
  cat(" raw rectangles: review/normalize before rate_tables_long()\n")
  invisible(x)
}

.rr_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != as.integer(x)) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}

.rr_nonnegative_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x != as.integer(x)) {
    stop(name, " must be a nonnegative integer.", call. = FALSE)
  }
  as.integer(x)
}

.rr_sheet_matrix <- function(raw) {
  values <- as.matrix(data.frame(lapply(raw, function(z) as.character(z)),
                                 stringsAsFactors = FALSE, check.names = FALSE))
  excel_rows <- suppressWarnings(as.integer(rownames(raw)))
  if (length(excel_rows) != nrow(raw) || anyNA(excel_rows)) excel_rows <- seq_len(nrow(raw))
  excel_cols <- vapply(names(raw), .rr_excel_col_number, integer(1))
  if (length(excel_cols) != ncol(raw) || anyNA(excel_cols)) excel_cols <- seq_len(ncol(raw))
  list(values = values, excel_rows = excel_rows, excel_cols = excel_cols)
}

.rr_excel_col_number <- function(x) {
  x <- toupper(as.character(x))
  if (!grepl("^[A-Z]+$", x)) return(NA_integer_)
  chars <- utf8ToInt(x) - utf8ToInt("A") + 1L
  out <- 0L
  for (v in chars) out <- out * 26L + v
  out
}

.rr_excel_col_letter <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1L) return(NA_character_)
  out <- character()
  while (n > 0L) {
    r <- (n - 1L) %% 26L
    out <- c(intToUtf8(utf8ToInt("A") + r), out)
    n <- (n - 1L) %/% 26L
  }
  paste0(out, collapse = "")
}

.rr_excel_range <- function(start_row, start_col, end_row, end_col) {
  paste0(.rr_excel_col_letter(start_col), start_row, ":",
         .rr_excel_col_letter(end_col), end_row)
}

.rr_blank_matrix <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

.rr_numeric_matrix <- function(x) {
  y <- trimws(as.character(x))
  y <- gsub(",", "", y, fixed = TRUE)
  y <- gsub("$", "", y, fixed = TRUE)
  pct <- grepl("%$", y)
  y[pct] <- sub("%$", "", y[pct])
  paren <- grepl("^\\(.*\\)$", y)
  y[paren] <- paste0("-", sub("^\\((.*)\\)$", "\\1", y[paren]))
  num <- suppressWarnings(as.numeric(y))
  !is.na(num) & !.rr_blank_matrix(x)
}

.rr_candidate_blocks <- function(values) {
  occupied <- !.rr_blank_matrix(values)
  nr <- nrow(occupied); nc <- ncol(occupied)
  if (!nr || !nc || !any(occupied)) return(list())

  visited <- matrix(FALSE, nrow = nr, ncol = nc)
  starts <- which(occupied)
  queue <- integer(length(starts))
  out <- list(); k <- 1L

  for (start in starts) {
    if (visited[start]) next

    head <- 1L
    tail <- 1L
    queue[1L] <- start
    visited[start] <- TRUE

    while (head <= tail) {
      idx <- queue[head]
      head <- head + 1L

      r <- ((idx - 1L) %% nr) + 1L
      c <- ((idx - 1L) %/% nr) + 1L
      neighbors <- integer()
      if (r > 1L) neighbors <- c(neighbors, idx - 1L)
      if (r < nr) neighbors <- c(neighbors, idx + 1L)
      if (c > 1L) neighbors <- c(neighbors, idx - nr)
      if (c < nc) neighbors <- c(neighbors, idx + nr)

      for (nidx in neighbors) {
        if (occupied[nidx] && !visited[nidx]) {
          visited[nidx] <- TRUE
          tail <- tail + 1L
          queue[tail] <- nidx
        }
      }
    }

    comp <- queue[seq_len(tail)]
    rr <- ((comp - 1L) %% nr) + 1L
    cc <- ((comp - 1L) %/% nr) + 1L
    out[[k]] <- list(rows = seq.int(min(rr), max(rr)),
                     cols = seq.int(min(cc), max(cc)))
    k <- k + 1L
  }

  out
}

.rr_score_candidate <- function(x) {
  blank <- .rr_blank_matrix(x)
  numeric <- .rr_numeric_matrix(x)
  nonblank_n <- sum(!blank)
  density <- nonblank_n / length(x)
  numeric_share <- if (nonblank_n) sum(numeric) / nonblank_n else 0

  nr <- nrow(x); nc <- ncol(x)
  header_rel <- NA_integer_
  header_score <- 0
  body_numeric_share <- numeric_share

  if (nr >= 2L) {
    candidates <- seq_len(min(3L, nr - 1L))
    scores <- numeric(length(candidates))
    body_shares <- numeric(length(candidates))
    for (j in seq_along(candidates)) {
      r <- candidates[j]
      hb <- !blank[r, ]
      h_n <- sum(hb)
      h_text <- sum(hb & !numeric[r, ])
      header_text_share <- if (h_n) h_text / h_n else 0
      header_numeric_share <- if (h_n) sum(hb & numeric[r, ]) / h_n else 0
      body <- x[(r + 1L):nr, , drop = FALSE]
      body_blank <- .rr_blank_matrix(body)
      body_num <- .rr_numeric_matrix(body)
      body_n <- sum(!body_blank)
      body_share <- if (body_n) sum(body_num) / body_n else 0
      body_shares[j] <- body_share
      header_fill <- h_n / nc
      numeric_contrast <- max(0, body_share - header_numeric_share)
      scores[j] <- 0.35 * header_text_share +
        0.30 * body_share +
        0.25 * numeric_contrast +
        0.10 * header_fill
    }
    j <- which.max(scores)
    header_score <- scores[j]
    body_numeric_share <- body_shares[j]
    if (header_score >= 0.62) header_rel <- candidates[j]
  }

  label_score <- 0
  if (nc >= 2L) {
    first <- x[, 1L]
    fb <- !.rr_blank_matrix(first)
    if (any(fb)) label_score <- sum(fb & !.rr_numeric_matrix(first)) / sum(fb)
  }

  size_score <- min(1, log1p(nonblank_n) / log(30))
  density_score <- max(0, min(1, (density - 0.15) / 0.85))
  score <- 0.18 * density_score +
    0.34 * body_numeric_share +
    0.22 * header_score +
    0.10 * label_score +
    0.16 * size_score
  score <- max(0, min(1, score))

  notes <- character()
  if (is.na(header_rel)) notes <- c(notes, "header uncertain")
  if (body_numeric_share < 0.40) notes <- c(notes, "low numeric body share")
  if (density < 0.50) notes <- c(notes, "sparse rectangle")
  if (!length(notes)) notes <- ""

  list(
    confidence = score,
    density = density,
    numeric_share = numeric_share,
    body_numeric_share = body_numeric_share,
    header_score = header_score,
    header_rel = header_rel,
    notes = paste(notes, collapse = "; ")
  )
}

.rr_guess_candidate_title <- function(values, row_index, col_indices,
                                      within_block_rows = integer(), lookback = 3L) {
  candidate_rows <- integer()
  if (length(within_block_rows)) candidate_rows <- c(candidate_rows, rev(within_block_rows))
  if (lookback > 0L && row_index > 1L) {
    lo <- max(1L, row_index - lookback)
    candidate_rows <- c(candidate_rows, rev(seq.int(lo, row_index - 1L)))
  }
  candidate_rows <- unique(candidate_rows)

  for (r in candidate_rows) {
    vals <- values[r, col_indices, drop = TRUE]
    vals <- vals[!.rr_blank_matrix(vals)]
    if (!length(vals)) next
    num <- .rr_numeric_matrix(vals)
    if (sum(num) / length(vals) > 0.25) next
    text <- vals[!num]
    if (!length(text)) next
    text <- trimws(as.character(text))
    text <- text[nzchar(text)]
    if (!length(text)) next
    label <- paste(text, collapse = " ")
    if (nchar(label) <= 100L) return(label)
  }
  ""
}

.rr_clean_table_name <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1])) return("")
  x <- trimws(as.character(x[1]))
  if (!nzchar(x)) return("")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  substr(x, 1L, 60L)
}

.rr_unique_table_names <- function(x) {
  x <- vapply(x, .rr_clean_table_name, character(1))
  x[!nzchar(x)] <- "rate_table"
  make.unique(x, sep = "_")
}

.rr_clean_headers <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  blank <- !nzchar(x)
  x[blank] <- paste0("column_", which(blank))
  make.unique(x, sep = "_")
}

.rr_trim_extracted_table <- function(x) {
  if (!nrow(x) || !ncol(x)) return(x)
  row_keep <- rowSums(!.rr_blank_matrix(as.matrix(data.frame(lapply(x, as.character),
                                                           check.names = FALSE)))) > 0L
  col_keep <- vapply(x, function(z) any(!.rr_blank_matrix(z)), logical(1))
  x <- x[row_keep, col_keep, drop = FALSE]
  rownames(x) <- NULL
  x
}

.rr_confidence_label <- function(x) {
  ifelse(x >= 0.75, "high", ifelse(x >= 0.50, "medium", "low"))
}
