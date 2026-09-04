rr_test_rows <- function(charter, coverage, term_name, term_value,
                         variable1 = NA_character_, level1 = NA_character_,
                         variable2 = NA_character_, level2 = NA_character_,
                         variable3 = NA_character_, level3 = NA_character_) {
  n <- max(length(term_value), length(level1), length(level2), length(level3), 1L)
  data.frame(
    state = rep("IL", n),
    charter = rep(charter, n),
    rate_eff_date = rep(as.Date("2026-01-01"), n),
    coverage = rep(coverage, n),
    term_name = rep(term_name, n),
    term_value = rep(term_value, length.out = n),
    variable1 = rep(variable1, length.out = n),
    level1 = rep(level1, length.out = n),
    variable2 = rep(variable2, length.out = n),
    level2 = rep(level2, length.out = n),
    variable3 = rep(variable3, length.out = n),
    level3 = rep(level3, length.out = n),
    stringsAsFactors = FALSE
  )
}

rr_test_factors <- function(include_three_way = FALSE) {
  out <- list(); k <- 1L
  for (ch in c("A", "B")) {
    for (cov in c("BI", "CL")) {
      out[[k]] <- rr_test_rows(ch, cov, "base_rate",
                               if (ch == "A") if (cov == "BI") 200 else 150 else if (cov == "BI") 210 else 160)
      k <- k + 1L
      ages <- c("16", "17", "18")
      vals <- c(1.5, 1.2, 1.0) * if (cov == "BI") 1 else .98
      # Deliberately omit CL age 18 for Charter B to exercise sparse coverage-wide cells.
      if (ch == "B" && cov == "CL") { ages <- ages[1:2]; vals <- vals[1:2] }
      out[[k]] <- rr_test_rows(ch, cov, "driver_age", vals, "driver_age", ages)
      k <- k + 1L
      out[[k]] <- rr_test_rows(ch, cov, "territory", c(.9, 1.1), "territory", c("T1", "T2"))
      k <- k + 1L
      grid <- expand.grid(age = c("16", "17"), gender = c("M", "F"), stringsAsFactors = FALSE)
      out[[k]] <- rr_test_rows(ch, cov, "age_gender", c(1.10, 1.05, .98, .99),
                               "driver_age", grid$age, "gender", grid$gender)
      k <- k + 1L
      if (isTRUE(include_three_way)) {
        g3 <- expand.grid(age = c("16", "17"), gender = c("M", "F"), married = c("N", "Y"), stringsAsFactors = FALSE)
        out[[k]] <- rr_test_rows(ch, cov, "three_way", rep(1, nrow(g3)),
                                 "driver_age", g3$age, "gender", g3$gender,
                                 "married", g3$married)
        k <- k + 1L
      }
    }
  }
  x <- do.call(rbind, out)
  x$factor_row_id <- seq_len(nrow(x))
  rownames(x) <- NULL
  x
}

rr_sort_factor_rows <- function(x) {
  x$factor_row_id <- NULL
  cols <- sort(names(x))
  x <- x[cols]
  key <- do.call(paste, c(lapply(x, function(v) if (inherits(v, "Date")) format(v) else as.character(v)), sep = "|"))
  x[order(key), , drop = FALSE]
}
