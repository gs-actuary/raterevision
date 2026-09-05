#' Diagnose differences between a reference rater and a candidate rater
#'
#' Uses grouped discrepancy scans and lightweight statistical models to identify
#' rating variables associated with missing or incorrect candidate premiums. The
#' models are diagnostic tools, not production predictive models: no tuning grid
#' or cross-validation is performed, and the default print method reports only
#' high-level findings.
#'
#' The input is normally created by [compare_rating_outputs()], with all useful
#' rating variables carried through via its `keep_cols` argument. In a migration
#' workflow, `current_premium` should represent the trusted/reference premium and
#' `proposed_premium` the candidate `ratingtables` premium.
#'
#' @param comparison Output from [compare_rating_outputs()].
#' @param predictors Optional character vector of columns to investigate. By
#'   default all non-ID, non-premium columns are used, including `coverage`.
#' @param absolute_tolerance Dollar tolerance below which a premium difference is
#'   treated as a match.
#' @param relative_tolerance Relative tolerance applied to the absolute reference
#'   premium. A row is wrong only when its absolute error exceeds both tolerances.
#' @param top_n Number of variables retained in the high-level suspect table.
#' @param model_sample Maximum number of rows used by the background models.
#' @param max_model_levels Maximum factor levels retained in a background model;
#'   rarer levels are collapsed to `<OTHER>`. Numeric variables with many unique
#'   values are quantile-binned for diagnostic modeling.
#' @param interaction_top Number of leading variables considered for pairwise
#'   interaction diagnostics. Set to `0` to skip interaction models.
#' @param seed Random seed used only when down-sampling for background models.
#'
#' @return An object of class `raterevision_rater_diagnosis` containing summary
#'   counts, variable/level diagnostics, model signals, interaction signals, and
#'   the fitted lightweight models for advanced inspection.
#' @export
diagnose_rating_output <- function(comparison, predictors = NULL,
                                   absolute_tolerance = 0.01,
                                   relative_tolerance = 1e-06,
                                   top_n = 5L,
                                   model_sample = 50000L,
                                   max_model_levels = 40L,
                                   interaction_top = 4L,
                                   seed = 1L) {
  if (!is.data.frame(comparison)) stop("comparison must be a data frame.", call. = FALSE)
  .rr_assert_cols(comparison, c("current_premium", "proposed_premium"), "comparison")
  
  absolute_tolerance <- .rr_nonnegative_number(absolute_tolerance, "absolute_tolerance")
  relative_tolerance <- .rr_nonnegative_number(relative_tolerance, "relative_tolerance")
  top_n <- .rr_diag_nonnegative_integer(top_n, "top_n")
  model_sample <- .rr_diag_positive_integer(model_sample, "model_sample")
  max_model_levels <- .rr_diag_positive_integer(max_model_levels, "max_model_levels")
  interaction_top <- .rr_diag_nonnegative_integer(interaction_top, "interaction_top")
  
  x <- as.data.frame(comparison, stringsAsFactors = FALSE)
  cur <- suppressWarnings(as.numeric(x$current_premium))
  prop <- suppressWarnings(as.numeric(x$proposed_premium))
  truth_present <- !is.na(cur)
  if (!any(truth_present)) {
    stop("comparison contains no non-missing current_premium reference values.", call. = FALSE)
  }
  candidate_missing <- truth_present & is.na(prop)
  both_present <- truth_present & !is.na(prop)
  signed_error <- prop - cur
  abs_error <- abs(signed_error)
  tolerance <- pmax(absolute_tolerance, relative_tolerance * abs(cur))
  wrong <- both_present & abs_error > tolerance
  material_mismatch <- candidate_missing | wrong
  matched <- truth_present & !material_mismatch
  
  x$.rr_missing <- candidate_missing
  x$.rr_wrong <- wrong
  x$.rr_mismatch <- material_mismatch
  x$.rr_signed_error <- signed_error
  x$.rr_abs_error <- abs_error
  
  if (is.null(predictors)) {
    reserved <- unique(c(
      attr(comparison, "id_cols"),
      "current_premium", "proposed_premium", "dollar_change", "percent_change",
      ".rr_missing", ".rr_wrong", ".rr_mismatch", ".rr_signed_error", ".rr_abs_error"
    ))
    predictors <- setdiff(names(x), reserved)
  } else {
    predictors <- unique(as.character(predictors))
    .rr_assert_cols(x, predictors, "comparison")
  }
  
  predictors <- predictors[vapply(x[predictors], .rr_diag_usable_predictor, logical(1))]
  if (!length(predictors)) {
    stop("No usable predictor columns are available. Carry rating variables into compare_rating_outputs() with keep_cols, or supply predictors explicitly.",
         call. = FALSE)
  }
  
  summary <- data.frame(
    comparison_rows = nrow(x),
    reference_premium_present = sum(truth_present),
    matched = sum(matched),
    candidate_missing = sum(candidate_missing),
    material_wrong = sum(wrong),
    total_mismatch = sum(material_mismatch),
    match_rate = if (sum(truth_present)) sum(matched) / sum(truth_present) else NA_real_,
    missing_rate = if (sum(truth_present)) sum(candidate_missing) / sum(truth_present) else NA_real_,
    wrong_rate = if (sum(both_present)) sum(wrong) / sum(both_present) else NA_real_,
    mean_absolute_error = if (any(both_present)) mean(abs_error[both_present], na.rm = TRUE) else NA_real_,
    mean_signed_error = if (any(both_present)) mean(signed_error[both_present], na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
  
  level_rows <- list()
  variable_rows <- list()
  li <- 1L; vi <- 1L
  overall_missing <- summary$missing_rate
  overall_mismatch <- if (sum(truth_present)) sum(material_mismatch) / sum(truth_present) else NA_real_
  overall_mae <- summary$mean_absolute_error
  
  for (v in predictors) {
    g <- .rr_diag_group_values(x[[v]])
    keys <- unique(g)
    groups <- lapply(keys, function(k) which(g == k))
    lev <- do.call(rbind, lapply(seq_along(groups), function(j) {
      ix <- groups[[j]]
      tp <- truth_present[ix]
      bp <- both_present[ix]
      data.frame(
        variable = v,
        level = keys[j],
        n = length(ix),
        reference_n = sum(tp),
        missing_n = sum(candidate_missing[ix]),
        mismatch_n = sum(material_mismatch[ix]),
        missing_rate = if (sum(tp)) sum(candidate_missing[ix]) / sum(tp) else NA_real_,
        mismatch_rate = if (sum(tp)) sum(material_mismatch[ix]) / sum(tp) else NA_real_,
        mean_signed_error = if (any(bp)) mean(signed_error[ix][bp], na.rm = TRUE) else NA_real_,
        mean_abs_error = if (any(bp)) mean(abs_error[ix][bp], na.rm = TRUE) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    rownames(lev) <- NULL
    level_rows[[li]] <- lev; li <- li + 1L
    
    eligible <- lev$reference_n >= max(5L, ceiling(0.0025 * max(1L, sum(truth_present))))
    if (!any(eligible)) eligible <- lev$reference_n > 0L
    e <- lev[eligible, , drop = FALSE]
    
    max_missing_lift <- .rr_safe_max(e$missing_rate - overall_missing, default = 0)
    max_mismatch_lift <- .rr_safe_max(e$mismatch_rate - overall_mismatch, default = 0)
    magnitude_ratio <- if (!is.na(overall_mae) && overall_mae > 0) {
      .rr_safe_max(e$mean_abs_error / overall_mae, default = 1)
    } else 1
    magnitude_signal <- max(0, min(1, (magnitude_ratio - 1) / 3))
    simple_score <- max(c(max_missing_lift, max_mismatch_lift, magnitude_signal), na.rm = TRUE)
    
    variable_rows[[vi]] <- data.frame(
      variable = v,
      levels = length(keys),
      max_missing_lift = max_missing_lift,
      max_mismatch_lift = max_mismatch_lift,
      max_error_magnitude_ratio = magnitude_ratio,
      simple_score = simple_score,
      stringsAsFactors = FALSE
    )
    vi <- vi + 1L
  }
  
  level_details <- do.call(rbind, level_rows)
  variable_summary <- do.call(rbind, variable_rows)
  rownames(level_details) <- NULL
  rownames(variable_summary) <- NULL
  
  model_index <- seq_len(nrow(x))
  if (length(model_index) > model_sample) {
    set.seed(seed)
    model_index <- sort(sample(model_index, model_sample))
  }
  model_x <- x[model_index, , drop = FALSE]
  model_variables <- variable_summary$variable[
    order(-variable_summary$simple_score, variable_summary$variable)
  ]
  model_variables <- utils::head(model_variables, 20L)
  prepared <- lapply(model_variables, function(v) .rr_diag_model_predictor(model_x[[v]], max_model_levels))
  names(prepared) <- model_variables
  
  model_signals <- list(); fits <- list(); mi <- 1L
  for (v in model_variables) {
    px <- prepared[[v]]
    if (length(unique(px[!is.na(px)])) < 2L) next
    
    missing_rows <- !is.na(model_x$current_premium)
    miss_fit <- .rr_diag_binary_model(model_x$.rr_missing[missing_rows], px[missing_rows])
    if (!is.null(miss_fit)) {
      model_signals[[mi]] <- data.frame(variable = v, outcome = "missing",
                                        signal = miss_fit$signal, stringsAsFactors = FALSE)
      fits[[paste0("missing__", v)]] <- miss_fit$fit
      mi <- mi + 1L
    }
    
    wrong_rows <- !model_x$.rr_missing & !is.na(model_x$current_premium) & !is.na(model_x$proposed_premium)
    wrong_fit <- .rr_diag_binary_model(model_x$.rr_wrong[wrong_rows], px[wrong_rows])
    if (!is.null(wrong_fit)) {
      model_signals[[mi]] <- data.frame(variable = v, outcome = "wrong",
                                        signal = wrong_fit$signal, stringsAsFactors = FALSE)
      fits[[paste0("wrong__", v)]] <- wrong_fit$fit
      mi <- mi + 1L
    }
    
    magnitude_rows <- wrong_rows & is.finite(model_x$.rr_abs_error)
    mag_fit <- .rr_diag_continuous_model(log1p(model_x$.rr_abs_error[magnitude_rows]), px[magnitude_rows])
    if (!is.null(mag_fit)) {
      model_signals[[mi]] <- data.frame(variable = v, outcome = "error_magnitude",
                                        signal = mag_fit$signal, stringsAsFactors = FALSE)
      fits[[paste0("magnitude__", v)]] <- mag_fit$fit
      mi <- mi + 1L
    }
  }
  model_signals <- .rr_diag_bind_model_signals(model_signals)
  
  model_best <- if (nrow(model_signals)) {
    stats::aggregate(signal ~ variable, model_signals, max, na.rm = TRUE)
  } else {
    data.frame(variable = character(), signal = numeric(), stringsAsFactors = FALSE)
  }
  names(model_best)[names(model_best) == "signal"] <- "model_score"
  variable_summary <- merge(variable_summary, model_best, by = "variable", all.x = TRUE, sort = FALSE)
  variable_summary$model_score[is.na(variable_summary$model_score)] <- 0
  variable_summary$evidence_score <- pmax(
    variable_summary$simple_score,
    0.75 * variable_summary$model_score + 0.25 * variable_summary$simple_score
  )
  variable_summary <- variable_summary[order(-variable_summary$evidence_score,
                                             -variable_summary$model_score,
                                             variable_summary$variable), , drop = FALSE]
  rownames(variable_summary) <- NULL
  
  interaction_signals <- .rr_diag_interactions(
    model_x = model_x,
    ranked_variables = variable_summary$variable,
    interaction_top = interaction_top,
    max_model_levels = min(max_model_levels, 12L)
  )
  
  suspects <- .rr_diag_suspects(
    variable_summary = variable_summary,
    level_details = level_details,
    model_signals = model_signals,
    top_n = top_n,
    overall_missing = overall_missing,
    overall_mismatch = overall_mismatch,
    overall_mae = overall_mae
  )
  
  out <- list(
    summary = summary,
    suspects = suspects,
    variable_summary = variable_summary,
    level_details = level_details,
    model_signals = model_signals,
    interaction_signals = interaction_signals,
    models = fits,
    settings = list(
      predictors = predictors,
      model_variables = model_variables,
      absolute_tolerance = absolute_tolerance,
      relative_tolerance = relative_tolerance,
      model_sample = model_sample,
      max_model_levels = max_model_levels,
      interaction_top = interaction_top,
      seed = seed
    )
  )
  class(out) <- "raterevision_rater_diagnosis"
  out
}

#' @export
print.raterevision_rater_diagnosis <- function(x, ...) {
  s <- x$summary[1, , drop = FALSE]
  cat("<raterevision_rater_diagnosis>\n")
  cat(" comparison rows:", format(s$comparison_rows, big.mark = ","), "\n")
  cat(" matched:", .rr_diag_pct(s$match_rate), "\n")
  cat(" candidate missing:", format(s$candidate_missing, big.mark = ","),
      "(", .rr_diag_pct(s$missing_rate), ")\n", sep = " ")
  cat(" material wrong:", format(s$material_wrong, big.mark = ","),
      "(", .rr_diag_pct(s$wrong_rate), " of non-missing candidate rows)\n", sep = " ")
  
  if (s$total_mismatch == 0L) {
    cat("\nNo material discrepancies were detected at the specified tolerances.\n")
  } else if (nrow(x$suspects)) {
    cat("\nLikely places to investigate:\n")
    for (i in seq_len(nrow(x$suspects))) {
      cat(" ", i, ". ", x$suspects$variable[i], " - ", x$suspects$conclusion[i], "\n", sep = "")
    }
  } else {
    cat("\nNo investigated rating variable meaningfully separates the discrepancies; check for a global error or an omitted predictor.\n")
  }
  
  if (nrow(x$interaction_signals)) {
    sig <- x$interaction_signals[x$interaction_signals$signal >= 0.01, , drop = FALSE]
    if (nrow(sig)) {
      sig <- utils::head(sig[order(-sig$signal), , drop = FALSE], 3L)
      cat("\nInteraction signals worth checking:\n")
      for (i in seq_len(nrow(sig))) {
        cat(" - ", sig$variable1[i], " x ", sig$variable2[i],
            " (", sig$outcome[i], ")\n", sep = "")
      }
    }
  }
  
  cat("\nDetailed grouped diagnostics are in $level_details; background model summaries are in $model_signals.\n")
  invisible(x)
}

.rr_nonnegative_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    stop(name, " must be a nonnegative number.", call. = FALSE)
  }
  as.numeric(x)
}

.rr_diag_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != as.integer(x)) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}

.rr_diag_nonnegative_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x != as.integer(x)) {
    stop(name, " must be a nonnegative integer.", call. = FALSE)
  }
  as.integer(x)
}

.rr_diag_usable_predictor <- function(x) {
  if (is.list(x) && !is.data.frame(x)) return(FALSE)
  vals <- x[!is.na(x)]
  length(vals) > 0L && length(unique(vals)) > 1L
}

.rr_diag_group_values <- function(x, bins = 10L) {
  if (is.numeric(x) && length(unique(x[!is.na(x)])) > max(20L, bins * 2L)) {
    probs <- seq(0, 1, length.out = bins + 1L)
    br <- unique(as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, type = 7)))
    if (length(br) >= 3L) {
      br[1] <- -Inf; br[length(br)] <- Inf
      z <- as.character(cut(x, breaks = br, include.lowest = TRUE, dig.lab = 8))
    } else {
      z <- as.character(x)
    }
  } else {
    z <- as.character(x)
  }
  z[is.na(x) | is.na(z) | !nzchar(trimws(z))] <- "<NA>"
  z
}

.rr_diag_model_predictor <- function(x, max_levels = 40L) {
  if (is.numeric(x) && length(unique(x[!is.na(x)])) > max_levels) {
    probs <- seq(0, 1, length.out = min(10L, max_levels) + 1L)
    br <- unique(as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, type = 7)))
    if (length(br) >= 3L) {
      br[1] <- -Inf; br[length(br)] <- Inf
      z <- as.character(cut(x, breaks = br, include.lowest = TRUE, dig.lab = 8))
    } else {
      z <- as.character(x)
    }
  } else {
    z <- as.character(x)
  }
  z[is.na(x) | is.na(z) | !nzchar(trimws(z))] <- "<NA>"
  
  tab <- sort(table(z), decreasing = TRUE)
  if (length(tab) > max_levels) {
    keep <- names(tab)[seq_len(max(1L, max_levels - 1L))]
    z[!(z %in% keep)] <- "<OTHER>"
  }
  factor(z)
}

.rr_diag_binary_model <- function(y, x) {
  ok <- !is.na(y) & !is.na(x)
  y <- as.logical(y[ok]); x <- droplevels(x[ok])
  if (length(y) < 20L || length(unique(y)) < 2L || nlevels(x) < 2L) return(NULL)
  d <- data.frame(y = as.integer(y), x = x)
  fit <- tryCatch(
    suppressWarnings(stats::glm(y ~ x, data = d, family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit) || !is.finite(fit$null.deviance) || fit$null.deviance <= 0) return(NULL)
  signal <- max(0, min(1, (fit$null.deviance - fit$deviance) / fit$null.deviance))
  list(signal = signal, fit = fit)
}

.rr_diag_continuous_model <- function(y, x) {
  ok <- is.finite(y) & !is.na(x)
  y <- y[ok]; x <- droplevels(x[ok])
  if (length(y) < 20L || stats::var(y) <= 0 || nlevels(x) < 2L) return(NULL)
  fit <- tryCatch(stats::lm(y ~ x, data = data.frame(y = y, x = x)), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  r2 <- tryCatch(summary(fit)$r.squared, error = function(e) NA_real_)
  if (!is.finite(r2)) return(NULL)
  list(signal = max(0, min(1, r2)), fit = fit)
}

.rr_diag_bind_model_signals <- function(xs) {
  if (!length(xs)) {
    return(data.frame(variable = character(), outcome = character(), signal = numeric(),
                      stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, xs)
  out <- out[order(-out$signal, out$variable, out$outcome), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.rr_diag_interactions <- function(model_x, ranked_variables,
                                  interaction_top = 4L, max_model_levels = 12L) {
  if (interaction_top < 2L || length(ranked_variables) < 2L) {
    return(.rr_empty_interactions())
  }
  vars <- utils::head(ranked_variables, interaction_top)
  pairs <- utils::combn(vars, 2L, simplify = FALSE)
  rows <- list(); ri <- 1L
  
  for (pair in pairs) {
    a <- pair[1]; b <- pair[2]
    x1 <- .rr_diag_model_predictor(model_x[[a]], max_model_levels)
    x2 <- .rr_diag_model_predictor(model_x[[b]], max_model_levels)
    
    outcomes <- list(
      missing = list(
        y = model_x$.rr_missing,
        rows = !is.na(model_x$current_premium),
        family = "binary"
      ),
      wrong = list(
        y = model_x$.rr_wrong,
        rows = !model_x$.rr_missing & !is.na(model_x$current_premium) & !is.na(model_x$proposed_premium),
        family = "binary"
      ),
      error_magnitude = list(
        y = log1p(model_x$.rr_abs_error),
        rows = !model_x$.rr_missing & is.finite(model_x$.rr_abs_error),
        family = "continuous"
      )
    )
    
    for (nm in names(outcomes)) {
      o <- outcomes[[nm]]
      keep <- o$rows & !is.na(x1) & !is.na(x2) & !is.na(o$y)
      if (sum(keep) < 40L) next
      d <- data.frame(y = o$y[keep], x1 = droplevels(x1[keep]), x2 = droplevels(x2[keep]))
      if (nlevels(d$x1) < 2L || nlevels(d$x2) < 2L) next
      
      signal <- NA_real_
      if (o$family == "binary") {
        if (length(unique(d$y)) < 2L) next
        add <- tryCatch(suppressWarnings(stats::glm(y ~ x1 + x2, data = d, family = stats::binomial())),
                        error = function(e) NULL)
        int <- tryCatch(suppressWarnings(stats::glm(y ~ x1 * x2, data = d, family = stats::binomial())),
                        error = function(e) NULL)
        if (!is.null(add) && !is.null(int) && is.finite(add$null.deviance) && add$null.deviance > 0) {
          signal <- max(0, min(1, (add$deviance - int$deviance) / add$null.deviance))
        }
      } else {
        if (!is.finite(stats::var(d$y)) || stats::var(d$y) <= 0) next
        add <- tryCatch(stats::lm(y ~ x1 + x2, data = d), error = function(e) NULL)
        int <- tryCatch(stats::lm(y ~ x1 * x2, data = d), error = function(e) NULL)
        if (!is.null(add) && !is.null(int)) {
          add_r2 <- summary(add)$r.squared
          int_r2 <- summary(int)$r.squared
          signal <- max(0, min(1, int_r2 - add_r2))
        }
      }
      
      if (is.finite(signal)) {
        rows[[ri]] <- data.frame(variable1 = a, variable2 = b, outcome = nm,
                                 signal = signal, stringsAsFactors = FALSE)
        ri <- ri + 1L
      }
    }
  }
  
  if (!length(rows)) return(.rr_empty_interactions())
  out <- do.call(rbind, rows)
  out <- out[order(-out$signal, out$variable1, out$variable2, out$outcome), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.rr_empty_interactions <- function() {
  data.frame(variable1 = character(), variable2 = character(), outcome = character(),
             signal = numeric(), stringsAsFactors = FALSE)
}

.rr_diag_suspects <- function(variable_summary, level_details, model_signals, top_n,
                              overall_missing, overall_mismatch, overall_mae) {
  if (!nrow(variable_summary) || top_n == 0L ||
      .rr_safe_max(variable_summary$evidence_score, default = 0) <= 1e-08) {
    return(data.frame(rank = integer(), variable = character(), evidence_score = numeric(),
                      strongest_signal = character(), conclusion = character(),
                      stringsAsFactors = FALSE))
  }
  vs <- utils::head(variable_summary, top_n)
  rows <- vector("list", nrow(vs))
  
  for (i in seq_len(nrow(vs))) {
    v <- vs$variable[i]
    lev <- level_details[level_details$variable == v, , drop = FALSE]
    sig <- model_signals[model_signals$variable == v, , drop = FALSE]
    strongest <- if (nrow(sig)) sig$outcome[which.max(sig$signal)] else "grouped_scan"
    
    eligible <- lev$reference_n >= max(5L, ceiling(0.0025 * max(1L, sum(lev$reference_n))))
    if (!any(eligible)) eligible <- lev$reference_n > 0L
    e <- lev[eligible, , drop = FALSE]
    
    if (strongest == "missing" || vs$max_missing_lift[i] >= vs$max_mismatch_lift[i] &&
        vs$max_missing_lift[i] >= 0.02) {
      j <- which.max(ifelse(is.na(e$missing_rate), -Inf, e$missing_rate))
      conclusion <- paste0(
        "missing premiums are concentrated at ", v, " = ", e$level[j],
        " (", .rr_diag_pct(e$missing_rate[j]), " missing vs ", .rr_diag_pct(overall_missing), " overall)"
      )
      strongest <- "missing"
    } else if (strongest == "wrong" || vs$max_mismatch_lift[i] >= 0.02) {
      j <- which.max(ifelse(is.na(e$mismatch_rate), -Inf, e$mismatch_rate))
      direction <- .rr_diag_direction(e$mean_signed_error[j])
      conclusion <- paste0(
        "premium mismatches are concentrated at ", v, " = ", e$level[j],
        " (", .rr_diag_pct(e$mismatch_rate[j]), " mismatched vs ", .rr_diag_pct(overall_mismatch), " overall",
        if (nzchar(direction)) paste0("; ", direction) else "", ")"
      )
      strongest <- "wrong"
    } else {
      j <- which.max(ifelse(is.na(e$mean_abs_error), -Inf, e$mean_abs_error))
      ratio <- if (!is.na(overall_mae) && overall_mae > 0) e$mean_abs_error[j] / overall_mae else NA_real_
      direction <- .rr_diag_direction(e$mean_signed_error[j])
      detail <- character()
      if (is.finite(ratio)) {
        detail <- c(detail, paste0("about ", format(round(ratio, 1), nsmall = 1),
                                   "x the overall mean absolute error"))
      }
      if (nzchar(direction)) detail <- c(detail, direction)
      conclusion <- paste0(
        "largest errors occur near ", v, " = ", e$level[j],
        if (length(detail)) paste0(" (", paste(detail, collapse = "; "), ")") else ""
      )
      strongest <- "error_magnitude"
    }
    
    rows[[i]] <- data.frame(
      rank = i,
      variable = v,
      evidence_score = vs$evidence_score[i],
      strongest_signal = strongest,
      conclusion = conclusion,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

.rr_diag_direction <- function(x) {
  if (length(x) != 1L || is.na(x) || !is.finite(x) || abs(x) < 1e-12) return("")
  paste0(
    "candidate averages ",
    format(round(abs(x), 2), nsmall = 2, trim = TRUE),
    if (x > 0) " higher" else " lower"
  )
}

.rr_safe_max <- function(x, default = NA_real_) {
  x <- x[is.finite(x)]
  if (!length(x)) default else max(x)
}

.rr_diag_pct <- function(x) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) return("NA")
  paste0(format(round(100 * x, 1), nsmall = 1), "%")
}
