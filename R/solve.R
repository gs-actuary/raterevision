#' Solve a target aggregate rate change
#'
#' Finds a single numeric parameter that makes an arbitrary proposed rating
#' workflow achieve a desired aggregate rate change from a supplied current
#' premium. The solver does not know or care whether the proposed workflow uses
#' the same specification, a structurally different specification, driver
#' averaging, custom R code, or territory-specific base rates. The user defines
#' the one-dimensional adjustment inside `rate_function`.
#'
#' A common territory-base use is to let `x` multiply every territory-specific
#' base rate, preserving the proposed territorial shape while solving one
#' overall rate-level parameter. If several base rates are allowed to move
#' independently against one aggregate target, the problem is underdetermined
#' and should not be passed to this scalar solver.
#' Independent targets (for example one target per coverage) can be solved by
#' calling this function once per independently adjustable parameter.
#'
#' @param current_premium Current aggregate premium, a positive finite scalar.
#' @param target Desired aggregate rate change as a decimal, e.g. `0.06`.
#' @param rate_function Function of one numeric parameter. It may return a
#'   premium scalar/vector directly, or any object handled by
#'   `premium_extractor`.
#' @param interval Initial two-value search interval for the parameter.
#' @param premium_extractor Optional function that converts the result of
#'   `rate_function` to a single aggregate premium.
#' @param tol Root-finding tolerance.
#' @param maxiter Maximum root-finding iterations.
#' @param extend_interval Passed to `stats::uniroot(extendInt = ...)`; one of
#'   `"no"`, `"yes"`, `"upX"`, or `"downX"`.
#' @param parameter_name Label used in printed output.
#'
#' @return A `raterevision_target_solution` object.
#' @export
solve_rate_target <- function(current_premium, target, rate_function,
                              interval = c(0.5, 1.5), premium_extractor = NULL,
                              tol = 1e-8, maxiter = 100L,
                              extend_interval = c("no", "yes", "upX", "downX"),
                              parameter_name = "multiplier") {
  extend_interval <- match.arg(extend_interval)
  if (length(current_premium) != 1L || !is.finite(current_premium) || current_premium <= 0) stop("current_premium must be a positive finite scalar.", call. = FALSE)
  if (length(target) != 1L || !is.finite(target) || target <= -1) stop("target must be a finite scalar greater than -1.", call. = FALSE)
  if (!is.function(rate_function)) stop("rate_function must be a function of one numeric parameter.", call. = FALSE)
  if (length(interval) != 2L || any(!is.finite(interval)) || interval[1] >= interval[2]) stop("interval must contain two increasing finite values.", call. = FALSE)
  if (!is.null(premium_extractor) && !is.function(premium_extractor)) stop("premium_extractor must be NULL or a function.", call. = FALSE)

  eval_count <- 0L
  premium_at <- function(x) {
    eval_count <<- eval_count + 1L
    ans <- rate_function(x)
    prem <- if (is.null(premium_extractor)) {
      if (!is.numeric(ans)) stop("rate_function must return numeric premium data unless premium_extractor is supplied.", call. = FALSE)
      sum(ans, na.rm = TRUE)
    } else {
      premium_extractor(ans)
    }
    if (length(prem) != 1L || !is.finite(prem)) stop("The aggregate proposed premium must be a finite scalar.", call. = FALSE)
    as.numeric(prem)
  }
  target_premium <- current_premium * (1 + target)
  objective <- function(x) premium_at(x) - target_premium

  root <- tryCatch(
    stats::uniroot(objective, interval = interval, tol = tol, maxiter = maxiter, extendInt = extend_interval),
    error = function(e) {
      stop("Could not bracket/solve the target rate change. Check the interval and ensure the chosen parameter moves aggregate premium through the target. Original error: ", conditionMessage(e), call. = FALSE)
    }
  )
  achieved <- premium_at(root$root)
  out <- list(
    parameter = root$root,
    parameter_name = parameter_name,
    current_premium = current_premium,
    target_change = target,
    target_premium = target_premium,
    achieved_premium = achieved,
    achieved_change = achieved / current_premium - 1,
    iterations = root$iter,
    estimated_precision = root$estim.prec,
    residual = root$f.root,
    evaluations = eval_count,
    interval = interval
  )
  class(out) <- "raterevision_target_solution"
  out
}

#' @export
print.raterevision_target_solution <- function(x, ...) {
  cat("<raterevision_target_solution>\n")
  cat(" ", x$parameter_name, ": ", format(x$parameter, digits = 10), "\n", sep = "")
  cat(" target change: ", formatC(100 * x$target_change, format = "f", digits = 4), "%\n", sep = "")
  cat(" achieved change: ", formatC(100 * x$achieved_change, format = "f", digits = 4), "%\n", sep = "")
  cat(" target premium: ", format(x$target_premium, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat(" achieved premium: ", format(x$achieved_premium, big.mark = ",", scientific = FALSE), "\n", sep = "")
  invisible(x)
}
