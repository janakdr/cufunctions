# Package-level environment for per-session tracking of parameter usage
.cuf_env <- new.env(parent = emptyenv())
.cuf_env$param_counts <- list()

.onLoad <- function(libname, pkgname) {
  .cuf_env$param_counts <- list()
  # Set default options only if user hasn't already set them (e.g. in .Rprofile)
  op <- options()
  op.cuf <- list(cufunctions.hints_defaults = TRUE)
  toset <- !(names(op.cuf) %in% names(op))
  if (any(toset)) options(op.cuf[toset])
}

#' Apply global option overrides and track explicit parameter usage for hints.
#'
#' Call at the top of each user-facing function body (after any
#' \code{deparse(substitute(...))} calls):
#'
#'   cuf_apply_defaults(match.call(), environment())
#'
#' For each formal parameter with a default value:
#' \itemize{
#'   \item If the user explicitly passed it: track for usage hints via
#'         \code{hint_option()}.
#'   \item If the user relied on the default: check
#'         \code{getOption("cufunctions.<param>")} and override if set.
#' }
#'
#' @param mc Result of \code{match.call()} in the caller.
#' @param env Result of \code{environment()} in the caller.
#' @keywords internal
cuf_apply_defaults <- function(mc, env) {
  fn <- sys.function(sys.parent())
  fmls <- formals(fn)
  explicit_args <- names(mc)[-1]  # drop the function name

  for (param in names(fmls)) {
    if (param == "...") next
    # Skip required params (no default value)
    if (identical(fmls[[param]], quote(expr = ))) next

    option_name <- paste0("cufunctions.", param)

    if (param %in% explicit_args) {
      # User explicitly passed this param -- track for hint
      hint_option(param, get(param, envir = env))
    } else {
      # User relied on default -- reset tracker and check for global override
      .cuf_env$param_counts[[param]] <- NULL
      opt_val <- getOption(option_name)
      if (!is.null(opt_val)) {
        assign(param, opt_val, envir = env)
      }
    }
  }
}

#' Emit a usage hint after repeated explicit use of the same non-default value.
#'
#' Fires at every multiple of \code{threshold} (default 3). Respects
#' \code{getOption("cufunctions.hints_defaults")} and is silenced when the
#' specific option is already set.
#'
#' @param param_name The parameter name (e.g. \code{"ebars"}).
#' @param value The value the user passed.
#' @param threshold How often to remind (every N-th call, default 3).
#' @keywords internal
hint_option <- function(param_name, value, threshold = 3L) {
  if (!isTRUE(getOption("cufunctions.hints_defaults", TRUE))) return(invisible())
  option_name <- paste0("cufunctions.", param_name)
  # If the user already has this option set, no need to hint
  if (!is.null(getOption(option_name))) return(invisible())

  # Skip non-scalar values (data vectors, factors, etc.)
  if (length(value) > 1) return(invisible())
  val_str <- deparse1(value)

  # Track consecutive uses of the same value per param.
  # Switching to a different value resets the count.
  tracker <- .cuf_env$param_counts[[param_name]]
  if (is.null(tracker) || !identical(tracker$val, val_str)) {
    tracker <- list(val = val_str, n = 1L)
  } else {
    tracker$n <- tracker$n + 1L
  }
  .cuf_env$param_counts[[param_name]] <- tracker

  if (tracker$n %% threshold == 0L) {
    message(sprintf(
      paste0("Tip: You've set %s=%s %d times. To make it the session default:\n",
             "  options(%s = %s)\n",
             "Add to ~/.Rprofile to persist across sessions.\n",
             "Suppress these hints: options(cufunctions.hints_defaults = FALSE)"),
      param_name, val_str, tracker$n,
      option_name, val_str))
  }
}
