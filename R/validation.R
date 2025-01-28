# assertions -------------------------------------------------------------------

assert_closure <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {
  if (!rlang::is_function(x) || rlang::is_primitive(x)) {
    cli::cli_abort(
      message = c(
        "{.arg {x_name}} must be a non-primitive function.",
        x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
      ),
      call = error_call,
      class = the$errors$invalid_argument
    )
  }
}

assert_symbols <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {
  non_symbols <- !map_lgl(x, rlang::is_symbol)
  if (any(non_symbols)) {
    cli::cli_abort(
      message = c(
        "{.arg {x_name}} must be a list of symbols.",
        x = "{.arg {x_name}} contains non-symbols at locations {.vec {which(non_symbols)}}."
      ),
      call = error_call,
      class = the$errors$invalid_argument
    )
  }
}

# Assert that no symbols in `expr` match the pattern `.t[0-9]+.` (e.g. `.t1.`)
assert_no_reserved_symbols <- function(
    expr,
    expr_name = rlang::caller_arg(expr),
    error_call = rlang::caller_env()
) {
  reserved_t_symbols <- find_symbols(expr, pattern = the$t_symbol_pattern)
  if (!is_empty(reserved_t_symbols)) {
    cli::cli_abort(
      message = c(
        paste(
          "{.arg {expr_name}} must not contain any symbols matching the",
          "pattern {.val {the$t_symbol_pattern}}."
        ),
        x = paste(
          "{.arg {expr_name}} contains{cli::qty(length(reserved_t_symbols))}",
          "{?the/} symbol{?s}: {.arg {reserved_t_symbols}}."
        ),
        i = "The {.pkg checkwriter} package reserves these symbols for use in checks and tests."
      )
    )
  }
  reserved_bullets_symbols <- find_symbols(
    expr,
    symbols = c(".vectorized_bullets.", ".unvectorized_bullets.")
  )
  if (!is_empty(reserved_bullets_symbols)) {
    cli::cli_abort(
      message = c(
        paste(
          "{.arg {expr_name}} must not contain the symbol {.arg .vectorized_bullets.}",
          "or the symbol {.arg .unvectorized_bullets.}."
        ),
        x = if (".vectorized_bullets." %in% reserved_bullets_symbols) {
          "{.arg {expr_name}} contains the symbol {.arg .vectorized_bullets.}."
        },
        x = if (".unvectorized_bullets." %in% reserved_bullets_symbols) {
          "{.arg {expr_name}} contains the symbol {.arg .unvectorized_bullets.}."
        },
        i = "The {.pkg checkwriter} package reserves these symbols for use in checks and tests."
      )
    )
  }
}

# checks -----------------------------------------------------------------------

check_is_env <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = the$errors$invalid_argument
  ) {
  if (rlang::is_environment(x)) {
    return(x)
  }
  cli::cli_abort(
    message = c(
      "{.arg {x_name}} must be an environment.",
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
    ),
    call = error_call,
    class = c("checkwriter_error", error_class)
  )
}

check_is_test <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = the$errors$invalid_argument
) {
  if (is_test(x)) {
    return(x)
  }
  cli::cli_abort(
    message = c(
      "{.arg {x_name}} must be a {.cls checkwriter_test}",
      x = "{.arg {x_name}} is {.obj_type_friendly {x}}."
    ),
    call = error_call,
    class = c("checkwriter_error", error_class)
  )
}

# friendlyr functions ----------------------------------------------------------

# TODO: Delete these, it is unlikely that you will need them!

at_loc_friendly <- function(loc, n_max = 5) {
  loc <- if (is.logical(loc)) which(loc & !is.na(loc)) else loc
  loc <- as.numeric(loc)
  n <- length(loc)
  at <- ngettext(min(n, n_max), "at location ", "at locations ")
  if (n > n_max) {
    paste0(at, "`", deparse(loc[seq(n_max)]), "` and ", n - n_max, " more")
  } else {
    paste0(at, "`", deparse(loc), "`")
  }
}

a_length_n_friendly <- function(len) {
  format_len <- function(x) format(x, scientific = FALSE, big.mark = ",")
  if (is.null(len)) {
    "a"
  } else if (length(len) == 2) {
    len <- format_len(len)
    paste0("a length [", len[[1]], "-", len[[2]], "]")
  } else if (len == 1) {
    "a scalar"
  } else if (len > 0) {
    paste0("a length-", format_len(len))
  } else {
    "an empty"
  }
}

length_n_friendly <- function(len) {
  format_len <- function(x) format(x, scientific = FALSE, big.mark = ",")
  if (is.null(len)) {
    ""
  } else if (length(len) == 2) {
    len <- format_len(len)
    paste0("length [", len[[1]], "-", len[[2]], "]")
  } else if (len > 0) {
    paste0("length-", format_len(len))
  } else {
    "empty"
  }
}

in_range_friendly <- function(lower, upper) {
  no_lower <- is.null(lower) || is.infinite(lower)
  no_upper <- is.null(upper) || is.infinite(upper)
  if (no_lower && no_upper) {
    ""
  } else if (no_lower) {
    paste("upper bounded by", upper)
  } else if (no_upper) {
    paste("lower bounded by", lower)
  } else {
    paste0("in range [", lower, ", ", upper, "]")
  }
}

upper1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

commas <- function(x, n = NULL) {
  if (isTRUE(length(x) > n)) {
    length(x) <- n
    paste0(paste(x, collapse = ", "), ", ...")
  } else {
    paste(x, collapse = ", ")
  }
}

oxford <- function(x, sep = ", ", last = "or", n = NULL) {
  x_len <- length(x)
  if (x_len <= 1) {
    return(paste(x))
  } else if (!is.null(n) && x_len > n) {
    length(x) <- n
    return(paste0(paste(x, collapse = sep), sep, " ..."))
  }
  if (x_len == 2) sep <- " "
  paste(paste(x[-x_len], collapse = sep), last, x[[x_len]], sep = sep)
}

truncate_bullets <- function(bullets, n = 5, postscript = NULL) {
  n_bullets <- length(bullets)
  if (n_bullets > n) {
    length(bullets) <- n
    c(bullets, paste("... and", n_bullets - n, "more problems."))
  } else {
    bullets
  }
}
