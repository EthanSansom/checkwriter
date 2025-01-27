# test -------------------------------------------------------------------------

# Get the test component of a <check> or <test> function as an expression
get_test <- function(x) {
  UseMethod("get_test")
}

set_test <- function(x, expr) {
  UseMethod("set_test")
}

get_test.checkwriter_test <- function(x) {
  fun_body(x)[[2]]
}

set_test.checkwriter_test <- function(x, expr) {
  fun_body(x)[[2]] <- expr
  x
}

get_test.checkwriter_check <- function(x) {
  # if (<RETURNS THIS>) { return(x) }
  fun_body(x)[[2]][[2]]
}

set_test.checkwriter_check <- function(x, expr) {
  fun_body(x)[[2]][[2]] <- expr
  x
}

# header -----------------------------------------------------------------------

# Get the default error header of a <check> function as an expression
get_default_header <- function(x) {
  UseMethod("get_default_header")
}

set_default_header <- function(x, expr) {
  UseMethod("set_default_header")
}

get_default_header.checkwriter_check <- function(x) {
  # if (test) { return(x) }
  # if (error_fast) { ... }
  # bind_test_results()
  # checkwriter_abort(header = error_header %||% <RETURNS THIS>, ...)
  fun_body(x)[[5]][[2]][[3]]
}

set_default_header.checkwriter_check <- function(x, expr) {
  fun_body(x)[[5]][[2]][[3]] <- expr
  x
}

get_default_header.checkwriter_slow_check <- function(x) {
  # if (test) { return(x) }
  # bind_test_results()
  # header <- error_header %||% <RETURNS THIS>
  fun_body(x)[[4]][[2]][[3]]
}

set_default_header.checkwriter_slow_check <- function(x, expr) {
  fun_body(x)[[4]][[2]][[3]] <- expr
  x
}

get_default_header.checkwriter_fast_check <- function(x) {
  # The default header of a <fast_check> is whatever value the caller supplies
  # to the `error_header` argument. It's possible that this argument has been
  # "fixed" away, in which case there is no `error_header` formal, but the error
  # header will instead be located in the `abort()` call of the <fast_check>.
  fmls <- fun_fmls(x)
  if (("error_header" %in% names(fmls)) && !rlang::is_missing(fmls$error_header)) {
    return(fmls$error_header)
  }
  # Get the error header from the abort call instead
  fun_body(x)[[3]][[2]]
}

set_default_header.checkwriter_fast_check <- function(x, expr) {
  fmls <- fun_fmls(x)
  if (("error_header" %in% names(fmls)) && !rlang::is_missing(fmls$error_header)) {
    redefault_formal(x, "error_header", expr)
  } else {
    fun_body(x)[[3]][[2]] <- expr
    x
  }
}

# bullets ----------------------------------------------------------------------

# Get the default error bullets of a <check> function as an expression
get_default_bullets <- function(x) {
  UseMethod("get_default_bullets")
}

set_default_bullets <- function(x, expr) {
  UseMethod("set_default_bullets")
}

get_default_bullets.checkwriter_check <- function(x) {
  # if (test) { return(x) }
  # if (error_fast) { ... }
  # bind_test_results()
  # checkwriter_abort(
  #   header = error_header %||% ...,
  #   bullets = error_bullets %||% <RETURNS THIS>,
  #   ...
  # )
  fun_body(x)[[5]][[3]][[3]]
}

set_default_bullets.checkwriter_check <- function(x, expr) {
  fun_body(x)[[5]][[3]][[3]] <- expr
  x
}

get_default_bullets.checkwriter_slow_check <- function(x) {
  # if (test) { return(x) }
  # bind_test_results()
  # checkwriter_abort(
  #   header = error_header %||% ...,
  #   bullets = error_bullets %||% <RETURNS THIS>,
  #   ...
  # )
  fun_body(x)[[4]][[3]][[3]]
}

set_default_bullets.checkwriter_slow_check <- function(x, expr) {
  fun_body(x)[[4]][[3]][[3]] <- expr
  x
}

get_default_bullets.checkwriter_fast_check <- function(x) {
  # See `get_default_header.checkwriter_fast_check()` for the reasoning here
  fmls <- fun_fmls(x)
  if (("error_bullets" %in% names(fmls)) && !rlang::is_missing(fmls$error_bullets)) {
    return(fmls$error_bullets)
  }
  fun_body(x)[[3]][[3]]
}

set_default_bullets.checkwriter_fast_check <- function(x, expr) {
  fmls <- fun_fmls(x)
  if (("error_bullets" %in% names(fmls)) && !rlang::is_missing(fmls$error_bullets)) {
    redefault_formal(x, "error_bullets", expr)
  } else {
    fun_body(x)[[3]][[3]] <- expr
    x
  }
}

# return -----------------------------------------------------------------------

# Get the non-error return value of a <check> function as an expression
get_return_value <- function(x) {
  UseMethod("get_return_value")
}

set_return_value <- function(x, expr) {
  UseMethod("set_return_value")
}

get_return_value.checkwriter_check <- function(x) {
  # if (test) { return(<RETURNS THIS>) }
  fun_body(x)[[2]][[3]][[2]][[2]]
}

set_return_value.checkwriter_check <- function(x, expr) {
  fun_body(x)[[2]][[3]][[2]][[2]] <- expr
  x
}

# vectorized -------------------------------------------------------------------

# TODO: We'll need specific getters and setters for the following:
# - checkwriter_check_vectorized      -> fully vectorized tests
# - checkwriter_check_semi_vectorized -> vectorized and un-vectorized tests
#
# In the semi-vectorized case, we'll need accessors `get_*_vectorized()` and
# `get_*_unvectorized()` to access each set of tests, bullets, etc.
