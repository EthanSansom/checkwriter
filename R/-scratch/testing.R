# Define Checks and Tests ------------------------------------------------------

# TODO: Define some example tests and checks to test with.

# Helper for emitting error messages
vec_must <- function(x_name, len = NULL, na_ok = TRUE) {
  paste(c(
    "`", x_name, "` must be a",
    if (!is.null(len)) c(" length ", len),
    if (!na_ok) " non-NA",
    " object."
  ), collapse = "")
}

# The interior of an example `test()` function
test_length_na <- function(x, len = NULL, na_ok = TRUE) {
  (.t1. <- null_or(len, len == length(x))) %and%
    (.t2. <- true_or(na_ok, !anyNA(x)))
}
class(test_length_na) <- c("checkwriter_test", "checkwriter_fun", "function")

# The interior of an example `check()` function
check_length_na <- function(
  # Test parameters
  x,
  len = NULL,
  na_ok = TRUE,

  # Test parameter names
  x_name = rlang::caller_arg(x),
  len_name = rlang::caller_arg(len_name),
  na_ok_name = rlang::caller_arg(na_ok),

  # Error customization
  error_call = rlang::caller_env(),
  error_class = NULL,
  error_header = NULL,
  error_bullets = NULL,
  error_fast = FALSE,
  error_dots = list()
) {

  # The test
  if (
    (.t1. <- null_or(len, len == length(x))) %and%
    (.t2. <- true_or(na_ok, !anyNA(x)))
  ) {
    return(x)
  }

  # Error fast block, use the caller supplied `error_header` and `error_bullets`
  if (error_fast) {
    checkwriter_abort(
      header = error_header,
      bullets = error_bullets,
      error_class = error_class,
      error_call = error_call,
      error_dots = error_dots
    )
  }

  # Bind the results in this env (setting untested results to NULL)
  bind_test_results(.t1., .t2.)

  # TODO: What if we moved these directly into the `checkwriter_abort()` call,
  # instead of assigning them. Then, the user has no way of fucking with the
  # `header` or `bullets` symbols!
  #
  # header <- error_header %||% vec_must(x_name, len = len, na_ok = na_ok)
  # bullets <- error_bullets %||% c(
  #   switch_bullets(.t1., fail = "{.arg {x_name}} is length {length(x)}."),
  #   switch_bullets(.t2., fail = "{.arg {x_name}} is NA at {.vec {which(is.na(x))}}.")
  # )
  #
  # TODO: I need to edit the functions in `accessors.R` to adapt to this new
  # version of <check> functions.

  checkwriter_abort(
    header = error_header %||% vec_must(x_name, len = len, na_ok = na_ok),
    bullets = error_bullets %||% c(
      switch_bullets(.t1., fail = "{.arg {x_name}} is length {length(x)}."),
      switch_bullets(.t2., fail = "{.arg {x_name}} is NA at {.vec {which(is.na(x))}}.")
    ),
    error_class = error_class,
    error_call = error_call,
    error_dots = error_dots
  )
}
class(check_length_na) <- c("checkwriter_check", "checkwriter_fun", "function")

# Example of a "fast" check where we've fixed the bullets and header
check_is_integer_fast <- function(x, x_name = rlang::caller_arg(x), error_bullets = "hey") {
  if (.t1. <- is.integer(x)) {
    return(x)
  }
  # NOTE: No need to bind the test results because the user supplied header and
  # bullets can't make use of them.
  checkwriter_abort(
    header = "error_header defefe",
    bullets = error_bullets,
    error_class = "my_error",
    error_call = rlang::caller_env(),
    error_dots = list()
  )
}
class(check_is_integer_fast) <- c(
  "checkwriter_fast_check",
  "checkwriter_check",
  "checkwriter_fun",
  "function"
)

# Example of a "slow" check where we're never going fast.
check_is_integer_slow <- function(
    x,
    x_name = rlang::caller_arg(x),
    error_call = rlang::caller_env(),
    error_class = "checkwriter_error",
    error_header = NULL,
    error_bullets = NULL,
    error_dots = list()
) {

  if (.t1. <- is.integer(x)) {
    return(x)
  }

  bind_test_results(.t1.)
  checkwriter_abort(
    header = error_header %||% "{.arg {x_name}} must be an integer.",
    bullets = error_bullets %||% "{.arg {x_name}} is {.obj_type_friendly {x}}.",
    error_class = error_class,
    error_call = error_call,
    error_dots = error_dots
  )
}
class(check_is_integer_slow) <- c(
  "checkwriter_slow_check",
  "checkwriter_check",
  "checkwriter_fun",
  "function"
)

# Testing ----------------------------------------------------------------------

load_all()
