# TODO: Define the user-facing `test()` function and class.

test <- function(
    .expr,
    ...,
    .env = rlang::caller_env()
  ) {
  check_is_env <- identity # TODO

  fmls <- rlang::pairlist2(x = , !!!rlang::exprs(...))
  body <- rlang::enexpr(.expr)
  env <- check_is_env(.env)

  out <- rlang::new_function(
    args = fmls,
    body = body,
    env = env
  )
  class(out) <- c("checkwriter_test", "function")
  out
}

is_test <- function(x) {
  inherits(x, "checkwriter_test")
}
