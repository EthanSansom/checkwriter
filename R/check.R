# check ------------------------------------------------------------------------

# TODO: Define the interface through which a user creates a <check>.
# TODO: Define the <check> class and sub-classes.

# TODO: Check {rlang} for how they implement some of their magic. The bottom
# of this file https://github.com/r-lib/rlang/blob/main/R/fn.R has some cool
# manipulation of primitive functions to take a look at.

# TODO:
# I think that `.header`, `.bullets`, etc. should be captured as expressions
# and, if you want to insert a quoted expression instead, you can splice it in.
#
# This is the real benefit of using {rlang}. It provides users with access to
# argument splicing with `!!!` and `!!`.
if (FALSE) {
  f <- function(expr) {
    rlang::enexpr(expr)
  }

  message1 <- function() { "my message" }

  f(message1())   # `message1()` is just quoted
  f(!!message1()) # `message1()` is evaluated, then quoted

  my_quoted_message_call <- quote(message1())
  f(my_quoted_message_call)   # Quotes `my_quoted_message_call`, bad
  f(!!my_quoted_message_call) # Quotes `message1()`, good

  eval(f(my_quoted_message_call)) |> print() |> class()
  eval(f(!!my_quoted_message_call)) |> print() |> class()
}

# TODO: Before moving onto the new_slow/fast_check functions, test `new_check()`
# so we know it's working as expected!
#
# TODO: See if you can just use `new_check()` and then for `new_slow_check()`
# and `new_fast_check()`, just remove the section of the body which is unique
# to the slow/fast check.
#
# Low level <check> creation, requires each component of a check to be spoon fed
new_check <- function(
    fmls,          # pairlist of formals
    fixed_fmls,    # named list of fixed formals and their defaults
    test,          # test expression
    return_value,  # return value expression
    header,        # header expression
    bullets,       # bullets expression
    env            # environment where the <check> is defined
  ) {

  # TODO: These should be internal errors
  stopifnot(
    is.pairlist(fmls) || is.list(fmls),
    is.list(fixed_fmls) && rlang::is_named(fixed_fmls),
    is_expr(test),
    is_expr(return_value),
    is_expr(bullets),
    is_expr(header),
    is.environment(env),
    # Some more invariants
    all(c("x", "x_name", "error_call") %in% rlang::names2(fmls)),
    rlang::is_missing(fmls$x)
  )

  test_results <- find_symbols(test, pattern = the$t_symbol_pattern)
  error_arguments <- error_arguments(
    error_fmls_nms = c(
      "error_header",
      "error_bullets",
      "error_class",
      "error_dots",
      "error_fast"
    ),
    fixed_fmls = fixed_fmls
  )
  slow_error_header <- slow_error_header(
    default_error_header = header,
    fixed_fmls_nms = rlang::names2(fixed_fmls)
  )
  slow_error_bullets <- slow_error_bullets(
    default_error_bullets = bullets,
    fixed_fmls_nms = rlang::names2(fixed_fmls)
  )

  body <- rlang::expr({
    if (!!test) {
      return(!!return_value)
    }
    if (!!error_arguments$error_fast) {
      checkwriter::checkwriter_abort(
        header = !!error_arguments$error_header,
        bullets = !!error_arguments$error_bullets,
        error_class = !!error_arguments$error_class,
        error_call = error_call,
        error_dots = !!error_arguments$error_dots
      )
    }
    checkwriter::bind_test_results(!!!test_results)
    checkwriter::checkwriter_abort(
      header = !!slow_error_header,
      bullets = !!slow_error_bullets,
      error_class = !!error_arguments$error_class,
      error_call = error_call,
      error_dots = !!error_arguments$error_dots
    )
  })

  out <- rlang::new_function(
    args = fmls,
    body = body,
    env = env
  )
  class(out) <- c(
    "checkwriter_check",
    "checkwriter_fun",
    "function"
  )
  out
}

new_fast_check <- function(
    fmls,
    fixed_fmls,
    test,
    return_value,
    bullets,
    header,
    env
) {

  # Remove and re-set components of a <check> to make a <fast_check>
  get_fast_abort_call <- function(x) { fun_body(x)[[3]][[3]][[2]] }
  set_last_abort_call <- function(x, expr) {
    fun_body(x)[[5]] <- expr
    x
  }
  remove_fast_if_block_and_bind_results <- function(x) {
    fun_body(x)[3:4] <- NULL
    x
  }

  out <- new_check(
    fmls = fmls,
    fixed_fmls = fixed_fmls,
    test = test,
    return_value = return_value,
    bullets = bullets,
    header = header,
    env = env
  )
  out <- set_last_abort_call(out, expr = get_fast_abort_call(out))
  out <- remove_fast_if_block_and_bind_results(out)

  class(out) <- c("checkwriter_fast_check", class(out))
  out
}

new_slow_check <- function(
    fmls,
    fixed_fmls,
    test,
    return_value,
    bullets,
    header,
    env
) {

  # Remove components of a <check> to make a <slow_check>
  remove_fast_if_block <- function(x) {
    fun_body(x)[[3]] <- NULL
    x
  }

  out <- new_check(
    fmls = fmls,
    fixed_fmls = fixed_fmls,
    test = test,
    return_value = return_value,
    bullets = bullets,
    header = header,
    env = env
  )
  out <- remove_fast_if_block(out)

  class(out) <- c("checkwriter_slow_check", class(out))
  out
}

# TODO: We'll need versions for:
# - <vectorized_check>, <slow> and <fast> also
# - <semi_vectorized_check>, <slow> and <fast> also

new_vectorized_check <- function(
    fmls,          # pairlist of formals
    fixed_fmls,    # named list of fixed formals and their defaults
    test,          # test expression
    return_value,  # return value expression
    bullets,       # bullets expression
    header,        # header expression
    mapper,        # mapper function used for vectorization

    # TODO: I provide `x_reindexed` and `x_renamed` as expressions, but the user
    # should supply them as a named function which takes `x` (`x_name`) and the
    # index of the first failure.
    x_reindexed,   # expression used to re-index x
    x_renamed,     # expression used to re-name x
    env            # environment where the <check> is defined
) {

  # TODO: Add internal errors for bad inputs

  # E.g. `purrr::map_lgl(x, \(x) is.integer(x) && 1L == length(x))`
  vectorized_test <- rlang::call2(
    .fn = mapper,
    rlang::expr(x),
    rlang::expr(\(x) { !!remove_t_assignments(test) })
  )

  test_results <- find_symbols(test, pattern = the$t_symbol_pattern)
  error_arguments <- error_arguments(
    error_fmls_nms = c(
      "error_header",
      "error_bullets",
      "error_class",
      "error_dots",
      "error_fast"
    ),
    fixed_fmls = fixed_fmls
  )
  slow_error_header <- slow_error_header(
    default_error_header = header,
    fixed_fmls_nms = rlang::names2(fixed_fmls)
  )
  slow_error_bullets <- slow_error_bullets(
    default_error_bullets = bullets,
    fixed_fmls_nms = rlang::names2(fixed_fmls)
  )

  body <- rlang::expr({
    if (all(.t0. <- !!vectorized_test)) {
      return(!!return_value)
    }
    if (!!error_arguments$error_fast) {
      checkwriter::checkwriter_abort(
        header = !!error_arguments$error_header,
        bullets = !!error_arguments$error_bullets,
        error_class = !!error_arguments$error_class,
        error_call = error_call,
        error_dots = !!error_arguments$error_dots
      )
    }

    # Force `x_name = rlang::caller_arg(x)` to evaluate before we re-assign `x`,
    # so that the original `x` expression is used as the name. Otherwise, the
    # `x` in `caller_arg(x)` is the new `x` due to lazy eval of `caller_arg(x)`.
    force(x_name)

    # We re-set `x` to the value which caused the first failure of the
    # `vectorized_test` and then re-run the `test` in terms of that `x` to
    # record which portions of the un-vectorized `test` failed.
    x <- !!x_reindexed
    x_name <- !!x_renamed
    !!test

    checkwriter::bind_test_results(!!!test_results)
    checkwriter::checkwriter_abort(
      header = !!slow_error_header,
      bullets = !!slow_error_bullets,
      error_class = !!error_arguments$error_class,
      error_call = error_call,
      error_dots = !!error_arguments$error_dots
    )
  })

  out <- rlang::new_function(
    args = fmls,
    body = body,
    env = env
  )
  class(out) <- c(
    "checkwriter_vectorized_check",
    "checkwriter_check",
    "checkwriter_fun",
    "function"
  )
  out
}

# Helpers ----------------------------------------------------------------------

error_arguments <- function(error_fmls_nms, fixed_fmls) {
  # If the required `error_*` formals appear in `fmls`, then we insert them as
  # a symbol within the <check>. Otherwise, if they've been fixed  (i.e. appear
  # in `fixed_fmls`) then we insert the fixed expression where the `error_*`
  # symbol would otherwise appear within the <check>.
  fixed_fmls_nms <- rlang::names2(fixed_fmls)
  append(
    # If the `error_*` argument has been fixed, use it's fixed value
    fixed_fmls[fixed_fmls_nms %in% error_fmls_nms],
    # Otherwise, insert the `error_*` argument as a symbol
    symbol_dict(setdiff(error_fmls_nms, fixed_fmls_nms))
  )
}

slow_error_header <- function(default_error_header, fixed_fmls_nms) {
  # If `error_header` has not been fixed, then the slow `checkwriter_abort()`
  # call should allow the caller of the <check> to supply their own header
  # `error_header` (NULL by default) before using the `default_error_header`
  if ("error_header" %in% fixed_fmls_nms) {
    default_error_header
  } else {
    rlang::expr(error_header %||% !!default_error_header)
  }
}

slow_error_bullets <- function(default_error_bullets, fixed_fmls_nms) {
  # See `slow_error_header()` for an explanation
  if ("error_bullets" %in% fixed_fmls_nms) {
    default_error_bullets
  } else {
    rlang::expr(error_bullets %||% !!default_error_bullets)
  }
}

# internal ---------------------------------------------------------------------

# These are functions which appear internally within a generated <check>.

# Bind missing (AKA untested) `.t*` results in the parent environment to `NULL`.
#'@export
bind_test_results <- function(...) {
  env <- rlang::caller_env()
  test_nms <- as.character(rlang::exprs(...))

  untested_at <- !rlang::env_has(env = env, nms = test_nms, inherit = FALSE)
  tests_to_bind <- vector("list", sum(untested_at))
  names(tests_to_bind) <- test_nms[untested_at]

  rlang::env_bind(env, !!!tests_to_bind)
}

# Unbind any `.t*.` test results from the parent environment.
#'@export
unbind_test_results <- function() {
  env <- rlang::caller_env()
  env_names <- rlang::env_names(env)
  rlang::env_unbind(
    env = env,
    nms = grep(the$t_symbol_pattern, env_names, value = TRUE),
    inherit = FALSE
  )
}

# Manage bullets used in the `check()` functions. Sets the default bullets for
# `pass`, `fail`, and `untested` if they're not named.
#'@export
switch_bullets <- function(
    result,
    pass = character(),
    fail = character(),
    untested = character()
) {

  if (is.null(result)) {
    if (!rlang::is_named(untested)) {
      names(untested) <- rep("i", length(untested))
    }
    return(untested)
  }
  if (result) {
    if (!rlang::is_named(pass)) {
      names(pass) <- rep("v", length(pass))
    }
    return(pass)
  }
  if (!rlang::is_named(fail)) {
    names(fail) <- rep("x", length(fail))
  }
  fail
}

# Custom `abort()` function
#'@export
checkwriter_abort <- function(
    header,
    bullets,
    error_class = character(),
    error_call = rlang::caller_env(),
    error_dots = list()
) {
  .envir <- rlang::caller_env()
  cli::cli_abort(
    message = c(header, bullets),
    class = c(the$errors$check_failure, error_class),
    !!!error_dots,
    call = error_call,
    .envir = .envir
  )
}
