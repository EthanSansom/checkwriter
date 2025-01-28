# check ------------------------------------------------------------------------

# TODO: Implement and see TODO below:
check_opts <- function(
    # Are my return values potentially missing?
    maybe_missing = FALSE,

    # Which `error_*` formals (besides `error_call`) should we keep
    keep_error_fmls = c("error_dots", "error_class", "error_header", "error_bullets"),

    # Which `arg_name` formals should we keep (besides `x_name`).
    # If `NULL`, keeps all of them.
    keep_arg_name_fmls = NULL
  ) {
  stucture(
    list(
      maybe_missing = check_bool(maybe_missing),
      keep_error_fmls = check_subset(
        keep_error_fmls,
        set = c("error_dots", "error_class", "error_header", "error_bullets"),
        empty_ok = TRUE
      ),
      keep_arg_name_fmls = check_chr(
        keep_arg_name_fmls,
        null_ok = TRUE,
        any_na_ok = FALSE
      )
    ),
    class = c("checkwriter_check_opts", "list")
  )
}

check <- function(
    test,
    header,
    bullets,
    env = rlang::caller_env(),
    # TODO: Make a `check_opts()` function which produces a <checkwriter_check_opts>
    # class object. That way you can save a default `my_check_opts = check_opts(...)`
    # object and pass it around
    #
    # TODO: See how many of these accumulate, you might be able to remove them
    opts = list(maybe_missing = FALSE)
  ) {

  # TODO: We'll want a fast way to provide default `header` and `bullets` by
  # capturing the `test` expression and using that somehow. Look at nice ways
  # to format expressions in {rlang}. Do similar error messages to the
  # defaults for `stopifnot()`.

  # TODO: Validate `opts`

  # TODO: Might want to try_fetch here in case either of these was spliced in
  # and explodes for `header` and `bullets`.
  test <- check_is_test(test)
  header <- rlang::enexpr(header)
  bullets <- rlang::enexpr(bullets)
  env <- check_is_env(env)
  caller_env <- rlang::caller_env()

  # Let's make the formals!
  test_fmls <- fun_fmls(test)
  name_fmls <- rlang::set_names(
    x = map(
      rlang::syms(names(test_fmls)),
      \(fml) rlang::expr(rlang::caller_arg(!!fml))
    ),
    nm = paste0(names(test_fmls), "_name")
  )
  error_fmls <- list(
    error_call = rlang::expr(rlang::caller_env()),
    error_header = NULL,
    error_bullets = NULL,
    error_class = NULL,
    error_fast = FALSE,
    error_dots = rlang::expr(list())
  )
  fmls <- rlang::pairlist2(
    !!!test_fmls,
    !!!name_fmls,
    !!!error_fmls
  )

  test <- rlang::expr(.t1. <- (!!get_test(test)))
  bullets <- if (is_checkwriter_call(bullets, switch_bullets, env = caller_env)) {
    rlang::call2(
      .fn = "internal_switch_bullets",
      .ns = "checkwriter",
      rlang::expr(.t1.),
      !!!rlang::call_args(bullets)
    )
  } else {
    rlang::expr(checkwriter::internal_switch_bullets(.t1., fail = !!bullets))
  }
  return_value <- new_return_value(test_fmls, maybe_missing = opts$maybe_missing)

  new_check(
    fmls = fmls,
    fixed_fmls = list(),
    test = test,
    return_value = return_value,
    header = header,
    bullets = bullets,
    env = env
  )
}

# TODO: Define the <check> class and sub-classes.

# TODO: Check {rlang} for how they implement some of their magic. The bottom
# of this file https://github.com/r-lib/rlang/blob/main/R/fn.R has some cool
# manipulation of primitive functions to take a look at.

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
    is.list(fixed_fmls) && (is_empty(fixed_fmls) || rlang::is_named(fixed_fmls)),
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

# vectorized check -------------------------------------------------------------

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

new_vectorized_fast_check <- function(
    fmls,
    fixed_fmls,
    test,
    return_value,
    bullets,
    header,
    mapper,
    x_reindexed,
    x_renamed,
    env
) {

  # Remove/re-set parts of a <vectorized_check> to make a <vectorized_fast_check>
  get_fast_abort_call <- function(x) { fun_body(x)[[3]][[3]][[2]] }
  set_last_abort_call <- function(x, expr) {
    fun_body(x)[[9]] <- expr
    x
  }
  remove_fast_if_block_and_retesting <- function(x) {
    fun_body(x)[3:8] <- NULL
    x
  }

  out <- new_vectorized_check(
    fmls = fmls,
    fixed_fmls = fixed_fmls,
    test = test,
    return_value = return_value,
    bullets = bullets,
    header = header,
    mapper = mapper,
    x_reindexed = x_reindexed,
    x_renamed = x_renamed,
    env = env
  )
  out <- set_last_abort_call(out, expr = get_fast_abort_call(out))
  out <- remove_fast_if_block_and_retesting(out)

  class(out) <- c("checkwriter_vectorized_fast_check", class(out))
  out
}

new_vectorized_slow_check <- function(
    fmls,
    fixed_fmls,
    test,
    return_value,
    bullets,
    header,
    mapper,
    x_reindexed,
    x_renamed,
    env
) {

  # Remove parts a <vectorized_check> to make a <vectorized_slow_check>
  remove_fast_if_block <- function(x) {
    fun_body(x)[[3]] <- NULL
    x
  }

  out <- new_vectorized_check(
    fmls = fmls,
    fixed_fmls = fixed_fmls,
    test = test,
    return_value = return_value,
    bullets = bullets,
    header = header,
    mapper = mapper,
    x_reindexed = x_reindexed,
    x_renamed = x_renamed,
    env = env
  )
  out <- set_last_abort_call(out, expr = get_fast_abort_call(out))
  out <- remove_fast_if_block(out)

  class(out) <- c("checkwriter_vectorized_slow_check", class(out))
  out
}

# semi-vectorized check --------------------------------------------------------

new_semi_vectorized_check <- function(
    fmls,          # pairlist of formals
    fixed_fmls,    # named list of fixed formals and their defaults
    test_novec,    # test expression (don't vectorize)
    test_vec,      # test expression (do vectorize)
    return_value,  # return value expression
    bullets_novec, # bullets expression for the un-vectorized test
    bullets_vec,   # bullets expression for the vectorized bullets
    header,        # header expression
    mapper,        # mapper function used for vectorization
    x_reindexed,   # expression used to re-index x
    x_renamed,     # expression used to re-name x
    env            # environment where the <check> is defined
) {

  # TODO: Add internal errors for bad inputs

  vectorized_test <- rlang::call2(
    .fn = mapper,
    rlang::expr(x),
    rlang::expr(\(x) { !!remove_t_assignments(test_vec) })
  )

  test_novec_results <- find_symbols(test_novec, pattern = the$t_symbol_pattern)
  test_vec_results <- find_symbols(test_vec, pattern = the$t_symbol_pattern)
  test_results <- append(test_novec_results, test_vec_results)

  # Within the `<semi_vectorized_check>`, we assign a copy of `bullets_novec` to
  # `unvectorized_bullets` *before* re-running the vectorized test with an
  # updated `x = x_reindexed` and `x_name = x_renamed`. This ensures, in the
  # event that the un-vectorized test passed and the vectorized test failed,
  # that any `pass` bullets from un-vectorized test refer to the correct `x` and
  # `x_name`. We later assign `bullets_vec` to `vectorized_bullets` so that the
  # `bullets_novec` and `bullets_vec` can be easily retrieved from an existing
  # <semi_vectorized_check>.
  bullets <- rlang::call2(
    .fn = "and_bullets",
    .ns = "checkwriter",
    # TODO: Add these to the list of reserved symbols.
    rlang::sym(".unvectorized_bullets."),
    rlang::sym(".vectorized_bullets.")
  )

  # In the case that we failed the un-vectorized test, we want to emit all
  # of `.unvectorized_bullets. <- bullets_novec` and any vectorized test `untested`
  # bullets in `bullets_vec`. If there are no `switch_bullets(untested = ...)` in
  # the vectorized bullets, we can emit them from these bullets.
  bullets_unvectorized_failure <- rlang::call2(
    .fn = "and_bullets",
    .ns = "checkwriter",
    rlang::sym(".unvectorized_bullets."),
    # TODO: Implement a `resolve_untested_bullets(bullets_vec)` to remove this
    # if there aren't any untested bullets to emit.
    bullets_vec
  )

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
  slow_error_bullets_novec <- slow_error_bullets(
    default_error_bullets = bullets_unvectorized_failure,
    fixed_fmls_nms = rlang::names2(fixed_fmls)
  )

  body <- rlang::expr({
    if ((!!test_novec) %and% all(.t0. <- !!vectorized_test)) {
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

    # We need to evaluate the un-vectorized bullets immediately to ensure that
    # they refer to the correct `x` and `x_name` prior to re-assignment of `x`
    # and `x_name` for the un-vectorized tests. This also makes it easy to
    # retrieve the vectorized and un-vectorized bullets from an existing check.
    .unvectorized_bullets. <- !!bullets_novec

    # If `.t0.` is undefined, then we never ran the vectorized test, so we
    # emit the un-vectorized test error.
    if (!rlang::env_has(".t0.")) {
      # We need to bind all test results, including the un-tested vectorized test
      # results, in case there are `switch_bullets(untested = ...)` bullets for
      # the vectorized tests.
      checkwriter::bind_test_results(!!!test_results)
      checkwriter::checkwriter_abort(
        header = !!slow_error_header,
        bullets = !!slow_error_bullets_novec,
        error_class = !!error_arguments$error_class,
        error_call = error_call,
        error_dots = !!error_arguments$error_dots
      )
    }

    # TODO: For error emission of the bullets to work correctly, we'll still
    #       have to make sure the `.t*.` symbols are unique between both the
    #       vectorized AND un-vectorized tests. This is because the default
    #       bullets can make use of the `.t*.` symbols and so we need to ensure
    #       that the bullets for vectorized tests don't refer to a result from
    #       an un-vectorized test. Notice that the `slow_error_bullets` which
    #       are emitted are the same for BOTH tests. In fact, we actually CAN'T
    #       unbind the old bullets, since we're allowing for `pass` hints.
    #
    # TLDR: I just realized that:
    # 1. We need to make sure that the vectorized AND un-vectorized tests need
    #    distinct `.t*` symbols, with the vectorized coming AFTER (in numeric
    #    order) the vectorized.

    force(x_name)
    x <- !!x_reindexed
    x_name <- !!x_renamed
    !!test_vec

    # We evaluate the vectorized bullets here so that they are easier to get
    # and set in existing <semi_vectorized_check>'s.
    .vectorized_bullets. <- !!bullets_vec

    # We only need to re-bind the vectorized test results, as these are the only
    # tests which we re-ran.
    checkwriter::bind_test_results(!!!test_vec_results)
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
    "checkwriter_semi_vectorized_check",
    "checkwriter_check",
    "checkwriter_fun",
    "function"
  )
  out
}

# TODO: Test <checkwriter_semi_vectorized_check>!
# TODO: <checkwriter_semi_vectorized_check> slow and fast versions

# Helpers ----------------------------------------------------------------------

new_return_value <- function(test_fmls, maybe_missing) {
  # If `x` is the only test formal with no default, then just return `x`
  no_default_fmls <- rlang::names2(keep(test_fmls, rlang::is_missing))
  if (identical(no_default_fmls, "x")) {
    return_value <- rlang::sym("x")
    if (maybe_missing) {
      return_value <- rlang::expr(rlang::maybe_missing(!!return_value))
    }
    return(return_value)
  }

  # If there are multiple no-default formals, then return a named list of
  # all such formals, e.g. `list(x = x, y = y, z = z)`.
  return_values <- symbol_dict(no_default_fmls)
  if (maybe_missing) {
    return_values <- map(
      return_values,
      \(value) rlang::expr(rlang::maybe_missing(!!value))
    )
  }
  rlang::expr(list(!!!return_values))
}


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

# user facing ------------------------------------------------------------------

# User facing helper to provide bullet options to `check()`
#'@export
switch_bullets <- function(pass, fail, untested) {
  # TODO: Update this error
  stop("Must be used within a call to `check()`.")
}

# internal ---------------------------------------------------------------------

# These are functions which appear internally within a generated <check>.

# Bind missing (AKA untested) `.t*` results in the parent environment to `NULL`
#'@export
bind_test_results <- function(...) {
  env <- rlang::caller_env()
  test_nms <- as.character(rlang::exprs(...))

  untested_at <- !rlang::env_has(env = env, nms = test_nms, inherit = FALSE)
  tests_to_bind <- vector("list", sum(untested_at))
  names(tests_to_bind) <- test_nms[untested_at]

  rlang::env_bind(env, !!!tests_to_bind)
}

# Unbind any `.t*.` test results from the parent environment
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
# `pass`, `fail`, and `untested` if they're not named
#'@export
internal_switch_bullets <- function(
    result,
    pass = character(),
    fail = character(),
    untested = character()
) {

  if (is.null(result)) {
    if (!rlang::is_named(untested)) {
      names(untested) <- rep("i", length(untested))
    }
    untested
  }
  else if (result) {
    if (!rlang::is_named(pass)) {
      names(pass) <- rep("v", length(pass))
    }
    pass
  }
  else {
    if (!rlang::is_named(fail)) {
      names(fail) <- rep("x", length(fail))
    }
    fail
  }
}

# TODO: Eventually this should use `friendlyr::friendlyr_abort()`
#
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
