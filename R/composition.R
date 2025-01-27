# TODO: Implement, I have mostly pseudo code.
and_checks <- function(..., .header = NULL, .env = rlang::caller_env()) {

  checks <- check_list_of_checks(rlang::list2(...))

  tests <- map(checks, get_test)
  bullets <- map(checks, get_bullets)
  return_values <- map(checks, get_return_value)

  # We'll need to re-map the symbols `.t*.` such that they're distinct. Every
  # test has symbols starting from `.t1.`, `.t2.`, ... etc. so to ensure they
  # are distinct between tests we increment the existing t-symbols by the number
  # of t-symbols encountered in all previous tests.
  t_syms_counts <- lengths(map(tests, find_symbols, pattern = "\\.t[0-9]+\\."))
  t_syms_increment <- cumsum(t_syms_counts[-1L])

  # We can skip the first test, since it's t-symbols can remain as `.t1.`, `.t2.`,
  # ... etc.
  i <- 2
  while (i <= length(checks)) {
    increment_by <- t_syms_increment[[i]]
    t_syms_count <- t_syms_counts[[i]]
    t_syms_map <- rlang::set_names(
      paste0(".t", seq(t_syms_count) + increment_by, "."),
      paste0(".t", seq(t_syms_count), ".")
    )
    # Only the test and bullets of a <check> depend on t-symbols
    tests[[i]] <- rename_symbols(tests[[i]], !!!t_syms_map)
    bullets[[i]] <- rename_symbols(bullets[[i]], !!!t_syms_map)
  }

  # TODO: We need to define `expr_and()`. It's like the `control-flow.R` function
  #       `%and%` but it can receive dots to `&&` together multiple tests.
  test <- rlang::call2("expr_and", !!!tests)

  # TODO: We should be able to just `c()` all of the bullets (since they return
  #       characters), but we might want a bullet managing function which we
  #       have finer control over.
  bullet <- rlang::call2("c", !!!bullets)

  # The return values of each check are either just `x` or a named list of the
  # form `list(x = x, y = y, ..., z = z)` returning every check value which
  # doesn't have a default. I'll punt the implmentation.
  return_value <- combine_return_values(return_values)

  # Now we re-make the <check> with the combined tests and bullets. We'll want
  # to either prevent slow, fast, and general checks from being mixed together,
  # OR we default to whichever is the most minimal in order from:
  # - slow
  # - fast
  # - general
  #
  # A slow <check> won't even need bullets and a fast <check> doesn't need a
  # `error_fast` formal or `if (error_fast) {}` code block.
  #
  # Issue a warning if the function was provided a mix of slow, fast, and
  # general checks. Explain which check type we're defaulting to.

  # We'll have to resolve combining the formals. This involves:
  # - preserving the order of supplied test formals, starting with `x`
  # - doing the same for `_name` formals. We should take the broadest
  #   possible set of these to avoid hanging any dependent `*_name` variable
  #   in the body of any <check>.
  # - taking the set of `error_*` functions corresponding to the type
  #   of <check> (slow, fast, or general) that we're creating
  #
  # We'll also need to resolve any conflicts in the default values of
  # same-named formals. We can do this by confirming that all same-named
  # test formals have identical expressions as their default values.
  #
  # Ex. Good: `len = NULL, len = NULL`. Bad: `len = NULL, len = Inf`.
  #
  # Conflicts in the `error_*` formals should be resolvable with some rules,
  # but maybe we can just be very strict.

  # I'm punting the actual implementation of combining the formals
  fmls <- combine_check_formals(checks)

  # Get the full set of t-symbols used in the <check>. We could also just
  # grab the t-symbols from the `test`, that way a `new_check()` doesn't need
  # to be explicitly passed a set of `t_syms`.
  t_syms <- rlang::syms(paste0(".t", seq(sum(t_syms_counts)), "."))

  # Now we create a new function with the appropriate body. We'll suppose this
  # is a general <check> for now, but we can easily implement each as it's own
  # `new_fast_check()`, `new_slow_check()`, `new_check()` function.
  #
  # Recall that we can use `rlang::expr` as a template and splice in any
  # custom expressions.
  body <- rlang::expr({
    if (!!test) {
      return(!!return_value)
    }

    if (error_fast) {
      checkwriter::checkwriter_abort(...) # insert args
    }

    bind_test_results(!!t_syms)
    checkwriter::checkwriter_abort(
      header = error_header %||% !!.header,
      bullets = error_bullets %||% !!bullet,
      ... # insert args
    )
  })

  # Finally, assemble the function. This will also happen within `new_check()`.
  out <- rlang::new_function(
    args = fmls,
    body = body,
    env = .env
  )
  class(out) <- c(
    "checkwriter_check",
    "checkwriter_fun",
    "function"
  )
  out
}
