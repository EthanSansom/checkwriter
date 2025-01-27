# Replace a `symbol` with an expression `replacement` in an expression `expr`.
replace_symbol <- function(expr, symbol, replacement) {
  switch(
    expr_type(expr),
    constant = expr,
    symbol = if (identical(expr, symbol)) replacement else expr,
    pairlist = as.pairlist(map(as.list(expr), replace_symbol)),
    call = {
      # Don't replace symbols which appear within a lambda. Do replace symbols
      # which appear as default arguments in the lambda's formals.
      if (identical(expr[[1]], quote(`function`))) {
        expr[[2]] <- replace_symbol(expr[[2]])
        expr
      } else {
        as.call(append(expr[[1]], map(as.list(expr[-1]), replace_symbol)))
      }
    }
  )
}

# TODO: I may never use this! Remove if so.
#
# Replace symbols in `expr` with an expression. `...` are named expressions,
# the names are the symbols to switch.
replace_symbols <- function(expr, ...) {

  symbols_dict <- rlang::list2(...)

  replace_symbols_recursive <- function(expr) {
    switch(
      expr_type(expr),
      # Base cases
      constant = expr,
      symbol = if (rlang::as_string(expr) %in% names(symbols_dict)) {
        symbols_dict[[rlang::as_string(expr)]]
      } else {
        expr
      },
      # Recursive cases
      pairlist = as.pairlist(map(as.list(expr), replace_symbols_recursive)),
      call = {
        # Don't replace symbols within an in-line function.
        # E.g. Given `function(x = a) { x*2 }` we might replace `a`, but we'd
        # ignore the `x` formal and the `x` in the body.
        if (identical(expr[[1]], quote(`function`))) {
          # `expr[[2]]` is a pairlist of the formals, we replace the defaults.
          expr[[2]] <- replace_symbols_recursive(expr[[2]])
          expr
        } else {
          # Replace symbols in the call argument
          as.call(append(expr[[1]], map(as.list(expr[-1]), replace_symbols_recursive)))
        }
      }
    )
  }
  replace_symbols_recursive(expr)
}

# Return symbols in an expression which match a `pattern` or are in `symbols`.
# Ignore symbols that appear as formals in, or the body of, a function definition.
find_symbols <- function(expr, symbols, pattern) {

  # TODO: Add some nicer error messages
  if (!is_expr(expr)) {
    cli::cli_abort(
      "`expr` must be a call, expression, or symbol, not {.obj_type_friendly {expr}}."
    )
  }
  rlang::check_exclusive(pattern, symbols, .require = TRUE)
  if (!rlang::is_missing(pattern)) stopifnot(rlang::is_string(pattern))
  if (!rlang::is_missing(symbols)) stopifnot(is.character(symbols), !anyNA(symbols))

  # TODO: You could also do tail recursion with `function(expr, found_symbols = list())`
  # and just append to that list before the next call.
  #
  # Add symbols from `expr` to `found_symbols`
  found_symbols <- list()
  find_symbols_recursive <- function(expr) {
    switch(
      expr_type(expr),
      # Base Cases
      constant = NULL,
      symbol = { found_symbols <<- append(found_symbols, expr) },
      # Recursive Cases
      pairlist = map(as.list(expr), find_symbols_recursive),
      call = {
        if (identical(expr[[1]], quote(`function`))) {
          # `expr[[2]]` is a pairlist of the formals, we check for our symbols
          # in the defaults arguments (e.g. `TRUE` in \(x = TRUE) { x }).
          map(as.list(expr[[2]]), find_symbols_recursive)
        } else {
          # Find symbols used as arguments in the call
          map(as.list(expr[-1]), find_symbols_recursive)
        }
      }
    )
  }
  # Writes to `found_symbols` with the global assignment operator `<<-`
  find_symbols_recursive(expr)

  # Prune the unmatched symbols (if required)
  if (rlang::is_empty(found_symbols)) {
    list()
  } else if (!rlang::is_missing(pattern)) {
    symbol_names <- map_chr(found_symbols, rlang::as_string)
    found_symbols[grepl(x = found_symbols, pattern = pattern)]
  } else if (!rlang::is_missing(symbols)) {
    symbol_names <- map_chr(found_symbols, rlang::as_string)
    found_symbols[symbol_names %in% symbols]
  } else {
    found_symbols
  }
}

# Whether `expr` contains any symbols in `symbols` or which matching a `pattern`
contains_symbols <- function(expr, symbols, pattern) {
  length(find_symbols(expr, symbols, pattern)) > 0
}

# Replace expressions in `expr` of the form `.t*. <- <value>` with `<value>`
remove_t_assignments <- function(expr) {
  switch(
    expr_type(expr),
    constant = ,
    symbol = expr,
    pairlist = as.pairlist(map(as.list(expr), remove_t_assignments)),
    call = {
      if (identical(expr[[1]], quote(`<-`)) && is_t_symbol(expr[[2]])) {
        expr[[3]]
      }
      else {
        as.call(append(expr[[1]], map(as.list(expr[-1]), remove_t_assignments)))
      }
    }
  )
}

is_t_symbol <- function(x) {
  rlang::is_symbol(x) && grepl(the$t_symbol_pattern, x)
}

# Return the type of an `expr` as a string
expr_type <- function(expr) {
  if (rlang::is_syntactic_literal(expr)) {
    "constant"
  } else if (rlang::is_symbol(expr)) {
    "symbol"
  } else if (rlang::is_pairlist(expr)) {
    "pairlist"
  } else if (rlang::is_call(expr)) {
    "call"
  } else {
    typeof(expr)
  }
}
