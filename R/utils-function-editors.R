# formals ----------------------------------------------------------------------

# Rename a formal argument `old_name` to `new_name` in a function `fun`
rename_formal <- function(fun, old_name, new_name) {
  fmls <- fun_fmls(fun)
  names(fmls)[old_name == names(fmls)] <- new_name
  fun_fmls(fun) <- fmls
  fun
}

# Remove a formal argument `fml_name` from a function `fun`
remove_formal <- function(fun, fml_name) {
  fmls <- fun_fmls(fun)
  fmls[fml_name == names(fmls)] <- NULL
  fun_fmls(fun) <- fmls
  fun
}

# Change the default value of formal argument `fml_name` in a function `fun` to
# an expression `expr`. Set the default to missing if `expr` is missing.
redefault_formal <- function(fun, fml_name, expr) {
  if (rlang::is_missing(expr)) {
    fun_fmls(fun)[[fml_name]] <- rlang::missing_arg()
  }
  else {
    fun_fmls(fun)[[fml_name]] <- expr
  }
  fun
}

# Replace a `symbol` in the formals pairlist of `fun` with a `replacement`
# expression
replace_symbol_in_formals <- function(fun, symbol, replacement) {
  fmls <- fun_fmls(fun)
  fun_fmls(fun) <- replace_symbol(fmls, symbol, replacement)
  fun
}
