# purrr stand-ins --------------------------------------------------------------

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_typed <- function(.x, .f, .type, ...) {
  out <- vapply(.x, .f, .type, ...)
  names(out) <- names(.x)
  out
}

map_chr <- function(.x, .f, ...) {
  map_typed(.x, .f, character(1), ...)
}

map_lgl <- function(.x, .f, ...) {
  map_typed(.x, .f, logical(1), ...)
}

# TODO: Confirm whether or not we ever use `map2()` and whether we really care
#       about the names thing.
map2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    rlang::set_names(out, names(.x))
  } else {
    rlang::set_names(out, NULL)
  }
}

map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}

# rlang stand-ins --------------------------------------------------------------

# All credit to {rlang}, these functions are nearly identical to `rlang::fn_fmls()`
# and `rlang::fn_body()`. I'm re-implementing them here for two reasons:
#
# 1. To skip their input validation (my inputs come validated at a higher level)
# 2. `rlang::fn_fmls<-` doesn't yet remove the `srcref` attribute for printing:
#    https://github.com/r-lib/rlang/issues/1662

fun_body <- function(fun) {
  body <- body(fun)
  if (rlang::is_call(body, "{")) {
    body
  }
  else {
    call("{", body)
  }
}

`fun_body<-` <- function(fun, value) {
  attrs <- attributes(fun)
  attrs$srcref <- NULL
  body(fun) <- value
  attributes(fun) <- attrs
  fun
}

fun_fmls <- function(fun) {
  formals(fun)
}

`fun_fmls<-` <- function(fun, value) {
  attrs <- attributes(fun)
  attrs$srcref <- NULL
  formals(fun) <- value
  attributes(fun) <- attrs
  fun
}

# misc -------------------------------------------------------------------------

# `rlang::is_expression()` alone is more strict than I want in some cases. Try
# `rlang::is_expression(quote(\(x) { x }))`, `rlang::is_call(quote(\(x) { x }))`
is_expr <- function(x) {
  rlang::is_expression(x) || rlang::is_call(x)
}

is_symbolish <- function(x) {
  rlang::is_symbol(x) || (rlang::is_string(x) && nchar(x) >= 1)
}

symbol_dict <- function(x) {
  rlang::set_names(rlang::syms(x), nm = x)
}

is_empty <- function(x) {
  length(x) == 0
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

`%notin%` <- Negate(`%in%`)
