# resolve_expr -----------------------------------------------------------------

# TODO: We'll want an alternate `cement_expr()` which just unpacks all of the
# function definitions of the test-control-flow functions (e.g. `%and%`, `null_or()`).
# - this will similarly create a `new_cementing_env()`

# TODO: Maybe use `resolution_functions <- as.namespace("checkmakers")`
# so that when I evaluate a namespaced `null_or` (e.g. `checkmakers::null_or`)
# in the resolution environment, it is still accessed correctly. I don't yet
# know if this will be an issue.

resolve_expr <- function(expr, env = empty_resolution_env()) {

  resolution_function_names <- rlang::env_names(the$resolution_functions)
  resolution_env <- new_resolution_env(parent = env)

  resolve_expr_recursive <- function(expr) {
    switch(
      expr_type(expr),

      # Base cases
      constant = expr,
      symbol = expr,

      # Recursive cases
      pairlist = as.pairlist(map(as.list(expr), resolve_expr_recursive)),
      call = {
        # Resolve the arguments of the call recursively
        expr <- as.call(append(expr[[1]], map(as.list(expr[-1]), resolve_expr_recursive)))

        # TODO: I think identifying calls like this is fragile, maybe make sure
        # that it matches one of the resolvable calls exactly?
        if (rlang::call_name(expr) %in% resolution_function_names) {
          # Evaluate the expression in a special environment where it will
          # any test-control-flow-functions will resolve themselves.
          expr <- eval(expr, resolution_env)
        }
        expr
      }
    )
  }
  resolve_expr_recursive(expr)
}

new_resolution_env <- function(parent = empty_resolution_env()) {
  rlang::env_clone(the$resolution_functions, parent = parent)
}

# TODO: We can probably add `!`, `&`, `&&`, `==`, `>=`, and more to the
#       basic resolution environment.
empty_resolution_env <- function() {
  rlang::new_environment(
    data = list(
      `<-` = base::`<-`,
      `(` = base::`(`
    )
  )
}

# control-flow functions -------------------------------------------------------

# These functions allow the definition of test functions which can be altered as
# our set of possible test-cases is reduced. Consider the following test:
#
# test_is_integer <- function(x, len = NULL, na_ok = TRUE) {
#   is.integer(x) && (is.null(len) || len == length(x)) && (na_ok || !anyNA(x))
# }
#
# `test_is_integer()` has test-cases for integers of length 1, 2, 3, ... etc.
# Suppose now that we partial out the `len` argument with `len = 1L`:
#
# test_is_scalar_integer <- purrr::partial(test_is_integer, len = 1L)
#
# Our test expression is now:
#
# is.integer(x) && (is.null(1L) || 1L == length(x)) && (na_ok || !anyNA(x))
#
# Annoyingly, we now test `is.null(1L)` every time `test_is_scalar_integer()` is
# called, even though we know that it always returns `TRUE`. Enter the control
# flow functions.
#
# test_is_integer <- function(x, len = NULL, na_ok = TRUE) {
#   is.integer(x) && null_or(len, len == length(x)) && true_or(na_ok, !anyNA(x))
# }
#
# When we finalize a partial-ed test function which contains any of these
# control flow functions, two things occur.
#
# 1. The control-flow function "resolves" itself by checking whether or not a
#    branch of the function is now redundant.
#
# In the `test_is_scalar_integer()` example, when we update `len = 1L`, we have
# the test expression `null_or(1L, 1L == length(x))`. `null_or()` recognizes
# that `1L` is not `NULL`, so we'll never enter the "null" branch, only the "or"
# branch. Our test expression thus resolves to:
#
# is.integer(x) && (1L == length(x)) && true_or(na_ok, !anyNA(x))
#
# 2. Each remaining control-flow function "cements" itself into it's underlying
#    {base} R function body to avoid being called.
#
# In `test_is_scalar_integer()`, we still don't know the value of `na_ok`, so
# the control-flow call `true_or(na_ok, !anyNA(x))` doesn't resolve. So,
# `true_or` instead cements iself into it's body `na_ok || !anyNA(x)`. Our
# test expression thus cements to:
#
# is.integer(x) && (1L == length(x)) && (na_ok || !anyNA(x))
#
# After "resolving" and "cementing" a function, there will never be a
# remaining control-flow function.

# How does this work?
#
# All control-flow functions have a cementing and a resolving version of
# themselves. When an expression is resolved (cemented) it is evaluated in
# a special environment where instead of evaluating the control-flow function
# call we evaluate it's resolving (cementing) counterpart instead, which returns
# a resolved (cemented) version of the control-flow call.

true_or <- function(x, or) x || or

the$cementing_functions$true_or <- function(x, or) {
  rlang::call2("||", rlang::enexpr(x), rlang::enexpr(or))
}

the$resolution_functions$true_or <- function(x, or) {
  x = rlang::enexpr(x)
  or = rlang::enexpr(or)
  env <- rlang::caller_env()
  x_resolved <- try_eval_fetch(x, env = env)

  if (isTRUE(x_resolved)) {
    TRUE
  } else if (isFALSE(x_resolved)) {
    or
  } else {
    rlang::call2("true_or", x = x, or = or)
  }
}

null_or <- function(x, or) is.null(x) || or

the$cementing_functions$null_or <- function(x, or) {
  # is.null(x) || or
  rlang::call2(
    "||",
    rlang::call2("is.null", rlang::enexpr(x), .ns = "base"),
    rlang::enexpr(or)
  )
}

the$resolution_functions$null_or <- function(x, or) {
  x <- rlang::enexpr(x)
  or <- rlang::enexpr(or)
  env <- rlang::caller_env()
  x_resolved <- try_eval_fetch(x, env = env)

  if (is.null(x_resolved)) {
    TRUE
  } else if (rlang::is_error(x_resolved)) {
    # TODO: Might need to add the namespace `.ns = "checkwriter"`
    rlang::call2("null_or", x = x, or = or)
  } else {
    or
  }
}

`%and%` <- function(lhs, rhs) lhs && rhs

the$cementing_functions$`%and%` <- function(lhs, rhs) {
  rlang::call2("&&", rlang::enexpr(lhs), rlang::enexpr(rhs))
}

# TODO: Resolve this to a call to `expr_and()`. Since `%and%` can't be namespaced
#       I'll only use it for internal implementations.
the$resolution_functions$`%and%` <- function(lhs, rhs) {
  lhs <- rlang::enexpr(lhs)
  rhs <- rlang::enexpr(rhs)
  env <- rlang::caller_env()
  lhs_resolved <- try_eval_fetch(lhs, env = env)
  rhs_resolved <- try_eval_fetch(rhs, env = env)

  lhs_state <- if (isTRUE(lhs_resolved)) {
    "true"
  } else if (isFALSE(lhs_resolved)) {
    "false"
  } else {
    "unknown"
  }
  rhs_state <- if (isTRUE(rhs_resolved)) {
    "true"
  } else if (isFALSE(rhs_resolved)) {
    "false"
  } else {
    "unknown"
  }
  state <- paste0("lhs_", lhs_state, "_rhs_", rhs_state)

  switch(
    state,
    lhs_false_rhs_false = ,
    lhs_false_rhs_true = ,
    lhs_false_rhs_unknown = ,
    lhs_true_rhs_false = FALSE,
    lhs_true_rhs_true = TRUE,
    lhs_true_rhs_unknown = rhs,
    lhs_unknown_rhs_false = FALSE,
    lhs_unknown_rhs_true = lhs,
    lhs_unknown_rhs_unknown = rlang::call2("%and%", lhs, rhs)
  )
}

# helpers ----------------------------------------------------------------------

# Evaluate an `expr` in the provided `env`, catching any error conditions
try_eval_fetch <- function(expr, env = rlang::caller_env()) {
  rlang::try_fetch(eval(expr, env), error = function(cnd) cnd)
}
