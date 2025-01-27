## -----------------------------------------------------------------------------
##
## Script: test-new-check.R
##
## Purpose: Test `new_check()`
##
## Date Created: 2025/01/26
##
## Author: ethansansom
##
## -----------------------------------------------------------------------------

load_all()

new_check |> formals() |> names()

check_is_integer <- new_check(
  fmls = rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_header = NULL,
    error_bullets = NULL,
    error_fast = FALSE,
    error_call = quote(rlang::caller_env())
  ),
  fixed_fmls = list(
    error_dots = list(),
    error_class = "custom_error"
  ),
  test = quote(.t1. <- is.integer(x)),
  return_value = quote(x),
  header = "{.arg {x_name}} must be an integer.",
  bullets = quote(c(x = "{.arg {x_name}} is {.obj_type_friendly {x}}.")),
  env = rlang::global_env()
)

check_is_integer(10L)
try(check_is_integer("A"))

check_is_integer_fast <- new_fast_check(
  fmls = rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_header = NULL,
    error_bullets = NULL,
    error_fast = FALSE,
    error_call = quote(rlang::caller_env())
  ),
  fixed_fmls = list(
    error_dots = list(),
    error_class = "custom_error"
  ),
  test = quote(.t1. <- is.integer(x)),
  return_value = quote(x),
  header = "{.arg {x_name}} must be an integer.",
  bullets = quote(c(x = "{.arg {x_name}} is {.obj_type_friendly {x}}.")),
  env = rlang::global_env()
)

check_is_integer_fast(10L)
try(check_is_integer_fast("A", error_header = "{.arg {x_name}} must be an integer."))

check_is_integer_slow <- new_slow_check(
  fmls = rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_header = NULL,
    error_bullets = NULL,
    error_fast = FALSE,
    error_call = quote(rlang::caller_env())
  ),
  fixed_fmls = list(
    error_dots = list(),
    error_class = "custom_error"
  ),
  test = quote(.t1. <- is.integer(x)),
  return_value = quote(x),
  header = "{.arg {x_name}} must be an integer.",
  bullets = quote(c(x = "{.arg {x_name}} is {.obj_type_friendly {x}}.")),
  env = rlang::global_env()
)

check_is_integer_slow(10L)
try(check_is_integer_slow("A"))

vec_check_is_integer <- new_vectorized_check(
  fmls = rlang::pairlist2(
    x = ,
    x_name = quote(rlang::caller_arg(x)),
    error_header = NULL,
    error_bullets = NULL,
    error_fast = FALSE,
    error_call = quote(rlang::caller_env())
  ),
  fixed_fmls = list(
    error_dots = list(),
    error_class = "custom_error"
  ),
  test = quote(.t1. <- is.integer(x)),
  return_value = quote(x),
  header = "{.arg {x_name}} must be an integer.",
  bullets = quote(c(x = "{.arg {x_name}} is {.obj_type_friendly {x}}.")),
  mapper = quote(map_lgl),
  x_reindexed = quote(x[[which.min(.t0.)]]),
  x_renamed = quote(paste0(x_name, "[[", which.min(.t0.), "]]")),
  env = rlang::global_env()
)
