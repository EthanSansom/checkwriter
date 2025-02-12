checkwriter
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

{checkwriter} is a package for writing and composing fast object tests
and checks with
[Tidyverse-style-guide-compliant](https://style.tidyverse.org/) error
messages. {checkwriter} provides the following core functions:

- `test()`, generate a test function which returns `TRUE` or `FALSE`
- `check()`, generate a check function which returns it’s input if that
  input passes a `test()` and emits an error otherwise
- `and_checks()`, combine several `check()` functions using `&&`. Inputs
  must pass every `test()` associated with the combined `check()`
  functions
- `or_checks()`, combine several `check()` functions using `||`. Inputs
  must pass at least one `test()` associated with the combined `check()`
  functions
- `vectorize_check()`, convert a scalar `check()` function to a
  vectorized `check()`

## Installation

⚠️ This package is still under construction. ⚠️

You can install the development version of checkwriter from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EthanSansom/checkwriter")
```

## Features

These are mostly un-implemented as of yet. Shown below are un-run
examples which showcase the {checkwriter} interface.

A `<checkwriter_test>` is a function which returns `TRUE` or `FALSE`.

``` r
test_is_integer <- test(is.integer(x))
test_is_integer(10L) # TRUE
test_is_integer("A") # FALSE
```

The first argument of a `<checkwriter_test>` functions is always `x`,
the object to be tested. Other required test arguments can be supplied
to `...`.

``` r
# Test whether `x` is length `len`. `null_or()` returns `NULL` if it's first 
# argument is `NULL`. This means that, by default, `test_length(x)` always 
# returns `TRUE`.
test_length <- test(null_or(len, len == length(x)), len = NULL)

test_length(1:5) # TRUE
test_length(1:5, len = 5L) # TRUE
test_length(1:5, len = 2L) # FALSE
```

A `<checkwriter_check>` is a function which returns its input if a test
is passed and emits an error otherwise.

``` r
check_is_integer <- check(
    test = test_is_integer,
    header = "{.arg {x_name}} must be an integer.",
    bullets = "{.arg {x_name}} is {.obj_type_friendly {x}}."
)

check_is_integer(10L)
#> [1] 10
try(check_is_integer("A"))
#> Error:
#> ! `"A"` must be an integer.
#> ✖ `"A"` is a string.
```

Note that we build up a `check()` function using a `test()`. The
generated check inherits all of the arguments of its input test (e.g.
`len`). Additionally, `check()` generates an argument
`arg_name = rlang::caller_arg(arg)` for each of it’s test arguments.
These names can then be used in error messages.

``` r
check_length <- check(
    .test = test_length,
    .header = "{.arg {x_name}} must be length {len}.",
    .bullets = "{.arg {x_name}} is length {length(x)}."
)

check_length(1:5, len = 5, x_name = "x")
#> [1] 1 2 3 4 5
try(check_length(1:5, len = 2L, x_name = "x"))
#> Error:
#> ! `x` must be length 2.
#> ✖ `x` is length 5.
```

Several checks can be composed with `and_checks()` to make a new
`<checkwriter_check>` function. By default, checks composed using
`and_checks()` emit a message with the error `bullets` associated with
the first failed test (e.g. `is.integer` or `len == length(x)`).

``` r
check_int <- and_checks(
  # If we're not checking the length `len` don't mention 
  # it in the error message.
    .header = if_null_else(
        len,
        "{.arg {x_name}} must be an integer.",
        "{.arg {x_name}} must be an integer of length {len}."
    ),
    check_is_integer,
    check_length
)

check_int(10L, len = 1L)
#> [1] 10
try(check_int(c(1L, 2L), len = 5L, x_name = "x"))
#> Error:
#> ! `x` must be an integer of length 5.
#> ✖ `x` is length 2.
try(check_int("B"))
#> Error:
#> ! `"B"` must be an integer.
#> ✖ `"B"` is a string.
```
