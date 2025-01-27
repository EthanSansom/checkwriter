# TODO: Check other packages for where stuff like this normally lives...
# {rlang} puts it here: https://github.com/r-lib/rlang/blob/main/R/aaa.R

# the --------------------------------------------------------------------------

the <- rlang::new_environment(
  list(
    resolution_functions = rlang::new_environment(),
    cementing_functions = rlang::new_environment(),
    errors = rlang::new_environment(
      list(
        invalid_argument = c("checkwriter_error", "checkwriter_invalid_arg_error"),
        check_failure = c("checkwriter_error", "checkwriter_check_fail_error")
      )
    ),
    # TODO: Reference this wherever you use the `t_symbol_pattern`
    t_symbol_pattern = "\\.t[0-9]+\\."
  )
)

# misc -------------------------------------------------------------------------

# TODO: This avoids the CRAN check associated with using zeallot::`%<-%`. I may
# just not use {zeallot}, we'll see. https://github.com/r-lib/zeallot/issues/57
utils::globalVariables(
  c(
    "old_fmls_names",
    "new_fmls_names"
  )
)
