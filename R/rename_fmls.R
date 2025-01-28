rename_fmls <- function(.fun, ...) {
  UseMethod("rename_fmls")
}

rename_fmls.checkwriter_test <- function(.fun, ...) {
  assert_closure(.fun)
  c(old_fmls_names, new_fmls_names) %<-% validate_rename_fmls_dots(.fun, ...)

  # Sequentially replacing formals (instead of doing it all at once) so that the
  # symbols (the formals) are predictably overwritten in the order that they are
  # provided to `...`.
  for (i in seq_along(new_fmls_names)) {
    old_name <- old_fmls_names[[i]]
    new_name <- new_fmls_names[[i]]
    old_fml <- rlang::sym(old_name)
    new_fml <- rlang::sym(new_name)
    old_test <- get_test(.fun)

    .fun <- .fun |>
      rename_formal(old_name, new_name) |>
      replace_symbol_in_formals(old_fml, new_fml) |>
      set_test(replace_symbol(old_test, old_fml, new_fml))
  }
  .fun
}

rename_fmls.checkwriter_check <- function(.fun, ...) {
  assert_closure(.fun)
  c(old_fmls_names, new_fmls_names) %<-% validate_rename_fmls_dots(.fun, ...)

  for (i in seq_along(new_fmls_names)) {
    old_name <- old_fmls_names[[i]]
    new_name <- new_fmls_names[[i]]
    old_fml <- rlang::sym(old_name)
    new_fml <- rlang::sym(new_name)

    old_test <- get_test(.fun)
    old_return <- get_return_value(.fun)
    old_header <- get_default_header(.fun)
    old_bullets <- get_default_bullets(.fun)

    .fun <- .fun |>
      rename_formal(old_name, new_name) |>
      replace_symbol_in_formals(old_fml, new_fml) |>
      set_test(replace_symbol(old_test, old_fml, new_fml)) |>
      set_return_value(replace_symbol(old_return, old_fml, new_fml)) |>
      set_default_header(replace_symbol(old_header, old_fml, new_fml)) |>
      set_default_bullets(replace_symbol(old_bullets, old_fml, new_fml))
  }
  .fun
}

# TODO: The way to go is create one MEGA validation function that checks for any
# input into `update_fmls()` and unpacks them appropriately into one of:
# - rename
# - re-default
# - fix/remove
# - a helper function which expands into one of the above ^ options
# We'll standardize the output into something that rename_fmls, fix_fmls, replace_fmls
# can recognize and THEN feed it into the appropriate section.
#
# TODO: We'll want to split apart the various bullet generation for each bad `...`
# input for use in other functions like `fix_fmls()` and `update_fmls()`.
validate_rename_fmls_dots <- function(
    .fun,
    ...,
    .fun_name = rlang::caller_arg(.fun),
    .error_call = rlang::caller_env()
) {
  dots <- rlang::exprs(...)
  dots_names <- rlang::names2(dots)

  duplicated_at <- duplicated(dots_names)
  if (any(duplicated_at)) {
    cli::cli_abort(
      message = c(
        "Argument names supplied to `...` must be unique..",
        x = "Argument name{?s} {.arg {dots_names[duplicated_at]}} {?are/is} duplicated."
      ),
      call = .error_call,
      class = the$errors$invalid_argument
    )
  }

  bullets <- character()
  unnamed_at <- dots_names == ""
  if (any(unnamed_at)) {
    bullets <- c(
      bullets,
      x = "`...` contains unnamed arguments {at_loc_friendly(unnamed_at)}."
    )
  }
  for (i in seq_along(dots)) {
    if (rlang::is_missing(dots[[i]])) {
      bullets <- c(
        bullets,
        x = paste0("Argument {.arg ", dots_names[[i]], "} at {.arg ..", i, "} is missing.")
      )
    }
    else if (!is_symbolish(dots[[i]])) {
      bullets <- c(
        bullets,
        x = paste0(
          "Argument {.arg ", dots_names[[i]], "} at {.arg ..", i, "} ",
          "is {.obj_type_friendly {dots[[", i, "]]}}."
        )
      )
    }
  }
  if (!is_empty(bullets)) {
    cli::cli_abort(
      message = c(
        "Arguments supplied to `...` must be named symbols or non-empty strings.",
        truncate_bullets(bullets)
      ),
      call = .error_call,
      class = the$errors$invalid_argument
    )
  }

  old_fmls_names <- unname(as_strings(dots))
  new_fmls_names <- dots_names

  nomatch_at <- old_fmls_names %notin% rlang::fn_fmls_names(.fun)
  if (any(nomatch_at)) {
    cli::cli_abort(
      message = c(
        "Can't rename formals that aren't in {.arg {(.fun_name)}}.",
        x = paste(
          "{.arg {(.fun_name)}} doesn't have {?a/the} formal argument{?s}",
          "{.arg {old_fmls_names[nomatch_at]}}."
        )
      ),
      call = .error_call,
      class = the$errors$invalid_argument
    )
  }
  if (any(c("x", "x_name") %in% old_fmls_names)) {
    cli::cli_abort(
      message = c(
        "Formals `x` and `x_name` in function {.arg {(.fun_name)}} must not be renamed.",
        x = if ("x" %in% old_fmls_names) "Attempted to rename formal {.arg x}.",
        x = if ("x_name" %in% old_fmls_names) "Attempted to rename formal {.arg x_name}."
      ),
      call = .error_call,
      class = the$errors$invalid_argument
    )
  }

  # If the caller attempts to rename a formal `arg_name` when `arg` exists, have
  # them explicitly rename `arg` instead, which will cause the corresponding
  # `arg_name` to be renamed automatically.
  arg_name_fmls_names <- grep("_name$", old_fmls_names, value = TRUE)
  if (!is_empty(arg_name_fmls_names)) {
    arg_fmls_names <- gsub("_name$", "", arg_name_fmls_names)
    if (any(arg_fmls_names %in% rlang::fn_fmls_names(.fun))) {
      cli::cli_abort(
        message = c(
          "Formals {.arg arg_name} cannot be renamed when the corresponding formal, {.arg arg}, exists.",
          x = "Attempted to rename formal {.arg {arg_name_fmls_names[[1]]}}.",
          i = "Rename the formal {.arg {arg_fmls_names[[1]]}} instead."
        ),
        call = .error_call,
        class = the$errors$invalid_argument
      )
    }
  }
  # Automatically rename `arg_name` if `arg` is being renamed and the
  # corresponding `arg_name` is an existing formal in `.fun`.
  arg_fmls_at <- paste0(old_fmls_names, "_name") %in% rlang::fn_fmls_names(.fun)
  if (any(arg_fmls_at)) {
    old_arg_fmls <- old_fmls_names[arg_fmls_at]
    old_arg_name_fmls <- paste0(old_arg_fmls, "_name")
    new_arg_name_fmls <- paste0(new_fmls_names[arg_fmls_at], "_name")

    # Insert each `arg_name` formal after the corresponding `arg` formal in the
    # `old_fmls_names`. Insert the corresponding `new_arg_name` after the `new_arg`
    # formal in `new_fmls_names`. We need to adjust `arg_fml_at` in each iteration
    # since we're changing the lengths of `old_fmls_names` and `new_fmls_names`.
    for (i in seq_along(old_arg_fmls)) {
      arg_fml_at <- which(old_arg_fmls[[i]] == old_fmls_names)
      old_fmls_names <- append(old_fmls_names, old_arg_name_fmls[[i]], after = arg_fml_at)
      new_fmls_names <- append(new_fmls_names, new_arg_name_fmls[[i]], after = arg_fml_at)
    }
  }

  list(
    old_fmls_names = old_fmls_names,
    new_fmls_names = new_fmls_names
  )
}
