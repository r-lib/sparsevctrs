validate_positions <- function(
  positions,
  length,
  len_values,
  call = rlang::caller_env()
) {
  if (!is.numeric(positions)) {
    cli::cli_abort(
      "{.arg positions} must be a integer vector, \\
      not {.obj_type_friendly {positions}}.",
      call = call
    )
  }

  if (any(is.infinite(positions))) {
    offenders <- which(is.infinite(positions))
    cli::cli_abort(
      c(
        x = "{.arg positions} must not contain infinite values.",
        i = "Infinite values at index: {offenders}."
      ),
      call = call
    )
  }

  if (any(is.nan(positions))) {
    offenders <- which(is.nan(positions))
    cli::cli_abort(
      c(
        x = "{.arg positions} must not contain NaN values.",
        i = "NaN values at index: {offenders}."
      ),
      call = call
    )
  }

  if (!is.integer(positions)) {
    if (any(round(positions) != positions, na.rm = TRUE)) {
      offenders <- which(round(positions) != positions)

      cli::cli_abort(
        c(
          x = "{.arg positions} must contain integer values.",
          i = "Non-integer values at index: {offenders}."
        ),
        call = call
      )
    }
  }

  len_positions <- length(positions)

  if (len_values != len_positions) {
    cli::cli_abort(
      "{.arg value} ({len_values}) and {.arg positions} ({len_positions}) \\
      must have the same length.",
      call = call
    )
  }

  if (anyDuplicated(positions) > 0) {
    offenders <- which(duplicated(positions))

    cli::cli_abort(
      c(
        x = "{.arg positions} must not contain any duplicate values.",
        i = "Duplicate values at index: {offenders}."
      ),
      call = call
    )
  }

  if (is.unsorted(positions)) {
    cli::cli_abort(
      "{.arg positions} must be sorted in increasing order.",
      call = call
    )
  }

  if (len_positions > 0 && max(positions) > length) {
    offenders <- which(positions > length)
    cli::cli_abort(
      c(
        x = "{.arg positions} value must not be larger than {.arg length}.",
        i = "Offending values at index: {offenders}."
      ),
      call = call
    )
  }

  if (len_positions > 0 && min(positions) < 1) {
    offenders <- which(positions < 1)
    cli::cli_abort(
      c(
        x = "{.arg positions} value must positive.",
        i = "Non-positive values at index: {offenders}."
      ),
      call = call
    )
  }
}

validate_values_double <- function(values, call = rlang::caller_env()) {
  if (!is.numeric(values)) {
    cli::cli_abort(
      "{.arg values} must be a numeric vector, \\
      not {.obj_type_friendly {values}}.",
      call = call
    )
  }

  if (any(is.infinite(values))) {
    offenders <- which(is.infinite(values))
    cli::cli_abort(
      c(
        x = "{.arg values} must not contain infinite values.",
        i = "Infinite values at index: {offenders}."
      ),
      call = call
    )
  }

  if (any(is.nan(values))) {
    offenders <- which(is.nan(values))
    cli::cli_abort(
      c(
        x = "{.arg values} must not contain NaN values.",
        i = "NaN values at index: {offenders}."
      ),
      call = call
    )
  }
}

validate_values_integer <- function(values, call = rlang::caller_env()) {
  values <- vctrs::vec_cast(values, integer())

  if (!is.integer(values)) {
    cli::cli_abort(
      "{.arg values} must be a integer vector, \\
      not {.obj_type_friendly {values}}.",
      call = call
    )
  }
}

validate_values_logical <- function(values, call = rlang::caller_env()) {
  if (!is.logical(values)) {
    cli::cli_abort(
      "{.arg values} must be a logical vector, \\
      not {.obj_type_friendly {values}}.",
      call = call
    )
  }
}

validate_length <- function(length, call = rlang::caller_env()) {
  check_number_whole(length, min = 0, call = call)
  if (length > .Machine$integer.max) {
    cli::cli_abort(
      "{.arg length} must be less than {(.Machine$integer.max)}, not {length}.",
      call = call
    )
  }
}
