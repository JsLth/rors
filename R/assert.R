assert <- function(x,
                   class = NULL,
                   len = NULL,
                   val = NULL,
                   nrow = NULL,
                   obj = NULL,
                   set = NULL,
                   abort = TRUE) {
  checks <- list()

  if (!is.null(class)) {
    checks[[1]] <- assert_class(x, class, abort)
  }

  if (!is.null(len)) {
    if (length(len) == 1) len <- rep(len, 2)
    checks[[2]] <- assert_length(x, min = len[1], max = len[2], abort)
  }

  if (!is.null(val)) {
    if (length(val) == 1) val <- rep(val, 2)
    checks[[3]] <- assert_value(x, min = val[1], max = val[2], abort)
  }

  if (!is.null(nrow) && length(nrow) == 2) {
    if (length(nrow) == 1) nrow <- rep(nrow, 2)
    checks[[4]] <- assert_nrow(x, min = nrow[1], nrow[2], abort)
  }

  if (!is.null(obj)) {
    checks[[5]] <- assert_identical(x, obj, abort)
  }

  if (!is.null(set)) {
    checks[[6]] <- assert_element(x, set)
  }

  do.call(all, checks)
}


assert_class <- function(x, class, abort = TRUE) {
  afuns <- lapply(class, function(c) {
    get0(paste0("is.", c), ifnotfound = \(obj) inherits(obj, c))
  })

  has_class <- any(vapply(afuns, do.call, list(x), FUN.VALUE = logical(1)))

  if (!has_class) {
    if (!abort) {
      return(FALSE)
    }

    var <- deparse(substitute(x, env = parent.frame()))

    cli::cli_div(theme = list(span.cls2 = list(
      before = "<", after = ">", color = "blue",
      vec_sep = ", ", vec_last = " or "
    )))

    cli::cli_abort(c(
      "x" = "{.var {var}} is expected to be of class {.cls2 {class}}.",
      "i" = "Got {.cls {class(x)}} instead."
    ))
  } else {
    TRUE
  }
}


assert_length <- function(x, min = 0L, max = Inf, abort = TRUE) {
  if (length(x) > max || length(x) < min) {
    if (!abort) {
      return(FALSE)
    }
    var <- deparse(substitute(x, env = parent.frame()))
    if (min == max) {
      cli::cli_abort(c(
        "x" = "{.var {var}} is expected to be of length {.val {min}}",
        "i" = "Got object of length {.val {length(x)}} instead."
      ))
    } else {
      cli::cli_abort(c(
        "x" = paste(
          "{.var {var}} is expected to be between length",
          "{.val {min}} and {.val {max}}."
        ),
        "i" = "Got object of length {.val {length(x)}} instead."
      ))
    }
  } else {
    TRUE
  }
}


assert_value <- function(x, min = -Inf, max = Inf, abort = TRUE) {
  if (x > max || x < min) {
    if (!abort) {
      return(FALSE)
    }
    var <- deparse(substitute(x, env = parent.frame()))
    if (min == max) {
      cli::cli_abort(c(
        "x" = "{.var {var}} is expected to equal {.val {min}}",
        "i" = "Got value {.val {x}} instead."
      ))
    } else {
      cli::cli_abort(c(
        "x" = paste(
          "{.var {var}} is expected to fall",
          "between {.val {min}} and {.val {max}}"
        ),
        "i" = "Got value {.val {x}} instead."
      ))
    }
  } else {
    TRUE
  }
}


assert_nrow <- function(x, min = 0L, max = Inf, abort = TRUE) {
  if (nrow(x) > max || nrow(x) < min) {
    if (!abort) {
      return(FALSE)
    }
    var <- deparse(substitute(x, env = parent.frame()))
    if (min == max) {
      cli::cli_abort(c(
        "x" = "{.var {var}} is expected to have {.val {min}} rows.",
        "i" = "Got an object with {.val {nrow(x)}} rows instead."
      ))
    } else {
      cli::cli_abort(c(
        "x" = paste(
          "{.var {var}} is expected to have between",
          "{.val {min}} and {.val {max}} rows."
        ),
        "i" = "Got an object with {.val {nrow(x)}} rows instead."
      ))
    }
  } else {
    TRUE
  }
}


assert_identical <- function(x, obj, abort = TRUE) {
  if (!identical(x, obj)) {
    if (!abort) {
      return(FALSE)
    }
    var <- deparse(substitute(x, env = parent.frame()))
    obj_var <- deparse(substitute(obj, env = parent.frame()))
    cli::cli_abort(c(
      "x" = "{.var {var}} is expected to be identical to {.var {obj_var}}."
    ))
  } else {
    TRUE
  }
}


assert_element <- function(x, set, abort = TRUE) {
  if (!is.element(x, set)) {
    if (!abort) {
      return(FALSE)
    }
    var <- deparse(substitute(x, env = parent.frame()))
    set_var <- deparse(substitute(set, env = parent.frame()))
    cli::cli_abort(c(
      "x" = "{.var {var}} is expected to be an element of {.var {set_var}}"
    ))
  } else {
    TRUE
  }
}
