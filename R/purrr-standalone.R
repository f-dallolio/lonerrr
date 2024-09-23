#' Apply a function to each element of a vector.
#' @name standalone-purrr-basic
NULL
#' @rdname standalone-purrr-basic
#' @export
map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}
#' @rdname standalone-purrr-basic
#' @export
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}
#' @rdname standalone-purrr-basic
#' @export
map_lgl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}
#' @rdname standalone-purrr-basic
#' @export
map_int <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, integer(1), ...)
}
#' @rdname standalone-purrr-basic
#' @export
map_dbl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, double(1), ...)
}
#' @rdname standalone-purrr-basic
#' @export
map_chr <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, character(1), ...)
}
#' @rdname standalone-purrr-basic
#' @export
map_vec <- function(.x, .f, ...) {
  list_simplify(map(.x, .f, ...))
}
.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = global_env())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

#' Map over two inputs
#' @name standalone-purrr-map2
NULL
#' @rdname standalone-purrr-map2
#' @export
map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}
#' @rdname standalone-purrr-map2
#' @export
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
#' @rdname standalone-purrr-map2
#' @export
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
#' @rdname standalone-purrr-map2
#' @export
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
#' @rdname standalone-purrr-map2
#' @export
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
#' @rdname standalone-purrr-map2
#' @export
map2_vec <- function(.x, .y, .f, ...) {
  list_simplify(map2(.x, .y, .f, ...))
}

#' Apply a function to each element of a vector, and its index.
#' @name standalone-purrr-imap
NULL
#' @rdname standalone-purrr-imap
#' @export
imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}
#' @rdname standalone-purrr-imap
#' @export
imap_vec <- function(.x, .f, ...) {
  list_simplify(imap(.x, .f, ...))
}

#' Map over multiple input simultaneously (in "parallel").
#' @name standalone-purrr-pmap
NULL
#' @rdname standalone-purrr-pmap
#' @export
pmap <- function(.l, .f, ...) {
  .f <- as.function(.f)
  args <- .rlang_purrr_args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}
#' @rdname standalone-purrr-pmap
#' @export
pmap_vec <- function(.l, .f, ...) {
  list_simplify(pmap(.l, .f, ...) )
}
.rlang_purrr_args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)
  
  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))
  
  args
}

#' Keep/discard elements based on their values.
#' @name standalone-purrr-keep
NULL
#' @rdname standalone-purrr-keep
#' @export
compact <- function(.x) {
  Filter(length, .x)
}
#' @rdname standalone-purrr-keep
#' @export
keep <- function(.x, .f, ...) {
  .x[.rlang_purrr_probe(.x, .f, ...)]
}
#' @rdname standalone-purrr-keep
#' @export
discard <- function(.x, .p, ...) {
  sel <- .rlang_purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
#' @rdname standalone-purrr-keep
#' @export

#' Apply a function to each element of a vector conditionally.
map_if <- function(.x, .p, .f, ..., .else) {
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x[!matches] <- map(.x[!matches], .else, ...)
  .x
}
.rlang_purrr_probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = global_env())
    map_lgl(.x, .p, ...)
  }
}

#' Modify elements selectively
#' @name standalone-purrr-modify
NULL
#' @rdname standalone-purrr-modify
#' @export
modify <- function(.x, .f, ...) {
  out <- map_vec(.x, .f, ...)
  vctrs::vec_cast(out, to = .x)
}
#' @rdname standalone-purrr-modify
#' @export
modify_if <- function(.x, .p, .f, ..., .else) {
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- modify(.x[matches], .f, ...)
  .x[!matches] <- modify(.x[!matches], .else, ...)
  .x
}

#' Combine list elements into a single data structure.
#' @name standalone-purrr-list-combine
NULL
#' @rdname standalone-purrr-list-combine
#' @export
list_c <- function(x) {
  inject(c(!!!x))
}
#' @rdname standalone-purrr-list-combine
#' @export
list_rbind <- function(x) {
  do.call(vec_rbind, x)
}
#' @rdname standalone-purrr-list-combine
#' @export
list_cbind <- function(x) {
  out <- do.call(vec_cbind, x)
  names(out) <- make.unique(names(out), sep = "_")
  out
}

#' Simplify a list to an atomic or S3 vector.
#' @name standalone-purrr-simplify
NULL
#' @rdname standalone-purrr-simplify
#' @export
list_simplify <- function(x) {
  if(is.list(x) && .can_simplify(x)) {
    nms <- names(x)
    x <- unlist(x, recursive = FALSE, use.names = FALSE)
    names(x) <- nms
  }
  x
}
.can_simplify <- function(x) {
  if(is.list(x)) {
    all(map_lgl(x, is.vector)) && all(lengths(x) == 1)
  } else {
    FALSE
  }
}

#' Transpose a list.
#' @name standalone-purrr-transpose
NULL
#' @rdname standalone-purrr-transpose
#' @export
list_transpose <- function(.l) {
  if (!length(.l)) {
    return(.l)
  }
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
    .l <- map(.l, function(x) {
      if (is.null(names(x))) {
        set_names(x, inner_names)
      } else {
        x
      }
    })
  }
  .l <- map(.l, as.list)
  map(fields, function(i) {
    list_simplify(map(.l, .subset2, i))
  })
}

#' Do every, some, or none of the elements of a list satisfy a predicate?
#' @name standalone-purrr-every
NULL
#' @rdname standalone-purrr-every
#' @export
every <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())
  
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
#' @rdname standalone-purrr-every
#' @export
some <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())
  
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
#' @rdname standalone-purrr-every
#' @export
none <- function(.x, .p, ...) {
  !every(.x, .p, ...)
}

#' Negate a predicate function so it selects what it previously rejected.
#' @name standalone-purrr-negate
NULL
#' @rdname standalone-purrr-negate
#' @export
negate <- function(.p) {
  .p <- as_function(.p, env = global_env())
  function(...) !.p(...)
}

#' Reduce a list to a single value by iteratively applying a binary function.
#' @name standalone-purrr-reduce
NULL
#' @rdname standalone-purrr-reduce
#' @export
reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
#' @rdname standalone-purrr-reduce
#' @export
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}

#' Accumulate intermediate results of a vector reduction.
#' @name standalone-purrr-accumulate
NULL
#' @rdname standalone-purrr-accumulate
#' @export
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
#' @rdname standalone-purrr-accumulate
#' @export
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

#' Find the value or position of the first match.
#' @name standalone-purrr-accumulate
NULL
#' @rdname standalone-purrr-accumulate
#' @export
detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
#' @rdname standalone-purrr-accumulate
#' @export
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
.rlang_purrr_index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}
