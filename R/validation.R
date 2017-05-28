
is_between <- function(x, a, b, inclusive = FALSE) {
  lt <- if (inclusive) `<=` else `<`
  stopifnot(lt(a, x) && lt(x, b))
  invisible(TRUE)
}
