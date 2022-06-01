Minus <- function(f) {
  if (is.numeric(f))
    return(-f)
  f <- match.fun(f)
  function(...) -f(...)
}
