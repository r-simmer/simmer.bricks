#' Wait a Number of Signals
#'
#' These bricks encapsulate \code{n} stops: wait for a sequence of \code{n}
#' signals. \code{wait_until} also traps and untraps the required signals.
#'
#' @param n number of \code{wait} activities to chain.
#' @inheritParams simmer::send
#'
#' @return \code{wait_n} returns \code{n} times \code{\link[simmer:send]{wait}}.
#' \code{wait_until} also adds \code{\link[simmer:send]{trap}} and
#' \code{\link[simmer:send]{untrap}} at the beginning and end, respectively,
#' of the chain of \code{wait}s (see examples below).
#' @export
#'
#' @examples
#' ## These are equivalent:
#' trajectory() %>%
#'   wait_n(3)
#'
#' trajectory() %>%
#'   wait() %>%
#'   wait() %>%
#'   wait()
#'
wait_n <- function(.trj, n=1) {
  if (!n) return(.trj)
  stopifnot(n >= 1)

  .trj %>%
    wait() %>%
    wait_n(n-1)
}

#' @rdname wait_n
#' @export
#'
#' @examples
#' ## These are equivalent:
#' trajectory() %>%
#'   wait_until("green")
#'
#' trajectory() %>%
#'   trap("green") %>%
#'   wait() %>%
#'   untrap("green")
#'
#' ## These are equivalent:
#' trajectory() %>%
#'   wait_until(c("one", "another"), 2)
#'
#' trajectory() %>%
#'   trap(c("one", "another")) %>%
#'   wait() %>%
#'   wait() %>%
#'   untrap(c("one", "another"))
#'
wait_until <- function(.trj, signals, n=1) {
  stopifnot(n >= 1)

  .trj %>%
    trap(signals) %>%
    wait_n(n) %>%
    untrap(signals)
}
