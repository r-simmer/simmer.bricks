#' Wait until a Signal is Received
#'
#' This brick encapsulates a stop: trap one or more signals, wait for them,
#' then untrap the signals and continue the trajectory.
#'
#' @inheritParams simmer::send
#'
#' @return Returns the following chain of activities: \code{\link[simmer:send]{trap}}
#' > \code{\link[simmer:send]{wait}} > \code{\link[simmer:send]{untrap}}
#' (see examples below).
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
wait_until <- function(.trj, signals) {
  .trj %>%
    trap(signals) %>%
    wait() %>%
    untrap(signals)
}
