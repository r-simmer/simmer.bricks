#' Delayed Release of a Resource
#'
#' This brick encapsulates a delayed release: the arrival releases the resource
#' and continues its way immediately, but the resource is busy for an additional
#' period of time.
#'
#' @inheritParams simmer::release
#' @inheritParams simmer::timeout
#' @inheritParams simmer::get_capacity
#' @inheritParams simmer::add_resource
#' @inheritParams simmer::clone
#'
#' @return Returns the following chain of activities: \code{\link[simmer]{clone}}
#' > \code{\link[simmer:clone]{synchronize}} (see examples below).
#' @export
#'
#' @examples
#' ## These are equivalent for a non-preemptive resource:
#' trajectory() %>%
#'   delayed_release("res1", 5, 1)
#'
#' trajectory() %>%
#'   clone(
#'     2,
#'     trajectory() %>%
#'       set_capacity("res1", -1, mod="+") %>%
#'       release("res1", 1),
#'     trajectory() %>%
#'       timeout(5) %>%
#'       set_capacity("res1", 1, mod="+")
#'   ) %>%
#'   synchronize(wait=FALSE)
#'
#' ## These are equivalent for a preemptive resource:
#' trajectory() %>%
#'   delayed_release("res2", 5, 1, preemptive=TRUE)
#'
#' trajectory() %>%
#'   clone(
#'     2,
#'     trajectory() %>%
#'       release("res2", 1),
#'     trajectory() %>%
#'       set_prioritization(c(rep(.Machine$integer.max, 2), 0)) %>%
#'       seize("res2", 1) %>%
#'       timeout(5) %>%
#'       release("res2", 1)
#'   ) %>%
#'   synchronize(wait=FALSE)
#'
delayed_release <- function(.trj, resource, task, amount=1, preemptive=FALSE, mon_all=FALSE) {
  if (!preemptive) {
    .clone <- clone(
      .trj, 2,
      trajectory() %>%
        set_capacity(resource, Minus(amount), mod="+") %>%
        release(resource, amount),
      trajectory() %>%
        timeout(task) %>%
        set_capacity(resource, amount, mod="+")
    )
  } else {
    .clone <- clone(
      .trj, 2,
      trajectory() %>%
        release(resource, amount),
      trajectory() %>%
        set_prioritization(c(rep(.Machine$integer.max, 2), 0)) %>%
        seize(resource, amount) %>%
        timeout(task) %>%
        release(resource, amount)
    )
  }

  .clone %>% synchronize(wait=FALSE, mon_all=mon_all)
}

#' @rdname delayed_release
#' @export
delayed_release_selected <- function(.trj, task, amount=1, preemptive=FALSE, mon_all=FALSE) {
  if (!preemptive) {
    .clone <- clone(
      .trj, 2,
      trajectory() %>%
        set_capacity_selected(Minus(amount), mod="+") %>%
        release_selected(amount),
      trajectory() %>%
        timeout(task) %>%
        set_capacity_selected(amount, mod="+")
    )
  } else {
    .clone <- clone(
      .trj, 2,
      trajectory() %>%
        release_selected(amount),
      trajectory() %>%
        set_prioritization(c(rep(.Machine$integer.max, 2), 0)) %>%
        seize_selected(amount) %>%
        timeout(task) %>%
        release_selected(amount)
    )
  }

  .clone %>% synchronize(wait=FALSE, mon_all=mon_all)
}
