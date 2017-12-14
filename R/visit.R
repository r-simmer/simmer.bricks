#' Visit a Resource
#'
#' These bricks encapsulate a resource visit: seize, spend some time and release.
#'
#' @inheritParams simmer::seize
#' @inheritParams simmer::timeout
#'
#' @return Returns the following chain of activities: \code{\link[simmer]{seize}}
#' > \code{\link[simmer]{timeout}} > \code{\link[simmer:seize]{release}}
#' (see examples below).
#' @export
#'
#' @examples
#' ## These are equivalent:
#' trajectory() %>%
#'   visit("res", 5, 1)
#'
#' trajectory() %>%
#'   seize("res", 1) %>%
#'   timeout(5) %>%
#'   release("res", 1)
#'
visit <- function(.trj, resource, task, amount=1) {
  .trj %>%
    seize(resource, amount) %>%
    timeout(task) %>%
    release(resource, amount)
}

#' @rdname visit
#' @export
#' @examples
#' ## These are equivalent:
#' trajectory() %>%
#'   visit_selected(5, 1)
#'
#' trajectory() %>%
#'   seize_selected(1) %>%
#'   timeout(5) %>%
#'   release_selected(1)
#'
visit_selected <- function(.trj, task, amount=1, id=0) {
  .trj %>%
    seize_selected(amount, id) %>%
    timeout(task) %>%
    release_selected(amount, id)
}
