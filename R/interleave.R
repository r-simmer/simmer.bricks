#' Interleaved Resources
#'
#' This brick encapsulates a chain of interleaved resources, i.e., the current
#' resource is not released until the next one in the chain is available. An
#' interesting property of such a pattern is that, if one resource is blocked for
#' some reason, the whole chain stops.
#'
#' @param resources character vector of resource names.
#' @inheritParams visit
#'
#' @details Both \code{task} and \code{amount} accept a list of values/functions,
#' instead of a single one, that should be of the same length as \code{resources},
#' so that each value/function is applied to the resource of the same index.
#'
#' The transition to the second and subsequent resources is guarded by a token,
#' an auxiliary resource whose capacity must be equal to the capacity + queue
#' size of the guarded resource, and its queue size must be infinite. For example,
#' if two resources are provided, \code{c("A", "B")}, the auxiliary resource will
#' be named \code{"B_token"}. If \code{capacity=2} and \code{queue_size=1} for B,
#' then \code{capacity=3} and \code{queue_size=Inf} must be the values for
#' B_token. But note that the user is responsible for adding such an auxiliary
#' resource to the simulation environment with the appropriate parameters.
#'
#' @return Returns the following chain of activities: \code{\link[simmer]{seize}}
#' (1) > \code{\link[simmer]{timeout}} > [\code{\link[simmer]{seize}} (token to 2)
#' > \code{\link[simmer:seize]{release}} (1) > \code{\link[simmer]{seize}} (2) >
#' \code{\link[simmer]{timeout}} > \code{\link[simmer:seize]{release}} (2) >
#' \code{\link[simmer:seize]{release}} (token to 2) > ... (repeat) ]
#' (see examples below). Thus, the total number of activities appended is
#' \code{length(resources) * 3 + (length(resources)-1) * 2}.
#' @export
#'
#' @examples
#' ## These are equivalent:
#' trajectory() %>%
#'   interleave(c("A", "B"), c(2, 10), 1)
#'
#' trajectory() %>%
#'   seize("A", 1) %>%
#'   timeout(2) %>%
#'   seize("B_token", 1) %>%
#'   release("A", 1) %>%
#'   seize("B", 1) %>%
#'   timeout(10) %>%
#'   release("B", 1) %>%
#'   release("B_token", 1)
#'
interleave <- function(.trj, resources, task, amount=1) {
  stopifnot(length(amount) == 1 || length(amount) == length(resources))
  stopifnot(length(task) == 1 || length(task) == length(resources))

  if (length(amount) == 1) amount <- rep(amount, length(resources))
  if (length(task) == 1) amount <- rep(task, length(resources))

  for (i in seq_along(resources)) {
    .trj %>%
      seize(resources[[i]], amount[[i]]) %>%
      timeout(task[[i]])

    if (i < length(resources)) .trj %>%
      seize(paste0(resources[[i+1]], "_token"), amount[[i+1]])

    .trj %>%
      release(resources[[i]], amount[[i]])

    if (i > 1) .trj %>%
      release(paste0(resources[[i]], "_token"), amount[[i]])
  }

  .trj
}
