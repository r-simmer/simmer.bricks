#' Perform Parallel Tasks
#'
#' This brick encapsulates the activity of \code{n} workers running parallel
#' sub-trajectories.
#'
#' @inheritParams simmer::clone
#' @inheritParams simmer::get_capacity
#' @param ... sub-trajectories or list of sub-trajectories to parallelise.
#' @param wait if \code{TRUE}, the arrival waits until all parallel sub-trajectories
#' are finished; if \code{FALSE}, the arrival continues as soon as the first
#' parallel task ends.
#'
#' @return Returns the following chain of activities: \code{\link[simmer]{clone}}
#' > \code{\link[simmer:clone]{synchronize}} (> \code{\link[simmer:send]{wait}}
#' > \code{\link[simmer:send]{untrap}} if \code{wait=FALSE}) (see examples below).
#' @export
#'
#' @examples
#' env <- simmer()
#' signal <- function() get_name(env)
#'
#' task.1 <- trajectory("task 1") %>%
#'   timeout(function() rexp(1))
#' task.2 <- trajectory("task 2") %>%
#'   timeout(function() rexp(1))
#'
#' ## These are equivalent:
#' trajectory() %>%
#'   do_parallel(
#'     task.1,
#'     task.2,
#'     .env = env, wait = TRUE
#'   )
#'
#' trajectory() %>%
#'   clone(
#'     n = 3,
#'     trajectory("original") %>%
#'       trap(signal) %>%
#'       wait() %>%
#'       wait() %>%
#'       untrap(signal),
#'     task.1[] %>%
#'       send(signal),
#'     task.2[] %>%
#'       send(signal)) %>%
#'   synchronize(wait = TRUE)
#'
#' ## These are equivalent:
#' trajectory() %>%
#'   do_parallel(
#'     task.1,
#'     task.2,
#'     .env = env, wait = FALSE
#'   )
#'
#' trajectory() %>%
#'   clone(
#'     n = 3,
#'     trajectory("original") %>%
#'       trap(signal),
#'     task.1[] %>%
#'       send(signal),
#'     task.2[] %>%
#'       send(signal)) %>%
#'   synchronize(wait = FALSE) %>%
#'   wait() %>%
#'   untrap(signal)
#'
do_parallel <- function(.trj, ..., .env, wait=TRUE, mon_all = FALSE) {
  tasks <- c(...)
  types <- sapply(tasks, inherits, what="trajectory")

  if (!all(types)) {
    dots <- substitute(c(...))[-1]
    args <- paste(sapply(dots, deparse)[!types], collapse="', '")
    stop("Arguments '", args, "' are not 'simmer' trajectories")
  }

  signal <- function() get_name(.env)
  tasks <- lapply(tasks, `[`)
  tasks <- lapply(tasks, send, signals=signal)

  if (wait) {
    original <- trajectory("original") %>%
      wait_until(signal, length(tasks))
    append <- trajectory()
  } else {
    original <- trajectory("original") %>%
      trap(signal)
    append <- trajectory() %>%
      wait() %>%
      untrap(signal)
  }

  do.call(clone, c(.trj, length(tasks)+1, original, tasks)) %>%
    synchronize(wait=wait) %>%
    join(append)
}
