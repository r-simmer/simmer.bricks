context("do_parallel")

test_that("a missing trajectory fails", {
  expect_error(do_parallel(trajectory(), simmer(), NULL))
})

test_that("do_parallel generates the correct sequence of activities", {
  t <- trajectory() %>%
    do_parallel(env, trajectory(), trajectory(), wait=TRUE)

  expect_equal(length(t), 2)
  expect_equal(get_n_activities(t), 8)
  expect_output(print(t[[1]]), paste0(
    "Clone.*3.*",
    "Fork 1.*Trap.*Wait.*Wait.*UnTrap.*",
    "Fork 2.*Send.*Fork 3.*Send"))
  expect_output(print(t[[2]]), "Synchronize.*1")

  t <- trajectory() %>%
    do_parallel(env, trajectory(), trajectory(), wait=FALSE)

  expect_equal(length(t), 4)
  expect_equal(get_n_activities(t), 7)
  expect_output(print(t[[1]]), paste0(
    "Clone.*3.*",
    "Fork 1.*Trap.*",
    "Fork 2.*Send.*Fork 3.*Send"))
  expect_output(print(t[[2]]), "Synchronize.*0")
  expect_output(print(t[[3]]), "Wait")
  expect_output(print(t[[4]]), "UnTrap")
})
